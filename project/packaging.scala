import sbt._
import com.typesafe.sbt.packager.Keys._
import sbt.Keys._
import com.typesafe.sbt.SbtNativePackager._

object Packaging {

  val sbtLaunchJarUrl = SettingKey[String]("sbt-launch-jar-url")
  val sbtLaunchJarLocation = SettingKey[File]("sbt-launch-jar-location")  
  val sbtLaunchJar = TaskKey[File]("sbt-launch-jar", "Resolves SBT launch jar")

  val stagingDirectory = SettingKey[File]("staging-directory")
  val stage = TaskKey[File]("stage")

  def localWindowsPattern = "[organisation]/[module]/[revision]/[module].[ext]"

  import util.control.Exception.catching  

  def downloadUrlForVersion(v: String) = (v split "[^\\d]" flatMap (i => catching(classOf[Exception]) opt (i.toInt))) match {
    case Array(0, 11, 3, _*)           => "http://repo.typesafe.com/typesafe/ivy-releases/org.scala-sbt/sbt-launch/0.11.3-2/sbt-launch.jar"
    case Array(0, 11, x, _*) if x >= 3 => "http://repo.typesafe.com/typesafe/ivy-releases/org.scala-sbt/sbt-launch/"+v+"/sbt-launch.jar"
    case Array(0, y, _*) if y >= 12    => "http://repo.typesafe.com/typesafe/ivy-releases/org.scala-sbt/sbt-launch/"+v+"/sbt-launch.jar"
    case _                             => "http://repo.typesafe.com/typesafe/ivy-releases/org.scala-tools.sbt/sbt-launch/"+v+"/sbt-launch.jar"
  }

  val settings: Seq[Setting[_]] = packagerSettings ++ deploymentSettings ++ mapGenericFilesToLinux ++ Seq(
    sbtLaunchJarUrl <<= sbtVersion apply downloadUrlForVersion,
    sbtLaunchJarLocation <<= target apply (_ / "sbt-launch.jar"),
    sbtLaunchJar <<= (sbtLaunchJarUrl, sbtLaunchJarLocation) map { (uri, file) =>
      import dispatch._
      if(!file.exists) {
         // oddly, some places require us to create the file before writing...
         IO.touch(file)
         val writer = new java.io.BufferedOutputStream(new java.io.FileOutputStream(file))
         try Http(url(uri) >>> writer)
         finally writer.close()
      }
      // TODO - GPG Trust validation.
      file
    },
    // GENERAL LINUX PACKAGING STUFFS
    maintainer := "Josh Suereth <joshua.suereth@typesafe.com>",
    packageSummary := "Simple Build Tool for Scala-driven builds",
    packageDescription := """This script provides a native way to run the Simple Build Tool,
  a build tool for Scala software, also called SBT.""",
    // Here we remove the jar file and launch lib from the symlinks:
    linuxPackageSymlinks <<= linuxPackageSymlinks map { links =>
      for { 
        link <- links
        if !(link.destination endsWith "sbt-launch-lib.bash")
        if !(link.destination endsWith "sbt-launch.jar")
      } yield link
    },
    // DEBIAN SPECIFIC    
    name in Debian <<= (sbtVersion) apply { (sv) => "sbt" /* + "-" + (sv split "[^\\d]" take 3 mkString ".")*/ },
    version in Debian <<= (version, sbtVersion) apply { (v, sv) =>
      val nums = (v split "[^\\d]")
      "%s-%s-build-%03d" format (sv, (nums.init mkString "."), nums.last.toInt + 1)
    },
    debianPackageDependencies in Debian ++= Seq("curl", "java2-runtime", "bash (>= 2.05a-11)"),
    debianPackageRecommends in Debian += "git",
    linuxPackageMappings in Debian <+= (sourceDirectory) map { bd =>
      (packageMapping(
        (bd / "debian/changelog") -> "/usr/share/doc/sbt/changelog.gz"
      ) withUser "root" withGroup "root" withPerms "0644" gzipped) asDocs()
    },
    
    // RPM SPECIFIC
    name in Rpm := "sbt",
    version in Rpm <<= sbtVersion apply { sv => (sv split "[^\\d]" filterNot (_.isEmpty) mkString ".") },
    rpmRelease := "1",
    rpmVendor := "typesafe",
    rpmUrl := Some("http://github.com/paulp/sbt-extras"),
    rpmLicense := Some("BSD"),
    
    
    // WINDOWS SPECIFIC
    name in Windows := "sbt",
    candleOptions ++= Seq("-ext", "WixUtilExtension"),
    lightOptions ++= Seq("-ext", "WixUIExtension",
                         "-ext", "WixUtilExtension",
                         "-cultures:en-us"),
    wixConfig <<= (sbtVersion, sourceDirectory in Windows) map makeWindowsXml,
    //wixFile <<= sourceDirectory in Windows map (_ / "sbt.xml"),
    mappings in packageMsi in Windows <+= sbtLaunchJar map { f => f -> "sbt-launch.jar" },
    mappings in packageMsi in Windows <++= sourceDirectory in Windows map { d => Seq(
      (d / "sbt.bat") -> "sbt.bat",
      (d / "sbtconfig.txt") -> "sbtconfig.txt"
    )},
    javacOptions := Seq("-source", "1.5", "-target", "1.5"),

    // Universal ZIP download install.  TODO - Share the above windows code, here....
    name in Universal := "sbt",
    mappings in Universal <+= sbtLaunchJar map { _ -> "bin/sbt-launch.jar" },
    mappings in Universal <+= sourceDirectory in Windows map { d => 
      (d / "sbt.bat") -> "bin/sbt.bat"
    },
    // TODO - Adapt global `sbt`/`sbt-launch-lib` scripts for universal install...
    
    // Misccelaneous publishing stuff...
    projectID in Debian    <<= (organization, sbtVersion) apply { (o,v) => ModuleID(o,"sbt",v) },
    projectID in Windows   <<= (organization, sbtVersion) apply { (o,v) => ModuleID(o,"sbt",v) },
    projectID in Rpm       <<= (organization, sbtVersion) apply { (o,v) => ModuleID(o,"sbt",v) },
    projectID in Universal <<= (organization, sbtVersion) apply { (o,v) => ModuleID(o,"sbt",v) },
    stagingDirectory <<= (target) apply { (t) => t / "stage" },
    stage <<= (stagingDirectory, mappings in Universal) map { (dir, m) =>
      val files = for((file, name) <- m)
                  yield file -> (dir / name)
      IO copy files
      dir
    }
  )
  
  def makeWindowsXml(sbtVersion: String, sourceDir: File): scala.xml.Node = {
    val version = (sbtVersion split "[^\\d]" filterNot (_.isEmpty)) match {
        case Array(major,minor,bugfix, _*) => Seq(major,minor,bugfix, "1") mkString "."
        case Array(major,minor) => Seq(major,minor,"0","1") mkString "."
        case Array(major) => Seq(major,"0","0","1") mkString "."
      }
      (
<Wix xmlns='http://schemas.microsoft.com/wix/2006/wi' 
     xmlns:util='http://schemas.microsoft.com/wix/UtilExtension'>
  <Product Id='ce07be71-510d-414a-92d4-dff47631848a' 
            Name='Simple Build Tool' 
            Language='1033'
            Version={version}
            Manufacturer='Typesafe, Inc.' 
            UpgradeCode='4552fb0e-e257-4dbd-9ecb-dba9dbacf424'>
      <Package Description='Simple Build Tool launcher script.'
                Comments='First attempt to create an SBT windows installer, bear with me.'
                Manufacturer='Typesafe, Inc.' 
                InstallScope='perMachine'
                InstallerVersion='200' 
                Compressed='yes' />
 
      <Media Id='1' Cabinet='sbt.cab' EmbedCab='yes' />
      
      <Property Id='NOTEPADEXE'>
        <DirectorySearch Id='NotePadContainer' Path='[SystemFolder]' Depth='0'>
          <FileSearch Id='NotepadFile' Name='notepad.exe'/>
        </DirectorySearch>
      </Property>
 
      <Directory Id='TARGETDIR' Name='SourceDir'>
        <Directory Id="ProgramMenuFolder">
          <Directory Id="ApplicationProgramsFolder" Name="sbt"/>
        </Directory>
        <Directory Id='ProgramFilesFolder' Name='PFiles'>
            <Directory Id='INSTALLDIR' Name='sbt'>
               <Component Id='SbtLauncherScript' Guid='DE0A5B50-0792-40A9-AEE0-AB97E9F845F5'>
                  <File Id='sbt_bat' Name='sbt.bat' DiskId='1' Source='sbt.bat'>
                     <util:PermissionEx User="Administrators" GenericAll="yes" />
                     <util:PermissionEx User="Users" GenericAll="yes" />
                  </File>
                  <File Id='sbt_sh' Name='sbt' DiskId='1' Source='sbt'>
                     <!-- <util:PermissionEx User="Users" Domain="[LOCAL_MACHINE_NAME]" GenericRead="yes" Read="yes" GenericExecute="yes" ChangePermission="yes"/> -->
                  </File>
               </Component>
               <Component Id='SbtConfigFile' Guid='*'>
                 <File Id='sbtconfig_txt' Name='sbtconfig.txt' DiskId='1' Source='sbtconfig.txt'>
                   <util:PermissionEx User="Administrators" GenericAll="yes" />
                   <util:PermissionEx User="Users" GenericAll="yes" />
                 </File>
               </Component>
               <Component Id='SbtLauncherJar' Guid='*'>
                  <File Id='sbt_launch_jar' Name='sbt-launch.jar' DiskId='1' Source='sbt-launch.jar' />
               </Component>               
               <Component Id='SbtLauncherPath' Guid='17EA4092-3C70-11E1-8CD8-1BB54724019B'>
                  <CreateFolder/>
                  <Environment Id="PATH" Name="PATH" Value="[INSTALLDIR]" Permanent="no" Part="last" Action="set" System="yes" />
                  <Environment Id="SBT_HOME" Name="SBT_HOME" Value="[INSTALLDIR]" Permanent="no" Action="set" System="yes" />
               </Component>
             </Directory>
         </Directory>
      </Directory>
           <!-- Step 2: Add the shortcut to your installer package -->
        <DirectoryRef Id="ApplicationProgramsFolder">
            <Component Id="ConfigShortcut" Guid="3370A26B-E8AB-4143-B837-CE9A8573BF61">
                <Shortcut Id="ConfigStartMenuShortcut"
                          Name="sbt configuration"
                          Description="Modify sbt configuration settings"
                          Target="[INSTALLDIR]sbtconfig.txt"
                          WorkingDirectory="INSTALLDIR"/>
                <RemoveFolder Id="ApplicationProgramsFolder" On="uninstall"/>
                <RegistryValue Root="HKCU" Key="Software\Typesafe\sbt" Name="installed" Type="integer" Value="1" KeyPath="yes"/>
             </Component>
        </DirectoryRef>
        
      <Feature Id='Complete' Title='Simple Build Tool' Description='The windows installation of Simple Build Tool.'
         Display='expand' Level='1' ConfigurableDirectory='INSTALLDIR'>
        <Feature Id='SbtLauncher' Title='Sbt Launcher Script' Description='The application which downloads and launches SBT.' Level='1' Absent='disallow'>
          <ComponentRef Id='SbtLauncherScript'/>
          <ComponentRef Id='SbtLauncherJar' />
          <ComponentRef Id='SbtConfigFile' />
        </Feature>
        <Feature Id='SbtLauncherPathF' Title='Add SBT to windows system PATH' Description='This will append SBT to your windows system path (Requires Restart).' Level='1'>
          <ComponentRef Id='SbtLauncherPath'/>
        </Feature>
        <Feature Id='SbtLauncherConfigF' Title='Add Menu Shortcuts' Description='This will add menu shortcuts for sbt configuration.' Level='1'>
          <ComponentRef Id='ConfigShortcut'/>
        </Feature>
      </Feature>
      <!--<Property Id="JAVAVERSION">
        <RegistrySearch Id="JavaVersion"
                        Root="HKLM"
                        Key="SOFTWARE\Javasoft\Java Runtime Environment"
                        Name="CurrentVersion"
                        Type="raw"/>
      </Property>
      <Condition Message="This application requires a JVM available.  Please install Java, then run this installer again.">
        <![CDATA[Installed OR JAVAVERSION]]>
      </Condition>-->
      <MajorUpgrade 
         AllowDowngrades="no" 
         Schedule="afterInstallInitialize"
         DowngradeErrorMessage="A later version of [ProductName] is already installed.  Setup will no exit."/>  
      <UIRef Id="WixUI_FeatureTree"/>
      <UIRef Id="WixUI_ErrorProgressText"/>
      <Property Id="WIXUI_INSTALLDIR" Value="INSTALLDIR"/>
      <WixVariable Id="WixUILicenseRtf" Value={sourceDir.getAbsolutePath + "\\License.rtf"} />
      
   </Product>
</Wix>
    )
  }
}
