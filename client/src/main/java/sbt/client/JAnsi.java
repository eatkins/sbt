package sbt.client;

class JAnsi {
  private static final boolean isWin = System.getProperty("os.name").toLowerCase().startsWith("windows");
  static void install() {
    if (isWin) org.fusesource.jansi.AnsiConsole.systemInstall();
  }
  static void uninstall() {
    if (isWin) org.fusesource.jansi.AnsiConsole.systemUninstall();
  }
}
