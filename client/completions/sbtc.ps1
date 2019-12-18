$scriptblock = {
  param($commandName, $line, $position)
  $len = $line.ToString().length
  $spaces = " " * ($position - $len)
  $arg="--completions=$line$spaces"
  & 'sbt.client.nativeclient.exe' @($arg)
}
Register-ArgumentCompleter -CommandName sbt.client.nativeclient.exe -ScriptBlock $scriptBlock
