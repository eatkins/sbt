$scriptblock = {
  param($commandName, $line, $position)
  $len = $line.ToString().length
  $spaces = " " * ($position - $len)
  $arg="--completions=$line$spaces"
  & 'sbtc.exe' @($arg)
}
Register-ArgumentCompleter -CommandName sbtc.exe -ScriptBlock $scriptBlock
