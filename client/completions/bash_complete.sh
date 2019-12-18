#!/bin/bash

_do_sbt_completions() {
  COMPREPLY=($(sbt.client.nativeclient "--completions=${COMP_LINE}"))
}

complete -F _do_sbt_completions sbtc
