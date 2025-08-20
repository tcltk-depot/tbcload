# all.tcl for tbcload tests
if {"::tcltest" ni [namespace children]} {
    package require tcltest 2.5
    namespace import -force ::tcltest::*
}

set ::tcltest::testsDirectory [file normalize [file dirname [info script]]]

configure -verbose bps

runAllTests
