#!/usr/bin/wish
#############################################################################
# Visual Tcl v1.20 Project
#

#################################
# GLOBAL VARIABLES
#
global barbscale; 
global barbscaleval; 
global check1; 
global checkbutton2; 
global checkbutton7; 
global checkbutton9; 
global chedk2; 
global colorbar; 
global contincval; 
global contmax; 
global contmaxval; 
global contminval; 
global contype1; 
global dateval; 
global filled_bar; 
global grid; 
global ig; 
global line_bar; 
global m1mode; 
global m3mode; 
global overincval; 
global overmaxval; 
global overminval; 
global overtype1; 
global overvartypeval; 
global overvarval; 
global plot_title; 
global result; 
global slablabval; 
global slabmode; 
global slabscaleval; 
global slabtype; 
global slabval; 
global statmaxval; 
global statmeanval; 
global statminval; 
global tile_bar; 
global timeslideval; 
global timeval; 
global tmplt; 
global vallabval; 
global varnone; 
global varnoneval; 
global vart; 
global vartype; 
global vartypeval; 
global varval; 
global windintvar; 
global winds; 
global windvar; 
global wint; 
global widget; 

#################################
# USER DEFINED PROCEDURES
#
proc init {argc argv} {
global {barbscale}
  set {barbscale} {20-4-2}
  global {barbscaleval}
  set {barbscaleval} {20-4-2}
  global {check1}
  set {check1} {0}
  global {checkbutton2}
  set {checkbutton2} {b}
  global {checkbutton7}
  set {checkbutton7} {0}
  global {checkbutton9}
  set {checkbutton9} {0}
  global {chedk2}
  set {chedk2} {0}
  global {colorbar}
  set {colorbar} {0}
  global {contincval}
  set {contincval} {0.000000}
  global {contmax}
  set {contmax} {0.000000}
  global {contmaxval}
  set {contmaxval} {0.000000}
  global {contminval}
  set {contminval} {0.000000}
  global {contype1}
  set {contype1} {cont_line}
  global {dateval}
  set {dateval} {Jan 01, 1900}
  global {filled_bar}
  set {filled_bar} {b}
  global {fsBox}
  global {grid}
  set {grid} {Grid 1}
  global {line_bar}
  set {line_bar} {b}
  global {overincval}
  set {overincval} {0.000000}
  global {overmaxval}
  set {overmaxval} {0.000000}
  global {overminval}
  set {overminval} {0.000000}
  global {overtype1}
  set {overtype1} {cont_line}
  global {overvartypeval}
  set {overvartypeval} {3-D}
  global {overvarval}
  set {overvarval} {No Variables}
  global {plot_title}
  set {plot_title} {}
  global {result}
  set {result} {}
  global {slablabval}
  set {slablabval} {Height}
  global {slabscaleval}
  set {slabscaleval} {2}
  global {slabtype}
  set {slabtype} {1}
  global {slabval}
  set {slabval} {}
  global {statmaxval}
  set {statmaxval} {}
  global {statmeanval}
  set {statmeanval} {}
  global {statminval}
  set {statminval} {}
  global {tile_bar}
  set {tile_bar} {b}
  global {timeslideval}
  set {timeslideval} {20}
  global {timeval}
  set {timeval} {00:00:00 GMT}
  global {tmplt}
  set {tmplt} {0}
  global {vallabval}
  set {vallabval} {}
  global {varnone}
  set {varnone} {0}
  global {varnoneval}
  set {varnoneval} {0}
  global {vart}
  set {vart} {1}
  global {vartype}
  set {vartype} {3D}
  global {vartypeval}
  set {vartypeval} {3-D}
  global {varval}
  set {varval} {No Variables}
  global {windintvar}
  set {windintvar} {2}
  global {winds}
  set {winds} {}
  global {windvar}
  set {windvar} {None}
  global {wint}
  set {wint} {menubutton50}

  global ig
  global m1mode
  global m3mode
  global slabmode
  
  set m1mode none 
  set m3mode none 
  set ig 1
  set slabmode 1
}

init $argc $argv


proc {contCB} {arg1 arg2} {
puts "contCB: arg1=$arg1 arg2=$arg2"
}

proc {cleanupCB} {} {
puts "cleanupCB:"
}

proc {fileload} {} {
global fsbox
  set fname [tk_getOpenFile -filetypes { { {RAMS header files} .txt} {{All Files} *}}]
  loadCB $fname
}

proc {gridCB} {args} {
puts "gridCB $args"
}

proc {help_info} {} {
tk_dialog .dia1 "Ringi Info" {Ringi -- with Tcl/Tk.  Version 1.01}  info 0 Dismiss
}

proc {loadCB} {arg1} {
puts "loadCB: arg1=$arg1"
}

proc {overvarCB} {arg1} {
puts "overvarCB: arg1=$arg1"
}

proc {plotCB} {} {
puts "plotCB"
}

proc {saveCB} {} {
puts "saveCB"
}

proc {show_cont_set} {arg1} {
puts "show_cont_set: arg1=$arg1"
}

proc {slabCB} {args} {
puts "slabCB $args"
}

proc {stenable} {wid} {
set path .top.slabframe
  foreach wname {slabtype1 slabtype2 slabtype3 slabtype4 slabtype5} {
    set wcmp "$path.$wname"
    if {[string compare $wid $wcmp] == 0 } {
      $wid config -state disabled
    } else {
      $wcmp config -state normal
    }
  }
}

proc {tgks} {args} {
puts "Tcl tgks: $args"
}

proc {tgks_init} {win} {
global ig

  if {$ig} {
    tgks init $win 
    tgks m1cmd "gridCB Zoom"
    set ig 0
  } else {
    puts "Already initialized tgks"
  }
}

proc {timeCB} {arg1} {
puts "timeCB: arg1=$arg1"
}

proc {varCB} {arg1} {
puts "varCB arg1=$arg1"
}

proc {main} {argc argv} {

}

proc {Window} {args} {
global vTcl
    set cmd [lindex $args 0]
    set name [lindex $args 1]
    set newname [lindex $args 2]
    set rest [lrange $args 3 end]
    if {$name == "" || $cmd == ""} {return}
    if {$newname == ""} {
        set newname $name
    }
    set exists [winfo exists $newname]
    switch $cmd {
        show {
            if {$exists == "1" && $name != "."} {wm deiconify $name; return}
            if {[info procs vTclWindow(pre)$name] != ""} {
                eval "vTclWindow(pre)$name $newname $rest"
            }
            if {[info procs vTclWindow$name] != ""} {
                eval "vTclWindow$name $newname $rest"
            }
            if {[info procs vTclWindow(post)$name] != ""} {
                eval "vTclWindow(post)$name $newname $rest"
            }
        }
        hide    { if $exists {wm withdraw $newname; return} }
        iconify { if $exists {wm iconify $newname; return} }
        destroy { if $exists {destroy $newname; return} }
    }
}

#################################
# VTCL GENERATED GUI PROCEDURES
#

proc vTclWindow. {base} {
    if {$base == ""} {
        set base .
    }
    ###################
    # CREATING WIDGETS
    ###################
    wm focusmodel $base passive
    wm geometry $base 200x200+0+0
    wm maxsize $base 1265 994
    wm minsize $base 1 1
    wm overrideredirect $base 0
    wm resizable $base 1 1
    wm withdraw $base
    wm title $base "vt.tcl"
    ###################
    # SETTING GEOMETRY
    ###################
}

proc vTclWindow.barb_top {base} {
    if {$base == ""} {
        set base .barb_top
    }
    if {[winfo exists $base]} {
        wm deiconify $base; return
    }
    ###################
    # CREATING WIDGETS
    ###################
    toplevel $base -class Toplevel  -background slategray4 -relief raised 
    wm focusmodel $base passive
    wm geometry $base 300x300+832+262
    wm maxsize $base 300 300
    wm minsize $base 300 300
    wm overrideredirect $base 0
    wm resizable $base 1 1
    wm sizefrom $base program
    wm title $base "Windbarb Settings"
    frame $base.wind_frame  -background slategray3 -borderwidth 4 -height 124 -relief raised  -width 8 
    menubutton $base.wind_frame.windintmenu  -activebackground White -activeforeground black  -background slategray1 -disabledforeground #3c3cbc -foreground black  -highlightbackground slategray4 -highlightcolor black -indicatoron 1  -menu .barb_top.wind_frame.windintmenu.m -padx 5 -pady 4  -relief raised -text 2 -textvariable windintvar -width 3 
    menu $base.wind_frame.windintmenu.m  -activebackground White -activeforeground black  -background slategray4 -cursor {} -disabledforeground #3c3cbc  -foreground black -tearoff 0 
    $base.wind_frame.windintmenu.m add command  -command {set windintvar 1} -label 1 
    $base.wind_frame.windintmenu.m add command  -command {set windintvar 2} -label 2 
    $base.wind_frame.windintmenu.m add command  -command {set windintvar 3} -label 3 
    $base.wind_frame.windintmenu.m add command  -command {set windintvar 4} -label 4 
    $base.wind_frame.windintmenu.m add command  -command {set windintvar 5} -label 5 
    label $base.wind_frame.label49  -background slategray1  -font -*-helvetica-bold-r-normal-*-10-*-*-*-p-*-iso8859-1  -foreground black -highlightbackground slategray4  -highlightcolor black -relief groove -text {Wind Interval} 
    menubutton $base.wind_frame.barbscale  -activebackground White -activeforeground black  -background slategray1 -disabledforeground #3c3cbc -foreground black  -highlightbackground slategray4 -highlightcolor black -indicatoron 1  -menu .barb_top.wind_frame.barbscale.m -padx 5 -pady 4 -relief raised  -text 20-4-2 -textvariable barbscaleval -width 7 
    menu $base.wind_frame.barbscale.m  -activebackground White -activeforeground black  -background slategray4 -cursor {} -disabledforeground #3c3cbc  -foreground black -tearoff 0 
    $base.wind_frame.barbscale.m add command  -command {set barbscaleval {50-10-5}} -label 50-10-5 
    $base.wind_frame.barbscale.m add command  -command {set barbscaleval {20-4-2}} -label 20-4-2 
    $base.wind_frame.barbscale.m add command  -command {set barbscaleval {10-2-1}} -label 10-2-1 
    label $base.wind_frame.label52  -background slategray1  -font -*-helvetica-bold-r-normal-*-10-*-*-*-p-*-iso8859-1  -foreground black -highlightbackground slategray4  -highlightcolor black -relief groove -text {Barb scale} 
    label $base.wind_frame.label6  -background slategray1  -font -Adobe-Helvetica-Bold-R-Normal--*-140-*-*-*-*-*-* -relief ridge  -text {Wind depiction settings} 
    button $base.done_butt  -background slategray1 -command {wm withdraw .barb_top}  -font -Adobe-Helvetica-Bold-R-Normal--*-140-*-*-*-*-*-* -padx 11  -pady 4 -text Done 
    button $base.save_butt  -background slategray1  -font -Adobe-Helvetica-Bold-R-Normal--*-140-*-*-*-*-*-* -padx 11  -pady 4 -text Save 
    ###################
    # SETTING GEOMETRY
    ###################
    place $base.wind_frame  -x 20 -y 30 -width 245 -height 220 -anchor nw 
    place $base.wind_frame.windintmenu  -x 5 -y 75 -anchor nw 
    place $base.wind_frame.label49  -x 5 -y 55 -anchor nw 
    place $base.wind_frame.barbscale  -x 110 -y 75 -anchor nw 
    place $base.wind_frame.label52  -x 110 -y 55 -anchor nw 
    place $base.wind_frame.label6  -x 0 -y 0 -anchor nw 
    place $base.done_butt  -x 190 -y 260 -anchor nw 
    place $base.save_butt  -x 110 -y 260 -anchor nw
}

proc vTclWindow.filled_top {base} {
    if {$base == ""} {
        set base .filled_top
    }
    if {[winfo exists $base]} {
        wm deiconify $base; return
    }
    ###################
    # CREATING WIDGETS
    ###################
    toplevel $base -class Toplevel  -background slategray4 -relief raised 
    wm focusmodel $base passive
    wm geometry $base 300x300+899+315
    wm maxsize $base 300 300
    wm minsize $base 300 300
    wm overrideredirect $base 0
    wm resizable $base 1 1
    wm sizefrom $base program
    wm title $base "Filled Contour Settings"
    frame $base.frame1  -background slategray3 -borderwidth 2 -height 67 -relief raised  -width 76 
    checkbutton $base.frame1.colorbar  -background slategray1 -offvalue n -onvalue b -text {Plot colorbar}  -variable filled_bar 
    button $base.done_butt  -background slategray1 -command {wm withdraw .filled_top}  -font -Adobe-Helvetica-Bold-R-Normal--*-140-*-*-*-*-*-* -padx 11  -pady 4 -text Done 
    button $base.save_butt  -background slategray1  -font -Adobe-Helvetica-Bold-R-Normal--*-140-*-*-*-*-*-* -padx 11  -pady 4 -text Save 
    ###################
    # SETTING GEOMETRY
    ###################
    place $base.frame1  -x 15 -y 20 -width 270 -height 200 -anchor nw 
    place $base.frame1.colorbar  -x 25 -y 40 -anchor nw 
    place $base.done_butt  -x 195 -y 225 -anchor nw 
    place $base.save_butt  -x 115 -y 225 -anchor nw
}

proc vTclWindow.general_top {base} {
    if {$base == ""} {
        set base .general_top
    }
    if {[winfo exists $base]} {
        wm deiconify $base; return
    }
    ###################
    # CREATING WIDGETS
    ###################
    toplevel $base -class Toplevel  -background slategray4 -relief raised 
    wm focusmodel $base passive
    wm geometry $base 300x300+900+346
    wm maxsize $base 300 300
    wm minsize $base 300 300
    wm overrideredirect $base 0
    wm resizable $base 1 1
    wm sizefrom $base program
    wm title $base "General Settings"
    frame $base.frame1  -background slategray3 -borderwidth 2 -height 67 -relief raised  -width 76 
    label $base.frame1.label0  -background slategray1  -font -Adobe-Helvetica-Bold-R-Normal--*-140-*-*-*-*-*-* -relief ridge  -text {General Options} 
    label $base.frame1.label1  -background slategray1 -foreground black -relief ridge  -text {Title : } 
    entry $base.frame1.title_entry  -background white -textvariable plot_title -width 35 
    button $base.done_butt  -background slategray1 -command {wm withdraw .general_top}  -font -Adobe-Helvetica-Bold-R-Normal--*-140-*-*-*-*-*-* -padx 11  -pady 4 -text Done 
    button $base.save_butt  -background slategray1  -font -Adobe-Helvetica-Bold-R-Normal--*-140-*-*-*-*-*-* -padx 11  -pady 4 -text Save 
    ###################
    # SETTING GEOMETRY
    ###################
    place $base.frame1  -x 15 -y 20 -width 270 -height 200 -anchor nw 
    place $base.frame1.label0  -x 0 -y 0 -anchor nw 
    place $base.frame1.label1  -x 5 -y 45 -anchor nw 
    place $base.frame1.title_entry  -x 5 -y 70 -anchor nw 
    place $base.done_butt  -x 195 -y 225 -anchor nw 
    place $base.save_butt  -x 115 -y 225 -anchor nw
}

proc vTclWindow.line_top {base} {
    if {$base == ""} {
        set base .line_top
    }
    if {[winfo exists $base]} {
        wm deiconify $base; return
    }
    ###################
    # CREATING WIDGETS
    ###################
    toplevel $base -class Toplevel  -background slategray4 -relief raised 
    wm focusmodel $base passive
    wm geometry $base 300x300+903+274
    wm maxsize $base 300 300
    wm minsize $base 300 300
    wm overrideredirect $base 0
    wm resizable $base 1 1
    wm sizefrom $base program
    wm title $base "Line Settings"
    frame $base.frame1  -background slategray3 -borderwidth 2 -height 67 -relief raised  -width 76 
    checkbutton $base.frame1.colorbar  -background slategray1 -offvalue n -onvalue b -text {Plot colorbar}  -variable line_bar 
    button $base.done_butt  -background slategray1 -command {wm withdraw .line_top}  -font -Adobe-Helvetica-Bold-R-Normal--*-140-*-*-*-*-*-* -padx 11  -pady 4 -text Done 
    button $base.save_butt  -background slategray1  -font -Adobe-Helvetica-Bold-R-Normal--*-140-*-*-*-*-*-* -padx 11  -pady 4 -text Save 
    ###################
    # SETTING GEOMETRY
    ###################
    place $base.frame1  -x 15 -y 15 -width 270 -height 200 -anchor nw 
    place $base.frame1.colorbar  -x 25 -y 40 -anchor nw 
    place $base.done_butt  -x 195 -y 225 -anchor nw 
    place $base.save_butt  -x 115 -y 225 -anchor nw
}

proc vTclWindow.stat_top {base} {
    if {$base == ""} {
        set base .stat_top
    }
    if {[winfo exists $base]} {
        wm deiconify $base; return
    }
    ###################
    # CREATING WIDGETS
    ###################
    toplevel $base -class Toplevel  -background slategray4 -relief raised 
    wm focusmodel $base passive
    wm geometry $base 300x180+804+199
    wm maxsize $base 300 180
    wm minsize $base 300 180
    wm overrideredirect $base 0
    wm resizable $base 1 1
    wm sizefrom $base program
    wm title $base "Slab Statistics"
    frame $base.statframe  -background slategray3 -borderwidth 4 -height 20  -highlightbackground slategray4 -highlightcolor black -relief raised  -width 20 
    label $base.statframe.label26  -background slategray1  -font -Adobe-Helvetica-Bold-R-Normal--*-140-*-*-*-*-*-*  -foreground black -highlightbackground slategray4  -highlightcolor black -relief ridge -text {Slab Statistics} 
    label $base.statframe.label27  -background slategray1 -foreground black  -highlightbackground slategray4 -highlightcolor black -relief ridge  -text Minimum -width 9 
    label $base.statframe.label28  -background slategray1 -foreground black  -highlightbackground slategray4 -highlightcolor black -relief ridge  -text Maximum -width 9 
    label $base.statframe.label29  -background slategray1 -foreground black  -highlightbackground slategray4 -highlightcolor black -relief ridge  -text Mean -width 9 
    label $base.statframe.slabmin  -background white -foreground black -highlightbackground slategray4  -highlightcolor black -relief sunken -textvariable statminval  -width 15 
    label $base.statframe.slabmax  -background white -foreground black -highlightbackground slategray4  -highlightcolor black -relief sunken -textvariable statmaxval  -width 15 
    label $base.statframe.slabmean  -background white -foreground black -highlightbackground slategray4  -highlightcolor black -relief sunken -textvariable statmeanval  -width 15 
    button $base.ok_butt  -background slategray1 -command {wm withdraw .stat_top} -padx 11  -pady 4 -text OK 
    ###################
    # SETTING GEOMETRY
    ###################
    place $base.statframe  -x 20 -y 20 -width 240 -height 100 -anchor nw 
    place $base.statframe.label26  -x 0 -y 0 -anchor nw 
    place $base.statframe.label27  -x 15 -y 25 -anchor nw 
    place $base.statframe.label28  -x 15 -y 45 -anchor nw 
    place $base.statframe.label29  -x 15 -y 65 -height 20 -anchor nw 
    place $base.statframe.slabmin  -x 85 -y 25 -anchor nw 
    place $base.statframe.slabmax  -x 85 -y 45 -anchor nw 
    place $base.statframe.slabmean  -x 85 -y 65 -anchor nw 
    place $base.ok_butt  -x 115 -y 125 -anchor nw
}

proc vTclWindow.tile_top {base} {
    if {$base == ""} {
        set base .tile_top
    }
    if {[winfo exists $base]} {
        wm deiconify $base; return
    }
    ###################
    # CREATING WIDGETS
    ###################
    toplevel $base -class Toplevel  -background slategray4 -relief raised 
    wm focusmodel $base passive
    wm geometry $base 300x300+840+273
    wm maxsize $base 300 300
    wm minsize $base 300 300
    wm overrideredirect $base 0
    wm resizable $base 1 1
    wm sizefrom $base program
    wm title $base "Tile Settings"
    frame $base.frame1  -background slategray3 -borderwidth 2 -height 67 -relief raised  -width 76 
    checkbutton $base.frame1.colorbar  -background slategray1 -offvalue n -onvalue b -text {Plot colorbar} -variable tile_bar 
    button $base.done_butt  -background slategray1 -command {wm withdraw .tile_top}  -font -Adobe-Helvetica-Bold-R-Normal--*-140-*-*-*-*-*-* -padx 11  -pady 4 -text Done 
    button $base.save_butt  -background slategray1  -font -Adobe-Helvetica-Bold-R-Normal--*-140-*-*-*-*-*-* -padx 11  -pady 4 -text Save 
    ###################
    # SETTING GEOMETRY
    ###################
    place $base.frame1  -x 15 -y 15 -width 270 -height 200 -anchor nw 
    place $base.frame1.colorbar  -x 25 -y 40 -anchor nw 
    place $base.done_butt  -x 195 -y 225 -anchor nw 
    place $base.save_butt  -x 115 -y 225 -anchor nw
}

proc vTclWindow.top {base} {
    if {$base == ""} {
        set base .top
    }
    if {[winfo exists $base]} {
        wm deiconify $base; return
    }
    ###################
    # CREATING WIDGETS
    ###################
    toplevel $base -class Toplevel \
        -background slategray4 -relief raised 
    wm focusmodel $base passive
    wm geometry $base 260x950+999+25
    wm iconname $base Ringi
    wm maxsize $base 260 950
    wm minsize $base 260 950
    wm overrideredirect $base 0
    wm resizable $base 1 1
    wm sizefrom $base program
    wm deiconify $base
    wm title $base "RAMS Interactive NCAR Graphics Interface"
    frame $base.frame0 \
        -background slategray1 -borderwidth 2 -highlightbackground slategray4 \
        -highlightcolor black -relief raised 
    menubutton $base.frame0.menubutton1 \
        -activebackground white -activeforeground black \
        -background slategray1 -disabledforeground #3c3cbc \
        -font -Adobe-Helvetica-Bold-R-Normal--*-140-*-*-*-*-*-* \
        -foreground black -highlightbackground slategray4 \
        -highlightcolor black -menu .top.frame0.menubutton1.m -padx 5 -pady 4 \
        -text File -underline 0 
    menu $base.frame0.menubutton1.m \
        -activebackground white -activeforeground black -background khaki \
        -cursor {} -disabledforeground #3c3cbc \
        -font -Adobe-Helvetica-Bold-R-Normal--*-140-*-*-*-*-*-* \
        -foreground black -tearoff 0 
    $base.frame0.menubutton1.m add command \
        -command fileload -label Load... 
    $base.frame0.menubutton1.m add command \
        -command saveCB -label {Save current frame} 
    $base.frame0.menubutton1.m add command \
        -command {cleanupCB; destroy .top;exit 0} -label Quit 
    menubutton $base.frame0.menubutton2 \
        -activebackground white -activeforeground black \
        -background slategray1 -disabledforeground #3c3cbc \
        -font -Adobe-Helvetica-Bold-R-Normal--*-140-*-*-*-*-*-* \
        -foreground black -highlightbackground slategray4 \
        -highlightcolor black -menu .top.frame0.menubutton2.m -padx 5 -pady 4 \
        -text Help -underline 0 
    menu $base.frame0.menubutton2.m \
        -activebackground white -activeforeground black -background cyan \
        -disabledforeground #3c3cbc \
        -font -Adobe-Helvetica-Bold-R-Normal--*-140-*-*-*-*-*-* \
        -foreground black -tearoff 0 
    $base.frame0.menubutton2.m add command \
        -command help_info -label Info... 
    menubutton $base.frame0.mbutt_opt \
        -background slategray1 \
        -font -Adobe-Helvetica-Bold-R-Normal--*-140-*-*-*-*-*-* \
        -menu .top.frame0.mbutt_opt.m -padx 5 -pady 4 -text Options \
        -underline 0 
    menu $base.frame0.mbutt_opt.m \
        -activebackground white -background khaki -cursor {} \
        -font -Adobe-Helvetica-Bold-R-Normal--*-140-*-*-*-*-*-* -tearoff 0 
    $base.frame0.mbutt_opt.m add command \
        -command {Window show .general_top} -label {General ...} 
    $base.frame0.mbutt_opt.m add command \
        -command {Window show .line_top} -label {Line contours ...} 
    $base.frame0.mbutt_opt.m add command \
        -command {Window show .filled_top} -label {Filled contours ...} 
    $base.frame0.mbutt_opt.m add command \
        -command {Window show .tile_top} -label {Tiles ...} 
    $base.frame0.mbutt_opt.m add command \
        -command {Window show .vector_top} -label {Wind vectors ...} 
    $base.frame0.mbutt_opt.m add command \
        -command {Window show .barb_top} -label {Wind barbs ...} 
    $base.frame0.mbutt_opt.m add command \
        -command {Window show .value_top} -label {Get Values} 
    menubutton $base.frame0.mbutt_tools \
        -background slategray1 \
        -font -Adobe-Helvetica-Bold-R-Normal--*-140-*-*-*-*-*-* \
        -menu .top.frame0.mbutt_tools.m -padx 5 -pady 4 -text Tools \
        -underline 0 
    menu $base.frame0.mbutt_tools.m \
        -activebackground white -background khaki \
        -font -Adobe-Helvetica-Bold-R-Normal--*-140-*-*-*-*-*-* -tearoff 0 
    $base.frame0.mbutt_tools.m add command \
        -command {Window show .stat_top} -label {Slab statistics ...} 
    frame $base.timeframe \
        -background slategray3 -borderwidth 4 -height 57 \
        -highlightbackground slategray4 -highlightcolor black -relief raised \
        -width 29 
    entry $base.timeframe.date \
        -background white -foreground black -highlightbackground slategray4 \
        -highlightcolor black -insertbackground black \
        -selectbackground #4848e1 -selectforeground black \
        -textvariable dateval -width 16 
    entry $base.timeframe.time \
        -background white -foreground black -highlightbackground slategray4 \
        -highlightcolor black -insertbackground black \
        -selectbackground #4848e1 -selectforeground black \
        -textvariable timeval -width 16 
    label $base.timeframe.label28 \
        -background slategray4 -borderwidth 3 \
        -font -Adobe-Helvetica-Bold-R-Normal--*-180-*-*-*-*-*-* \
        -foreground white -highlightbackground slategray4 \
        -highlightcolor black -relief ridge -text Time 
    scale $base.timeframe.timeslide \
        -activebackground white -background slategray1 -command timeCB \
        -foreground black -highlightbackground slategray4 -length 0 \
        -orient horizontal -showvalue 0 -to 20.0 -troughcolor slategray4 \
        -variable timeslideval -width 13 
    frame $base.slabframe \
        -background slategray3 -borderwidth 4 -height 114 \
        -highlightbackground slategray4 -highlightcolor black -relief raised \
        -width 76 
    radiobutton $base.slabframe.slabtype1 \
        -activebackground white -activeforeground black \
        -background slategray1 \
        -command {if {$slabmode != 2 && $slabmode!=3} {
  tgks m1cmd "slabCB Type \"X-Y Sigma-z\""
  tgks m1mode h2
} else {
  slabCB Type "X-Y Sigma-z"
} 
stenable .top.slabframe.slabtype1
set slabmode 1} \
        -disabledforeground slategray2 -foreground black \
        -highlightbackground slategray4 -highlightcolor black -state disabled \
        -text {X-Y Sigma-z} -value 1 -variable slabtype -width 12 
    radiobutton $base.slabframe.slabtype2 \
        -activebackground white -activeforeground black \
        -background slategray1 \
        -command {if {$slabmode!=1 && $slabmode!=3} {
  tgks m1cmd "slabCB Type \"X-Y Cartesian\""
  tgks m1mode h2
} else {
  slabCB Type "X-Y Cartesian"
} 
stenable .top.slabframe.slabtype2
set slabmode 2} \
        -disabledforeground slategray2 -foreground black \
        -highlightbackground slategray4 -highlightcolor black \
        -text {X-Y Cartesian} -value 2 -variable slabtype -width 12 
    radiobutton $base.slabframe.slabtype3 \
        -activebackground white -activeforeground black \
        -background slategray1 \
        -command {tgks m1cmd "slabCB Type \"X-Z\""
if {$slabmode < 4} {
  tgks m1mode h2
} else {
  tgks m1mode v2
}
set slabmode 4
stenable .top.slabframe.slabtype3} \
        -disabledforeground slategray2 -foreground black \
        -highlightbackground slategray4 -highlightcolor black -text X-Z \
        -value 3 -variable slabtype 
    radiobutton $base.slabframe.slabtype4 \
        -activebackground white -activeforeground black \
        -background slategray1 \
        -command {tgks m1cmd "slabCB Type \"Y-Z\""
tgks m1mode v2
set slabmode 5
stenable .top.slabframe.slabtype4
} \
        -disabledforeground slategray2 -foreground black \
        -highlightbackground slategray4 -highlightcolor black -text Y-Z \
        -value 4 -variable slabtype 
    frame $base.slabframe.frame25 \
        -background slategray2 -borderwidth 4 -height 30 \
        -highlightbackground slategray4 -highlightcolor black -relief groove \
        -width 30 
    scale $base.slabframe.frame25.slaboffscale \
        -activebackground white -background slategray1 \
        -command {slabCB slaboffscale} -foreground black -from 1.0 \
        -highlightbackground slategray4 -orient horizontal -relief raised \
        -troughcolor slategray4 -variable slabscaleval 
    label $base.slabframe.frame25.slablab \
        -background slategray1 -foreground black \
        -highlightbackground slategray4 -highlightcolor black -justify left \
        -relief ridge -text Height -textvariable slablabval -width 10 
    label $base.slabframe.frame25.vallab \
        -background white -foreground black -highlightbackground slategray4 \
        -highlightcolor black -relief sunken -textvariable vallabval \
        -width 12 
    label $base.slabframe.label29 \
        -background slategray4 -borderwidth 3 \
        -font -Adobe-Helvetica-Bold-R-Normal--*-180-*-*-*-*-*-* \
        -foreground white -highlightbackground slategray4 \
        -highlightcolor black -relief ridge -text Slab 
    radiobutton $base.slabframe.slabtype5 \
        -activebackground White -activeforeground black \
        -background slategray1 \
        -command {if {$slabmode!=1 && $slabmode!=2} {
  tgks m1cmd "slabCB Type \"X-Y Pressure\""
  tgks m1mode h2
} else {
  slabCB Type "X-Y Pressure"
} 
stenable .top.slabframe.slabtype5
set slabmode 3} \
        -disabledforeground slategray2 -foreground black \
        -highlightbackground slategray4 -highlightcolor black \
        -text {X-Y Pressure} -value 5 -variable slabtype -width 12 
    frame $base.gridframe \
        -background slategray3 -borderwidth 4 -height 76 \
        -highlightbackground slategray4 -highlightcolor black -relief raised \
        -width 86 
    label $base.gridframe.label32 \
        -background slategray4 -borderwidth 3 \
        -font -Adobe-Helvetica-Bold-R-Normal--*-180-*-*-*-*-*-* \
        -foreground white -highlightbackground slategray4 \
        -highlightcolor black -relief ridge -text Grid 
    menubutton $base.gridframe.grid \
        -background slategray1 -borderwidth 3 -disabledforeground #3c3cbc \
        -font {Helvetica -14 bold} -highlightbackground slategray4 \
        -indicatoron 1 -menu .top.gridframe.grid.m -padx 4 -pady 3 \
        -relief raised -text {Grid 1} -textvariable grid 
    menu $base.gridframe.grid.m \
        -cursor {} -tearoff 0 
    $base.gridframe.grid.m add command \
        -command {set grid {Grid 1}; gridCB $grid} -label {Grid 1} 
    $base.gridframe.grid.m add command \
        -command {set grid {Grid 2}; gridCB $grid} -label {Grid 2} 
    frame $base.parframe \
        -background slategray3 -borderwidth 4 -height 48 \
        -highlightbackground slategray4 -highlightcolor black -relief raised \
        -width 29 
    frame $base.parframe.windframe \
        -background black -borderwidth 3 -height 76 \
        -highlightbackground slategray4 -highlightcolor black -relief raised \
        -width 95 
    label $base.parframe.windframe.label46 \
        -background white -borderwidth 3 \
        -font -*-helvetica-bold-r-normal-*-14-*-*-*-p-*-iso8859-1 \
        -foreground black -highlightbackground slategray4 \
        -highlightcolor black -relief ridge -text WInds 
    menubutton $base.parframe.windframe.windmenu \
        -background slategray1 -highlightbackground slategray4 -indicatoron 1 \
        -menu .top.parframe.windframe.windmenu.m -padx 4 -pady 3 \
        -relief raised -text None -textvariable windvar -width 12 
    menu $base.parframe.windframe.windmenu.m \
        -cursor {} -tearoff 0 
    $base.parframe.windframe.windmenu.m add command \
        -command {set windvar {None}} -label None 
    $base.parframe.windframe.windmenu.m add command \
        -command {set windvar {Barbs}} -label Barbs 
    $base.parframe.windframe.windmenu.m add command \
        -command {set windvar {Vectors}} -label Vectors 
    $base.parframe.windframe.windmenu.m add command \
        -command {set windvar {Streamlines}} -label Streamlines 
    label $base.parframe.label1 \
        -background slategray4 -borderwidth 3 \
        -font -Adobe-Helvetica-Bold-R-Normal--*-180-*-*-*-*-*-* \
        -foreground white -highlightbackground slategray4 \
        -highlightcolor black -relief ridge -text Parameters 
    frame $base.parframe.frame4 \
        -background black -borderwidth 3 -highlightbackground slategray4 \
        -highlightcolor black -relief raised 
    menubutton $base.parframe.frame4.var \
        -activebackground White -activeforeground black \
        -background slategray1 -disabledforeground #3c3cbc -foreground black \
        -highlightbackground slategray4 -highlightcolor black -indicatoron 1 \
        -menu .top.parframe.frame4.var.m -padx 5 -pady 4 -relief raised \
        -text {No Variables} -textvariable varval -width 12 
    menu $base.parframe.frame4.var.m \
        -activebackground White -activeforeground black -background khaki \
        -cursor {} -disabledforeground #3c3cbc -foreground black -tearoff 0 
    label $base.parframe.frame4.label2 \
        -background white -borderwidth 3 \
        -font -Adobe-Helvetica-Bold-R-Normal--*-140-*-*-*-*-*-* \
        -foreground black -highlightbackground slategray4 \
        -highlightcolor black -relief ridge -text {Primary Field} 
    radiobutton $base.parframe.frame4.rb_filled \
        -background slategray1 -text Filled -value filled -variable contype1 
    radiobutton $base.parframe.frame4.rb_tile \
        -background slategray1 -text Tile -value tile -variable contype1 
    radiobutton $base.parframe.frame4.rb_line \
        -background slategray1 -text Line -value cont_line -variable contype1 
    frame $base.parframe.frame4.csetframe \
        -background gray30 -borderwidth 4 -height 57 \
        -highlightbackground slategray4 -highlightcolor white -relief ridge \
        -width 76 
    label $base.parframe.frame4.csetframe.label2 \
        -background slategray4 -foreground white \
        -highlightbackground slategray4 -highlightcolor black -relief ridge \
        -text {Contour Settings} 
    entry $base.parframe.frame4.csetframe.contmin \
        -background white -foreground black -highlightbackground slategray4 \
        -highlightcolor black -insertbackground black \
        -selectbackground #4848e1 -selectforeground black \
        -textvariable contminval -width 10 
    entry $base.parframe.frame4.csetframe.contmax \
        -background white -foreground black -highlightbackground slategray4 \
        -highlightcolor black -insertbackground black \
        -selectbackground #4848e1 -selectforeground black \
        -textvariable contmaxval -width 10 
    entry $base.parframe.frame4.csetframe.continc \
        -background white -foreground black -highlightbackground slategray4 \
        -highlightcolor black -insertbackground black \
        -selectbackground #4848e1 -selectforeground black \
        -textvariable contincval -width 10 
    label $base.parframe.frame4.csetframe.label17 \
        -background slategray1 -foreground black \
        -highlightbackground slategray4 -highlightcolor black -text Min 
    label $base.parframe.frame4.csetframe.label18 \
        -background slategray1 -foreground black \
        -highlightbackground slategray4 -highlightcolor black -text Max 
    label $base.parframe.frame4.csetframe.label19 \
        -background slategray1 -foreground black \
        -highlightbackground slategray4 -highlightcolor black -text Inc 
    button $base.parframe.frame4.csetframe.button20 \
        -activebackground White -activeforeground black \
        -background slategray1 -borderwidth 0 -command {contCB Set 1} \
        -disabledforeground #3c3cbc \
        -font -Adobe-Helvetica-Bold-R-Normal--*-100-*-*-*-*-*-* \
        -foreground black -highlightbackground slategray4 \
        -highlightcolor black -padx 11 -pady 2 -text Set -width 4 
    button $base.parframe.frame4.csetframe.button21 \
        -activebackground White -activeforeground black \
        -background slategray1 -borderwidth 0 -command {contCB Reset 1} \
        -disabledforeground #3c3cbc \
        -font -Adobe-Helvetica-Bold-R-Normal--*-100-*-*-*-*-*-* \
        -foreground black -highlightbackground slategray4 \
        -highlightcolor black -padx 11 -pady 2 -text Reset -width 4 
    button $base.parframe.frame4.csetframe.button22 \
        -activebackground White -activeforeground black \
        -background slategray1 -borderwidth 0 -command {contCB Default 1} \
        -disabledforeground #3c3cbc \
        -font -Adobe-Helvetica-Bold-R-Normal--*-100-*-*-*-*-*-* \
        -foreground black -highlightbackground slategray4 \
        -highlightcolor black -padx 11 -pady 0 -text Default -width 4 
    menubutton $base.parframe.frame4.vartype_menu \
        -background slategray1 -indicatoron 1 \
        -menu .top.parframe.frame4.vartype_menu.m -padx 4 -pady 3 \
        -relief raised -text 3-D -textvariable vartypeval -width 3 
    menu $base.parframe.frame4.vartype_menu.m \
        -cursor {} -tearoff 0 
    $base.parframe.frame4.vartype_menu.m add command \
        -command {set vartypeval 3-D
varCB $vartypeval} -label 3-D 
    $base.parframe.frame4.vartype_menu.m add command \
        -command {set vartypeval 2-D
varCB $vartypeval} -label 2-D 
    $base.parframe.frame4.vartype_menu.m add command \
        -command {set vartypeval Soil
varCB $vartypeval} -label Soil 
    frame $base.parframe.sec_frame \
        -background black -borderwidth 3 -highlightbackground slategray4 \
        -highlightcolor black -relief raised 
    label $base.parframe.sec_frame.label2 \
        -background white -borderwidth 3 \
        -font -Adobe-Helvetica-Bold-R-Normal--*-140-*-*-*-*-*-* \
        -foreground black -highlightbackground slategray4 \
        -highlightcolor black -relief ridge -text {Overlay Field} 
    radiobutton $base.parframe.sec_frame.rb_filled \
        -background slategray1 -text Vectors -value vectors \
        -variable overtype1 
    radiobutton $base.parframe.sec_frame.rb_line \
        -background slategray1 -text Line -value cont_line \
        -variable overtype1 
    frame $base.parframe.sec_frame.csetframe \
        -background gray30 -borderwidth 2 -height 57 \
        -highlightbackground slategray4 -highlightcolor black -relief ridge \
        -width 76 
    label $base.parframe.sec_frame.csetframe.label2 \
        -background slategray4 -foreground white \
        -highlightbackground slategray4 -highlightcolor black -relief ridge \
        -text {Contour Settings} 
    entry $base.parframe.sec_frame.csetframe.contmin \
        -background white -foreground black -highlightbackground slategray4 \
        -highlightcolor black -insertbackground black \
        -selectbackground #4848e1 -selectforeground black \
        -textvariable overminval -width 10 
    entry $base.parframe.sec_frame.csetframe.contmax \
        -background white -foreground black -highlightbackground slategray4 \
        -highlightcolor black -insertbackground black \
        -selectbackground #4848e1 -selectforeground black \
        -textvariable overmaxval -width 10 
    entry $base.parframe.sec_frame.csetframe.continc \
        -background white -foreground black -highlightbackground slategray4 \
        -highlightcolor black -insertbackground black \
        -selectbackground #4848e1 -selectforeground black \
        -textvariable overincval -width 10 
    label $base.parframe.sec_frame.csetframe.label17 \
        -background slategray1 -foreground black \
        -highlightbackground slategray4 -highlightcolor black -text Min 
    label $base.parframe.sec_frame.csetframe.label18 \
        -background slategray1 -foreground black \
        -highlightbackground slategray4 -highlightcolor black -text Max 
    label $base.parframe.sec_frame.csetframe.label19 \
        -background slategray1 -foreground black \
        -highlightbackground slategray4 -highlightcolor black -text Inc 
    button $base.parframe.sec_frame.csetframe.button20 \
        -activebackground White -activeforeground black \
        -background slategray1 -borderwidth 0 -command {contCB Set 2} \
        -disabledforeground #3c3cbc \
        -font -Adobe-Helvetica-Bold-R-Normal--*-100-*-*-*-*-*-* \
        -foreground black -highlightbackground slategray4 \
        -highlightcolor black -padx 11 -pady 2 -text Set -width 4 
    button $base.parframe.sec_frame.csetframe.button21 \
        -activebackground White -activeforeground black \
        -background slategray1 -borderwidth 0 -command {contCB Reset 2} \
        -disabledforeground #3c3cbc \
        -font -Adobe-Helvetica-Bold-R-Normal--*-100-*-*-*-*-*-* \
        -foreground black -highlightbackground slategray4 \
        -highlightcolor black -padx 11 -pady 2 -text Reset -width 4 
    button $base.parframe.sec_frame.csetframe.button22 \
        -activebackground White -activeforeground black \
        -background slategray1 -borderwidth 0 -command {contCB Default 2} \
        -disabledforeground #3c3cbc \
        -font -Adobe-Helvetica-Bold-R-Normal--*-100-*-*-*-*-*-* \
        -foreground black -highlightbackground slategray4 \
        -highlightcolor black -padx 11 -pady 2 -text Default -width 4 
    menubutton $base.parframe.sec_frame.var \
        -background slategray1 -highlightbackground slategray4 -indicatoron 1 \
        -menu .top.parframe.sec_frame.var.m -padx 5 -pady 4 -relief raised \
        -text {No Variables} -textvariable overvarval -width 12 
    menu $base.parframe.sec_frame.var.m \
        -cursor {} -tearoff 0 
    menubutton $base.parframe.sec_frame.vartype_menu \
        -background slategray1 -indicatoron 1 \
        -menu .top.parframe.sec_frame.vartype_menu.m -padx 5 -pady 4 \
        -relief raised -text 3-D -textvariable overvartypeval -width 3 
    menu $base.parframe.sec_frame.vartype_menu.m \
        -cursor {} -tearoff 0 
    $base.parframe.sec_frame.vartype_menu.m add command \
        -command {set overvartypeval 3-D
overvarCB $overvartypeval} \
        -label 3-D 
    $base.parframe.sec_frame.vartype_menu.m add command \
        -command {set overvartypeval 2-D
overvarCB $overvartypeval} \
        -label 2-D 
    $base.parframe.sec_frame.vartype_menu.m add command \
        -command {set overvartypeval Soil
overvarCB $overvartypeval} \
        -label Soil 
    button $base.draw \
        -activebackground white -activeforeground black \
        -background slategray1 -borderwidth 3 \
        -command {plotCB; tgks m1mode rect} -disabledforeground #3c3cbc \
        -font -Adobe-Helvetica-Bold-R-Normal--*-180-*-*-*-*-*-* \
        -foreground black -highlightbackground slategray3 -padx 10 -pady 4 \
        -text Draw 
    button $base.unzoom \
        -activebackground white -activeforeground black \
        -background slategray1 -borderwidth 3 -command {gridCB UnZoom} \
        -disabledforeground slategray2 \
        -font -Adobe-Helvetica-Bold-R-Normal--*-180-*-*-*-*-*-* \
        -foreground black -highlightbackground slategray3 -padx 11 -pady 4 \
        -state disabled -text UnZoom 
    ###################
    # SETTING GEOMETRY
    ###################
    pack $base.frame0 \
        -in .top -anchor center -expand 0 -fill x -side top 
    pack $base.frame0.menubutton1 \
        -in .top.frame0 -anchor center -expand 0 -fill none -side left 
    pack $base.frame0.menubutton2 \
        -in .top.frame0 -anchor center -expand 0 -fill none -side right 
    place $base.frame0.mbutt_opt \
        -x 50 -y 0 -anchor nw 
    place $base.frame0.mbutt_tools \
        -x 135 -y 0 -anchor nw 
    place $base.timeframe \
        -x 10 -y 155 -width 240 -height 95 -anchor nw 
    place $base.timeframe.date \
        -x 75 -y 5 -anchor nw 
    place $base.timeframe.time \
        -x 75 -y 30 -anchor nw 
    place $base.timeframe.label28 \
        -x 0 -y 0 -anchor nw 
    place $base.timeframe.timeslide \
        -x 0 -y 60 -width 230 -anchor nw 
    place $base.slabframe \
        -x 10 -y 255 -width 240 -height 170 -anchor nw 
    place $base.slabframe.slabtype1 \
        -x 55 -y 5 -anchor nw 
    place $base.slabframe.slabtype2 \
        -x 55 -y 30 -anchor nw 
    place $base.slabframe.slabtype3 \
        -x 175 -y 20 -anchor nw 
    place $base.slabframe.slabtype4 \
        -x 175 -y 45 -anchor nw 
    place $base.slabframe.frame25 \
        -x 0 -y 80 -width 230 -height 80 -anchor nw 
    place $base.slabframe.frame25.slaboffscale \
        -x 5 -y 36 -width 220 -height 43 -anchor nw -bordermode ignore 
    place $base.slabframe.frame25.slablab \
        -x 0 -y 0 -anchor nw 
    place $base.slabframe.frame25.vallab \
        -x 100 -y 0 -anchor nw 
    place $base.slabframe.label29 \
        -x 0 -y 0 -anchor nw 
    place $base.slabframe.slabtype5 \
        -x 55 -y 55 -anchor nw 
    place $base.gridframe \
        -x 10 -y 100 -width 240 -height 50 -anchor nw 
    place $base.gridframe.label32 \
        -x 0 -y 0 -anchor nw 
    place $base.gridframe.grid \
        -x 95 -y 10 -anchor nw -bordermode ignore 
    place $base.parframe \
        -x 10 -y 430 -width 240 -height 515 -anchor nw 
    place $base.parframe.windframe \
        -x 5 -y 245 -width 215 -height 45 -anchor nw 
    place $base.parframe.windframe.label46 \
        -x 0 -y 0 -anchor nw 
    place $base.parframe.windframe.windmenu \
        -x 80 -y 10 -anchor nw -bordermode ignore 
    place $base.parframe.label1 \
        -x 0 -y 0 -anchor nw 
    place $base.parframe.frame4 \
        -x 9 -y 36 -width 220 -height 210 -anchor nw -bordermode ignore 
    place $base.parframe.frame4.var \
        -x 15 -y 30 -height 25 -anchor nw 
    place $base.parframe.frame4.label2 \
        -x 55 -y 0 -anchor nw 
    place $base.parframe.frame4.rb_filled \
        -x 20 -y 60 -anchor nw 
    place $base.parframe.frame4.rb_tile \
        -x 87 -y 63 -width 50 -height 22 -anchor nw -bordermode ignore 
    place $base.parframe.frame4.rb_line \
        -x 70 -relx 0.35 -y 10 -rely 0.25 -anchor nw 
    place $base.parframe.frame4.csetframe \
        -x 5 -y 90 -width 205 -height 110 -anchor nw 
    place $base.parframe.frame4.csetframe.label2 \
        -x 45 -y 0 -anchor nw 
    place $base.parframe.frame4.csetframe.contmin \
        -x 5 -y 25 -anchor nw 
    place $base.parframe.frame4.csetframe.contmax \
        -x 5 -y 50 -anchor nw 
    place $base.parframe.frame4.csetframe.continc \
        -x 5 -y 75 -anchor nw 
    place $base.parframe.frame4.csetframe.label17 \
        -x 85 -y 25 -anchor nw 
    place $base.parframe.frame4.csetframe.label18 \
        -x 85 -y 50 -anchor nw 
    place $base.parframe.frame4.csetframe.label19 \
        -x 85 -y 75 -anchor nw 
    place $base.parframe.frame4.csetframe.button20 \
        -x 135 -y 30 -anchor nw 
    place $base.parframe.frame4.csetframe.button21 \
        -x 135 -y 50 -anchor nw 
    place $base.parframe.frame4.csetframe.button22 \
        -x 135 -y 70 -height 27 -anchor nw 
    place $base.parframe.frame4.vartype_menu \
        -x 147 -y 34 -width 54 -height 24 -anchor nw -bordermode ignore 
    place $base.parframe.sec_frame \
        -x 5 -y 295 -width 220 -height 210 -anchor nw 
    place $base.parframe.sec_frame.label2 \
        -x 60 -y 0 -anchor nw 
    place $base.parframe.sec_frame.rb_filled \
        -x 100 -y 60 -anchor nw 
    place $base.parframe.sec_frame.rb_line \
        -x 45 -y 60 -anchor nw 
    place $base.parframe.sec_frame.csetframe \
        -x 5 -y 90 -width 205 -height 110 -anchor nw 
    place $base.parframe.sec_frame.csetframe.label2 \
        -x 45 -y 0 -anchor nw 
    place $base.parframe.sec_frame.csetframe.contmin \
        -x 5 -y 25 -anchor nw 
    place $base.parframe.sec_frame.csetframe.contmax \
        -x 5 -y 50 -anchor nw 
    place $base.parframe.sec_frame.csetframe.continc \
        -x 5 -y 75 -anchor nw 
    place $base.parframe.sec_frame.csetframe.label17 \
        -x 85 -y 25 -anchor nw 
    place $base.parframe.sec_frame.csetframe.label18 \
        -x 85 -y 50 -anchor nw 
    place $base.parframe.sec_frame.csetframe.label19 \
        -x 85 -y 75 -anchor nw 
    place $base.parframe.sec_frame.csetframe.button20 \
        -x 135 -y 30 -anchor nw 
    place $base.parframe.sec_frame.csetframe.button21 \
        -x 135 -y 50 -anchor nw 
    place $base.parframe.sec_frame.csetframe.button22 \
        -x 45 -relx 0.45 -y 35 -rely 0.35 -height 27 -anchor nw 
    place $base.parframe.sec_frame.var \
        -x 15 -y 35 -width 117 -height 24 -anchor nw -bordermode ignore 
    place $base.parframe.sec_frame.vartype_menu \
        -x 140 -y 33 -width 56 -height 26 -anchor nw -bordermode ignore 
    place $base.draw \
        -x 10 -y 35 -width 70 -height 60 -anchor nw 
    place $base.unzoom \
        -x 170 -y 35 -width 85 -anchor nw 
}

proc vTclWindow.v {base} {
    if {$base == ""} {
        set base .v
    }
    if {[winfo exists $base]} {
        wm deiconify $base; return
    }
    ###################
    # CREATING WIDGETS
    ###################
    toplevel $base -class Toplevel
    wm focusmodel $base passive
    wm geometry $base 800x800+0+0
    wm maxsize $base 1137 834
    wm minsize $base 1 1
    wm overrideredirect $base 0
    wm resizable $base 1 1
    wm deiconify $base
    wm title $base "RingiView"
    frame $base.f \
        -background black -height 75 -width 125 
    bind $base.f <Map> {
        tgks_init .v.f
    }
    ###################
    # SETTING GEOMETRY
    ###################
    pack $base.f \
        -in .v -anchor center -expand 1 -fill both -side top 
}

proc vTclWindow.value_top {base} {
    if {$base == ""} {
        set base .value_top
    }
    if {[winfo exists $base]} {
        wm deiconify $base; return
    }
    ###################
    # CREATING WIDGETS
    ###################
    toplevel $base -class Toplevel \
        -background slategray4 
    wm focusmodel $base passive
    wm geometry $base 287x321+907+276
    wm maxsize $base 1265 994
    wm minsize $base 1 1
    wm overrideredirect $base 0
    wm resizable $base 1 1
    wm title $base "Get Values"
    frame $base.f \
        -background slategray3 -borderwidth 2 -height 75 \
        -highlightbackground slategray4 -relief raised -width 125 
    button $base.f.rect \
        -background slategray1 \
        -command {tgks m3mode rect
tgks m3cmd {values}
set vallabvar {Use right mouse button to select area}
.value_top.f.rect config -state disabled
.value_top.f.point config -state normal} \
        -padx 9 -pady 3 -text {get area values} 
    button $base.f.point \
        -background slategray1 \
        -command {tgks m3mode point
tgks m3cmd {values}
set vallabvar {Use right mouse button to select point}
.value_top.f.point config -state disabled
.value_top.f.rect config -state normal} \
        -padx 9 -pady 3 -text {get point values} 
    label $base.f.vallab \
        -background slategray2 -borderwidth 1 -textvariable vallabvar 
    entry $base.f.x1 \
        -textvariable vx1 -width 4 
    entry $base.f.y1 \
        -textvariable vy1 -width 4 
    entry $base.f.x2 \
        -textvariable vx2 -width 4 
    entry $base.f.y2 \
        -textvariable vy2 -width 4 
    label $base.f.loclab \
        -background slategray3 -borderwidth 1 -text Location: 
    label $base.f.sizlab \
        -background slategray3 -borderwidth 1 -text Size: 
    entry $base.f.wid \
        -textvariable vwid -width 4 
    entry $base.f.hgt \
        -textvariable vhgt -width 4 
    button $base.dismiss \
        -background slategray1 \
        -command {tgks m3mode none
set vallabvar {}
.value_top.f.rect config -state normal
.value_top.f.point config -state normal
Window destroy .value_top} \
        -padx 9 -pady 3 -text Close 
    ###################
    # SETTING GEOMETRY
    ###################
    place $base.f \
        -x 5 -y 5 -width 275 -height 270 -anchor nw -bordermode ignore 
    place $base.f.rect \
        -x 20 -y 225 -anchor nw -bordermode ignore 
    place $base.f.point \
        -x 145 -y 225 -anchor nw -bordermode ignore 
    place $base.f.vallab \
        -x 20 -y 195 -width 241 -height 18 -anchor nw -bordermode ignore 
    place $base.f.x1 \
        -x 75 -y 10 -anchor nw -bordermode ignore 
    place $base.f.y1 \
        -x 115 -y 10 -anchor nw -bordermode ignore 
    place $base.f.x2 \
        -x 155 -y 10 -anchor nw -bordermode ignore 
    place $base.f.y2 \
        -x 195 -y 10 -anchor nw -bordermode ignore 
    place $base.f.loclab \
        -x 15 -y 15 -anchor nw -bordermode ignore 
    place $base.f.sizlab \
        -x 40 -y 45 -anchor nw -bordermode ignore 
    place $base.f.wid \
        -x 75 -y 40 -anchor nw -bordermode ignore 
    place $base.f.hgt \
        -x 115 -y 40 -anchor nw -bordermode ignore 
    place $base.dismiss \
        -x 110 -y 285 -anchor nw -bordermode ignore 
}

proc vTclWindow.vector_top {base} {
    if {$base == ""} {
        set base .vector_top
    }
    if {[winfo exists $base]} {
        wm deiconify $base; return
    }
    ###################
    # CREATING WIDGETS
    ###################
    toplevel $base -class Toplevel  -background slategray4 -relief raised 
    wm focusmodel $base passive
    wm geometry $base 300x300+752+244
    wm maxsize $base 300 300
    wm minsize $base 300 300
    wm overrideredirect $base 0
    wm resizable $base 1 1
    wm sizefrom $base program
    wm title $base "Wind Vector Settings"
    frame $base.wind_frame  -background slategray3 -borderwidth 4 -height 124 -relief raised  -width 8 
    menubutton $base.wind_frame.windintmenu  -activebackground White -activeforeground black  -background slategray1 -disabledforeground #3c3cbc -foreground black  -highlightbackground slategray4 -highlightcolor black -indicatoron 1  -menu .vector_top.wind_frame.windintmenu.m -padx 5 -pady 4  -relief raised -text 2 -textvariable windintvar -width 3 
    menu $base.wind_frame.windintmenu.m  -activebackground White -activeforeground black  -background slategray4 -disabledforeground #3c3cbc -foreground black  -tearoff 0 
    $base.wind_frame.windintmenu.m add command  -command {set windintvar 1} -label 1 
    $base.wind_frame.windintmenu.m add command  -command {set windintvar 2} -label 2 
    $base.wind_frame.windintmenu.m add command  -command {set windintvar 3} -label 3 
    $base.wind_frame.windintmenu.m add command  -command {set windintvar 4} -label 4 
    $base.wind_frame.windintmenu.m add command  -command {set windintvar 5} -label 5 
    label $base.wind_frame.label49  -background slategray1  -font -*-helvetica-bold-r-normal-*-10-*-*-*-p-*-iso8859-1  -foreground black -highlightbackground slategray4  -highlightcolor black -relief groove -text {Wind Interval} 
    menubutton $base.wind_frame.barbscale  -activebackground White -activeforeground black  -background slategray1 -disabledforeground #3c3cbc -foreground black  -highlightbackground slategray4 -highlightcolor black -indicatoron 1  -menu .vector_top.wind_frame.barbscale.m -padx 5 -pady 4  -relief raised -text 20-4-2 -textvariable barbscaleval -width 7 
    menu $base.wind_frame.barbscale.m  -activebackground White -activeforeground black  -background slategray4 -disabledforeground #3c3cbc -foreground black  -tearoff 0 
    $base.wind_frame.barbscale.m add command  -command {set barbscaleval {50-10-5}} -label 50-10-5 
    $base.wind_frame.barbscale.m add command  -command {set barbscaleval {20-4-2}} -label 20-4-2 
    $base.wind_frame.barbscale.m add command  -command {set barbscaleval {10-2-1}} -label 10-2-1 
    label $base.wind_frame.label52  -background slategray1  -font -*-helvetica-bold-r-normal-*-10-*-*-*-p-*-iso8859-1  -foreground black -highlightbackground slategray4  -highlightcolor black -relief groove -text {Barb scale} 
    label $base.wind_frame.label6  -background slategray1  -font -Adobe-Helvetica-Bold-R-Normal--*-140-*-*-*-*-*-* -relief ridge  -text {Wind depiction settings} 
    button $base.done_butt  -background slategray1 -command {wm withdraw .vector_top}  -font -Adobe-Helvetica-Bold-R-Normal--*-140-*-*-*-*-*-* -padx 11  -pady 4 -text Done 
    button $base.save_butt  -background slategray1  -font -Adobe-Helvetica-Bold-R-Normal--*-140-*-*-*-*-*-* -padx 11  -pady 4 -text Save 
    ###################
    # SETTING GEOMETRY
    ###################
    place $base.wind_frame  -x 20 -y 30 -width 245 -height 220 -anchor nw 
    place $base.wind_frame.windintmenu  -x 5 -y 75 -anchor nw 
    place $base.wind_frame.label49  -x 5 -y 55 -anchor nw 
    place $base.wind_frame.barbscale  -x 110 -y 75 -anchor nw 
    place $base.wind_frame.label52  -x 110 -y 55 -anchor nw 
    place $base.wind_frame.label6  -x 0 -y 0 -anchor nw 
    place $base.done_butt  -x 195 -y 260 -anchor nw 
    place $base.save_butt  -x 115 -y 260 -anchor nw
}

Window show .
Window show .top
Window show .v

main $argc $argv
