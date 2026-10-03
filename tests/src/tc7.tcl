# Source of tbcfiles*/tc7.tbc: aux data whose index arrays are Tcl_Size in
# Tcl 9. A foreach over many variables and a dict update of many keys each
# allocate one of them; sized as int, they overran their heap blocks. With
# 26 variables the overrun outgrows any allocator's rounding.
proc foreachmany {} {
    set r {}
    foreach {a b c d e f g h i j k l m n o p q r_ s t u v w x y z} \
            {1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26} {
        lappend r $a $b $c $d $e $f $g $h $i $j $k $l $m \
                  $n $o $p $q $r_ $s $t $u $v $w $x $y $z
    }
    return $r
}
proc dictupdatemany {} {
    set d {}
    for {set i 1} {$i <= 26} {incr i} {dict set d k$i $i}
    dict update d k1 v1 k2 v2 k3 v3 k4 v4 k5 v5 k6 v6 k7 v7 k8 v8 k9 v9 \
            k10 v10 k11 v11 k12 v12 k13 v13 k14 v14 k15 v15 k16 v16 k17 v17 \
            k18 v18 k19 v19 k20 v20 k21 v21 k22 v22 k23 v23 k24 v24 k25 v25 k26 v26 {
        set sum [expr {$v1+$v2+$v3+$v4+$v5+$v6+$v7+$v8+$v9+$v10+$v11+$v12+$v13
                       +$v14+$v15+$v16+$v17+$v18+$v19+$v20+$v21+$v22+$v23+$v24
                       +$v25+$v26}]
    }
    return $sum
}
