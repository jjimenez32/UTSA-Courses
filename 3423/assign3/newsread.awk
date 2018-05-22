BEGIN {larticles = 0; lgroups = 0; lcs=0; lcsd=0; luser = 0.0;
       ruarticles = 0;rugroups = 0; rucs = 0; rucsd = 0; ruuser =0.0;
       riarticles = 0; rigroups = 0; rics = 0; ricsd = 0; riuser = 0.0;
       rgarticles = 0; rggroups = 0; rgcs = 0; rgcsd = 0; rguser = 0.0;}

($6~/lonestar/ && $7~/exit/) {larticles += $9; lgroups += $11}
($6~/lonestar/ && $7~/group/ && $8~/2313$/) {lcs += $9}
($6~/lonestar/ && $7~/group/ && $8~/2413.d$/){lcsd += $9}
($6~/lonestar/ && $7~/times/) {luser += $13}

($6~/runner/ && $7~/exit/) {ruarticles += $9; rugroups += $11}
($6~/runner/ && $7~/group/ && $8~/2413$/) {rucs += $9}
($6~/runner/ && $7~/group/ && $8~/2413.d$/){rucsd += $9}
($6~/runner/ && $7~/times/) {ruuser += $13}

($6~/ringer/ && $7~/exit/) {riarticles += $9; rigroups += $11}
($6~/ringer/ && $7~/group/ && $8~/2413$/) {rics += $9}
($6~/ringer/ && $7~/group/ && $8~/2413.d$/){ricsd += $9}
($6~/ringer/ && $7~/times/) {riuser += $13}

($6~/ring[0-9][0-9]/ && $7~/exit/) {rgarticles += $9; rggroups += $11}
($6~/ring[0-9][0-9]/ && $7~/group/ && $8~/2413$/) {rgcs += $9}
($6~/ring[0-9][0-9]/ && $7~/group/ && $8~/2413.d$/){rgcsd += $9}
($6~/ring[0-9][0-9]/ && $7~/times/) {rguser += $13}



(NR == 1) {start = $1" "$2" "$3}
(NR == NR) {stop = $1" "$2" "$3}



END {print"			News Reader Summary\n\n"
print"		lonestar	runner		ringer		rings\n"
print"Articles:	"larticles"		"ruarticles"		"riarticles"		"rgarticles
print"Groups:		"lgroups"		"rugroups"		"rigroups"		"rggroups
print"Cs2413:		"lcs"		"rucs"		"rics"		"rgcs
print"Cs2413.d:	"lcsd"		"rucsd"		"ricsd"		"rgcsd
print"User Time:	"luser"		"ruuser"		"riuser"		"rguser"\n"
print"Start Time = "start"		End Time = "stop}
	

