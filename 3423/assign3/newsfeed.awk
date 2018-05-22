BEGIN {saccept = 0; sreject =0; scan = 0;
       naccept = 0; nreject =0; ncan = 0;
       qaccept = 0; qreject =0; qcan = 0;}

($5~/swrinde/ && $4 == "-") {sreject++}
($5~/swrinde/ && $4 == "+") {saccept++}
($5~/swrinde/ && $4 == "c") {scan++; saccept++;}
($5~/swrinde/ && $4 == "j") {saccept++}

($5 == "news.cais.net" && $4 == "-") {nreject++}
($5 == "news.cais.net" && $4 == "+") {naccept++}
($5 == "news.cais.net" && $5 == "c") {ncan++; naccept++;}
($5 == "news.cais.net" && $5 == "j") {naccept++}

($5 == "?" && $4 == "-") {qreject++}
($5 == "?" && $4 == "+") {qaccept++}
($5 == "?" && $4 == "c") {qcan++; qaccept++;}
($5 == "?" && $4 == "j") {qaccept++}

(NR == 1) {start = $1" "$2" "$3}
(NR == NR) {stop = $1" "$2" "$3}

END{print"			Incoming News Feed Summary\n\n"
print"		accepted	rejected	canceled"
print"swrinde:	"saccept"		"sreject"		"scan
print"news.cais.net:	"naccept"		"nreject"		"ncan
print"?:		"qaccept"		"qreject"		"qcan"\n"
print"Start Time = "start"	End Time = "stop}
