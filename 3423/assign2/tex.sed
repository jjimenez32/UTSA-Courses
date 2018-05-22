s/\\/\\verb\+\\\+/g
s/%/\\%/g
s/\^/\\\^/g
s/--/-\\hspace\{.01cm\}-/g




1i \
\\documentstyle[11pt]{article}\
\\begin{document}


1i \
\\begin{center} {\\bf


1a \
\} \\end{center}


/NAME/s/^/\\item[/
/NAME/s/$/] \\hfill \\\\/
/SYNOPSIS/s/^/\\item[/
/SYNOPSIS/s/$/] \\hfill \\\\/
/DESCRIPTION/s/^/\\item[/
/DESCRIPTION/s/$/] \\hfill \\\\/
/OPTIONS/s/^/\\item[/
/OPTIONS/s/$/] \\hfill \\\\/
/TESTS/s/^/\\item[/
/TESTS/s/$/] \\hfill \\\\/
/EXPRESSIONS/s/^/\\item[/
/EXPRESSIONS/s/$/] \\hfill \\\\/
/ACTIONS/s/^/\\item[/
/ACTIONS/s/$/] \\hfill \\\\/
/OPERATORS/s/^/\\item[/
/OPERATORS/s/$/] \\hfill \\\\/


2i \
\\begin{description}
$a \
\\end{description}
$a \
\\end{document}

/^[. 	][. 	]*[-+]/s/$/ \\\\/ 
