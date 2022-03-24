#!/bin/awk

BEGIN {
    sum = 0
    deflst = 0
    indlst = 0
    posslst = 0
}

/definitely lost:/ { sum += $4; deflst = $4 }
/indirectly lost:/ { sum += $4; indlst = $4 }
/possibly lost:/ { sum += $4; posslst = $4 }

END { if (sum > 0) print "Definitely lost:", deflst, "\\nIndirectly lost:", indlst, "\\nPossibly lost:", posslst }
