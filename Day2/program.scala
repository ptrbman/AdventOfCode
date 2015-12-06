val lines = io.Source.fromFile("input.txt").getLines

var totalPaper = 0
var totalRibbon = 0

for (l <- lines) {

    if (l.length > 0) {
        val values = l.split('x').map(_.toInt).sortWith(_ < _)
    	val a = values(0)
	val b = values(1)
	val c = values(2)
	totalPaper = totalPaper + 3*a*b + 2*a*c + 2*b*c
	totalRibbon = totalRibbon + 2*a + 2*b + a*b*c
    }
}

println(totalPaper)
println(totalRibbon)

