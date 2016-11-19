bin:
	ghc -o hiss main.lhs

html:
	pandoc -f markdown+lhs Hiss/Data.lhs -s > main.html
	pandoc -f markdown+lhs Hiss/Read.lhs -s >> main.html
	pandoc -f markdown+lhs Hiss/Write.lhs -s >> main.html
	pandoc -f markdown+lhs main.lhs -s >> main.html
