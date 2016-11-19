bin:
	ghc -o hiss -Wall main.lhs

clean:
	rm *.o *.hi Hiss/*.o Hiss/*.hi
	rm main.html

html:
	pandoc -f markdown+lhs Hiss/Data.lhs -s > main.html
	pandoc -f markdown+lhs Hiss/Read.lhs -s >> main.html
	pandoc -f markdown+lhs Hiss/Analyze.lhs -s >> main.html
	pandoc -f markdown+lhs Hiss/Interpret.lhs -s >> main.html
	pandoc -f markdown+lhs main.lhs -s >> main.html
