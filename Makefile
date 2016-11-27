$BOOTSTRAP = scheme/bootstrap

bin:
	ghc -o hiss -Wall main.lhs

bootstrap_script: $($BOOTSTRAP)/wrap-intrinsics.scm $($BOOTSTRAP)/cxr.scm \
                  $($BOOTSTRAP)/list.scm $($BOOTSTRAP)/env.scm
	cat $^ > __bootstrap.scm

clean:
	rm *.o *.hi Hiss/*.o Hiss/*.hi
	rm main.html

html:
	pandoc -f markdown+lhs Hiss/Data.lhs -s > main.html
	pandoc -f markdown+lhs Hiss/Read.lhs -s >> main.html
	pandoc -f markdown+lhs Hiss/Analyze.lhs -s >> main.html
	pandoc -f markdown+lhs Hiss/Interpret.lhs -s >> main.html
	pandoc -f markdown+lhs main.lhs -s >> main.html
