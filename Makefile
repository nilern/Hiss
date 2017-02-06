$BOOTSTRAP = scheme/bootstrap

bin:
	ghc -o hiss -Wall main.lhs

bootstrap_script: $($BOOTSTRAP)/wrap-intrinsics.scm $($BOOTSTRAP)/cxr.scm \
                  $($BOOTSTRAP)/list.scm \
									$($BOOTSTRAP)/env.scm $($BOOTSTRAP)/expand.scm \
									$($BOOTSTRAP)/main.scm
	cat $^ > __bootstrap__.scm

clean:
	rm *.o *.hi Hiss/*.o Hiss/*.hi
	rm main.html

html: src/Hiss/Data.lhs src/Hiss/Read.lhs src/Hiss/Analyze.lhs \
	    src/Hiss/Interpret.lhs src/Hiss/Primops.lhs app/Main.lhs
	cat $^ | pandoc -f markdown+lhs -s -o main.html
