compile:
	@ alex -o __build/Alex.hs Alex.x 
	@ happy -o __build/Happy.hs Happy.y
	@ ghc -no-keep-hi-files -no-keep-o-files -o __build/demo __build/Alex.hs __build/Happy.hs demo.hs

demo:
	@ __build/demo

clear:
	@ rm __build/*