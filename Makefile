
#
# Examples
#
boot::  examples/hello-world/index.html \
        examples/hello-edit/index.html 
	echo done        

examples/hello-world/index.html: examples/src/Main.hs
	stack build
	mkdir -p examples/hello-world
	stack exec aframe-blueprint-examples hello-world $@

examples/hello-edit/index.html: examples/src/Main.hs
	stack build
	mkdir -p examples/hello-edit
	stack exec aframe-blueprint-examples hello-edit $@

clean:
	rm -f examples/*/*html

server::
	npm run dev &

haskell::
	fswatch -0 examples/src/ | xargs -0 -n 1 -I {} make boot
