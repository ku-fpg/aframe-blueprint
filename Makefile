
#
# Examples
#
examples/hello-world/index.html: examples/src/Main.hs
	mkdir -p examples/hello-world
	cabal exec runghc examples/src/Main.hs hello-world $@


server::
	npm run dev &
