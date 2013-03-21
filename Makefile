REBAR=rebar

DEPS_EBIN = `find . -name ebin -type d`

all:
	@$(REBAR) get-deps
	for a in deps/*; do cd $$a; make; cd -; done
	@$(REBAR) compile
	@$(REBAR) doc
clean:
	for a in deps/*; do cd $$a; make clean; cd -; done
	@$(REBAR) clean

