REBAR=rebar

DEPS_EBIN = `find . -name ebin -type d`

all:
	for a in deps/*; do cd $$a; make; cd -; done
	@$(REBAR) compile
clean:
	for a in deps/*; do cd $$a; make clean; cd -; done
	@$(REBAR) clean

