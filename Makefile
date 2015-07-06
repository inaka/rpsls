REBAR ?= ./rebar

ERL := erl -pa ebin -pa deps/*/ebin +Bc +K true -smp enable -s lager ${ERL_ARGS}

all:
	${REBAR} get-deps && ${REBAR} compile

clean:
	${REBAR} clean

build_plt: all
	dialyzer --verbose --build_plt --apps kernel stdlib sasl erts ssl tools os_mon runtime_tools crypto inets xmerl webtool snmp public_key mnesia syntax_tools compiler --output_plt rpsls.plt -pa deps/*/ebin ebin

analyze: all
	dialyzer --verbose -pa deps/*/ebin --plt rpsls.plt -Wunmatched_returns -Werror_handling ebin

doc: all
	${REBAR} skip_deps=true doc

xref: all
	${REBAR} skip_deps=true xref

shell: all
	${ERL} -boot start_sasl

run: all
	${ERL} -boot start_sasl -s rpsls

compile: clean
	${REBAR} compile

test2: compile 
	find . -type f -name "*.erl" -exec cp {} ebin \; #Copy the files to the known directory
	${REBAR} -v 3 ct skip_deps=true 
	rm ebin/*.erl 