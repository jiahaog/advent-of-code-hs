exec_name := 'advent-of-code-exe'

run: build
	@stack exec ${exec_name} -- ${day} ${part}

build:
	@stack build

.PHONY: run build
