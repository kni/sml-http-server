all:
	@echo "make targets: poly, mlton, clean."

poly: net-server net-server/os-constants.sml
	polyc -o t-poly t.mlp

mlton: net-server net-server/os-constants.sml
	mlton -default-ann 'allowFFI true' -output t-mlton t.mlb

net-server:
	git clone https://github.com/kni/sml-net-server.git net-server

net-server/os-constants.sml:
	cd net-server; make os-constants.sml

clean:
	rm -rf t-poly t-mlton
	test -h net-server || rm -rf net-server
