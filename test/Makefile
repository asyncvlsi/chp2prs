
runtest:
	@if [ -x run.sh ] ; \
	then \
		(./run.sh);\
	else \
		(echo "Error: no run.sh file") \
	fi

prsimunit:
	echo $(unit)
	@if [ -x $ACT_HOME/bin/test_writer -a -d unit_tests/$unit ] ; \
	then \
		($ACT_HOME/bin/test_writer unit_tests/$unit/test_writer.txt unit_tests/$unit/test.prsim --prsim --reset --_reset)
	

#	ls
#	./aflat "test.act" > "test2.prs"
#    ./prsim "test2.prs"
#    source "test2.prsim"
#    echo "test 2 complete"
