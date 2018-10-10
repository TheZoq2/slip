ring_åklagaren:
	echo "Ringer åklagaren"
	stack build
	stack exec slip-exe
	stack exec slip-exe > output.md
	cleaver output.md

