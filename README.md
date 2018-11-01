# Contract Templates

## Example

Make sure you have a contract called Bet.fst and then do:

1. `contract_templates.exe extract Bet.fst -v Bet.json`
2. modify the `Bet.json` file:
	* change `"filename" : "Bet.fst"` to `"filename" : "Bet2.fst"`
	* change the value of the `"ticker"` parameter to `"ABC"`
	* change the value of the `"strike"` parameter to `1234`
3. `contract_templates.exe generate Bet.fst -v Bet.json`
4. Now the corresponding fields in the contract should be different in the Bet.fst and Bet2.fst files.
