# PCSCEMV
 Source code: Delphi

## Working parts of EMV:
1. ATR|ATS
2. Get AID by PSE
3. Get AID by appliation list
4. Select application
5. Format PDOL
6. Execute GPO
7. Get records from AFL
8. Make SDA
9. Make DDA
10. Check PIN
11. Fill CDOL1 and CDOL2
12. Execute AC1 (with CDA support)
13. Check ARQC (bank part)
14. Make ARPC (bank part)
15. Execute external authenticate
16. Execute AC2 (with CDA support)
17. Check ARQC cryptogram
18. Issuer scripts processing

## Working parts of qVSDC:
1. ATR|ATS
2. Get AID by PSE
3. Get AID by appliation list
4. Select application
5. Format PDOL
6. Execute GPO
7. Get records from AFL
8. Make fDDA
9. External authenticate command
10. Issuer scripts processing
 
## Useful links:

EMV specifications
http://www.emvco.com/specifications.aspx?id=155

Excelent explanation of EMV
http://www.openscdp.org/scripts/emv/index.html

Fully working terminal written in Ruby.
https://code.google.com/p/ruby-pboc2-lib/source/browse/trunk/lib/pboc.rb

EMV kernel written in C++
https://github.com/ntufar/EMV/tree/master/EMV_Library

Resources (keys, country codes, etc):
https://github.com/binaryfoo/emv-bertlv/tree/master/src/main/resources


