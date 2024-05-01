# Testen

In deze map zitten alle testen die ik heb geschreven.

## Unit testen

In de map unit zitten de unit tests. Deze testen de move functionaliteit van individuele stukken, 
net als een pin test.
Ook wordt hier de parser getest. In `san.pl` Kijk ik of de parser alle mogelijk SAN-notaties kan parsen.
Mijn parser kan technisch gezien meer parsen (san moves die nooit kunnen voorkomen, zoals `e5=Q`), 
maar die zullen later opgevangen worden.

## Integratie testen

Er is één grote integratie test, geschreven in python. Deze test neemt een map met pgn-bestanden en kijkt voor elk
bestand of op elk punt in het spel kijkt of alle mogelijke zetten in die positie overeen komen met alle mogelijke zetten
van mijn engine (als het `TEST`-argument wordt meegegeven). 