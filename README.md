# prolog-todo

- Webová aplikace Todo-list v SWI-Prolog
- Používá vestavěnou HTTP knihovnu

- Server posílá generovanou HTML stránku, která obsahuje vstupní pole na zadávání textu, tlačítko na přidávání nových položek a seznam již existujících položek s tlačítky na upravování a mazání.

- Server podporuje POST, PUT a DELETE dotazy pro tvoření, upravování a mazání položek

- Položky se ukládají lokálně pomocí vestavěné knihovny persistency, popř. do csv souboru (pokud se nepovede implementace, pak do cookies webu nebo zůstávají in-memory) 