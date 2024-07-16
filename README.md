# prolog-todo

- Webová aplikace Todo-list v SWI-Prolog
- Používá vestavěnou HTTP knihovnu

- Server posílá generovanou HTML stránku, která obsahuje vstupní pole na zadávání textu, tlačítko na přidávání nových položek a seznam již existujících položek s tlačítky na upravování a mazání.

- Server podporuje dotazy pro tvoření, upravování a mazání položek.

- Položky se ukládají jako predikáty lokálně do souboru `tasks.db`.