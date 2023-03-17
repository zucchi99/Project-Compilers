# Parte 3

## Richieste

* Si hanno le seguenti procedure predefinite:

  * **write**:
    * `writeInt`
    * `writeReal`
    * `writeChar`
    * `writeString`
  * **read** (hanno argomento per riferimento):
    * `readInt`
    * `readReal`
    * `readChar`
    * `readString`

Si hanno le **operazioni**:

* *aritmetiche*
* *booleane*
* *relazionali standard*

I tipi **base** ammessi sono:

* *Interi*
* *Booleani*
* *Real*
* *Caratteri*
* *Stringhe*

I tipi **composti** ammessi sono:

* *Array*
* *Puntatori*

I tipi devono avere, se necessario, **costruttori e/o operazioni di selezione relative**.

Vanno previsti:

* *Condizionali semplici*
* *Iterazione indeterminata*

## Implementazioni

* [ ] Progettazione/Implementazione della **sintassi astratta**
* [ ] Implementazione del **pretty-printer** (da albero di sintassi astratta a sintassi concerta *legale*)
* [ ] Progettazione **type-system**
* [ ] Implementazione del **lexer** (*Alex*) e **parser** (*Happy*) tramite *BNFC* da raffinare
* [ ] Implementazione del **type-checker** (con messaggi di errore)
* [ ] Definizione del **data-type** con cui codificare il three-address code
* [ ] Implementazione del **three-address code** (vedi specifiche)
* [ ] Predisposizione dei **test case significativi**

### Specifiche

#### Three-Address Code

* Gli assegnamenti devono valutare l-value prima di r-value;
* L’ordine delle espressioni e della valutazione degli argomenti si può scegliere a piacere (motivandolo);
* Le espressioni booleane nelle guardie devono essere valutate con short-cut. In altri contesti si
può scegliere a piacere (motivandolo).

#### Test case significativi

Per ogni test di ogni funzione, che dato un nome di file esega:

* Il parsing del contenuto
* L'analisi di semantica statica
* Il pretty-print del sorgente
* Generazione + pretty-print del three-address code.  
 
Nel pretty-print del three-address code si serializzino gli identificatori che vengono dal programma aggiungendo l’informazione relativa al punto di dichiarazione
nel sorgente originale

## Link Utili

* Specifiche del linguaggio: <https://wiki.freepascal.org/Standard_Pascal>
* Relazione: <https://www.overleaf.com/8232468873pmgsmgkysvqv>
* Link e-learning: <https://elearning.uniud.it/moodle/course/view.php?id=4006>
