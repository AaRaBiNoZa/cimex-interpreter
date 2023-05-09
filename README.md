# Cimex (latin - bug)

## Uruchomienie
W głównym katalogu rozwiązanie kompiluje się do pliku wykonywalnego `interpreter` za pomocą 
```
make
```
Interpreter można uruchomić na plikach w trybie cichym (domyślnie) lub z dodatkowymi informacjami (wypisanie drzewa abstrakcyjnego programu, pretty print kodu):
- zwykłe uruchomienie za pomocą `./interpreter plik`
- dodatkowe informacje `interpreter -v plik`

W przypadku czytania ze standardowego wejścia, jest dostępna tylko opcja cicha. (uruchamiamy oczywiście przez `./interpreter`)

## Przykładowe programy
W kodzie interpretera i typecheckera można zauważyć błędy "Runtime", "Static" oraz "Impossible". Pierwsze dwa są
raczej oczywiste. Błąd "Impossible" oznacza napotkanie sytuacji, która nie powinna mieć prawa się stać.

Bad:
- runtime_bad - błędy w czasie wykonania
- static_bad - błędy "łapane" przez typechecker, przed wykonaniem
- syntax_bad - błędy składniowe
  
Good:
- zawiera pliki jak w poleceniu, a poza tym example[1-3].cx to programy oddane przy deklaracji języka i opisane poniżej.

## Opis

Jest to język imperatywny o składni podobnej do Latte/c, z największą różnicą polegającą na obsłudze wcięć jako bloków 
kodu i braku obowiązku używania średników (python like). Reszta jest jak w c, przy czym
można przekazywać parametry przez referencję (a nie wskaźnik) dodając przy deklaracji funkcji, przed nazwą parametru symbol &.



Oprócz interpretera dostępny jest statyczny typechecker, który sprawdza typy, 
dublowane deklaracje zmiennych w tym samym bloku, break/continue poza pętlą, return poza
ciałem funkcji itd.

Dodatkowo język zawiera wielowymiarowe tablice przekazywane przez zmienną.
(Tak dokładniej to wartość samej tablicy to wskaźnik, gdy użyje się na tablicy
"print" zostanie wypisana lokacja w pamięci, ale to jedyny moment, w którym
użytkownik ma dostęp do tego wskaźnika. Dzięki temu przypisanie to 
po prostu przypisanie przez wartość, które tylko zachowuje się jak przypisanie przez
referencję.)
Tablic nie można przekazywać w funkcjach na dwa sposoby, nielegalny
jest parametr "int[][] &x" i taki przypadek jest wyłapywany statycznie. Tablice domyślnie przekazywane są
przez referencję (znów - tak naprawdę jest to przekazanie przez wartość wskaźnika, ale to kwestie "pod maską".
Zachowanie odpowiada przekazaniu przez referencję.)

Dla wielowymiarowej tablicy np. int [][] x = new int[][] [4][4], wartością x jest wskaźnik do tablicy wskaźników na tablice typu int, x[0] to wskaźnik na tablicę typu int, x[0][0] to wartość typu int.

Program to lista instrukcji, które wykonywane są po kolei. Funkcje są definiowane jak w c, przy czym
w cimex nie ma dostępnego typu void. Każda funkcja zwraca jakąś wartość. W przypadku pominięcia "return" w ciele funkcji, zwracana jest
domyślna wartość dla danego typu (int 0, bool false, string "", array (array pointer) -1).

Funkcje zezwalają na rekurencję, ale kolejność deklaracji funkcji ma znaczenie.

Instrukcje: pusta, blok instrukcji (lista instrukcji), if, if else, przypisanie, return, while, break, continue, deklaracja funkcji, deklaracja/definicja zmiennej, wyrażenie - podobnie do c.

Zmienne muszą być zadeklarowane przed użyciem, opcjonalnie mogą być zainicjalizowane - jeśli to się nie stanie zostaną zainicjalizowane z wartością domyślną (opisane wyżej przy pominięciu return).
Obowiązują typowe zasady widoczności - zmienne zadeklarowane w bloku nie są widoczne poza nim i przesłaniają zmienne o tej samej nazwie spoza bloku. W obrębie bloku zmienne muszą mieć unikalne nazwy (dokładnie to samo tyczy się funkcji).

Ogólnie można patrzec na ten język jak na Latte z tablicami, obsługą wcięć i brakiem typu void.

# Negocjacje
Jeśli coś jest zbyt ubogie i nie zasługuje na opisaną ilość punktów - np. brak wzajemnej rekurencji, jestem gotów dodać do języka pętlę foreach.

# Nietypowe konstrukcje
Deklaracje i tworzenie tablic:
```
int[][] x
int[][] y = new int[][] [5][6] // tablica o wymiarach 5x6 elementów typu int
```

Obsługa wcięć: 

Taki kawałek kodu
```
int func(int x, int y):
    x = 2 * x
    return x + y

print(func(1,2))
```
jest równoznaczny z poniższym:
```
int func (int x, int y) :
{
  x = 2 * x;
  return x + y
};
print (func (1, 2))
```
(Dzięki temu można pisać kod ze znaną z języka c składnią, ale
 można pominąć nawiasy klamrowe)

# Gramatyka
Gramatyka w LBNF znajduje się w pliku Cimex.cf, jest to zmodyfikowana gramatyka języka Latte.

Główne zmiany to layout i tablice (i pętla if, przez użycie layoutu).

# Tabelka i przykładowe programy:

```
  Na 15 punktów
  01 (trzy typy) +
  02 (literały, arytmetyka, porównania) +
  03 (zmienne, przypisanie) +
  04 (print) +
  05 (while, if) +
  06 (funkcje lub procedury, rekurencja) + FUNKCJE rekurencyjne (bez procedur)
  07 (przez zmienną / przez wartość / in/out) + ZMIENNA/WARTOŚĆ
  08 (zmienne read-only i pętla for) -
  Na 20 punktów
  09 (przesłanianie i statyczne wiązanie) + 
  10 (obsługa błędów wykonania) +
  11 (funkcje zwracające wartość) +
  Na 30 punktów
  12 (4) (statyczne typowanie) +
  13 (2) (funkcje zagnieżdżone ze statycznym wiązaniem) +
  14 (1/2) (rekordy/listy/tablice/tablice wielowymiarowe) + 2 TABLICE WIELOWYMIAROWE
  15 (2) (krotki z przypisaniem) -
  16 (1) (break, continue) + 
  17 (4) (funkcje wyższego rzędu, anonimowe, domknięcia) -
  18 (3) (generatory) -

Razem: 29
```

## Programy:

1. (01, 02, 03, 04, 05, 16)
```
int x = 0, y = 4
bool b = true
string s = "Ala ma kota"

while (y > x):
    x = x + 1
    y = y - 1
    if (b) b = !b; endif
    if (!b):
        break
    endif

print(x)
print(s)
print(b)
```

2. (06, 07, 09, 11, 13)
```
int x = 20, y = 15, z = 3

int f(int &q):
    q = q + x
    return q

int g(int p):
    int x = 10
    bool h():
        return (x == 10)

    print(h())
    x = 11
    print(h())
    return p + x

print(z) // will print 3
f(z)
print(z) // will printt 23

// next code will show on output the following things (in this order)
// true
// false
// 26
// 20

print(g(y))
print(x)
```

3. (14)
```
bool[] t = new bool [] [2] // false filled array of length 2
int[][] ints = new int [][] [2][4] // 0 filled array with dimensions 2x4
int[][] ints2

t[1] = true

int add_one(int [][] arr, int x, int y):
    int f = 0
    int s = 0
    while (f < x):
        s = 0
        while (s < y):
            arr[f][s] = arr[f][s] + 1
            s = s + 1
        f = f + 1

add_one(ints, 2, 4)

// now ints will be full of ones
print(ints[0][0])
ints2 = ints
ints2[1][1] = 100

// will print 100
print(ints[1][1])
```

4. Rekurencja
```
int factorial(int n):
    if (n <= 1):
        return n
    endif
    return (n * factorial(n - 1))

print(factorial(5))
```

5. Błędy wykonania
    - statyczne: 
    ```
    int factorial(int n):
    if (n <= 1):
        return n
    endif
    return (n * factorial(n - 1))

    bool x

    factorial(x)
    ```
    Program zostanie zatrzymany przez statyczny typecheck z komunikatem np. "Failure: Argument type does not match parameter type at: 9:1"
    ```
    int x
    int y = 5

    int x = 5
    ```
    Przykładowy błąd - 
    "Failure: Cannot redeclare a variable in the same block 4:5"
    - dynamiczne: 
    ```
    int[] a = new int[] [5]

    a[5]
    ```
    Przykładowy błąd - "Runtime error - access past end of array at (3,1)"