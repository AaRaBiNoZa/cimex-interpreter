# Cimex
Jest to język imperatywny o składni podobnej do Latte/c, z największą różnicą polegającą na obsłudze wcięć jako bloków 
kodu i braku obowiązku używania średników (python like).

Dodatkowo zawiera wielowymiarowe tablice przekazywane przez zmienną.

Program to lista instrukcji, które wykonywane są po kolei.

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
Gramatyka w LBNF znajduje się w pliku Cimex.cf, jest to lekko zmodyfikowana gramatyka języka Latte.

Główne zmiany to layout i tablice.

# Tabelka i przykładowe programy:

```
  Na 15 punktów
  01 (trzy typy) +
  02 (literały, arytmetyka, porównania) +
  03 (zmienne, przypisanie) +
  04 (print) +
  05 (while, if) +
  06 (funkcje lub procedury, rekurencja) + FUNKCJE (bez procedur)
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
    if (b) b = !b
    if (!b):
        break

print(x)
print(s)
print(b)
```

2. (07, 09, 11, 13)
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
bool[] t = bool[] [2] // false filled array of length 2
int[][] ints = new int [][] [2][4] // 0 filled array with dimensions 2x4

t[1] = true

int add_one(int [][] arr, int x, int y):
    int f = 0, s = 0
    while (f < x):
        s = 0
        while (s < y):
            arr[x][y] = arr[x][y] + 1
            s = s + 1
        f = f + 1

add_one(ints, 2, 4)
// now ints will be full of ones
```