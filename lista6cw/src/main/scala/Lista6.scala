object Lista6 extends App {
  //Task 1
  //przekazanie przez nazwe
  def whileLoop(condition: => Boolean)(expression: => Unit): Unit = {
    if (condition) {
      expression
      whileLoop(condition)(expression)
    }
  }

  //test
  var counter = 0;
  whileLoop(counter < 3)(
    {
      println(counter)
      counter = counter + 1
    }
  )

  //Task2
  // Swap
  def swap[A](xs: Array[A], i: Int, j: Int) = {
    val temp = xs(i);
    xs(i) = xs(j);
    xs(j) = temp;
  }

  val xs = Array(1, 2, 3, 4, 5);
  swap(xs, 0, 1)
  //  println(xs.toList);

  //tablica do psotrotowania; m,n ideksy elementow stnowiacych poczatek i koniec podtablicy. Zwraca element rodzielajacy pivot danej podtablicy(element srodkowy w naszym przypadku)
  def choosePivot[A](tab: Array[A], m: Int, n: Int) = tab((m + n) / 2);

  //tablica do psotrotowania; m,n ideksy elementow stnowiacych poczatek i koniec podtablicy
  //Jej zadaniem jest  ustawienie elementów tablicy mniejszych od pivota przed nim, a większych od niego  po nim.
  //Zmiennej i przypisywany jest indeks elementu większego od pivota, który  znajduje się przed nim (czyli po niewłaściwej stronie),
  // a zmiennej j indeks  elementu mniejszego od pivota, który znajduje się za nim (czyli także po  niewłaściwej stronie).
  // Poszukiwania elementów większych od pivota zaczynają się od  pierwszego elementu tablicy, a elementów mniejszych od niego od końca tablicy.
  // Elementy te są zamieniane miejscami. Takie zamiany powtarzają się dopóki elementy  nie będą właściwie ustawione (zgodnie z zamierzeniem)
  // czyli dopóki indeks i jest  mniejszy lub równy indeksowi j. Na koniec zwracana jest para (i, j), na podstawie  której można podzielić tablicę tab[l..r]
  // na podtablice: tab[l..j] zawierającą  elementy mniejsze od pivota i tab[i..r] zawierającą elementy większe od pivota.
  def partition(tab: Array[Int], l: Int, r: Int): (Int, Int) = {
    var i = l
    var j = r
    val pivot = choosePivot(tab, l, r)
    while (i <= j) {
      while (tab(i) < pivot) i += 1
      while (pivot < tab(j)) j -= 1
      if (i <= j) {
        swap(tab, i, j)
        i += 1
        j -= 1
      }
    }
    (i, j)
  }


  //Najpierw jest sprawdzana poprawność indeksu  początkowego i końcowego tablicy – ten pierwszy musi oczywiście być mniejszy od  tego drugiego.
  // Następnie indeksy są podane poprawnie to wartości (i,j) jest  przypisany wynik funkcji partition wywołanej dla argumentów, które przekazano
  // funkcji quick. Następuje sprawdzenie która z podtablic (utworzonych na podstawie  wyników funkcji partition – patrz opis do tej funkcji)
  // jest krótsza i ta tablica  jest sortowana rekurencyjnie w pierwszej kolejności, w drugiej kolejności jest  sortowana tablica dłuższa
  // (usprawnienie polegające na tym, by
  // żądanie sortowania  dłuższej tablicy odkładać na stos, dzięki czemu wielkość stosu jest ograniczana do  lg n).
  def quick(tab: Array[Int], l: Int, r: Int): Unit =
    if (l < r) {
      val (i, j) = partition(tab, l, r)
      if ((j - l) < (r - i)) {
        quick(tab, l, j)
        quick(tab, i, r)
      } else {
        quick(tab, i, r)
        quick(tab, l, j)
      }
    } else ()

  //test
  var arr: Array[Int] = Array(1, 2, 4, 3, 13, 45, 6);
  def quickSort(tab: Array[Int]) = quick(tab, 0, tab.length - 1);
  quickSort(arr);
  println(arr.toList)
}


