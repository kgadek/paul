;; Krótki wstęp:
;    W notacji infiksowej: a + b
;    W notacji prefiksowej: + a b
; Otoczmy to drugie nawiasami: (+ a b) i mamy wyrażenie w CL. Można pisać (+ 1 2 3 4 5). Tak samo:
; (if warunek instrukcje-true instrukcje-false)
; Jeśli chcemy w if dodać więcej instrukcji np do true to zwykle używamy zwrotu (progn ...),
; które jest odpowiednikiem begin ... end z pascala lub { ... } z C/C++.
; By zapisać listę piszemy '(1 2 3). Ten pojedynczy cudzysłów (którego nie zamykamy!) mówi, że to
; jest lista, którą mamy wziąć dosłownie. Porównaj:
;  * '(if 1 2 3)  ; to jest po prostu lista
;  (IF 1 2 3)     ; ...mówiłem, że jeśli chodzi o takie rzeczy to CL jest case-insensitive?
;  * (if 1 2 3)   ; A to jest odpalenie if-a. 1 nie jest nullem więc jest prawdą więc
;  2              ; rezultatem jest 2
; Ten pojedynczy cudzysłów jest równoważny makru quote, tzn.: '(1 2 3) <=> (quote (1 2 3))
; Quote pozwala też pisać pojedyncze litery, np: 'a
; Oprócz tego listę można tworzyć używając funkcji (lub makra, nie pamiętam) list, np: (list 1 2 3)
; Wartość true to wszystko, co nie jest pustą listą. Pusta lista to oczywiście '()
; lub równoważnie NIL lub też (list)


; OK, LET'S BEGIN!



;>>>> #!/usr/bin/env python
;>>>> # -*- coding: utf-8
; Nagłówków nie ma; z kodowaniem są czasem problemy (gdy powstał CL to o unicode jeszcze nikt nie myślał).
; Nigdy nie twierdziłem, że CL jest idealny, jest raczej jak bardzo gibki dziadek -- dużo przeszedł,
; dużo umie, udowodnił, że jest potężny, ale nadal to jest dziadek... Z drugiej strony wnuczek
; Python nie umie wszystkiego, co umie CL; dziadek CL umie za to wszystko to, co Python (tylko musi się
; czasem nagimnastykować).



;>>>> def nextArithm(lst,d):
;>>>>     """ Zwraca następny element lst -- ciągu arytmetycznego o różnicy r. """
;>>>>     return lst[-1]+d

(defun nextArithm (lst d) ; deklaracja funkcji jest b. podobna.
                          ; UWAGA: zauważ, że w CL elementy listy *nie* są oddzielone przecinkami!
  ; pierwszy haczyk: funkcja last zwraca listę! Pustą gddy argument jest pustą listą, oraz
  ; zawierającą pojedynczy element w przeciwnym przypadku.
  (+ (first (last lst))
     d)) ; Ach, no i bardzo ważna rzecz -- edytor podświetlający nawiasy. Myślisz, że ktokolwiek je liczy?
         ; Nope! Poza tym zauważ wcięcia; to one powodują, że nikt nigdy nawiasów nie liczy bo "wszystko widać".
         ; Tak tak, widać! Kwestia przyzwyczajenia.
  ; No i jest tak jak w ocamlu -- ostatnie wyrażenie funkcji jest wartością zwracaną. W sumie to raczej
  ; w ocamlu jest jak w lispie... :P


;>>>> def isArithm(lst):
(defun isArithm (lst)
;>>>>     """ Sprawdza, czy podany ciąg jest arytmetyczny.
;>>>>     Zwraca różnicę ciągu. """
" Sprawdza, czy podany ciąg jest arytmetyczny.
Zwraca różnicę ciągu. "
;>>>>     if len(lst) <= 2: return lst and lst[0] or False
(if (<= (length lst)
	2)
    (or NIL (and lst
		 (first lst))) ; hmm nie widziałem, by w CL ktokolwiek używał takiego skrótu
                               ; no ale spróbujmy linijka w linijkę przetłumaczyć
    ; Zgodnie z tym, co mówiłem -- ostatnia wartość jest wartością zwracaną, dlatego reszta
    ; programu będzie w "bloku else". Użylibyśmy progn (bo będzie więcej instrukcji, ALE
    ; zaraz zdefiniujemy sobie zmienne -- sama deklaracja już nam utworzy blok więc progn
    ; byłoby nadmiarowe.
;>>>>     d = lst[1] - lst[0]
;>>>>     p = lst[0] - d
  (let* ( (d (- (second lst)
		(first lst)))
	  (p (- (first lst)
		d))
	  ) ;; WIEM, że to nie jest ładne. Za to jest proste: let* przyjmuje co najmniej dwa parametry:
            ;; listę definicji zmiennych oraz następnie instrukcje.
            ;; Definicja zmiennych jest oczywiście listą (wszystko jest listą w CL!) i ma postać
            ;; (zmienna wartość).
;>>>>     for x in lst:
    (loop for x in lst do ; strasznie lubię to makro (MAKRO!) -- jest bardzo rozbudowane i jego wewn.
	                  ; parser jest kompletny w sensie Turinga :D No ale mniejsza o to w tej chwili...
	  ; ...teraz jest mini problem zw. z pozbyciem się tego return. HM.
;>>>>         if x-p != d: return False
	  ; dobra, chciałem by było lispowo i ładnie funkcyjnie, ale tzw. "UJ z tym"
	  (if (not (equal (- x p)
			  d))
	      (return d)) ; Cztery rzeczy warte odnotowania:
	                  ;  1) ten return wychodzi z pętli loop bo wewnątrz niej się znajduje.
	                  ;     Szczegół implementacyjny: loop, funkcje, bloki warunkowe w większości
	                  ;     definiują blok, z którego można wyjść właśnie przy pomocy tego return.
                          ;     Znając te szczegóły można wychodzić z bloków zewnętrznych używając
	                  ;     bardziej rozbudowanej postaci return-from czy jakoś tak...
                          ;  2) if może przyjąć dwa argumenty -- po prostu nie ma "bloku else"
	                  ;  3) wartość zwrócona pętli jest wartością zwróconą funkcji, bo pętla
	                  ;     jest ostatnim wyrażeniem
	                  ;  4) CL jest prawdziwie wieloparadygmatowy. Można pisać funkcyjnie, można
	                  ;     pisać strukturalnie, można pisać obiektowo (o CLOS -- Common Lisp
	                  ;     Object System -- Korzycki wspomniał; od siebie dodam, że CLOS to
	                  ;     najpotężniejszy system obiektowy jaki znam a C# i Java mogą się schować.
	                  ;     Np. przy polimorfiźmie: C++ ma single-dispatch, Java -- używając tricku
	                  ;     -- ma double dispatch, CLOS ma po prostu multiple dispatch. PS. CLOS jest
	                  ;     napisany przy pomocy makr i nie jest zdefiniowany w samym języku ;P )
                          ; PS. Tak, wiem że na samym początku mogłem użyć return.
;>>>>         d = x-p
;>>>>         p = x
	  (setf d (- x p) ; setf przyjmuje pary: zmienna wartość...
		p x)      ; ...co w sumie jest kłamstwem, bo zamiast zmiennej może być wyrażenie ;]
;>>>>     return d
	  return d))))
                ; Primo: emacs mi podpowiedział w status-barze, który nawias zamykam. Ja serio nigdy
                ;  nie liczyłem nawiasów. I tak, to jest powód dla którego do CL używam emacsa, but
                ;  still VIM FOREVER! PS. Mówiłem, że emacs jest napisany w lispie?
                ; Secundo: tutaj kolejny powód, dla którego kocham makro (loop...). Popatrz na konstrukcję:
                ;    (loop for x in lst do ... return d)
                ;  ten teges potrafi dużo -- iterować po hashmapach, kolekcjonować wartości, etc etc.
                ;  Drobnostką jest słówko return które jest częścią pętli. Isn't that cool ?! A to
                ;  wszystko jest pisane makrami (sam język tego nie miał), dlatego tak bardzo wielbię
                ;  makra lispa.


;>>>> def isGeom(lst):
;>>>>     """ Sprawdza, czy podany ciąg jest geometryczny.
;>>>>     Zwraca iloraz ciągu. """
(defun isGeom (lst)
  "Sprawdza, czy podany ciąg jest geometryczny. Zwraca iloraz ciągu."
;>>>>     if len(lst) <= 2: return lst and lst[0] or False
  (if (<= (length lst)
	  2)
      (first lst) ; w sumie to tamto pythonowe można zapisać po prostu tak ;)
;>>>>     zrs = [i for i,x in enumerate(lst) if x == 0]
    (let* ((zrs (loop for i from 0 ; moje ukochane loop!
		      and x in lst
		      when (equal x 0)
		      collect i)))
;>>>>     if zrs and not (zrs == range(len(lst)) or zrs == range(len(lst))[1:]):
;>>>>         return False
;>>>>     if zrs: return 0
      (if zrs
	(let ((rng (loop for i from 0 and x in lst collect i))) ; wiem, że to jest dłuższe, ALE
	  (if (or (equal zrs rng)                               ; 1) zaraz sobie napiszę funkcję range
		  (equal zrs (rest rng)))                       ; 2) w pythonie dwa razy wołaliśmy range,
	      (return-from isGeom nil)                          ;    tutaj tylko jeden raz
	    (return-from isGeom 0)))                            ; 3) w pythonie lista[1:] kopiuje CAŁĄ listę,
	                                                        ;    tj. ma złożoność O(n). W CL ta operacja
	                                                        ;    trwa O(1) bo czemu niby miałaby trwać
	                                                        ;    dłużej? :P
	                                                        ; 4) tym razem użyłem czegoś takiego jak
	                                                        ;  return-from nazwaFunkcji wartość ;P
;>>>>     r = float(lst[1])/float(lst[0])
;>>>>     p = lst[0]/r
	(let* ((r (/ (second lst)
		     (first lst))) ; oczywiście da się robić let w let dowolną ilość razy.
	       (p (/ (first lst)   ; Aa i jeszcze odpowiedź na pytanie: czemu let* a nie let? To drugie też
		     r)))          ; istnieje, ale ma nieco inne działanie.
			       ; Na przykład, niech A=3 B=4
			       ; (let ((a 5) (b a)) ...) <-- to tworzy nowe zmienne, które oczywiście
                               ;                             przesłaniają stare A i B, natomiast
                               ;                             wartość B to będzie 3. Let korzysta ze
                               ;                             starych wiązań (tych spoza let) przy ustalaniu
                               ;                             wartości
                               ; (let* ((a 5) (b a)) ... ) <-- tutaj B jest ustalane na 5 -- przy ustalaniu
                               ;                               wartości let* korzysta także z tego, co sam
                               ;                               zdefiniował
;>>>>     for x in lst:
;>>>>         if float(x)/float(p) != r: return False
;>>>>         r = float(x)/float(p)
;>>>>         p = x
;>>>>     return r
	  (loop for x in lst do
		(when (not (equal (/ x p) ; kolejna ciekawostka: (* (/ 1 3) 3) daje dokładnie 1.
				  r))     ; jakim cudem? Bo CL posiada ułamki ;P 1/3 to dokładnie 1/3.
		  (return nil))           ; Jakby trzeba było mieć float to (float (/ 1 3)) no ale po co?
		(setf r (/ x p)
		      p x)
		return r)))))) ;; Może jest brzydziej niż w pythonie, ale jest szybciej asymptotycznie,
                               ;; szybciej praktycznie (kompilacja do natywnej binarki) i dokładniej.
                               ;; Jakby szybkość była kluczowa, to można jeszcze dopisać informacje
                               ;; o typach (np. przyjmij, że lst jest listą floatów) -- wtedy kompilator
                               ;; jeszcze mocniej zoptymalizuje binarkę.
                               ;; Pod tym względem CL bije Pythona na głowę ;) Pod względem czytelności
                               ;; nie no ale cóż... There is no such a thing like free sandwitch.



;>>>> def nextGeom(lst,r):
;>>>>     """ Zwraca następny element lst -- ciągu geometrycznego o ilorazie r. """
;>>>>     return lst[-1]*r
(defun nextGeom (lst r)
  "Zwraca następny element lst -- ciągu geometrycznego o ilorazie r"
  (* r (first (last lst)))) ; taa, dość dziwne, że last zwraca listę...



;>>>> def isFib(lst):
;>>>>     """ Stwierdza, czy lst jest ciągiem Fibonacciego (musi zaczynać się
;>>>>     od [1,1,2,3,5,...]). Zwraca parę -- dwa ostatnie wyrazy ciągu. """
;>>>>     if not lst: return False
;>>>>     if len(lst) <= 2:
;>>>>         if len(lst) == 1: return lst[0]==1 and 1 or False
;>>>>         return lst==[1,1] and [1,1] or False
;>>>>     d = lst[:2]
;>>>>     for i in lst[2:]:
;>>>>         if i != sum(d): return False
;>>>>         d = [d[1], i]
;>>>>     return d
;>>>> def nextFib(lst,d):
;>>>>     """ Zwraca następny element lst -- ciągu Fibonacciego o dwóch poprzednich
;>>>>     elementach d. """
;>>>>     return sum(d)
;>>>> def isMFib(lst):
;>>>>     """ Stwierdza, czy lst jest metaciągiem Fibonacciego (musi zaczynać się
;>>>>     od [1,2,2,4,8,32,...]). Zwraca parę -- dwa ostatnie wyrazy ciągu. """
;>>>>     if not lst: return False
;>>>>     if len(lst) <= 2:
;>>>>         if len(lst) == 1: return lst[0]==1 and 1 or False
;>>>>         return lst==[1,1] and [1,2] or False
;>>>>     d = lst[:2]
;>>>>     for i in lst[2:]:
;>>>>         if i != d[0]*d[1]: return False
;>>>>         d = [d[1], i]
;>>>>     return d
;>>>> def nextMFib(lst,d):
;>>>>     """ Zwraca następny element lst -- metaciągu Fibonacciego o dwóch poprzednich
;>>>>     elementach d. """
;>>>>     return d[0]*d[1]
;>>>> def diffSub(lst):
;>>>>     """ Zwraca listę pochodnych ciągu lst. """
;>>>>     res = []
;>>>>     p = lst[0]
;>>>>     for x in lst[1:]:
;>>>>         res.append(x-p)
;>>>>         p = x
;>>>>     return res
;>>>> def diffDiv(lst):
;>>>>     """ Zwraca listę ilorazów par ciągu lst lub False gdy lista zawiera zero. """
;>>>>     if [i for i in lst if i==0]: return False
;>>>>     res = []
;>>>>     p = lst[0]
;>>>>     for x in lst[1:]:
;>>>>         res.append(x/p)
;>>>>         p = x
;>>>>     return res
;>>>> 
;>>>> testers = (isArithm, isGeom, isFib, isMFib)
;>>>> resulters = (nextArithm, nextGeom, nextFib, nextMFib)
;>>>> 
;>>>> differs = (diffSub, diffDiv)
;>>>> dediffers = (nextArithm, nextGeom)
;>>>> 
;>>>> def dfs(lst,lvl=7):
;>>>>     """ Zgaduje następny element ciągu lst. """
;>>>>     if lvl < 1 or not lst: return False
;>>>>     for i,f in enumerate(testers):
;>>>>         res = f(lst)
;>>>>         if res: return resulters[i](lst,res)
;>>>>     for i,d in enumerate(differs):
;>>>>         res = dfs(d(lst), lvl-1)
;>>>>         if res: return dediffers[i](lst,res)
;>>>>     return False
;>>>> 
;>>>> if __name__ == '__main__':
;>>>>     print dfs(input())
