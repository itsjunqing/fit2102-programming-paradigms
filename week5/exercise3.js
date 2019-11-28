const students = ['Valentino Dalton','Hayden Walton','Jane Bryant','Ronald Hayes','Journey Bradshaw','Matias Guzman','Jaylah Hunt','Dangelo Russell','Giovani Hendricks','Faith Tapia','Jonas Foster','Perla Palmer','Sara Ferrell','Ezequiel Choi','David Spencer','Chaim Kennedy','Ty Mccann','Noelle Moses','Shayna Contreras','Lorelai Thornton','Brianna Hernandez','Keely Rojas','Gemma Casey','Elaine Merritt','Santino Estrada','Dayana Roach','Roberto Hubbard','Lucille Matthews','Sariah Gaines','Marisa Sharp','Nancy Odom','Gerald Greene','Julie Gay','Beatrice Wallace','Carly York','Shirley Reid','Grant Wood','Angelo Ferguson','Gianni Camacho','Kenya Rivers','Peyton Roman','Leonard Flores','Lincoln French','Ryland Evans','Elsie Yoder','Kristian Ochoa','Clarence Benitez','Kole Banks','Jeffrey Heath','Nasir Davenport','Grady Nielsen','Zayden Parrish','Christine Blackwell','Javon Garrett','Kaydence Phelps','Uriel Garcia','Antwan Park','Sharon Rangel','Nelson Gutierrez','Gilbert Carroll','Ashlynn Small','Jordyn Owens','Lilah Obrien','Nyla Crane','Jillian Becker','Wyatt Vaughan','Christian Oconnor','Alexandra Francis','Amari Hebert','Richard Peterson','Fisher Keith','Carlee Galloway','Pablo Compton','Gabriella Horn','Sylvia Leblanc','Serena Berry','Uriah Jensen','Cesar Gilbert','Amelie Cooley','Elianna Cross','Selena Shaw','Leon Ramsey','Salma Reese','Cameron Valenzuela','Madilynn Daugherty','Judith Castro','Tia Stokes','Martin Ritter','Jameson Bowen','Ramon Ewing','Jorge Austin','Precious Walsh','Timothy Scott','Laurel Lang','Addison Mayo','Abigayle Walker','Aimee Pham','Adam Harris','Mckinley Gilmore','Devon Cantrell']
const marks = ['84.51','42.85','57.03','52.99','65.39','35.57','11.88','61.11','61.70','56.75','29.37','57.52','29.91','23.82','65.98','66.19','41.97','49.83','89.81','73.43','65.34','36.04','26.93','69.62','62.29','100.00','64.56','26.02','37.39','72.88','76.87','11.56','64.37','54.36','37.90','53.10','30.05','7.23','62.38','66.83','34.46','47.67','52.64','47.11','42.91','37.53','40.05','58.42','67.54','54.26','27.43','64.56','43.99','33.41','55.36','30.65','46.18','9.50','30.74','42.86','78.97','63.15','42.26','43.44','53.71','48.35','8.16','59.47','76.78','27.44','59.80','48.85','78.54','46.37','47.15','18.98','48.49','6.73','51.98','52.87','43.16','30.64','0.00','69.87','50.71','72.59','28.07','40.23','7.15','53.49','61.51','10.91','41.24','59.17','44.35','42.08','50.44','14.68','20.82','31.30']
const 
    I = x=> x,
    K = x=> y=> x,
    cons = x=> y=> f=> f(x)(y),
    head = l=> l(K),
    tail = l=> l(K(I)),
    fold = f=> i=> l=> l?fold(f)(f(i)(head(l)))(tail(l)):i,
    flip = f=>a=>b=>f(b)(a),
    reverse = fold(flip(cons))(undefined),
    forEach = f=>l=>fold(K(I(f)))(undefined)(l),
    map = f=> l=> l?cons(f(head(l)))(map(f)(tail(l))):undefined,
    fromArray = a=> reverse(a.reduce((c,v)=>cons(v)(c), undefined)),
    concat = a=> b=> a?cons(head(a))(concat(tail(a))(b)):b,
    zip = f => a => b => 
        a && b 
            ? cons(f(head(a))(head(b)))(zip(f)(tail(a))(tail(b))) 
            : undefined,
    filter = f => l => fold(l=>x=>f(x)?cons(x)(l):l)(undefined)(l),
    sort = f => l => !l ? undefined 
        : ((h,r)=>
            concat
            (sort(f)(filter(x=>f(x)(h))(r)))
            (cons(h)(sort(f)(filter(x=>!f(x)(h))(r))))
        )(head(l),tail(l)),
  compose = f => g => x => f(g(x)),
  name = ([name,_])=>name,
  mark = ([_,mark])=>mark,
  print = forEach(console.log);

// use the functions above to find the 
// name of the student who passed with the lowest mark
// const lissie = zip(x=>y=>[x,y])(fromArray(students))(fromArray(marks));
// const flittie = filter(x=> mark(x) >= 50)(myList);
// const sortie = sort(a => b => a < b)(myFlit);

print(sort(x => y => x < y)(filter(x=> mark(x) >= 50)(zip(x=>y=>[x,y])(fromArray(students))(fromArray(marks)))));











