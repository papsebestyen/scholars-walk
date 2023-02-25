rm(list = ls())
gc()

setwd("C:/Users/PSebi/Desktop/r_project")

library(dplyr)
library(stringr)
library(tidyverse)

# Az egész kód az alábbi paper replikációja a Rajk Szakkollégium kurzusfelvételi adatbázisán
# https://files.eric.ed.gov/fulltext/ED599254.pdf

# Read data ---------------------------------------------------------------

# Adatok beolvasása UTF-8 kódolásban. A leadott adatban a nevek hashelve vannak biztonsági okokból.
course_data <-
  read_csv(
    "course_data_hashed.csv",
    locale = locale(encoding = "UTF8"),
  )

# Data wrangling ----------------------------------------------------------

course_data <- course_data %>%
  mutate(
    # Év-szemeszter változó létrehozása
    year_semester = paste(
      str_replace(academic_year, '/', '_'),
      ifelse(semester == 'ősz', 1, 2),
      sep = '_'
    ),
    # Szemeszterek reprezentálása számmal
    t = dense_rank(year_semester),
  ) %>%
  # Sorbarendezés név és szemeszter alapján
  arrange(name, year_semester)

course_data <- course_data %>%
  # Csak olyan kurzusokat vizsgálok, amelyek legalább két különböző szemeszterben elindultak
  group_by(course) %>%
  filter(length(unique(t)) >= 2) %>%
  ungroup() %>%
  # Csak a teljesített kurzusokat fogom modellezni
  filter(grade %in% c("Megfelelt", "Kiválóan megfelelt"))

# Graph edge data extraction ----------------------------------------------

get_edges <- function(df) {
  # Ez a függvény a kurzus-kurzus élek kinyerésére szolgál
  new_data <- NULL
  # Végig iterál az utolsó előtti szemeszterig
  for (r in min(df$t):max(df$t) - 1) {
    # A következő félévben felvett kurzusok vektora
    next_course_vec <-
      rename(df[df$t == r + 1, "course"], next_course = course)
    # Descartes szorzat a t és t+1 szemeszterben felvett kurzusok alapján
    # Az éleket a new_data mátrixhoz hozzáadja
    new_data <-
      rbind(new_data, crossing(df[df$t == r, ], next_course_vec))
  }
  return(new_data)
}

# Kurzus-kurzus él adatok kiszámolása
course_edge_data <- course_data %>%
  group_by(name) %>%
  group_modify(~ get_edges(.x))

# Matrix and vector utility functions -------------------------------------

make_sq_matrix <- function(m) {
  # Ez a függvény egy nem négyzetes mátrixból csinál négyzetes mátrixot úgy,
  # hogy a nevek uniója alapján mindent sort és oszlopot kiegészít ezekkel a dimenziókkal.
  # Az eddig nem létezett (hiányzó) értékeket 0-val tölti fel.
  
  # A sor és oszlopnevek uniója sorba rendezve
  all_names <- sort(union(rownames(m), colnames(m)))
  
  # Készít egy üres (0-val teli) mátrixot
  sq_m <-
    matrix(
      0,
      nrow = length(all_names),
      ncol = length(all_names),
      dimnames = list(all_names, all_names)
    )
  # Az eredeti mátrix értékeivel feltölti az új mátrixot
  sq_m[rownames(m), colnames(m)] <- m
  return(sq_m)
}

fill_cols <- function(m, cols, value = 0) {
  # Ez a függvény egy eredeti mátrix oszlopait kiegészíti a megadott
  # oszlopnevek alapján. Ahol nem volt korábban adat, ott a value paraméter
  # értékei fognak szerepelni.
  
  # Új mátrix létrehozása
  nm <-
    matrix(
      value,
      nrow = dim(m)[1],
      ncol = length(cols),
      dimnames = list(rownames(m), cols)
    )
  
  # Régi mátrix értékeinek beemelése
  nm[rownames(m), colnames(m)] <- m
  return(nm)
}

fill_names <- function(v, v_names, values = 0) {
  # Ez a függvény egy eredeti vektor neveit kiegészíti a megadott
  # nevek alapján. Ahol nem volt korábban adat, ott a value paraméter
  # értékei fognak szerepelni.
  
  # Új üres vektor
  nv <- rep(values, length(v_names))
  # Nevek megadása
  names(nv) <- v_names
  # Régi vektor értékeinek beemelése
  nv[names(v)] <- v
  return(nv)
}

get_crossm <- function(formula, data) {
  # Ez a függvény egy nevekkel ellátot kereszttáblát hoz létre egy mátrix objektumban
  
  # Kereszttábla létrehozása
  xtab <- xtabs(formula = formula, data = data)
  
  # Átalakítás mátrixá
  m <- as.matrix.data.frame(xtab)
  
  # Sor és oszlopnevek megadása
  dimnames(m) <-
    list(rownames(xtab),
         colnames(xtab))
  
  # Sorok és oszlopok rendezése névsorrend alapján
  m <- m[sort(rownames(m)), sort(colnames(m))]
  return(m)
}

# Transition matrix extraction --------------------------------------------

get_trans_m <- function(edge_data, t_range = NULL) {
  # Ez a függvény hozza létre az átmenet mátrixot egy adott időszakra (t_range) nézve
  
  # Ha megvan adva időszak, akkor rászűr az adatban
  if (!is.null(t_range)) {
    edge_data <- filter(edge_data, t %in% t_range)
  }
  
  # Kiszámolja a kereszttáblát a t és t+1 időszaki kurzusokra, majd átalakítja négyzetes mátrixá
  trans_m <-
    get_crossm( ~ course + next_course, data = edge_data) %>%
    make_sq_matrix()
  
  # Azoknak a kurzusoknak a korrigálása, amelyekből nem indulnak ki élek
  # Ezen csúcsok megtalálása
  end_nodes <- apply(trans_m, 1, sum) == 0
  # A csúcsok kiegészítése egyesekkel
  trans_m[end_nodes, end_nodes] <- diag(sum(end_nodes))
  
  # Leosztás a sorösszegekkel, hogy soronként 1 legyen az összeg. 
  # Így lesznek valószínűségi értékek
  trans_m <- trans_m / apply(trans_m, 1, sum)
  
  return(trans_m)
}

# Course popularity functions ---------------------------------------------

get_course_pop <- function(data, t_range = NULL) {
  # Ez a függvény kiszámolja egy adott időszak alatt (t_range) a kurzusok népszerűségét
  
  # Az adott időszak alatti kereszttábla a részvevők és a kurzusok között
  pop_m <- data %>%
    filter(t %in% t_range) %>%
    get_crossm(~ course + name, data = .)
  
  # Sorösszegek (összes kurzus felvétel) elosztva az adott időszak hallgatóinak számával
  pop_vec <- apply(pop_m, 1, sum) / dim(pop_m)[2]
  # pop_vec <- replace(pop_vec, pop_vec == 0, 1e-99)
  return(pop_vec)
}

# Course history functions ------------------------------------------------

get_history_matrix <- function(course_data, t_semester) {
  # Ez a függvény tanulónként kiszámolja egy adott 
  # évig (t_semester) felvett kurzusokat mátrix formában.
  
  m <- course_data %>%
    # Szűrés az időszakra
    subset(t < t_semester) %>%
    # Kereszttávla a név és kurzusok alapján
    get_crossm( ~ name + course, data = .)
  
  return(m)
}

get_history <- function(m_hist, student) {
  # Ez a függvény egy előzmény mátrixból kiszedi 
  # egy adott tanuló korábban felvett kurzusait.
  
  if (student %in% rownames(m_hist)) {
    # Ha vett fel korábban kurzusokat, akkor visszaadja azok nevét,
    # amelyet legalább 1-szer felvett.
    history <- m_hist[student, ]
    return(names(subset(history, history >= 1)))
  } else {
    # Ha nem vett fel, akkor egy üres vektort ad vissza
    return(c())
  }
}

subset_history <- function(vec, history) {
  # Ez a függvény a korábbi kurzusoktól eltérőrő értékeketet
  # szűri ki egy vektorból nevek alapján.
  valid <- setdiff(names(vec), history)
  return(vec[valid])
}

# Personality vector functions --------------------------------------------

get_person_courses <- function(data, semester_t) {
  # Ez a függvény az adott évi (semester_t) felvett kurzusok mátrixát adja vissza.
  person_m <- data %>%
    # Szűrés az adott szemeszterre
    filter(t == semester_t) %>%
    # Kereszttábla a név és kurzus alapján
    get_crossm( ~ name + course, data = .)
  return(person_m)
}

get_state_m <- function(data, semester_t){
  # Ez a függvény személy vektorokat ad vissza.
  
  # Először kiszámolja a felvett kurzusok mátrixát
  person_m <- get_person_courses(data = data, semester_t = semester_t)
  # Majd leosztja az adott évben felvett kruzusok számával személyenként (sorösszegek)
  person_m <- person_m / apply(person_m, 1, sum)
  
  return(person_m)
}

# Scholars Walk algorithm -------------------------------------------------

scholars_walk <-
  function(model, u, alpha, beta, K, pop, tol = 10 ^ -10) {
    # Ez a függvény a Scholars Walk agoritmust alapján 
    # számolja ki a kurzus valószínűségeket egy tanulóra.
    
    # Iterációk száma kezdetben 0
    k <- 0
    # Mátrix ami tárolja majd a valószínűségeket iterációnként
    p <- matrix(u, nrow = 1, ncol = length(u))
    repeat {
      # Iterációk számának növelése
      k <- k + 1
      
      # Kiszámolja az átmenet valószínűségeket, illetve hozzáadja 
      # az ujraindítás értékeit is bizonyos arányban (1-alpha)
      # Az új értékeket hozzáadja az eredmény mátrixhoz
      p <- rbind(p, alpha * p[k, ] %*% model + (1 - alpha) * u)
      
      # Normalizálja az utolsó eredmény a sorösszeggel való leosztással
      p[k + 1,] <-
        p[k + 1,] / norm(as.matrix(p[k + 1,]), type = '1')
      
      # Ha az iterációk száma nagyobb mint a paraméterben megadott, vagy 
      # ha nagyon keveset változott a valószínűségi vektor, akkor megáll 
      # az iterációkban.
      if ((norm(p[k + 1,] - p[k,], type = '2') < tol) | (k > K)) {
        break
      }
    }
    
    # Végül pedig bünteti a népszerű kurzusokat beta alapján
    p[k + 1, ] <- p[k + 1, ] * (pop ^ -beta)
    return(p[k + 1, ])
  }

# Prediction and recommendation functions ---------------------------------

get_recommendation <- function(v_prob, history, n_course) {
  # Ez a függvény a valószínűségi vektor alapján kiszámolja az adott számú (n_course)
  # ajánlásokat úgy, hogy abban ne legyenek benne a korábban felvett kurzusok.
  
  recom <- v_prob %>%
    # Korábbi kurzusok kiszedése
    subset_history(history = history) %>%
    # Sorbarendezés
    sort(decreasing = T) %>%
    # A megfelelő számú ajánlás kinyerése
    head(n_course)
  
  # Úgy adja vissza az értékeket, hogy a felvett kurzusok mellett 1 szerepeljen
  return(recom * 0 + 1)
}

calc_recall <- function(predicted, target) {
  # Ez a függvény kiszámolja a klasszifikációhoz tartozó recall értéket
  # a cél és prediktált adatot alapján.
  
  # Csak azokat a kurzusokat veszi figyelembe amelyek indultak az adott félévben,
  # illetve azokat az embereket veszi figyelembe, akik vettek fel kurzust.
  v_student <- intersect(rownames(predicted), rownames(target))
  v_course <- intersect(colnames(predicted), colnames(target))
  v_preds <-  predicted[v_student, v_course]
  v_target <- target[v_student, v_course]
  
  # A cél és prediktált mátrixot szorzata ott lesz csak 1, ahol mindkét mátrix 
  # 1-es értéket vett fel. Ezek lesznek a eltalált kurzusok.
  # Az eltalált kurzusokat leosztva a ténylegesen felvett kurzusok számával
  # kiszámolható a recall értéke is. Ezt a számot tanulónként kiátlagolom.
  recall <-
    mean(apply(v_target * v_preds, 1, sum) / apply(v_target, 1, sum))
  return(recall)
}

# Training and cross validation -------------------------------------------

cross_validate <- function(training_window, alpha, beta, K) {
  # Ez a függvény a cross validációt csinálja, visszatérési értéke az átlagos recall.
  
  # Ez a vektor tartalmazza a recall score-okat minden mintán
  train_score <- NULL
  
  # Iterálás az időablaknak megfelelően
  for (semester_t in seq(training_window, max(course_data$t) - 1)) {
    
    # Az időablak
    t_range <- seq(semester_t - training_window + 1, semester_t)
    
    # Átmenet mátrix az adott időablakon
    trans_m <-
      get_trans_m(edge_data = course_edge_data, t_range = t_range)
    
    # Kurzus nevek vektora
    course_names <- rownames(trans_m)
    
    # A szemeszternek megfelelő személy vektor az összes névvel
    state_m <-
      get_state_m(data = course_data, semester_t = semester_t) %>%
      fill_cols(m = .,
                cols = course_names,
                value = 0)
    
    # Kurzusok népszerűségét tartalmazó vektor
    course_pop <-
      get_course_pop(course_data, t_range = t_range) %>%
      fill_names(v = .,
                 v_names = course_names,
                 values = 1e-99)
    
    # A ténylegesen felvett kurzusok
    target <-
      get_person_courses(data = course_data, semester_t = semester_t + 1)
    
    # A tanulmányi előzmény mátrix
    stud_history <- get_history_matrix(course_data, semester_t)
    
    preds <- NULL
    pred_studs <- NULL
    
    # Végig iterál minden diákon
    for (student in rownames(target)) {
      
      # Ha az előző szemeszterben vett fel kurzust
      if (student %in% rownames(state_m)) {
        
        # Predikció az adott paraméterek mellett
        p_rec <- scholars_walk(
          model = trans_m,
          u = state_m[student,],
          alpha = alpha,
          beta = beta,
          K = K,
          pop = course_pop
        )
        
        # Ajánlás vektor a megfelelő nevekkel
        recom <-
          get_recommendation(
            v_prob = p_rec,
            history = get_history(stud_history, student = student),
            n_course = sum(target[student, ])
          ) %>%
          fill_names(v_names = course_names, values = 0)
        
        # Az adott diák ajánlásainak hozzáadása a többihez
        preds <- rbind(preds, recom)
        # Név hozzáadása a név vektorhoz (a kieső diákok miatt)
        pred_studs <- append(pred_studs, student)
      }
    }
    # Nevek hozzárendelése a sorokhoz
    rownames(preds) <- pred_studs
    
    # Recall kiszámítása és hozzáadása a korábbi mintákhoz
    recall <- calc_recall(preds, target)
    train_score <- append(train_score, recall)
  }
  # Az átlagos recall visszaadása
  return(mean(train_score))
}

# Hiperparaméter optimalizáció

# Gridek definiálása
grids <-
  list(
    alpha = c(
      1e-4,
      1e-3,
      1e-2,
      1e-1,
      0.2,
      0.4,
      0.6,
      0.7,
      0.8,
      0.85,
      0.9,
      0.99,
      0.999
    ),
    beta = seq(0, 0.8, 0.025),
    K = c(1, 3, 5, 100, 1000),
    training_window = c(5, 6, 7)
  )

# Paraméter tér megadása
param_space <-
  crossing(
    alpha = grids$alpha,
    beta = grids$beta,
    K = grids$K,
    training_window = grids$training_window
  )

# Minden paraméter kombinációt kipróbál
# Körülbelül 2 óráig fut
for (i in 1:nrow(param_space)) {
  alpha <- as.numeric(param_space[i, "alpha"])
  beta <- as.numeric(param_space[i, "beta"])
  K <- as.numeric(param_space[i, "K"])
  training_window <- as.numeric(param_space[i, "training_window"])
  
  # Logolás a folyamatról
  print(
    sprintf(
      "%s/%s iteration: alpha = %s, beta = %s, K = %s, training window = %s",
      i,
      nrow(param_space),
      alpha,
      beta,
      K,
      training_window
    )
  )
  
  # Átlagos recall kiszámítása (cross-validation)
  score <-
    cross_validate(
      training_window = training_window,
      alpha = alpha,
      beta = beta,
      K = K
    )
  
  # Recall hozzárendelése a paraméterekhez
  param_space[i, 'score'] <- score
}

# Legjobb 5 eset
param_space %>% 
  arrange(desc(score)) %>% 
  head(5)

# A legjobb modell a következő paraméterekkel volt:
#   alpha = 0.99
#   beta = 0
#   K = 5
#   training window = 5
# A recall ebben az esetben 0.204