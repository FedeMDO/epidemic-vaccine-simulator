globals
[
  nb-infected-previous ;; Número de personas infectadas en el tick anterior
  border               ;; Los parches que representan el borde amarillo.
  angle                ;; Heading for individuals FIXME *** entender uso, sospecho que es para que la persona "encare" una direccion u otra dependiendo del continente *** 
  beta-n               ;; El número medio de nuevas infecciones secundarias por infectado en este tick
  gamma                ;; El número medio de nuevas recuperaciones por infectado en este tick
  r0                   ;; El número de infecciones secundarias que surgen debido a un único infeccioso introducido en una población totalmente susceptible.
]

;; personas
turtles-own 
[
  susceptible?         ;; Realiza un seguimiento de si la persona era inicialmente susceptible
  infected?            ;; Si es true, la persona está infectada.
  cured?               ;; Si es true, la persona ha vivido una infección. No se pueden volver a infectar.
  inoculated?          ;; Si es true, la persona ha sido vacunada.
  isolated?            ;; Si es true, la persona está aislada y no puede infectar a nadie.
  hospitalized?        ;; Si es true, la persona está hospitalizada y se recuperará en la mitad del tiempo promedio de recuperación.
  ambulance?           ;; Si es true, la persona es una ambulancia y transportará a las personas infectadas al hospital.

  infection-length     ;; Cuánto tiempo ha estado infectada la persona.
  recovery-time        ;; Tiempo (en horas) que tarda la persona en recuperarse de la infección.
  isolation-tendency   ;; Posibilidad de que la persona se ponga en cuarentena durante cualquier hora en la que esté infectada.
  hospital-going-tendency ;; Posibilidad de que una persona infectada vaya al hospital cuando esté infectada

  continent            ;; En qué continente vive una persona, las personas en el continente 1 son cuadrados, las personas en el continente 2 son círculos.

  nb-infected          ;; Número de infecciones secundarias causadas por una persona infectada en este tick
  nb-recovered         ;; Número de personas recuperadas en este tick
]

;;;
;;; SETUP PROCEDURES
;;;

to setup
  clear-all
  setup-globals
  setup-people
  setup-ambulance
  reset-ticks
end

to setup-globals
  ask patch (- max-pxcor / 2 ) 0 [ set pcolor white ]
  ask patch (max-pxcor / 2 ) 0 [ set pcolor white ]

  set border patches with [(pxcor =  0 and abs (pycor) >= 0)]
  ask border [ set pcolor yellow ]
end

;; Cree el número de personas inicial.
;; Los que viven a la izquierda son cuadrados; los de la derecha, círculos.
to setup-people
  create-turtles initial-people
    [ setxy random-xcor random-ycor
      ifelse xcor <= 0
      [ set continent 1 ]
      [ set continent 2 ]

      set cured? false
      set isolated? false
      set hospitalized? false
      set ambulance? false
      set infected? false
      set susceptible? true

      assign-tendency

      ifelse continent = 1
        [ set shape "square" ]
        [ set shape "circle" ]

      set size 0.5

      ;; Cada individuo tiene un 5% de probabilidad de comenzar infectado.
      if (random-float 100 < 5)
      [ set infected? true
        set susceptible? false
        set infection-length random recovery-time
      ]

      ifelse (not infected?) and (random-float 100 < inoculation-chance)
        [ set inoculated? true
          set susceptible? false ]
        [ set inoculated? false ]

      assign-color
      ]

    if links? [ make-network ]
end

to setup-ambulance
  create-turtles initial-ambulance
  [
    ifelse random 2 < 1
    [
      set continent 1
      setxy (- max-pxcor / 2) 0
    ]
    [
      set continent 2
      setxy (max-pxcor / 2) 0
    ]

    set cured? false
    set isolated? false
    set hospitalized? false
    set infected? false
    set inoculated? false
    set susceptible? false

    set ambulance? true

    set shape "person"
    set color yellow
  ]
end

to assign-tendency ;; Turtle procedure

  set isolation-tendency random-normal average-isolation-tendency average-isolation-tendency / 4
  set hospital-going-tendency random-normal average-hospital-going-tendency average-hospital-going-tendency / 4
  set recovery-time random-normal average-recovery-time average-recovery-time / 4

  ;; Asegúrese de que el tiempo de recuperación se encuentre entre 0 y 2 veces el tiempo de recuperación promedio
  if recovery-time > average-recovery-time * 2 [ set recovery-time average-recovery-time * 2 ]
  if recovery-time < 0 [ set recovery-time 0 ]

  ;; Lo mismo ocurre con las tendencias de aislamiento y hospitalización.
  if isolation-tendency > average-isolation-tendency * 2 [ set isolation-tendency average-isolation-tendency * 2 ]
  if isolation-tendency < 0 [ set isolation-tendency 0 ]

  if hospital-going-tendency > average-hospital-going-tendency * 2 [ set hospital-going-tendency average-hospital-going-tendency * 2 ]
  if hospital-going-tendency < 0 [ set hospital-going-tendency 0 ]
end


;; Las personas se muestran en 5 colores diferentes dependiendo de su salud.
;; el verde es un sobreviviente de la infección (recovered)
;; el azul es una inoculación exitosa (vacunado)
;; rojo es una persona infectada (infected)
;; el blanco no está infectado, inoculado ni curado (susceptible)
;; amarillo es una ambulancia (ambulance)
to assign-color ;; turtle procedure

  ifelse cured?
    [ set color green ]
    [ ifelse inoculated?
      [ set color blue ]
      [ ifelse infected?
        [set color red ]
        [set color white]]]
  if ambulance?
    [ set color yellow ]
end


to make-network
  ask turtles
  [
    create-links-with turtles-on neighbors
  ]
end


;;;
;;; GO PROCEDURES
;;;


to go
  if all? turtles [ not infected? ]
    [ stop ]
  ask turtles
    [ clear-count ]

  ask turtles
    [ if not isolated? and not hospitalized? and not ambulance?
        [ move ] ]

  ask turtles
    [ if infected? and not isolated? and not hospitalized?
         [ infect ] ]

  ask turtles
    [ if not isolated? and not hospitalized? and infected? and (random 100 < isolation-tendency)
        [ isolate ] ]

  ask turtles
    [ if not isolated? and not hospitalized? and infected? and (random 100 < hospital-going-tendency)
        [ hospitalize ] ]

  ask turtles
  [
    if ambulance?
    [
      move
      ask turtles-on neighbors
      [
        if (ambulance? = false) and (infected? = true)
        [ hospitalize ]
      ]
    ]
  ]

  ask turtles
    [ if infected?
       [ maybe-recover ]
    ]

  ask turtles
    [ if (isolated? or hospitalized?) and cured?
        [ unisolate ] ]

  ask turtles
    [ assign-color
      calculate-r0 ]

  tick
end


to move  ;; turtle procedure
  if travel?
  [
    if random 100 < (travel-tendency) and not ambulance?  ;; hasta un 1% de posibilidades de viajar
    [ set xcor (- xcor) ]
  ]

  ifelse continent = 1
  [
    ifelse xcor > (- 0.5)  ;; y en patch de frontera
    [
      set angle random-float 180
      let new-patch patch-at-heading-and-distance angle (-1)
      if new-patch != nobody
      [
        move-to new-patch
      ]
    ]
    [ ;; si esta en el continente 1 y no en la frontera
      ifelse xcor < (min-pxcor + 0.5)  ;; en el borde del mundo
      [
        set angle random-float 180
      ]
      [
        set angle random-float 360  ;; mundo interior
      ]
      rt angle

      ifelse ambulance?
      [
        fd intra-mobility * 5  ;; las ambulancias se mueven 5 veces más rápido que las personas
      ]
      [
        fd intra-mobility
      ]
    ]

  ]
  [ ;; en el continente 2
    ifelse xcor < 1  ;; y en los patches de la frontera (amarillo)
    [
      set angle random-float 180
      let new-patch patch-at-heading-and-distance angle (1)
      if new-patch != nobody
      [
        move-to new-patch
      ]
    ]
    [ ;; si en el continente 2 y no en la frontera
      ifelse xcor > (max-pxcor - 1) ;; en el borde del mundo
      [
        set angle random-float 180
      ]
      [
        set angle random-float 360
      ]
      lt angle

      ifelse ambulance?
      [
        fd intra-mobility * 5
      ]
      [
        fd intra-mobility
      ]
    ]

  ]
end

to clear-count
  set nb-infected 0
  set nb-recovered 0
end

to maybe-recover
  set infection-length infection-length + 1

      ;; Si las personas han estado infectadas durante más tiempo que el de recuperación
      ;; entonces existe la posibilidad de recuperación
      ifelse not hospitalized?
      [
        if infection-length > recovery-time
        [
          if random-float 100 < recovery-chance
          [
            set infected? false
            set cured? true
            set nb-recovered (nb-recovered + 1)
          ]
        ]
      ]
      [ ;; Si está hospitalizado, recupérese en una quinta parte del tiempo de recuperación
        if infection-length > (recovery-time / 5)
        [
          set infected? false
          set cured? true
          set nb-recovered (nb-recovered + 1 )
        ]
      ]
end

;; Para mostrar mejor que se ha producido el aislamiento, el patch debajo de la persona se vuelve gris
to isolate ;; turtle procedure
  set isolated? true
  move-to patch-here ;; mover al centro del patch
  ask (patch-at 0 0) [ set pcolor gray - 3 ]
end

;; Despues de quitar el aislamiento, el patch vuelve a su color normal.
to unisolate  ;; turtle procedure
  set isolated? false
  set hospitalized? false

  ask (patch-at 0 0) [ set pcolor black ]

  ask border [ set pcolor yellow ]                      ;; colorear patches en la frontera (amarillo)
  ask (patch (- max-pxcor / 2) 0) [ set pcolor white ]  ;; colorear hospital del continente izquierdo (blanco)
  ask (patch (max-pxcor / 2) 0) [ set pcolor white ]    ;; colorear hospital del continente derecho (blanco)
end

;; Para hospitalizar, mudarse al patch hospitalario en el continente de residencia actual
to hospitalize ;; turtle procedure
  set hospitalized? true
  set pcolor black
  ifelse continent = 1
  [
    move-to patch (- max-pxcor / 2) 0
  ]
  [
    move-to patch (max-pxcor / 2) 0
  ]
  set pcolor white
end

;; Las personas infectadas que no están aisladas u hospitalizadas tienen la posibilidad de transmitir su enfermedad a sus vecinos susceptibles.
;; Si el vecino está vinculado, la posibilidad de transmisión de enfermedades se duplica.
to infect  ;; turtle procedure

    let caller self

    let nearby-uninfected (turtles-on neighbors)
    with [ not infected? and not cured? and not inoculated? ]
    if nearby-uninfected != nobody
    [
       ask nearby-uninfected
       [
           ifelse link-neighbor? caller
           [
             if random 100 < infection-chance * 2 ;; el doble de probabilidades de infectar a un contacto estrecho (persona vinculada)
             [
               set infected? true
               set nb-infected (nb-infected + 1)
             ]
           ]
           [
             if random 100 < infection-chance
             [
               set infected? true
               set nb-infected (nb-infected + 1)
             ]
           ]
       ]

    ]

end


to calculate-r0

  let new-infected sum [ nb-infected ] of turtles
  let new-recovered sum [ nb-recovered ] of turtles
  set nb-infected-previous (count turtles with [ infected? ] + new-recovered - new-infected)  ;; Numero de personas infectadas en el tick previo
  let susceptible-t (initial-people - (count turtles with [ infected? ]) - (count turtles with [ cured? ]))  ;; Numero actual de susceptibles
  let s0 count turtles with [ susceptible? ] ;; Numero inicial de susceptibles

  ifelse nb-infected-previous < 10
  [ set beta-n 0 ]
  [
    set beta-n (new-infected / nb-infected-previous)       ;; Este es el número promedio de nuevas infecciones secundarias por infectado en este tick
  ]

  ifelse nb-infected-previous < 5
  [ set gamma 0 ]
  [
    set gamma (new-recovered / nb-infected-previous)     ;; Este es el número promedio de nuevas recuperaciones por infectado en este tick
  ]

  if ((initial-people - susceptible-t) != 0 and (susceptible-t != 0))   ;; Prevenir division por 0
  [
    ;; Esto se deriva de la integración de dI / dS = (beta * SI - gamma * I) / (-beta * SI)
    ;; Suponiendo que se introdujo un individuo infectado al principio, y por lo tanto contando I(0) como insignificante,
    ;; obtenemos la relación
    ;; N - gamma * ln(S(0)) / beta = S(t) - gamma * ln(S(t)) / beta, donde N es la población 'susceptible' inicial.
    ;; Dado que N >> 1
    ;; Usando esto, tenemos R_0 = beta * N / gamma = N * ln(S(0) / S(t)) / (K-S(t))
    set r0 (ln (s0 / susceptible-t) / (initial-people - susceptible-t))
    set r0 r0 * s0 ]
end