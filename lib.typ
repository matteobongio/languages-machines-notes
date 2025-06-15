#import "@preview/showybox:2.0.3": showybox

#let NeFSM = [ N$epsilon$FSM ]

#let PDM(read, push, pop) =$#read thin #push slash #pop$

#let TM(read, write, move) = $#read slash #write thin #move$
#let blank = $cal(B)$
#let tml = $upright(L)$
#let tmr = $upright(R)$


#let bluebox(title, text, ..opts) = {
  showybox(
    title-style: (
      weight: 900,
      color: blue.darken(40%),
      sep-thickness: 0pt,
      align: center
    ),
    frame: (
      title-color: blue.lighten(80%),
      border-color: blue.darken(40%),
      body-color: blue.lighten(90%),
      thickness: (left: 1pt),
      radius: (top-right: 5pt, bottom-right:5pt, rest: 0pt)
    ),
    title: title,
    text,
    ..opts
  )
}
#let Examplebox(text, ..opts) = {
  showybox(
    title-style: (
      weight: 900,
      color: orange.darken(40%),
      sep-thickness: 0pt,
      align: center
    ),
    frame: (
      title-color: orange.lighten(80%),
      border-color: orange.lighten(40%),
      body-color: orange.lighten(90%),
      thickness: (left: 1pt),
      radius: (top-right: 5pt, bottom-right:5pt, rest: 0pt)
    ),
    title: "Examples",
    text,
    ..opts
  )
}
#let Notationbox(text, ..opts) = {
  showybox(
    title-style: (
      weight: 900,
      color: green.darken(40%),
      sep-thickness: 0pt,
      align: center
    ),
    frame: (
      title-color: green.lighten(80%),
      border-color: green.lighten(40%),
      body-color: green.lighten(90%),
      thickness: (left: 1pt),
      radius: (top-right: 5pt, bottom-right:5pt, rest: 0pt)
    ),
    title: "Notation",
    text,
    ..opts
  )
}
