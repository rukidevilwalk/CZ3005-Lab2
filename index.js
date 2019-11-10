//-- Prolog Session
import subway_interactor from './subway-prolog.js'
var session = pl.create()
session.consult(subway_interactor)
session.consult(":- use_module(library(dom)).");
session.consult(":- use_module(library(js)).");
//-- Constants and Variables
const user_avatar = 'https://image.flaticon.com/icons/svg/1400/1400241.svg'
const subway_avatar = 'https://image.flaticon.com/icons/svg/1995/1995600.svg'
const messages = {
  greetings: `
  Welcome To Subway!
  <br/>
  <br/>
  What kind of meal would you like?
  <br/>
  `,
  bread_choices: `
  <br/>
  What about your bread?
  <br/>
  `,
  meat_choices: `
  <br/>
  Meat meat? One meat
  <br/>
  `,
  veggie_choices: `
  <br/>
  Time for some greens! One for now
  <br/>
  `,
  all_sauce_choices: `
  <br/>
  Choose one of our tasty sauces
  <br/>
  `,
  non_fat_sauce_choices: `
  <br/>
  Chose one of our fat-free sauces
  <br/>
  Honey_Mustard
  <br/>
  Sweet_Onion
  `,
  all_top_up_choices: `
  <br/>
  One of top-ups
  <br/>
  `,
  non_cheese_topup_choices: `
  <br/>
  One of cheese free top-ups
  <br/>
  `,
  side_choices: `
  <br/>
  Finally, choose one side
  <br/>
  `
}
let orderContents = {}

let currentProgress = 'meals'

let nextItem = false

$('#nextItem').hide()

const updateDialogueBox = (type, contents) => {

  if (type == 'staff') {
    $('#subway-header')
      .empty()
      .append(contents)
      .end()

    $('#subway-contents')
      .empty()
      .end()
  }

  if (type == 'user') {
    $('#user-contents')
      .empty()
      .append('I would like ' + contents)
      .end()
  }

}

// Listener for dynamically created buttons
$(document).click(function (e) {
  if ($(e.target).is("button")) {

    if ($(e.target).text() == 'Next Ingredient')
      nextItem = true

    buttonClicked($(e.target).text())

  }

})

// Carry out logic whenever an item button is clicked
function buttonClicked(fact) {

  // Remove current buttons
  $("#btn-group").empty()

  // Add user's response
  updateDialogueBox('user', fact)
  console.log(currentProgress)
  // Carry out functions based on current progress
  switch (currentProgress) {
    case 'meals':
      orderContents.meal = fact
      if (fact == 'vegan' || fact == 'veggie') {
        orderContents.meat = 'NO MEAT'
      } else if (fact == 'value') {
        orderContents.topup = 'NO TOPUP'
      }
      session.query(`selected(${fact},meals), show_meals(X).`)
      session.answer(answer => {
        if (pl.type.is_substitution(answer)) {
          updateDialogueBox(
            'staff',
            `Going for <b>${orderContents.meal}</b> meal alrighty! ${messages.bread_choices}`
          )
          session.query("options(breads).")
          session.answer()
        }
        currentProgress = 'breads'
      })

      break
    case 'breads':
      orderContents.bread = fact.toUpperCase()
      session.query(`selected(${fact},breads).`)
      session.query(`ask_meats(X).`)
      session.answer(answer => {
        if (pl.type.is_substitution(answer)) {
          let result = answer.lookup('X')
          if (result == '[]') {
            updateDialogueBox(
              'staff',
              `<b>${orderContents.bread}</b> was just freshly baked by our chef Since you chose <b>${
              orderContents.meal
              }</b> meal, no meat options for you. ${
              messages.veggie_choices
              }`
            )
            console.log('creating veggie buttons')
            session.query("options(veggies).")
            session.answer()
            currentProgress = 'veggies'
          } else {
            // meat
            updateDialogueBox(
              'staff',
              `<b>${orderContents.bread}</b> was just freshly baked by our chef${
              messages.meat_choices
              }`
            )
            console.log('creating meat buttons')
            console.log('setting next item to visible')
            $('#nextItem').show()
            session.query("options(meats).")
            session.answer()
            currentProgress = 'meats'

          }
        
        }
      })
      break
    case 'meats':


      if (nextItem) {
        session.query(`ask_veggies(X).`)
        session.answer(answer => {
          if (pl.type.is_substitution(answer)) {
            updateDialogueBox(
              'staff',
              `Juicy and tender <b>${orderContents.meat}</b>! ${messages.veggie_choices}`
            )
            nextItem = false;
            session.query("options(veggies).")
            session.answer()
          }

          currentProgress = 'veggies'
        })
      } else {
        orderContents.meat = fact.toUpperCase()
        session.query(`selected(${fact},meats).`)
      }


      break
    case 'veggies':
      if (nextItem) {
        session.query(`ask_sauces(X).`)
        session.answer(answer => {
          if (pl.type.is_substitution(answer)) {
            let result = answer.lookup('X')
            if (result == '[[honey_mustard, sweet_onion]]') {
              updateDialogueBox(
                'staff',
                `<b>${orderContents.veggie}</b> just arrived today morning from New Zealands! and  becuase you chose <b>${orderContents.meal}</b> ${messages.non_fat_sauce_choices}`
              )
              console.log('generating non fat sauces')
              session.query("options(sauces).")
              session.answer()
            } else {
              updateDialogueBox(
                'staff',
                `<b>${orderContents.veggie}</b> just arrived today morning from New Zealands! ${messages.all_sauce_choices}</b>`
              )
              console.log('generating all sauces')
              session.query("options(sauces).")
              session.answer()
            }
            nextItem = false
          }
        })
        currentProgress = 'sauces'
      } else {
        orderContents.veggie = fact.toUpperCase()
        session.query(`selected(${fact},veggies).`)
      }


      break
    case 'sauces':
      if (nextItem) {

        session.query(`ask_topups(X).`)
        session.answer(answer => {
          if (pl.type.is_substitution(answer)) {
            let result = answer.lookup('X')
            if (result == '[]') {
              updateDialogueBox(
                'staff',
                `<b>${orderContents.sauce}</b> is our crowd favourite <br/> Becuase you chose <b>${orderContents.meal}</b> meal, no top-up options for you ${messages.side_choices}`
              )

              session.query("options(sides).")
              session.answer()
              currentProgress = 'sauces'
            } else if (result == '[[avocado, egg_mayo]]') {
              updateDialogueBox(
                'staff',
                `<b>${orderContents.sauce}</b> is our crowd favourite <br/> Becuase you chose <b>${orderContents.meal}</b> meal, no cheese top-up for you ${messages.non_cheese_topup_choices}`
              )

              session.query("options(topups).")
              session.answer()
              currentProgress = 'topups'
            } else {
              updateDialogueBox(
                'staff',
                `<b>${orderContents.sauce}</b> is our crowd favourite ${messages.all_top_up_choices}`
              )

              session.query("options(topups).")
              session.answer()
              currentProgress = 'topups'
            }
            nextItem = false
          }
        })
      } else {
        orderContents.sauce = fact.toUpperCase()
        session.query(`selected(${fact},sauces).`)
      }

      break
    case 'topups':
      if (nextItem) {
        session.query(`ask_sides(X).`)
        session.answer(answer => {
          if (pl.type.is_substitution(answer)) {
            updateDialogueBox(
              'staff',
              ` <b>${orderContents.topup}</b>? Good choice ${messages.side_choices}`
            )
            nextItem = false
            session.query("options(sides).")
            session.answer()
          }
        })
        currentProgress = 'sides'
      } else {
        orderContents.topup = fact.toUpperCase()
        session.query(`selected(${fact},topups).`)
      }



      break
    case 'sides':
      orderContents.side = fact.toUpperCase()
      session.query(`selected(${fact},sides).`)
      updateDialogueBox(
        'staff',
        `Okay! Your order
          <br/>
          <br/>
          Meal
          <br/>
          <b>${orderContents.meal}</b>
          <br/>
          <br/>
          Bread
          <br/>
          <b>${orderContents.bread}</b>
          <br/>
          <br/>
          Meat
          <br/>
          <b>${orderContents.meat}</b>
          <br/>
          <br/>
          Veggie
          <br/>
          <b>${orderContents.veggie}</b>
          <br/>
          <br/>
          Sauce
          <br/>
          <b>${orderContents.sauce}</b> 
          <br/>
          <br/>
          Topup
          <br/>
          <b>${orderContents.topup}</b>
          <br/>
          <br/>
          Side
          <br/>
          <b>${orderContents.side}</b>
          <br/>
          <br/>
          is being prepared.Thank you for choosing Subway!
          If you would like to make new order, refresh the page 
          `
      )
      break
    case 'end':
      break
    default:
      break
  }


}

// ---- Print Messages
updateDialogueBox('staff', messages.greetings)
$("#btn-group").empty()
session.query("options(meals).")
session.answer()
