import subway_interactor from './subway-prolog.js'
var session = pl.create()
session.consult(subway_interactor)
session.consult(":- use_module(library(dom)).");
session.consult(":- use_module(library(js)).");

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
let orderContents = {
  meat: ``,
  veggie: ``,
  sauce: ``,
  topup: ``,
  side: ``,
}

let currentProgress = 'meals'

let nextItem = false

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

    if ($(e.target).text() == 'Next Ingredient' || $(e.target).text() == 'Confirm Order') {
      nextItem = true
      buttonClicked($(e.target).text())
    } else {
      buttonClicked($(e.target).text())
    }
  }
})

// Carry out logic whenever an item button is clicked
function buttonClicked(fact) {

  // Carry out functions based on current progress
  switch (currentProgress) {
    case 'meals':
      // Add user's response
      orderContents.meal = fact.toUpperCase()
      updateDialogueBox('user', orderContents.meal)
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
          $("#btn-group").empty()
          session.query("options(breads).")
          session.answer()
        }
        currentProgress = 'breads'
      })

      break
    case 'breads':
      // Add user's response

      orderContents.bread = fact.toUpperCase()
      updateDialogueBox('user', orderContents.bread)
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
            $("#btn-group").empty()
            console.log('setting to veggies')
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
            $("#btn-group").empty()
            console.log('setting to meat')
            session.query("options(meats).")
            session.answer()
            currentProgress = 'meats'

          }

        }
      })
      $('#nextItem').show()
      break
    case 'meats':


      if (nextItem) {
        // Add user's response
        updateDialogueBox('user', orderContents.meat)
        session.query(`ask_veggies(X).`)
        session.answer(answer => {
          if (pl.type.is_substitution(answer)) {
            updateDialogueBox(
              'staff',
              `Juicy and tender <b>${orderContents.meat}</b>! ${messages.veggie_choices}`
            )
            nextItem = false;
            $("#btn-group").empty()
            session.query("options(veggies).")
            session.answer()
          }

          currentProgress = 'veggies'
        })
      } else {

        orderContents.meat = orderContents.meat + (orderContents.meat != '' ? ' , ' : '') + fact.toUpperCase()

        session.query(`selected(${fact},meats).`)
      }


      break
    case 'veggies':
      if (nextItem) {
        // Add user's response
        updateDialogueBox('user', orderContents.veggie)
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
              $("#btn-group").empty()
              session.query("options(sauces).")
              session.answer()
            } else {
              updateDialogueBox(
                'staff',
                `<b>${orderContents.veggie}</b> just arrived today morning from New Zealands! ${messages.all_sauce_choices}</b>`
              )
              console.log('generating all sauces')
              $("#btn-group").empty()
              session.query("options(sauces).")
              session.answer()
            }
            nextItem = false
          }
        })
        currentProgress = 'sauces'
      } else {
        orderContents.veggie = orderContents.veggie + (orderContents.veggie != '' ? ' , ' : '') + fact.toUpperCase()
        session.query(`selected(${fact},veggies).`)
      }


      break
    case 'sauces':
      if (nextItem) {
        // Add user's response
        updateDialogueBox('user', orderContents.sauce)
        session.query(`ask_topups(X).`)
        session.answer(answer => {
          if (pl.type.is_substitution(answer)) {
            let result = answer.lookup('X')
            if (result == '[]') {
              updateDialogueBox(
                'staff',
                `<b>${orderContents.sauce}</b> is our crowd favourite <br/> Becuase you chose <b>${orderContents.meal}</b> meal, no top-up options for you ${messages.side_choices}`
              )
              $("#btn-group").empty()
              session.query("options(sides).")
              session.answer()
              $("#nextItem").html('Confirm Order');
              currentProgress = 'sides'
            } else if (result == '[[avocado, egg_mayo]]') {
              updateDialogueBox(
                'staff',
                `<b>${orderContents.sauce}</b> is our crowd favourite <br/> Becuase you chose <b>${orderContents.meal}</b> meal, no cheese top-up for you ${messages.non_cheese_topup_choices}`
              )
              $("#btn-group").empty()
              session.query("options(topups).")
              session.answer()
              currentProgress = 'topups'
            } else {
              updateDialogueBox(
                'staff',
                `<b>${orderContents.sauce}</b> is our crowd favourite ${messages.all_top_up_choices}`
              )
              $("#btn-group").empty()
              session.query("options(topups).")
              session.answer()
              currentProgress = 'topups'
            }
            nextItem = false
          }
        })
      } else {
        orderContents.sauce = orderContents.sauce + (orderContents.sauce != '' ? ' , ' : '') + fact.toUpperCase()
        session.query(`selected(${fact},sauces).`)
      }

      break
    case 'topups':
      if (nextItem) {
        updateDialogueBox('user', orderContents.topup)
        session.query(`ask_sides(X).`)
        session.answer(answer => {
          if (pl.type.is_substitution(answer)) {
            updateDialogueBox(
              'staff',
              ` <b>${orderContents.topup}</b>? Good choice ${messages.side_choices}`
            )
            nextItem = false
            $("#btn-group").empty()
            session.query("options(sides).")
            session.answer()
          }
        })
        $("#nextItem").html('Confirm Order');
        currentProgress = 'sides'
      } else {
        orderContents.topup = orderContents.topup + (orderContents.topup != '' ? ' , ' : '') + fact.toUpperCase()
        session.query(`selected(${fact},topups).`)
      }



      break
    case 'sides':
      if (nextItem) {
        $('#nextItem').hide()
        $('#selection-area').hide()
        session.answer()
        updateDialogueBox(
          'staff',
          `Okay! Your order is:
              `
        )
        session.query(`displayOrder(1).`)
        session.answer()
        // updateDialogueBox(
        //   'staff',
        //   `Okay! Your order
        //       <br/>
        //       <br/>
        //       Meal
        //       <br/>
        //       <b>${orderContents.meal}</b>
        //       <br/>
        //       <br/>
        //       Bread
        //       <br/>
        //       <b>${orderContents.bread}</b>
        //       <br/>
        //       <br/>
        //       Meat
        //       <br/>
        //       <b>${orderContents.meat}</b>
        //       <br/>
        //       <br/>
        //       Veggie
        //       <br/>
        //       <b>${orderContents.veggie}</b>
        //       <br/>
        //       <br/>
        //       Sauce
        //       <br/>
        //       <b>${orderContents.sauce}</b> 
        //       <br/>
        //       <br/>
        //       Topup
        //       <br/>
        //       <b>${orderContents.topup}</b>
        //       <br/>
        //       <br/>
        //       Side
        //       <br/>
        //       <b>${orderContents.side}</b>
        //       <br/>
        //       <br/>
        //       is being prepared.Thank you for choosing Subway!
        //       If you would like to make new order, refresh the page 
        //       `
        // )
      } else {
        orderContents.side = orderContents.side + (orderContents.side != '' ? ' , ' : '') + fact.toUpperCase()
        session.query(`selected(${fact},sides).`)
      }


      break
    case 'end':
      break
    default:
      break
  }


}

// ---- Print Messages
updateDialogueBox('staff', messages.greetings)
console.log('1')
$("#btn-group").empty()
session.query("options(meals).")
session.answer()
$('#nextItem').hide()
