import subway_interactor from './subway-core.js/index.js'
var session = pl.create()
session.consult(subway_interactor)
session.consult(":- use_module(library(dom)).");
session.consult(":- use_module(library(js)).");
session.consult(":- use_module(library(lists)).");
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
  Choose one of our fat-free sauces
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
      session.query(`selected(${fact},meals).`)
      session.answer()
      updateDialogueBox(
        'staff',
        `Going for <b>${orderContents.meal}</b> meal alrighty! ${messages.bread_choices}`
      )

      $("#btn-group").empty()
      session.query("options(breads).")
      session.answer()

      currentProgress = 'breads'
      break

    case 'breads':
      // Add user's response

      orderContents.bread = fact.toUpperCase()
      updateDialogueBox('user', orderContents.bread)
      session.query(`selected(${fact},breads).`)
      session.answer()
      session.query(`get_meats(X).`)
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
        session.query(`get_veggies(X).`)
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
        session.answer()
      }


      break
    case 'veggies':
      if (nextItem) {
        // Add user's response
        updateDialogueBox('user', orderContents.veggie)
        session.query(`get_sauces(X).`)
        session.answer(answer => {
          if (pl.type.is_substitution(answer)) {
            let result = answer.lookup('X')
            if (result == '[[honey_mustard, sweet_onion]]') {
              updateDialogueBox(
                'staff',
                `<b>${orderContents.veggie}</b> just arrived today morning from New Zealands! and  becuase you chose <b>${orderContents.meal}</b> ${messages.non_fat_sauce_choices}`
              )

              $("#btn-group").empty()
              session.query("options(sauces).")
              session.answer()
            } else {
              updateDialogueBox(
                'staff',
                `<b>${orderContents.veggie}</b> just arrived today morning from New Zealands! ${messages.all_sauce_choices}</b>`
              )

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
        session.answer()
      }


      break
    case 'sauces':
      if (nextItem) {
        // Add user's response
        updateDialogueBox('user', orderContents.sauce)
        session.query(`get_topups(X).`)
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
        session.answer()
      }

      break
    case 'topups':
      if (nextItem) {
        updateDialogueBox('user', orderContents.topup)
        session.query(`get_sides(X).`)
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
        session.answer()
      }



      break
    case 'sides':
      if (nextItem) {
        $('#nextItem').hide()
        $('#selection-area').hide()
        updateDialogueBox(
          'staff',
          `Okay! Your order is:
              `
        )
        console.log('Displaying Selections!')
        session.query(`displaySelections(1).`)
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
        session.answer()
      }


      break
    case 'end':
      break
    default:
      break
  }


}

updateDialogueBox('staff', messages.greetings)
console.log('Version' + ' 1')
$("#btn-group").empty()
session.query("options(meals).")
session.answer()
$('#nextItem').hide()
