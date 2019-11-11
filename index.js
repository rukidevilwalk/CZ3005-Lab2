import subway_interactor from './subway-core.js'


console.log('Version' + ' 1')
// Init

//Init prolog session
var session = pl.create()
session.consult(subway_interactor)
session.consult(":- use_module(library(dom)).")
session.consult(":- use_module(library(js)).")
session.consult(":- use_module(library(lists)).")

// Init interaction for first ingredient
updateDialogueBox('staff', ' Welcome to Subway! What kind of meal would you like?')
$("#btn-group").empty()
session.query("options(meals).")
session.answer()
$('#nextItem').hide()

// Declare variables
let currentProgress = 'meals'
let nextItem = false

const messages = {
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
// For ingredients with multiple selections
let orderContents = {
  meat: ``,
  veggie: ``,
  sauce: ``,
  topup: ``,
  side: ``,
}



function updateDialogueBox(type, contents) {
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
    if (contents == '') {
      $('#user-contents')
        .empty()
        .append('None for me')
        .end()
    } else {
      $('#user-contents')
        .empty()
        .append('I would like ' + contents)
        .end()
    }

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

  let selectedIngredient = selectedIngredient

  // Carry out functions based on current progress
  switch (currentProgress) {
    case 'meals':
      updateDialogueBox('user', selectedIngredient)
      session.query(`selected(${fact},meals).`)
      session.answer()
      updateDialogueBox(
        'staff',
        `Going for <b>${selectedIngredient}</b> meal alrighty! ${messages.bread_choices}`
      )

      $("#btn-group").empty()
      session.query("options(breads).")
      session.answer()

      currentProgress = 'breads'
      break

    case 'breads':
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
              `<b>${selectedIngredient}</b> was just freshly baked by our chef Since you chose <b>${
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
              `<b>${selectedIngredient}</b> was just freshly baked by our chef${
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

        orderContents.meat = orderContents.meat + (orderContents.meat != '' ? ' , ' : '') + selectedIngredient
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
        orderContents.veggie = orderContents.veggie + (orderContents.veggie != '' ? ' , ' : '') + selectedIngredient
        session.query(`selected(${fact},veggies).`)
        session.answer()
      }
      break

    case 'sauces':
      if (nextItem) {
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
        orderContents.sauce = orderContents.sauce + (orderContents.sauce != '' ? ' , ' : '') + selectedIngredient
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
        orderContents.topup = orderContents.topup + (orderContents.topup != '' ? ' , ' : '') + selectedIngredient
        session.query(`selected(${fact},topups).`)
        session.answer()
      }
      break

    case 'sides':
      if (nextItem) {
        $('#nextItem').hide()
        $('#selection-area').hide()
        updateDialogueBox(
          'staff', 'Here is your order:')
        session.query(`displaySelections(1).`)
        session.answer()
      } else {
        orderContents.side = orderContents.side + (orderContents.side != '' ? ' , ' : '') + selectedIngredient
        session.query(`selected(${fact},sides).`)
        session.answer()
      }
      break

    default:
      break
  }

}

