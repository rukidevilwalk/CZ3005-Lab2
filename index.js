import subway_interactor from './subway-core.js'


console.log('Version' + ' 1')
// Init

//Init prolog session
var session = pl.create()
session.consult(subway_interactor)
session.consult(":- use_module(library(dom)).")
session.consult(":- use_module(library(js)).")
session.consult(":- use_module(library(lists)).")

// Declare variables
let currentProgress = 'meals'
let nextItem = false

// For ingredients with multiple selections
let orderContents = {
  meat: ``,
  veggie: ``,
  sauce: ``,
  topup: ``,
  side: ``,
}

function updateUserDialogueBox(contents) {
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

function updateStaffDialogueBox(contents) {
  $('#subway-header')
    .empty()
    .append(contents)
    .end()

  $('#subway-contents')
    .empty()
    .end()
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

  let selectedIngredient = fact.toUpperCase()

  // Carry out functions based on current progress
  switch (currentProgress) {
    case 'meals':
      updateUserDialogueBox(selectedIngredient)
      session.query(`selected(${fact},meals).`)
      session.answer()
      updateStaffDialogueBox(
        `Going for <b>${selectedIngredient}</b> meal alrighty! ${messages.bread_choices}`
      )

      $("#btn-group").empty()
      session.query("options(breads).")
      session.answer()

      currentProgress = 'breads'
      break

    case 'breads':
      updateUserDialogueBox(orderContents.bread)
      session.query(`selected(${fact},breads).`)
      session.answer()
      session.query(`get_meats(X).`)
      session.answer(answer => {
        if (pl.type.is_substitution(answer)) {
          let result = answer.lookup('X')
          if (result == '[]') {
            updateStaffDialogueBox(
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
            updateStaffDialogueBox(
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
        updateUserDialogueBox(orderContents.meat)
        session.query(`get_veggies(X).`)
        session.answer(answer => {
          if (pl.type.is_substitution(answer)) {
            updateStaffDialogueBox(
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
        updateUserDialogueBox(orderContents.veggie)
        session.query(`get_sauces(X).`)
        session.answer(answer => {
          if (pl.type.is_substitution(answer)) {
            let result = answer.lookup('X')
            if (result == '[[honey_mustard, sweet_onion]]') {
              updateStaffDialogueBox(
                `<b>${orderContents.veggie}</b> just arrived today morning from New Zealands! and  becuase you chose <b>${orderContents.meal}</b> ${messages.non_fat_sauce_choices}`
              )

              $("#btn-group").empty()
              session.query("options(sauces).")
              session.answer()
            } else {
              updateStaffDialogueBox(
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
        updateUserDialogueBox(orderContents.sauce)
        session.query(`get_topups(X).`)
        session.answer(answer => {
          if (pl.type.is_substitution(answer)) {
            let result = answer.lookup('X')
            if (result == '[]') {
              updateStaffDialogueBox(
                `<b>${orderContents.sauce}</b> is our crowd favourite <br/> Becuase you chose <b>${orderContents.meal}</b> meal, no top-up options for you ${messages.side_choices}`
              )
              $("#btn-group").empty()
              session.query("options(sides).")
              session.answer()
              $("#nextItem").html('Confirm Order');
              currentProgress = 'sides'
            } else if (result == '[[avocado, egg_mayo]]') {
              updateStaffDialogueBox(
                `<b>${orderContents.sauce}</b> is our crowd favourite <br/> Becuase you chose <b>${orderContents.meal}</b> meal, no cheese top-up for you ${messages.non_cheese_topup_choices}`
              )
              $("#btn-group").empty()
              session.query("options(topups).")
              session.answer()
              currentProgress = 'topups'
            } else {
              updateStaffDialogueBox(
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
        updateUserDialogueBox(orderContents.topup)
        session.query(`get_sides(X).`)
        session.answer(answer => {
          if (pl.type.is_substitution(answer)) {
            updateStaffDialogueBox(
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
        updateStaffDialogueBox('Here is your order:')
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

// Init interaction for first ingredient
updateStaffDialogueBox(' Welcome to Subway! What kind of meal would you like?')
$("#btn-group").empty()
session.query("options(meals).")
session.answer()
$('#nextItem').hide()