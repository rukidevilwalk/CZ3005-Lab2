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
      // Update Dialogue
      updateUserDialogueBox(selectedIngredient)
      updateStaffDialogueBox(selectedIngredient +
        'has been selected!<br /> What would you like for your bread?')

      // Call prolog to assert selected item 
      session.query(`selected(${fact},meals).`)
      session.answer()

      //Call prolog to update menu items
      $("#btn-group").empty()
      session.query("options(breads).")
      session.answer()

      currentProgress = 'breads'
      break

    case 'breads':
      // Update Dialogue
      updateUserDialogueBox(selectedIngredient)

      // Call prolog to assert selected item 
      session.query(`selected(${fact},breads).`)
      session.answer()

      // Call prolog to check if meat needs to be displayed 
      session.query(`get_meats(X).`)
      session.answer(answer => {
        if (pl.type.is_substitution(answer)) {
          // If result is empty, that means vegan or veggies was selected
          if (answer.lookup('X') == '[]') {
            // Update Dialogue
            updateStaffDialogueBox(selectedIngredient +
              'has been selected!<br /> What would you like for your veggies?')

            //Call prolog to update menu items
            $("#btn-group").empty()
            session.query("options(veggies).")
            session.answer()
            currentProgress = 'veggies'
          } else {
            // Update Dialogue
            updateStaffDialogueBox(selectedIngredient +
              'has been selected!<br /> What would you like for your meat?')
            $("#btn-group").empty()

            //Call prolog to update menu items
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
        // Update Dialogue
        updateUserDialogueBox(selectedIngredient)

        // Call prolog to check if meat needs to be displayed 
        session.query(`get_veggies(X).`)
        session.answer(answer => {
          if (pl.type.is_substitution(answer)) {
            // Update Dialogue
            updateStaffDialogueBox(selectedIngredient +
              'has been selected!<br /> What would you like for your veggies?')

            //Call prolog to update menu items
            nextItem = false;
            $("#btn-group").empty()
            session.query("options(veggies).")
            session.answer()
          }

          currentProgress = 'veggies'
        })
      } else {

        orderContents.meat = orderContents.meat + (orderContents.meat != '' ? ' , ' : '') + selectedIngredient
        // Call prolog to assert selected item 
        session.query(`selected(${fact},meats).`)
        session.answer()
      }
      break

    case 'veggies':
      if (nextItem) {
        // Update Dialogue
        updateUserDialogueBox(selectedIngredient)

        // Call prolog to check what kind of sauces need to be displayed 
        // non-fat sauces for healthy meals
        session.query(`get_sauces(X).`)
        session.answer(answer => {
          if (pl.type.is_substitution(answer)) {
            let result = answer.lookup('X')
            if (result == '[[honey_mustard, sweet_onion]]') {
              // Update Dialogue
              updateStaffDialogueBox(selectedIngredient +
                'has been selected!<br /> Since you wanted a healthy meal, what would you like for your non-fat sauces?')

              //Call prolog to update menu items
              $("#btn-group").empty()
              session.query("options(sauces).")
              session.answer()
            } else {
              // Update Dialogue
              updateStaffDialogueBox(selectedIngredient +
                'has been selected!<br /> What would you like for your sauces?')

              //Call prolog to update menu items
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
        // Call prolog to assert selected item 
        session.query(`selected(${fact},veggies).`)
        session.answer()
      }
      break

    case 'sauces':
      if (nextItem) {
        // Update Dialogue
        updateUserDialogueBox(selectedIngredient)

        // Call prolog to check if topups need to be displayed 
        session.query(`get_topups(X).`)
        session.answer(answer => {
          if (pl.type.is_substitution(answer)) {
            // If result is empty, that means value meal was selected
            if (answer.lookup('X') == '[]') {
              // Update Dialogue
              updateStaffDialogueBox(selectedIngredient +
                'has been selected!<br /> Since you wanted a value meal, there will be no topups. <br/> What would you like for your sides?')

              //Call prolog to update menu items
              $("#btn-group").empty()
              session.query("options(sides).")
              session.answer()
              $("#nextItem").html('Confirm Order');
              currentProgress = 'sides'
            } else if (result == '[[avocado, egg_mayo]]') {
              // Update Dialogue
              updateStaffDialogueBox(selectedIngredient +
                'has been selected!<br /> Since you wanted a vegan meal, there will only be non-cheese topups. <br/> What would you like for your topups?')

              //Call prolog to update menu items
              $("#btn-group").empty()
              session.query("options(topups).")
              session.answer()
              currentProgress = 'topups'
            } else {
              // Update Dialogue
              updateStaffDialogueBox(selectedIngredient +
                'has been selected!<br /> What would you like for your topups?')

              //Call prolog to update menu items
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
        // Call prolog to assert selected item 
        session.query(`selected(${fact},sauces).`)
        session.answer()
      }
      break

    case 'topups':
      if (nextItem) {
        // Update Dialogue
        updateUserDialogueBox(selectedIngredient)
        updateStaffDialogueBox(selectedIngredient +
          'has been selected!<br /> What would you like for your sides?')

        //Call prolog to update menu items
        nextItem = false
        $("#btn-group").empty()
        session.query("options(sides).")
        session.answer()
        $("#nextItem").html('Confirm Order');
        currentProgress = 'sides'
      } else {
        orderContents.topup = orderContents.topup + (orderContents.topup != '' ? ' , ' : '') + selectedIngredient
        // Call prolog to assert selected item 
        session.query(`selected(${fact},topups).`)
        session.answer()
      }
      break

    case 'sides':
      if (nextItem) {

        $('#nextItem').hide()
        $('#selection-area').hide()

        // Update Dialogue
        updateUserDialogueBox(selectedIngredient)
        updateStaffDialogueBox('Here is your order:')

        // Call Prolog to display final order
        session.query(`displaySelections(1).`)
        session.answer()
      } else {
        orderContents.side = orderContents.side + (orderContents.side != '' ? ' , ' : '') + selectedIngredient
        // Call prolog to assert selected item 
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