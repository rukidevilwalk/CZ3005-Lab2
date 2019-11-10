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
  greetings: `Hello Ben !
  <br/>
  Welcome To Subway!
  <br/>
  <br/>
  You look unhealthy, please choose veggie meal!
  <br/>
  <br/>
  <li class="list-group-item">Veggie
  </li>
  <li class="list-group-item">Healthy
  </li>
  <li class="list-group-item">Normal
  </li>
  <li class="list-group-item">Value
  </li>
  <li class="list-group-item">  Vegan
  </li>
  `,
  bread_choices: `
  <br/>
  <br/>
  What about your bread?
  <br/>
  <br/>
  Flatbread
  <br/>
  Honey_Oat
  <br/>
  Italian
  <br/>
  Hearthy_Italian
  <br/>
  Wheat
  `,
  meat_choices: `
  <br/>
  <br/>
  Meat meat? One meat
  <br/>
  <br/>
  Chicken
  <br/>
  Beef
  <br/>
  Ham
  <br/>
  Bacon
  <br/>
  Salmon
  <br/>
  Tuna
  <br/>
  Turkey
  `,
  veggie_choices: `
  <br/>
  <br/>
  Time for some greens! One for now
  <br/>
  <br/>
  Cucumber
  <br/>
  Green_Peppers
  <br/>
  Lettuce
  <br/>
  Red_Onions
  <br/>
  Tomatoes
  `,
  all_sauce_choices: `
  <br/>
  <br/>
  Choose one of our tasty sauces
  <br/>
  <br/>
  Chipotle
  <br/>
  BBQ
  <br/>
  Ranch
  <br/>
  Sweet_Chilli
  <br/>
  Mayo
  <br/>
  Honey_Mustard
  <br/>
  Sweet_Onion
  `,
  non_fat_sauce_choices: `
  <br/>
  <br/>
  Chose one of our fat-free sauces
  <br/>
  <br/>
  Honey_Mustard
  <br/>
  Sweet_Onion
  `,
  all_top_up_choices: `
  <br/>
  <br/>
  One of top-ups
  <br/>
  <br/>
   American
  <br/>
  Monterey_Jack
  <br/>
   Cheddar
  <br/>
   Avocado
  <br/>
  Egg_mayo
  `,
  non_cheese_topup_choices: `
  <br/>
  <br/>
  One of cheese free top-ups
  <br/>
  <br/>
   Avocado
  <br/>
  Egg_mayo
  `,
  side_choices: `
  <br/>
  <br/>
  Finally, choose one side
  <br/>
  <br/>
  Chips
  <br/>
  Cookies
  <br/>
   Drinks
  `
}
const user_order = {}
const steps = [
  'meals',
  'breads',
  'meats',
  'veggies',
  'sauces',
  'topups',
  'sides',
  'end'
]
let progress = 0
//-- END Constants and Variables

//-- Helper Functions
const formatAMPM = date => {
  let hours = date.getHours()
  let minutes = date.getMinutes()
  let ampm = hours >= 12 ? 'PM' : 'AM'
  hours = hours % 12
  hours = hours ? hours : 12 // the hour '0' should be '12'
  minutes = minutes < 10 ? '0' + minutes : minutes
  let strTime = hours + ':' + minutes + ' ' + ampm
  return strTime
}

const updateChat = (who, text) => {
  let date = formatAMPM(new Date())
  if (who == 'subway') {
    $('#subway-date')
      .empty()
      .append(date)
      .end()

    $('#subway-contents')
      .empty()
      .append(text)
      .end()
  } else {
    $('#user-date')
      .empty()
      .append(date)
      .end()

    $('#user-contents')
      .empty()
      .append('I would like ' + text)
      .end()
  }

}

// Listener for dynamically created buttons
$(document).click(function (e) {
  if ($(e.target).is("button"))
    buttonClicked($(e.target).text())
})


function buttonClicked(fact) {

  // Remove current buttons
  $("#btn-group").empty()

  // Add user's response
  updateChat('user', fact)

  // Carry out functions based on current progress
  switch (steps[progress]) {
    case 'meals':
      user_order.meal = fact
      if (fact == 'vegan' || fact == 'veggie') {
        user_order.meat = '❌ NO MEAT'
      } else if (fact == 'value') {
        user_order.topup = '❌ NO TOPUP'
      }
      session.query(`selected(${fact},meals), show_meals(X).`)
      session.answer(answer => {
        if (pl.type.is_substitution(answer)) {
          updateChat(
            'subway',
            `Going for <b>${user_order.meal}</b> meal alrighty! ${messages.bread_choices}`
          )
          session.query("options(breads).")
          session.answer()
        }
        progress = 1
      })

      break
    case 'breads':
      user_order.bread = fact.toUpperCase()
      session.query(`selected(${fact},breads).`)
      session.query(`ask_meats(X).`)
      session.answer(answer => {
        if (pl.type.is_substitution(answer)) {
          let result = answer.lookup('X')
          if (result == '[]') {
            // vegan or vegie
            updateChat(
              'subway',
              `<b>${user_order.bread.toUpperCase()}</b> was just freshly baked by our chef Since you chose <b>${
              user_order.meal
              }</b> meal, no meat options for you. ${
              messages.veggie_choices
              }`
            )
            console.log('creating veggie buttons')
            session.query("options(veggies).")
            session.answer()
            progress = 3
          } else {
            // meat
            updateChat(
              'subway',
              `<b>${user_order.bread.toUpperCase()}</b> was just freshly baked by our chef${
              messages.meat_choices
              }`
            )
            console.log('creating meat buttons')
            session.query("options(meats).")
            session.answer()
            progress = 2
          }
        }
      })
      break
    case 'meats':
      user_order.meat = fact.toUpperCase()
      session.query(`selected(${fact},meats).`)
      session.query(`ask_veggies(X).`)
      session.answer(answer => {
        if (pl.type.is_substitution(answer)) {
          updateChat(
            'subway',
            `Juicy and tender <b>${user_order.meat}</b>! ${messages.veggie_choices}`
          )

          session.query("options(veggies).")
          session.answer()
        }
        progress = 3
      })
      break
    case 'veggies':
      user_order.veggie = fact.toUpperCase()
      session.query(`selected(${fact},veggies).`)
      session.query(`ask_sauces(X).`)
      session.answer(answer => {
        if (pl.type.is_substitution(answer)) {
          let result = answer.lookup('X')
          if (result == '[[honey_mustard, sweet_onion]]') {
            updateChat(
              'subway',
              `<b>${user_order.veggie}</b> just arrived today morning from New Zealands! and  becuase you chose <b>${user_order.meal}</b> ${messages.non_fat_sauce_choices}`
            )
            console.log('generating non fat sauces')
            session.query("options(sauces).")
            session.answer()
          } else {
            updateChat(
              'subway',
              `<b>${user_order.veggie}</b> just arrived today morning from New Zealands! ${messages.all_sauce_choices}</b>`
            )
            console.log('generating all sauces')
            session.query("options(sauces).")
            session.answer()
          }
        }
      })
      progress = 4
      break
    case 'sauces':
      user_order.sauce = fact.toUpperCase()
      session.query(`selected(${fact},sauces).`)
      session.query(`ask_topups(X).`)
      session.answer(answer => {
        if (pl.type.is_substitution(answer)) {
          let result = answer.lookup('X')
          if (result == '[]') {
            updateChat(
              'subway',
              `<b>${user_order.sauce}</b> is our crowd favourite <br/> Becuase you chose <b>${user_order.meal}</b> meal, no top-up options for you ${messages.side_choices}`
            )

            session.query("options(sides).")
            session.answer()
            progress = 6
          } else if (result == '[[avocado, egg_mayo]]') {
            updateChat(
              'subway',
              `<b>${user_order.sauce}</b> is our crowd favourite <br/> Becuase you chose <b>${user_order.meal}</b> meal, no cheese top-up for you ${messages.non_cheese_topup_choices}`
            )

            session.query("options(topups).")
            session.answer()
            progress = 5
          } else {
            updateChat(
              'subway',
              `<b>${user_order.sauce}</b> is our crowd favourite ${messages.all_top_up_choices}`
            )

            session.query("options(topups).")
            session.answer()
            progress = 5
          }
        }
      })
      break
    case 'topups':
      user_order.topup = fact.toUpperCase()
      session.query(`selected(${fact},topups).`)
      session.query(`ask_sides(X).`)
      session.answer(answer => {
        if (pl.type.is_substitution(answer)) {
          updateChat(
            'subway',
            ` <b>${user_order.topup}</b>? Good choice ${messages.side_choices}`
          )

          session.query("options(sides).")
          session.answer()
        }
      })
      progress = 6
      break
    case 'sides':
      user_order.side = fact.toUpperCase()
      session.query(`selected(${fact},sides).`)
      updateChat(
        'subway',
        `Okay! Your order
          <br/>
          <br/>
          Meal
          <br/>
          <b>${user_order.meal}</b>
          <br/>
          <br/>
          Bread
          <br/>
          <b>${user_order.bread}</b>
          <br/>
          <br/>
          Meat
          <br/>
          <b>${user_order.meat}</b>
          <br/>
          <br/>
          Veggie
          <br/>
          <b>${user_order.veggie}</b>
          <br/>
          <br/>
          Sauce
          <br/>
          <b>${user_order.sauce}</b> 
          <br/>
          <br/>
          Topup
          <br/>
          <b>${user_order.topup}</b>
          <br/>
          <br/>
          Side 🍴
          <br/>
          <b>${user_order.side}</b>
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
updateChat('subway', messages.greetings)
$("#btn-group").empty()
session.query("options(meals).")
session.answer()
