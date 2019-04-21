package com.gitlab.vladimirp1.myapplication

import android.app.Activity
import android.os.Parcelable
import android.view.View
import android.widget.Button
import android.widget.TextView
import com.gitlab.vladimirp1.parser.*
import kotlinx.android.parcel.Parcelize
import java.lang.ref.WeakReference
import java.util.*
import kotlin.concurrent.schedule

@Parcelize
data class CalculatorState(var exprString : String, var memString : String) : Parcelable

class InputHandler(val activity: Activity, val state : CalculatorState){
    private val digitsView = activity.findViewById<TextView>(R.id.digitsView)

    private val buttonIds = arrayOf(
            R.id.point,
            R.id.memplus,
            R.id.memminus,
            R.id.key0,
            R.id.key1,
            R.id.key2,
            R.id.key3,
            R.id.key4,
            R.id.key5,
            R.id.key6,
            R.id.key7,
            R.id.key8,
            R.id.key9,
            R.id.times,
            R.id.divide,
            R.id.plus,
            R.id.minus,
            R.id.clr,
            R.id.backspace,
            R.id.equals
    )

    fun handleDigit(d : Char) {
        // After anything
        state.exprString += d
    }

    fun handleOperator(d : Char) {
        // Only after a digit or a point
        if (state.exprString.isNotEmpty() && (state.exprString.last() in '0'..'9' || state.exprString.last() == '.')) {
            state.exprString += d
        }
    }

    fun handlePoint() {
        // After an operator only ONCE
        var count = 0
        for (i in state.exprString.reversed()) {
            if(i == '.') {
                ++count
            } else if(!i.isDigit()) {
                break
            }
        }
        if(count > 0) return
        state.exprString += '.'
    }

    fun handleBackspace() {
        // We must have chars in editor
        if(state.exprString.length > 0) {
            state.exprString = state.exprString.dropLast(1)
        }
    }

    fun handleClear() {
        state.exprString = ""
    }

    fun handleEquals() {
        if(state.exprString.isNotEmpty() && state.exprString != ".") {
            var result = 0.0
            try {
                result = CalculatorParser.parseExpression(state.exprString)
            } catch(e : Throwable) {
                digitsView.setTextColor(activity.resources.getColor(R.color.colorError))
                val activityRef = WeakReference(activity)
                Timer().schedule(500) {
                    activityRef.get()?.runOnUiThread{
                        val a = activityRef.get()
                        if (a != null) {
                            a.findViewById<TextView>(R.id.digitsView)?.setTextColor(a.resources.getColor(R.color.colorNormal))
                        }
                    }
                }
                return
            }
            state.exprString = "%.6f".format(Locale.ROOT, result)
        }
    }

    fun handleMemPlus() {
        handleEquals()
        state.memString = state.exprString
    }

    fun handleMemMinus() {
         state.exprString += state.memString
    }

    fun handleKey(v : View) {
        digitsView.text = (v as Button).text
        when (v.id) {
            R.id.key0,
            R.id.key1,
            R.id.key2,
            R.id.key3,
            R.id.key4,
            R.id.key5,
            R.id.key6,
            R.id.key7,
            R.id.key8,
            R.id.key9 -> handleDigit(v.text[0])

            R.id.times -> handleOperator('*')
            R.id.divide -> handleOperator('/')
            R.id.plus -> handleOperator('+')
            R.id.minus -> handleOperator('-')
            R.id.clr -> handleClear()
            R.id.backspace -> handleBackspace()
            R.id.equals -> handleEquals()
            R.id.point -> handlePoint()

            R.id.memplus -> handleMemPlus()
            R.id.memminus -> handleMemMinus()

        }
        digitsView.text = state.exprString
    }

    init {
        for (buttonId in buttonIds) {
            val view = activity.findViewById<Button>(buttonId)
            view.setOnClickListener { if(it != null) handleKey(it) }
        }
        digitsView.text = state.exprString
    }


}