package com.gitlab.vladimirp1.myapplication

import android.support.v7.app.AppCompatActivity
import android.os.Bundle

class MainActivity : AppCompatActivity() {

    private val CALCULATOR_STATE = "com.gitlab.vladimirp1.myapplication.CalculatorState";
    private lateinit var state : CalculatorState;


    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        supportActionBar?.hide();
        setContentView(R.layout.activity_main)
        if (savedInstanceState != null) {
            state = savedInstanceState.getParcelable<CalculatorState>(CALCULATOR_STATE) ?: CalculatorState("", "");
        } else state = CalculatorState("", "")

        InputHandler(this, state)
    }

    override fun onSaveInstanceState(outState: Bundle) {
        outState.putParcelable(CALCULATOR_STATE, state);
        super.onSaveInstanceState(outState)
    }
}
