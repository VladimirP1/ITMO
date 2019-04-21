package com.gitlab.vladimirp1.photofeed

import java.lang.ref.WeakReference

fun <T>T.weak() = WeakReference(this)