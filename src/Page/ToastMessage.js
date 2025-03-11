/*
 https://github.com/ak1211/smartmeter-route-b-app
 SPDX-License-Identifier: MIT
 SPDX-FileCopyrightText: 2023 Akihiro Yamamoto <github.com/ak1211>
*/
"use strict";

// Bootstrap toast new
export const bootstrapToastShow = function () {
	var toastElList = [].slice.call(document.querySelectorAll(".toast"));
	var toastList = toastElList.map(function (toastEl) {
		return new bootstrap.Toast(toastEl, { autohide: false });
	});
	toastList.map(function (toast) { toast.show(); })
};
