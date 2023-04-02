/*
 https://github.com/ak1211/smartmeter-route-b-app
 Copyright (c) 2023 Akihiro Yamamoto.
 Licensed under the MIT License.
 See LICENSE file in the project root for full license information.
*/
"use strict";

// Bootstrap toast new
export const bootstrapToastShow = function () {
	var toastElList = [].slice.call(document.querySelectorAll(".toast"));
	var toastList = toastElList.map(function (toastEl) {
		return new bootstrap.Toast(toastEl, {autohide: false});
	});
	toastList.map(function (toast) { toast.show(); })
};
