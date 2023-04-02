/*
 https://github.com/ak1211/smartmeter-route-b-app
 Copyright (c) 2023 Akihiro Yamamoto.
 Licensed under the MIT License.
 See LICENSE file in the project root for full license information.
*/
"use strict";

// Web Serial API 

export const getReaderImpl = (serialport) => { return serialport.readable.getReader(); };

export const cancelReaderImpl = (reader) => { return reader.cancel(); };

export const releaseLockReaderImpl = (reader) => { return reader.releaseLock(); };

export const readImpl = (reader) => { return reader.read(); };

export const getWriterImpl = (serialport) => { return serialport.writable.getWriter(); };

export const releaseLockWriterImpl = (writer) => { return writer.releaseLock(); };

export const writeImpl = (writer, chunk) => { return writer.write(chunk); };

export const requestPortImpl = () => { return navigator.serial.requestPort(); };

export const openPortImpl = (serialport, options) => { return serialport.open(options); };

export const closePortImpl = (serialport) => { return serialport.close(); };

export const forgetPortImpl = (serialport) => { return serialport.forget(); };
