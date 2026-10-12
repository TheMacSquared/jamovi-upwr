
'use strict';

import ProgressStream from '../utils/progressstream';

export function events() {
    let stream = new ProgressStream();
    stream.resolve(undefined);
    return stream;
}

export function promptSignIn(opts) {

}

export function currentUser() {
    return null;
}

export async function getAuthToken(forceRefresh = false) {
    return null;
}

export async function signOut() {

}

export async function waitForSignIn() {

}

export function init() {

}

// called once the instance has connected. an instance can't connect without a
// session, so before this there's no pod to send anything to.
export function beginSync() {

}

export interface IEmbedOptions {
    channelId: number;
}

export async function embed(options: IEmbedOptions): Promise<{ [x: string]: any }> {
    return { };
}

export default { init, beginSync, currentUser, events, getAuthToken, signOut, promptSignIn, waitForSignIn, embed };
