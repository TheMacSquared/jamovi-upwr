'use strict';
import { HTMLElementCreator as HTML }  from './htmlelementcreator';
import upwrLogo from '../assets/upwr-logotyp-pl-poziomy.png';

class jamoviIcon {
    el: HTMLElement;

    constructor(version: string) {
        this.el = HTML.parse(`<div class="icon-info-box">
            <div class="icon-version" style="flex-direction: column; align-items: center; gap: 16px;">
                <img src="${upwrLogo}" style="height: 80px;" alt="UPWr">
                <div class="version-text">jUPWR 1.0 <span style="font-size: 0.8em; color: #BBBBBB;">(jamovi ${this.cleanVersion(version)})</span></div>
            </div>
        </div>`);
    }

    cleanVersion(version: string): string {
        var i = -1;
        let n = 3;

        while (n-- && i++ < version.length) {
            i = version.indexOf('.', i);
            if (i < 0) break;
        }

        if (i !== -1) 
            return version.substring(0, i);
        
        return version;
    }
}

export default jamoviIcon;