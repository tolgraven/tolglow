function wu(n,t,e){return n==null?t+e:n+Math.max(1,Math.floor((t-n)/e)+1)*e}var No=class{constructor({now:t=()=>performance.now(),raf:e=globalThis.requestAnimationFrame?.bind(globalThis),cancelRaf:i=globalThis.cancelAnimationFrame?.bind(globalThis),timer:s=globalThis.setTimeout.bind(globalThis),cancelTimer:r=globalThis.clearTimeout.bind(globalThis)}={}){Object.assign(this,{now:t,raf:e,cancelRaf:i,timer:s,cancelTimer:r}),this.pending=!1}cancel(){this.rafId!=null&&this.cancelRaf?.(this.rafId),this.timerId!=null&&this.cancelTimer(this.timerId),this.rafId=this.timerId=null,this.pending=!1}at(t,e,i=.5){this.cancel(),this.pending=!0;let s=o=>{if(this.rafId=this.timerId=null,o+i<t){r();return}this.pending=!1,e(o)},r=()=>{let o=Math.max(0,t-this.now());if(this.raf&&o<=18)try{this.rafId=this.raf(s);return}catch(a){if(a.name!=="NotSupportedError")throw a;this.raf=null}this.raf?this.timerId=this.timer(r,o-16):this.timerId=this.timer(()=>s(this.now()),Math.max(1,o))};r()}},Uo=class{constructor(t,e=globalThis){this.changed=t,this.host=e,this.active=!1,this.tick=this.tick.bind(this)}setActive(t){t===this.active||!this.host.requestAnimationFrame||(this.active=t,t?this.start():(this.host.cancelAnimationFrame(this.raf),this.host.clearTimeout(this.timer)))}start(){this.gaps=[],this.samples=0,this.previous=null,this.raf=this.host.requestAnimationFrame(this.tick)}tick(t){if(!this.active)return;this.samples++;let e=this.previous===null?0:t-this.previous;if(this.previous=t,e>=3&&e<=120&&this.gaps.push(e),this.gaps.length<16&&this.samples<40){this.raf=this.host.requestAnimationFrame(this.tick);return}if(this.gaps.length>=8){this.gaps.sort((o,a)=>o-a);let i=this.gaps[Math.floor(this.gaps.length/4)],s=this.gaps.map(o=>o/Math.max(1,Math.round(o/i))).sort((o,a)=>o-a),r=Math.round(1e3/s[Math.floor(s.length/2)]);(this.hz==null||Math.abs(this.hz-r)>=2)&&(this.hz=r,this.changed(r))}this.active&&(this.timer=this.host.setTimeout(()=>this.start(),3e4))}};var bn=n=>JSON.stringify([n?.["runtime-id"]??null,n?.["show-id"]??null,n?.epoch??null]);var Un={LEFT:0,MIDDLE:1,RIGHT:2,ROTATE:0,DOLLY:1,PAN:2},Oi={ROTATE:0,PAN:1,DOLLY_PAN:2,DOLLY_ROTATE:3},ld=0,rh=1,cd=2;var os=1,hd=2,Js=3,Fn=0,qe=1,De=2,yi=0,$s=1,$r=2,oh=3,ah=4,Xa=5;var pn=100,ud=101,dd=102,fd=103,pd=104,qa=200,js=201,md=202,gd=203,lh=204,ch=205,xd=206,_d=207,vd=208,yd=209,Md=210,Sd=211,bd=212,wd=213,Ed=214,da=0,fa=1,pa=2,Ns=3,ma=4,ga=5,xa=6,_a=7,Ya=0,Td=1,Ad=2,qi=0,jr=1,Qr=2,to=3,as=4,eo=5,io=6,no=7;var hh=300,ls=301,Qs=302,Za=303,Ka=304,cn=1e3,Gi=1001,va=1002,Ce=1003,Rd=1004;var so=1005;var oe=1006,Ja=1007;var Bi=1008;var ti=1009,uh=1010,dh=1011,tr=1012,$a=1013,oi=1014,ui=1015,de=1016,ja=1017,Qa=1018,cs=1020,fh=35902,ph=35899,mh=1021,gh=1022,ai=1023,Di=1026,mn=1027,On=1028,tl=1029,Bn=1030,el=1031;var il=1033,ro=33776,oo=33777,ao=33778,lo=33779,nl=35840,sl=35841,rl=35842,ol=35843,al=36196,ll=37492,cl=37496,hl=37488,ul=37489,co=37490,dl=37491,fl=37808,pl=37809,ml=37810,gl=37811,xl=37812,_l=37813,vl=37814,yl=37815,Ml=37816,Sl=37817,bl=37818,wl=37819,El=37820,Tl=37821,Al=36492,Rl=36494,Cl=36495,Pl=36283,Il=36284,ho=36285,Dl=36286;var Rr=2300,ya=2301,ha=2302,Yc=2303,Zc=2400,Kc=2401,Jc=2402;var Cd=3200;var uo=0,Pd=1,zi="",Re="srgb",Cn="srgb-linear",Cr="linear",Qt="srgb";var ua=7680;var Id=519,Dd=512,Ld=513,Nd=514,er=515,Ud=516,Fd=517,xh=518,_h=519,Od=35044,Yi=35048;var gn="300 es",Ei=2e3,Us=2001;function Op(n){for(let t=n.length-1;t>=0;--t)if(n[t]>=65535)return!0;return!1}function Bp(n){return ArrayBuffer.isView(n)&&!(n instanceof DataView)}function Pr(n){return document.createElementNS("http://www.w3.org/1999/xhtml",n)}function Bd(){let n=Pr("canvas");return n.style.display="block",n}var Eu={},Fs=null;function vh(...n){let t="THREE."+n.shift();Fs?Fs("log",t,...n):console.log(t,...n)}function zd(n){let t=n[0];if(typeof t=="string"&&t.startsWith("TSL:")){let e=n[1];e&&e.isStackTrace?n[0]+=" "+e.getLocation():n[1]='Stack trace not available. Enable "THREE.Node.captureStackTrace" to capture stack traces.'}return n}function It(...n){n=zd(n);let t="THREE."+n.shift();if(Fs)Fs("warn",t,...n);else{let e=n[0];e&&e.isStackTrace?console.warn(e.getError(t)):console.warn(t,...n)}}function Ut(...n){n=zd(n);let t="THREE."+n.shift();if(Fs)Fs("error",t,...n);else{let e=n[0];e&&e.isStackTrace?console.error(e.getError(t)):console.error(t,...n)}}function Kn(...n){let t=n.join(" ");t in Eu||(Eu[t]=!0,It(...n))}function kd(n,t,e){return new Promise(function(i,s){function r(){switch(n.clientWaitSync(t,n.SYNC_FLUSH_COMMANDS_BIT,0)){case n.WAIT_FAILED:s();break;case n.TIMEOUT_EXPIRED:setTimeout(r,e);break;default:i()}}setTimeout(r,e)})}var Hd={[da]:da,[pa]:xa,[ma]:ma,[Ns]:ga,[fa]:fa,[xa]:pa,[_a]:_a,[ga]:Ns},Li=class{addEventListener(t,e){this._listeners===void 0&&(this._listeners={});let i=this._listeners;i[t]===void 0&&(i[t]=[]),i[t].indexOf(e)===-1&&i[t].push(e)}hasEventListener(t,e){let i=this._listeners;return i===void 0?!1:i[t]!==void 0&&i[t].indexOf(e)!==-1}removeEventListener(t,e){let i=this._listeners;if(i===void 0)return;let s=i[t];if(s!==void 0){let r=s.indexOf(e);r!==-1&&s.splice(r,1)}}dispatchEvent(t){let e=this._listeners;if(e===void 0)return;let i=e[t.type];if(i!==void 0){t.target=this;let s=i.slice(0);for(let r=0,o=s.length;r<o;r++)s[r].call(this,t);t.target=null}}},$e=["00","01","02","03","04","05","06","07","08","09","0a","0b","0c","0d","0e","0f","10","11","12","13","14","15","16","17","18","19","1a","1b","1c","1d","1e","1f","20","21","22","23","24","25","26","27","28","29","2a","2b","2c","2d","2e","2f","30","31","32","33","34","35","36","37","38","39","3a","3b","3c","3d","3e","3f","40","41","42","43","44","45","46","47","48","49","4a","4b","4c","4d","4e","4f","50","51","52","53","54","55","56","57","58","59","5a","5b","5c","5d","5e","5f","60","61","62","63","64","65","66","67","68","69","6a","6b","6c","6d","6e","6f","70","71","72","73","74","75","76","77","78","79","7a","7b","7c","7d","7e","7f","80","81","82","83","84","85","86","87","88","89","8a","8b","8c","8d","8e","8f","90","91","92","93","94","95","96","97","98","99","9a","9b","9c","9d","9e","9f","a0","a1","a2","a3","a4","a5","a6","a7","a8","a9","aa","ab","ac","ad","ae","af","b0","b1","b2","b3","b4","b5","b6","b7","b8","b9","ba","bb","bc","bd","be","bf","c0","c1","c2","c3","c4","c5","c6","c7","c8","c9","ca","cb","cc","cd","ce","cf","d0","d1","d2","d3","d4","d5","d6","d7","d8","d9","da","db","dc","dd","de","df","e0","e1","e2","e3","e4","e5","e6","e7","e8","e9","ea","eb","ec","ed","ee","ef","f0","f1","f2","f3","f4","f5","f6","f7","f8","f9","fa","fb","fc","fd","fe","ff"],Tu=1234567,Ds=Math.PI/180,Jn=180/Math.PI;function ir(){let n=Math.random()*4294967295|0,t=Math.random()*4294967295|0,e=Math.random()*4294967295|0,i=Math.random()*4294967295|0;return($e[n&255]+$e[n>>8&255]+$e[n>>16&255]+$e[n>>24&255]+"-"+$e[t&255]+$e[t>>8&255]+"-"+$e[t>>16&15|64]+$e[t>>24&255]+"-"+$e[e&63|128]+$e[e>>8&255]+"-"+$e[e>>16&255]+$e[e>>24&255]+$e[i&255]+$e[i>>8&255]+$e[i>>16&255]+$e[i>>24&255]).toLowerCase()}function kt(n,t,e){return Math.max(t,Math.min(e,n))}function yh(n,t){return(n%t+t)%t}function zp(n,t,e,i,s){return i+(n-t)*(s-i)/(e-t)}function kp(n,t,e){return n!==t?(e-n)/(t-n):0}function Er(n,t,e){return(1-e)*n+e*t}function Hp(n,t,e,i){return Er(n,t,1-Math.exp(-e*i))}function Vp(n,t=1){return t-Math.abs(yh(n,t*2)-t)}function Gp(n,t,e){return n<=t?0:n>=e?1:(n=(n-t)/(e-t),n*n*(3-2*n))}function Wp(n,t,e){return n<=t?0:n>=e?1:(n=(n-t)/(e-t),n*n*n*(n*(n*6-15)+10))}function Xp(n,t){return n+Math.floor(Math.random()*(t-n+1))}function qp(n,t){return n+Math.random()*(t-n)}function Yp(n){return n*(.5-Math.random())}function Zp(n){n!==void 0&&(Tu=n);let t=Tu+=1831565813;return t=Math.imul(t^t>>>15,t|1),t^=t+Math.imul(t^t>>>7,t|61),((t^t>>>14)>>>0)/4294967296}function Kp(n){return n*Ds}function Jp(n){return n*Jn}function $p(n){return n>0&&Number.isInteger(n)&&2**Math.round(Math.log2(n))===n}function jp(n){return Math.pow(2,Math.ceil(Math.log(n)/Math.LN2))}function Mh(n){return Math.pow(2,Math.floor(Math.log(n)/Math.LN2))}function Qp(n,t,e,i,s){let r=Math.cos,o=Math.sin,a=r(e/2),l=o(e/2),c=r((t+i)/2),h=o((t+i)/2),u=r((t-i)/2),d=o((t-i)/2),f=r((i-t)/2),p=o((i-t)/2);switch(s){case"XYX":n.set(a*h,l*u,l*d,a*c);break;case"YZY":n.set(l*d,a*h,l*u,a*c);break;case"ZXZ":n.set(l*u,l*d,a*h,a*c);break;case"XZX":n.set(a*h,l*p,l*f,a*c);break;case"YXY":n.set(l*f,a*h,l*p,a*c);break;case"ZYZ":n.set(l*p,l*f,a*h,a*c);break;default:It("MathUtils: .setQuaternionFromProperEuler() encountered an unknown order: "+s)}}function Sh(n){switch(n.constructor){case Uint32Array:return 4294967295;case Uint16Array:return 65535;case Uint8Array:case Uint8ClampedArray:return 255;case Int32Array:return 2147483647;case Int16Array:return 32767;case Int8Array:return 127;default:return}}function Vd(n,t){if(t.constructor===Float32Array)return n;let e=Sh(t);if(e===void 0)throw new Error("THREE.MathUtils: Invalid component type.");return Math.max(n/e,-1)}function Gd(n,t){if(t.constructor===Float32Array)return n;let e=Sh(t);if(e===void 0)throw new Error("THREE.MathUtils: Invalid component type.");return Math.round(n*e)}var nr={DEG2RAD:Ds,RAD2DEG:Jn,generateUUID:ir,clamp:kt,euclideanModulo:yh,mapLinear:zp,inverseLerp:kp,lerp:Er,damp:Hp,pingpong:Vp,smoothstep:Gp,smootherstep:Wp,randInt:Xp,randFloat:qp,randFloatSpread:Yp,seededRandom:Zp,degToRad:Kp,radToDeg:Jp,isPowerOfTwo:$p,ceilPowerOfTwo:jp,floorPowerOfTwo:Mh,setQuaternionFromProperEuler:Qp,normalize:Gd,denormalize:Vd},et=class{static{this.prototype.isVector2=!0}constructor(t=0,e=0){this.x=t,this.y=e}get width(){return this.x}set width(t){this.x=t}get height(){return this.y}set height(t){this.y=t}set(t,e){return this.x=t,this.y=e,this}setScalar(t){return this.x=t,this.y=t,this}setX(t){return this.x=t,this}setY(t){return this.y=t,this}setComponent(t,e){switch(t){case 0:this.x=e;break;case 1:this.y=e;break;default:throw new Error("THREE.Vector2: index is out of range: "+t)}return this}getComponent(t){switch(t){case 0:return this.x;case 1:return this.y;default:throw new Error("THREE.Vector2: index is out of range: "+t)}}clone(){return new this.constructor(this.x,this.y)}copy(t){return this.x=t.x,this.y=t.y,this}add(t){return this.x+=t.x,this.y+=t.y,this}addScalar(t){return this.x+=t,this.y+=t,this}addVectors(t,e){return this.x=t.x+e.x,this.y=t.y+e.y,this}addScaledVector(t,e){return this.x+=t.x*e,this.y+=t.y*e,this}sub(t){return this.x-=t.x,this.y-=t.y,this}subScalar(t){return this.x-=t,this.y-=t,this}subVectors(t,e){return this.x=t.x-e.x,this.y=t.y-e.y,this}multiply(t){return this.x*=t.x,this.y*=t.y,this}multiplyScalar(t){return this.x*=t,this.y*=t,this}divide(t){return this.x/=t.x,this.y/=t.y,this}divideScalar(t){return this.multiplyScalar(1/t)}applyMatrix3(t){let e=this.x,i=this.y,s=t.elements;return this.x=s[0]*e+s[3]*i+s[6],this.y=s[1]*e+s[4]*i+s[7],this}min(t){return this.x=Math.min(this.x,t.x),this.y=Math.min(this.y,t.y),this}max(t){return this.x=Math.max(this.x,t.x),this.y=Math.max(this.y,t.y),this}clamp(t,e){return this.x=kt(this.x,t.x,e.x),this.y=kt(this.y,t.y,e.y),this}clampScalar(t,e){return this.x=kt(this.x,t,e),this.y=kt(this.y,t,e),this}clampLength(t,e){let i=this.length();return this.divideScalar(i||1).multiplyScalar(kt(i,t,e))}floor(){return this.x=Math.floor(this.x),this.y=Math.floor(this.y),this}ceil(){return this.x=Math.ceil(this.x),this.y=Math.ceil(this.y),this}round(){return this.x=Math.round(this.x),this.y=Math.round(this.y),this}roundToZero(){return this.x=Math.trunc(this.x),this.y=Math.trunc(this.y),this}negate(){return this.x=-this.x,this.y=-this.y,this}dot(t){return this.x*t.x+this.y*t.y}cross(t){return this.x*t.y-this.y*t.x}lengthSq(){return this.x*this.x+this.y*this.y}length(){return Math.sqrt(this.x*this.x+this.y*this.y)}manhattanLength(){return Math.abs(this.x)+Math.abs(this.y)}normalize(){return this.divideScalar(this.length()||1)}angle(){return Math.atan2(-this.y,-this.x)+Math.PI}angleTo(t){let e=Math.sqrt(this.lengthSq()*t.lengthSq());if(e===0)return Math.PI/2;let i=this.dot(t)/e;return Math.acos(kt(i,-1,1))}distanceTo(t){return Math.sqrt(this.distanceToSquared(t))}distanceToSquared(t){let e=this.x-t.x,i=this.y-t.y;return e*e+i*i}manhattanDistanceTo(t){return Math.abs(this.x-t.x)+Math.abs(this.y-t.y)}setLength(t){return this.normalize().multiplyScalar(t)}lerp(t,e){return this.x+=(t.x-this.x)*e,this.y+=(t.y-this.y)*e,this}lerpVectors(t,e,i){return this.x=t.x+(e.x-t.x)*i,this.y=t.y+(e.y-t.y)*i,this}equals(t){return t.x===this.x&&t.y===this.y}fromArray(t,e=0){return this.x=t[e],this.y=t[e+1],this}toArray(t=[],e=0){return t[e]=this.x,t[e+1]=this.y,t}fromBufferAttribute(t,e){return this.x=t.getX(e),this.y=t.getY(e),this}rotateAround(t,e){let i=Math.cos(e),s=Math.sin(e),r=this.x-t.x,o=this.y-t.y;return this.x=r*i-o*s+t.x,this.y=r*s+o*i+t.y,this}random(){return this.x=Math.random(),this.y=Math.random(),this}*[Symbol.iterator](){yield this.x,yield this.y}},Qe=class{constructor(t=0,e=0,i=0,s=1){this.isQuaternion=!0,this._x=t,this._y=e,this._z=i,this._w=s}static slerpFlat(t,e,i,s,r,o,a){let l=i[s+0],c=i[s+1],h=i[s+2],u=i[s+3],d=r[o+0],f=r[o+1],p=r[o+2],x=r[o+3];if(u!==x||l!==d||c!==f||h!==p){let m=l*d+c*f+h*p+u*x;m<0&&(d=-d,f=-f,p=-p,x=-x,m=-m);let g=1-a;if(m<.9995){let _=Math.acos(m),y=Math.sin(_);g=Math.sin(g*_)/y,a=Math.sin(a*_)/y,l=l*g+d*a,c=c*g+f*a,h=h*g+p*a,u=u*g+x*a}else{l=l*g+d*a,c=c*g+f*a,h=h*g+p*a,u=u*g+x*a;let _=1/Math.sqrt(l*l+c*c+h*h+u*u);l*=_,c*=_,h*=_,u*=_}}t[e]=l,t[e+1]=c,t[e+2]=h,t[e+3]=u}static multiplyQuaternionsFlat(t,e,i,s,r,o){let a=i[s],l=i[s+1],c=i[s+2],h=i[s+3],u=r[o],d=r[o+1],f=r[o+2],p=r[o+3];return t[e]=a*p+h*u+l*f-c*d,t[e+1]=l*p+h*d+c*u-a*f,t[e+2]=c*p+h*f+a*d-l*u,t[e+3]=h*p-a*u-l*d-c*f,t}get x(){return this._x}set x(t){this._x=t,this._onChangeCallback()}get y(){return this._y}set y(t){this._y=t,this._onChangeCallback()}get z(){return this._z}set z(t){this._z=t,this._onChangeCallback()}get w(){return this._w}set w(t){this._w=t,this._onChangeCallback()}set(t,e,i,s){return this._x=t,this._y=e,this._z=i,this._w=s,this._onChangeCallback(),this}clone(){return new this.constructor(this._x,this._y,this._z,this._w)}copy(t){return this._x=t.x,this._y=t.y,this._z=t.z,this._w=t.w,this._onChangeCallback(),this}setFromEuler(t,e=!0){let i=t._x,s=t._y,r=t._z,o=t._order,a=Math.cos,l=Math.sin,c=a(i/2),h=a(s/2),u=a(r/2),d=l(i/2),f=l(s/2),p=l(r/2);switch(o){case"XYZ":this._x=d*h*u+c*f*p,this._y=c*f*u-d*h*p,this._z=c*h*p+d*f*u,this._w=c*h*u-d*f*p;break;case"YXZ":this._x=d*h*u+c*f*p,this._y=c*f*u-d*h*p,this._z=c*h*p-d*f*u,this._w=c*h*u+d*f*p;break;case"ZXY":this._x=d*h*u-c*f*p,this._y=c*f*u+d*h*p,this._z=c*h*p+d*f*u,this._w=c*h*u-d*f*p;break;case"ZYX":this._x=d*h*u-c*f*p,this._y=c*f*u+d*h*p,this._z=c*h*p-d*f*u,this._w=c*h*u+d*f*p;break;case"YZX":this._x=d*h*u+c*f*p,this._y=c*f*u+d*h*p,this._z=c*h*p-d*f*u,this._w=c*h*u-d*f*p;break;case"XZY":this._x=d*h*u-c*f*p,this._y=c*f*u-d*h*p,this._z=c*h*p+d*f*u,this._w=c*h*u+d*f*p;break;default:It("Quaternion: .setFromEuler() encountered an unknown order: "+o)}return e===!0&&this._onChangeCallback(),this}setFromAxisAngle(t,e){let i=e/2,s=Math.sin(i);return this._x=t.x*s,this._y=t.y*s,this._z=t.z*s,this._w=Math.cos(i),this._onChangeCallback(),this}setFromRotationMatrix(t){let e=t.elements,i=e[0],s=e[4],r=e[8],o=e[1],a=e[5],l=e[9],c=e[2],h=e[6],u=e[10],d=i+a+u;if(d>0){let f=.5/Math.sqrt(d+1);this._w=.25/f,this._x=(h-l)*f,this._y=(r-c)*f,this._z=(o-s)*f}else if(i>a&&i>u){let f=2*Math.sqrt(1+i-a-u);this._w=(h-l)/f,this._x=.25*f,this._y=(s+o)/f,this._z=(r+c)/f}else if(a>u){let f=2*Math.sqrt(1+a-i-u);this._w=(r-c)/f,this._x=(s+o)/f,this._y=.25*f,this._z=(l+h)/f}else{let f=2*Math.sqrt(1+u-i-a);this._w=(o-s)/f,this._x=(r+c)/f,this._y=(l+h)/f,this._z=.25*f}return this._onChangeCallback(),this}setFromUnitVectors(t,e){let i=t.dot(e)+1;return i<1e-8?(i=0,Math.abs(t.x)>Math.abs(t.z)?(this._x=-t.y,this._y=t.x,this._z=0,this._w=i):(this._x=0,this._y=-t.z,this._z=t.y,this._w=i)):(this._x=t.y*e.z-t.z*e.y,this._y=t.z*e.x-t.x*e.z,this._z=t.x*e.y-t.y*e.x,this._w=i),this.normalize()}angleTo(t){return 2*Math.acos(Math.abs(kt(this.dot(t),-1,1)))}rotateTowards(t,e){let i=this.angleTo(t);if(i===0)return this;let s=Math.min(1,e/i);return this.slerp(t,s),this}identity(){return this.set(0,0,0,1)}invert(){return this.conjugate()}conjugate(){return this._x*=-1,this._y*=-1,this._z*=-1,this._onChangeCallback(),this}dot(t){return this._x*t._x+this._y*t._y+this._z*t._z+this._w*t._w}lengthSq(){return this._x*this._x+this._y*this._y+this._z*this._z+this._w*this._w}length(){return Math.sqrt(this._x*this._x+this._y*this._y+this._z*this._z+this._w*this._w)}normalize(){let t=this.length();return t===0?(this._x=0,this._y=0,this._z=0,this._w=1):(t=1/t,this._x=this._x*t,this._y=this._y*t,this._z=this._z*t,this._w=this._w*t),this._onChangeCallback(),this}multiply(t){return this.multiplyQuaternions(this,t)}premultiply(t){return this.multiplyQuaternions(t,this)}multiplyQuaternions(t,e){let i=t._x,s=t._y,r=t._z,o=t._w,a=e._x,l=e._y,c=e._z,h=e._w;return this._x=i*h+o*a+s*c-r*l,this._y=s*h+o*l+r*a-i*c,this._z=r*h+o*c+i*l-s*a,this._w=o*h-i*a-s*l-r*c,this._onChangeCallback(),this}slerp(t,e){let i=t._x,s=t._y,r=t._z,o=t._w,a=this.dot(t);a<0&&(i=-i,s=-s,r=-r,o=-o,a=-a);let l=1-e;if(a<.9995){let c=Math.acos(a),h=Math.sin(c);l=Math.sin(l*c)/h,e=Math.sin(e*c)/h,this._x=this._x*l+i*e,this._y=this._y*l+s*e,this._z=this._z*l+r*e,this._w=this._w*l+o*e,this._onChangeCallback()}else this._x=this._x*l+i*e,this._y=this._y*l+s*e,this._z=this._z*l+r*e,this._w=this._w*l+o*e,this.normalize();return this}slerpQuaternions(t,e,i){return this.copy(t).slerp(e,i)}random(){let t=2*Math.PI*Math.random(),e=2*Math.PI*Math.random(),i=Math.random(),s=Math.sqrt(1-i),r=Math.sqrt(i);return this.set(s*Math.sin(t),s*Math.cos(t),r*Math.sin(e),r*Math.cos(e))}equals(t){return t._x===this._x&&t._y===this._y&&t._z===this._z&&t._w===this._w}fromArray(t,e=0){return this._x=t[e],this._y=t[e+1],this._z=t[e+2],this._w=t[e+3],this._onChangeCallback(),this}toArray(t=[],e=0){return t[e]=this._x,t[e+1]=this._y,t[e+2]=this._z,t[e+3]=this._w,t}fromBufferAttribute(t,e){return this._x=t.getX(e),this._y=t.getY(e),this._z=t.getZ(e),this._w=t.getW(e),this._onChangeCallback(),this}toJSON(){return this.toArray()}_onChange(t){return this._onChangeCallback=t,this}_onChangeCallback(){}*[Symbol.iterator](){yield this._x,yield this._y,yield this._z,yield this._w}},R=class{static{this.prototype.isVector3=!0}constructor(t=0,e=0,i=0){this.x=t,this.y=e,this.z=i}set(t,e,i){return i===void 0&&(i=this.z),this.x=t,this.y=e,this.z=i,this}setScalar(t){return this.x=t,this.y=t,this.z=t,this}setX(t){return this.x=t,this}setY(t){return this.y=t,this}setZ(t){return this.z=t,this}setComponent(t,e){switch(t){case 0:this.x=e;break;case 1:this.y=e;break;case 2:this.z=e;break;default:throw new Error("THREE.Vector3: index is out of range: "+t)}return this}getComponent(t){switch(t){case 0:return this.x;case 1:return this.y;case 2:return this.z;default:throw new Error("THREE.Vector3: index is out of range: "+t)}}clone(){return new this.constructor(this.x,this.y,this.z)}copy(t){return this.x=t.x,this.y=t.y,this.z=t.z,this}add(t){return this.x+=t.x,this.y+=t.y,this.z+=t.z,this}addScalar(t){return this.x+=t,this.y+=t,this.z+=t,this}addVectors(t,e){return this.x=t.x+e.x,this.y=t.y+e.y,this.z=t.z+e.z,this}addScaledVector(t,e){return this.x+=t.x*e,this.y+=t.y*e,this.z+=t.z*e,this}sub(t){return this.x-=t.x,this.y-=t.y,this.z-=t.z,this}subScalar(t){return this.x-=t,this.y-=t,this.z-=t,this}subVectors(t,e){return this.x=t.x-e.x,this.y=t.y-e.y,this.z=t.z-e.z,this}multiply(t){return this.x*=t.x,this.y*=t.y,this.z*=t.z,this}multiplyScalar(t){return this.x*=t,this.y*=t,this.z*=t,this}multiplyVectors(t,e){return this.x=t.x*e.x,this.y=t.y*e.y,this.z=t.z*e.z,this}applyEuler(t){return this.applyQuaternion(Au.setFromEuler(t))}applyAxisAngle(t,e){return this.applyQuaternion(Au.setFromAxisAngle(t,e))}applyMatrix3(t){let e=this.x,i=this.y,s=this.z,r=t.elements;return this.x=r[0]*e+r[3]*i+r[6]*s,this.y=r[1]*e+r[4]*i+r[7]*s,this.z=r[2]*e+r[5]*i+r[8]*s,this}applyNormalMatrix(t){return this.applyMatrix3(t).normalize()}applyMatrix4(t){let e=this.x,i=this.y,s=this.z,r=t.elements,o=1/(r[3]*e+r[7]*i+r[11]*s+r[15]);return this.x=(r[0]*e+r[4]*i+r[8]*s+r[12])*o,this.y=(r[1]*e+r[5]*i+r[9]*s+r[13])*o,this.z=(r[2]*e+r[6]*i+r[10]*s+r[14])*o,this}applyQuaternion(t){let e=this.x,i=this.y,s=this.z,r=t.x,o=t.y,a=t.z,l=t.w,c=2*(o*s-a*i),h=2*(a*e-r*s),u=2*(r*i-o*e);return this.x=e+l*c+o*u-a*h,this.y=i+l*h+a*c-r*u,this.z=s+l*u+r*h-o*c,this}project(t){return this.applyMatrix4(t.matrixWorldInverse).applyMatrix4(t.projectionMatrix)}unproject(t){return this.applyMatrix4(t.projectionMatrixInverse).applyMatrix4(t.matrixWorld)}transformDirection(t){let e=this.x,i=this.y,s=this.z,r=t.elements;return this.x=r[0]*e+r[4]*i+r[8]*s,this.y=r[1]*e+r[5]*i+r[9]*s,this.z=r[2]*e+r[6]*i+r[10]*s,this.normalize()}divide(t){return this.x/=t.x,this.y/=t.y,this.z/=t.z,this}divideScalar(t){return this.multiplyScalar(1/t)}min(t){return this.x=Math.min(this.x,t.x),this.y=Math.min(this.y,t.y),this.z=Math.min(this.z,t.z),this}max(t){return this.x=Math.max(this.x,t.x),this.y=Math.max(this.y,t.y),this.z=Math.max(this.z,t.z),this}clamp(t,e){return this.x=kt(this.x,t.x,e.x),this.y=kt(this.y,t.y,e.y),this.z=kt(this.z,t.z,e.z),this}clampScalar(t,e){return this.x=kt(this.x,t,e),this.y=kt(this.y,t,e),this.z=kt(this.z,t,e),this}clampLength(t,e){let i=this.length();return this.divideScalar(i||1).multiplyScalar(kt(i,t,e))}floor(){return this.x=Math.floor(this.x),this.y=Math.floor(this.y),this.z=Math.floor(this.z),this}ceil(){return this.x=Math.ceil(this.x),this.y=Math.ceil(this.y),this.z=Math.ceil(this.z),this}round(){return this.x=Math.round(this.x),this.y=Math.round(this.y),this.z=Math.round(this.z),this}roundToZero(){return this.x=Math.trunc(this.x),this.y=Math.trunc(this.y),this.z=Math.trunc(this.z),this}negate(){return this.x=-this.x,this.y=-this.y,this.z=-this.z,this}dot(t){return this.x*t.x+this.y*t.y+this.z*t.z}lengthSq(){return this.x*this.x+this.y*this.y+this.z*this.z}length(){return Math.sqrt(this.x*this.x+this.y*this.y+this.z*this.z)}manhattanLength(){return Math.abs(this.x)+Math.abs(this.y)+Math.abs(this.z)}normalize(){return this.divideScalar(this.length()||1)}setLength(t){return this.normalize().multiplyScalar(t)}lerp(t,e){return this.x+=(t.x-this.x)*e,this.y+=(t.y-this.y)*e,this.z+=(t.z-this.z)*e,this}lerpVectors(t,e,i){return this.x=t.x+(e.x-t.x)*i,this.y=t.y+(e.y-t.y)*i,this.z=t.z+(e.z-t.z)*i,this}cross(t){return this.crossVectors(this,t)}crossVectors(t,e){let i=t.x,s=t.y,r=t.z,o=e.x,a=e.y,l=e.z;return this.x=s*l-r*a,this.y=r*o-i*l,this.z=i*a-s*o,this}projectOnVector(t){let e=t.lengthSq();if(e===0)return this.set(0,0,0);let i=t.dot(this)/e;return this.copy(t).multiplyScalar(i)}projectOnPlane(t){return Sc.copy(this).projectOnVector(t),this.sub(Sc)}reflect(t){return this.sub(Sc.copy(t).multiplyScalar(2*this.dot(t)))}angleTo(t){let e=Math.sqrt(this.lengthSq()*t.lengthSq());if(e===0)return Math.PI/2;let i=this.dot(t)/e;return Math.acos(kt(i,-1,1))}distanceTo(t){return Math.sqrt(this.distanceToSquared(t))}distanceToSquared(t){let e=this.x-t.x,i=this.y-t.y,s=this.z-t.z;return e*e+i*i+s*s}manhattanDistanceTo(t){return Math.abs(this.x-t.x)+Math.abs(this.y-t.y)+Math.abs(this.z-t.z)}setFromSpherical(t){return this.setFromSphericalCoords(t.radius,t.phi,t.theta)}setFromSphericalCoords(t,e,i){let s=Math.sin(e)*t;return this.x=s*Math.sin(i),this.y=Math.cos(e)*t,this.z=s*Math.cos(i),this}setFromCylindrical(t){return this.setFromCylindricalCoords(t.radius,t.theta,t.y)}setFromCylindricalCoords(t,e,i){return this.x=t*Math.sin(e),this.y=i,this.z=t*Math.cos(e),this}setFromMatrixPosition(t){let e=t.elements;return this.x=e[12],this.y=e[13],this.z=e[14],this}setFromMatrixScale(t){let e=this.setFromMatrixColumn(t,0).length(),i=this.setFromMatrixColumn(t,1).length(),s=this.setFromMatrixColumn(t,2).length();return this.x=e,this.y=i,this.z=s,this}setFromMatrixColumn(t,e){return this.fromArray(t.elements,e*4)}setFromMatrix3Column(t,e){return this.fromArray(t.elements,e*3)}setFromEuler(t){return this.x=t._x,this.y=t._y,this.z=t._z,this}setFromColor(t){return this.x=t.r,this.y=t.g,this.z=t.b,this}equals(t){return t.x===this.x&&t.y===this.y&&t.z===this.z}fromArray(t,e=0){return this.x=t[e],this.y=t[e+1],this.z=t[e+2],this}toArray(t=[],e=0){return t[e]=this.x,t[e+1]=this.y,t[e+2]=this.z,t}fromBufferAttribute(t,e){return this.x=t.getX(e),this.y=t.getY(e),this.z=t.getZ(e),this}random(){return this.x=Math.random(),this.y=Math.random(),this.z=Math.random(),this}randomDirection(){let t=Math.random()*Math.PI*2,e=Math.random()*2-1,i=Math.sqrt(1-e*e);return this.x=i*Math.cos(t),this.y=e,this.z=i*Math.sin(t),this}*[Symbol.iterator](){yield this.x,yield this.y,yield this.z}},Sc=new R,Au=new Qe,Ft=class{static{this.prototype.isMatrix3=!0}constructor(t,e,i,s,r,o,a,l,c){this.elements=[1,0,0,0,1,0,0,0,1],t!==void 0&&this.set(t,e,i,s,r,o,a,l,c)}set(t,e,i,s,r,o,a,l,c){let h=this.elements;return h[0]=t,h[1]=s,h[2]=a,h[3]=e,h[4]=r,h[5]=l,h[6]=i,h[7]=o,h[8]=c,this}identity(){return this.set(1,0,0,0,1,0,0,0,1),this}copy(t){let e=this.elements,i=t.elements;return e[0]=i[0],e[1]=i[1],e[2]=i[2],e[3]=i[3],e[4]=i[4],e[5]=i[5],e[6]=i[6],e[7]=i[7],e[8]=i[8],this}extractBasis(t,e,i){return t.setFromMatrix3Column(this,0),e.setFromMatrix3Column(this,1),i.setFromMatrix3Column(this,2),this}setFromMatrix4(t){let e=t.elements;return this.set(e[0],e[4],e[8],e[1],e[5],e[9],e[2],e[6],e[10]),this}multiply(t){return this.multiplyMatrices(this,t)}premultiply(t){return this.multiplyMatrices(t,this)}multiplyMatrices(t,e){let i=t.elements,s=e.elements,r=this.elements,o=i[0],a=i[3],l=i[6],c=i[1],h=i[4],u=i[7],d=i[2],f=i[5],p=i[8],x=s[0],m=s[3],g=s[6],_=s[1],y=s[4],S=s[7],T=s[2],w=s[5],v=s[8];return r[0]=o*x+a*_+l*T,r[3]=o*m+a*y+l*w,r[6]=o*g+a*S+l*v,r[1]=c*x+h*_+u*T,r[4]=c*m+h*y+u*w,r[7]=c*g+h*S+u*v,r[2]=d*x+f*_+p*T,r[5]=d*m+f*y+p*w,r[8]=d*g+f*S+p*v,this}multiplyScalar(t){let e=this.elements;return e[0]*=t,e[3]*=t,e[6]*=t,e[1]*=t,e[4]*=t,e[7]*=t,e[2]*=t,e[5]*=t,e[8]*=t,this}determinant(){let t=this.elements,e=t[0],i=t[1],s=t[2],r=t[3],o=t[4],a=t[5],l=t[6],c=t[7],h=t[8];return e*o*h-e*a*c-i*r*h+i*a*l+s*r*c-s*o*l}invert(){let t=this.elements,e=t[0],i=t[1],s=t[2],r=t[3],o=t[4],a=t[5],l=t[6],c=t[7],h=t[8],u=h*o-a*c,d=a*l-h*r,f=c*r-o*l,p=e*u+i*d+s*f;if(p===0)return this.set(0,0,0,0,0,0,0,0,0);let x=1/p;return t[0]=u*x,t[1]=(s*c-h*i)*x,t[2]=(a*i-s*o)*x,t[3]=d*x,t[4]=(h*e-s*l)*x,t[5]=(s*r-a*e)*x,t[6]=f*x,t[7]=(i*l-c*e)*x,t[8]=(o*e-i*r)*x,this}transpose(){let t,e=this.elements;return t=e[1],e[1]=e[3],e[3]=t,t=e[2],e[2]=e[6],e[6]=t,t=e[5],e[5]=e[7],e[7]=t,this}getNormalMatrix(t){return this.setFromMatrix4(t).invert().transpose()}transposeIntoArray(t){let e=this.elements;return t[0]=e[0],t[1]=e[3],t[2]=e[6],t[3]=e[1],t[4]=e[4],t[5]=e[7],t[6]=e[2],t[7]=e[5],t[8]=e[8],this}setUvTransform(t,e,i,s,r,o,a){let l=Math.cos(r),c=Math.sin(r);return this.set(i*l,i*c,-i*(l*o+c*a)+o+t,-s*c,s*l,-s*(-c*o+l*a)+a+e,0,0,1),this}scale(t,e){return Kn("Matrix3: .scale() is deprecated. Use .makeScale() instead."),this.premultiply(bc.makeScale(t,e)),this}rotate(t){return Kn("Matrix3: .rotate() is deprecated. Use .makeRotation() instead."),this.premultiply(bc.makeRotation(-t)),this}translate(t,e){return Kn("Matrix3: .translate() is deprecated. Use .makeTranslation() instead."),this.premultiply(bc.makeTranslation(t,e)),this}makeTranslation(t,e){return t.isVector2?this.set(1,0,t.x,0,1,t.y,0,0,1):this.set(1,0,t,0,1,e,0,0,1),this}makeRotation(t){let e=Math.cos(t),i=Math.sin(t);return this.set(e,-i,0,i,e,0,0,0,1),this}makeScale(t,e){return this.set(t,0,0,0,e,0,0,0,1),this}equals(t){let e=this.elements,i=t.elements;for(let s=0;s<9;s++)if(e[s]!==i[s])return!1;return!0}fromArray(t,e=0){for(let i=0;i<9;i++)this.elements[i]=t[i+e];return this}toArray(t=[],e=0){let i=this.elements;return t[e]=i[0],t[e+1]=i[1],t[e+2]=i[2],t[e+3]=i[3],t[e+4]=i[4],t[e+5]=i[5],t[e+6]=i[6],t[e+7]=i[7],t[e+8]=i[8],t}clone(){return new this.constructor().fromArray(this.elements)}},bc=new Ft,Ru=new Ft().set(.4123908,.3575843,.1804808,.212639,.7151687,.0721923,.0193308,.1191948,.9505322),Cu=new Ft().set(3.2409699,-1.5373832,-.4986108,-.9692436,1.8759675,.0415551,.0556301,-.203977,1.0569715);function tm(){let n={enabled:!0,workingColorSpace:Cn,spaces:{},convert:function(s,r,o){return this.enabled===!1||r===o||!r||!o||(this.spaces[r].transfer===Qt&&(s.r=ln(s.r),s.g=ln(s.g),s.b=ln(s.b)),this.spaces[r].primaries!==this.spaces[o].primaries&&(s.applyMatrix3(this.spaces[r].toXYZ),s.applyMatrix3(this.spaces[o].fromXYZ)),this.spaces[o].transfer===Qt&&(s.r=Ls(s.r),s.g=Ls(s.g),s.b=Ls(s.b))),s},workingToColorSpace:function(s,r){return this.convert(s,this.workingColorSpace,r)},colorSpaceToWorking:function(s,r){return this.convert(s,r,this.workingColorSpace)},getPrimaries:function(s){return this.spaces[s].primaries},getTransfer:function(s){return s===zi?Cr:this.spaces[s].transfer},getToneMappingMode:function(s){return this.spaces[s].outputColorSpaceConfig.toneMappingMode||"standard"},getLuminanceCoefficients:function(s,r=this.workingColorSpace){return s.fromArray(this.spaces[r].luminanceCoefficients)},define:function(s){Object.assign(this.spaces,s)},_getMatrix:function(s,r,o){return s.copy(this.spaces[o].fromXYZ).multiply(this.spaces[r].toXYZ)},_getDrawingBufferColorSpace:function(s){return this.spaces[s].outputColorSpaceConfig.drawingBufferColorSpace},_getUnpackColorSpace:function(s=this.workingColorSpace){return this.spaces[s].workingColorSpaceConfig.unpackColorSpace}},t=[.64,.33,.3,.6,.15,.06],e=[.2126,.7152,.0722],i=[.3127,.329];return n.define({[Cn]:{primaries:t,whitePoint:i,transfer:Cr,toXYZ:Ru,fromXYZ:Cu,luminanceCoefficients:e,workingColorSpaceConfig:{unpackColorSpace:Re},outputColorSpaceConfig:{drawingBufferColorSpace:Re}},[Re]:{primaries:t,whitePoint:i,transfer:Qt,toXYZ:Ru,fromXYZ:Cu,luminanceCoefficients:e,outputColorSpaceConfig:{drawingBufferColorSpace:Re}}}),n}var qt=tm();function ln(n){return n<.04045?n*.0773993808:Math.pow(n*.9478672986+.0521327014,2.4)}function Ls(n){return n<.0031308?n*12.92:1.055*Math.pow(n,.41666)-.055}var _s,Ma=class{static getDataURL(t,e="image/png"){if(/^data:/i.test(t.src)||typeof HTMLCanvasElement>"u")return t.src;let i;if(t instanceof HTMLCanvasElement)i=t;else{_s===void 0&&(_s=Pr("canvas")),_s.width=t.width,_s.height=t.height;let s=_s.getContext("2d");t instanceof ImageData?s.putImageData(t,0,0):s.drawImage(t,0,0,t.width,t.height),i=_s}return i.toDataURL(e)}static sRGBToLinear(t){if(typeof HTMLImageElement<"u"&&t instanceof HTMLImageElement||typeof HTMLCanvasElement<"u"&&t instanceof HTMLCanvasElement||typeof ImageBitmap<"u"&&t instanceof ImageBitmap){let e=Pr("canvas");e.width=t.width,e.height=t.height;let i=e.getContext("2d");i.drawImage(t,0,0,t.width,t.height);let s=i.getImageData(0,0,t.width,t.height),r=s.data;for(let o=0;o<r.length;o++)r[o]=ln(r[o]/255)*255;return i.putImageData(s,0,0),e}else if(t.data){let e=t.data.slice(0);for(let i=0;i<e.length;i++)e instanceof Uint8Array||e instanceof Uint8ClampedArray?e[i]=Math.floor(ln(e[i]/255)*255):e[i]=ln(e[i]);return{data:e,width:t.width,height:t.height}}else return It("ImageUtils.sRGBToLinear(): Unsupported image type. No color space conversion applied."),t}},em=0,Os=class{constructor(t=null){this.isTextureSource=!0,Object.defineProperty(this,"id",{value:em++}),this.uuid=ir(),this.data=t,this.dataReady=!0,this.version=0}getSize(t){let e=this.data;return typeof HTMLVideoElement<"u"&&e instanceof HTMLVideoElement?t.set(e.videoWidth,e.videoHeight,0):typeof VideoFrame<"u"&&e instanceof VideoFrame?t.set(e.displayWidth,e.displayHeight,0):e!==null?t.set(e.width||0,e.height||0,e.depth||0):t.set(0,0,0),t}set needsUpdate(t){t===!0&&this.version++}toJSON(t){let e=t===void 0||typeof t=="string";if(!e&&t.images[this.uuid]!==void 0)return t.images[this.uuid];let i={uuid:this.uuid,url:""},s=this.data;if(s!==null){let r;if(Array.isArray(s)){r=[];for(let o=0,a=s.length;o<a;o++)s[o].isDataTexture?r.push(wc(s[o].image)):r.push(wc(s[o]))}else r=wc(s);i.url=r}return e||(t.images[this.uuid]=i),i}};function wc(n){return typeof HTMLImageElement<"u"&&n instanceof HTMLImageElement||typeof HTMLCanvasElement<"u"&&n instanceof HTMLCanvasElement||typeof ImageBitmap<"u"&&n instanceof ImageBitmap?Ma.getDataURL(n):n.data?{data:Array.from(n.data),width:n.width,height:n.height,type:n.data.constructor.name}:(It("Texture: Unable to serialize Texture."),{})}var im=0,Ec=new R,We=class n extends Li{constructor(t=n.DEFAULT_IMAGE,e=n.DEFAULT_MAPPING,i=Gi,s=Gi,r=oe,o=Bi,a=ai,l=ti,c=n.DEFAULT_ANISOTROPY,h=zi){super(),this.isTexture=!0,Object.defineProperty(this,"id",{value:im++}),this.uuid=ir(),this.name="",this.source=new Os(t),this.mipmaps=[],this.mapping=e,this.channel=0,this.wrapS=i,this.wrapT=s,this.magFilter=r,this.minFilter=o,this.anisotropy=c,this.format=a,this.internalFormat=null,this.type=l,this.offset=new et(0,0),this.repeat=new et(1,1),this.center=new et(0,0),this.rotation=0,this.matrixAutoUpdate=!0,this.matrix=new Ft,this.generateMipmaps=!0,this.mipmapsAutoUpdate=!0,this.premultiplyAlpha=!1,this.flipY=!0,this.unpackAlignment=4,this.colorSpace=h,this.userData={},this.updateRanges=[],this.version=0,this.onUpdate=null,this.renderTarget=null,this.isRenderTargetTexture=!1,this.isArrayTexture=!!(t&&t.depth&&t.depth>1),this.pmremVersion=0,this.isPMREMTexture=!1,this.normalized=!1}get width(){return this.source.getSize(Ec).x}get height(){return this.source.getSize(Ec).y}get depth(){return this.source.getSize(Ec).z}get image(){return this.source.data}set image(t){this.source.data=t}updateMatrix(){this.matrix.setUvTransform(this.offset.x,this.offset.y,this.repeat.x,this.repeat.y,this.rotation,this.center.x,this.center.y)}addUpdateRange(t,e){this.updateRanges.push({start:t,count:e})}clearUpdateRanges(){this.updateRanges.length=0}clone(){return new this.constructor().copy(this)}copy(t){return this.name=t.name,this.source=t.source,this.mipmaps=t.mipmaps.slice(0),this.mapping=t.mapping,this.channel=t.channel,this.wrapS=t.wrapS,this.wrapT=t.wrapT,this.magFilter=t.magFilter,this.minFilter=t.minFilter,this.anisotropy=t.anisotropy,this.format=t.format,this.internalFormat=t.internalFormat,this.type=t.type,this.normalized=t.normalized,this.offset.copy(t.offset),this.repeat.copy(t.repeat),this.center.copy(t.center),this.rotation=t.rotation,this.matrixAutoUpdate=t.matrixAutoUpdate,this.matrix.copy(t.matrix),this.generateMipmaps=t.generateMipmaps,this.mipmapsAutoUpdate=t.mipmapsAutoUpdate,this.premultiplyAlpha=t.premultiplyAlpha,this.flipY=t.flipY,this.unpackAlignment=t.unpackAlignment,this.colorSpace=t.colorSpace,this.renderTarget=t.renderTarget,this.isRenderTargetTexture=t.isRenderTargetTexture,this.isPMREMTexture=t.isPMREMTexture,this.isArrayTexture=t.isArrayTexture,this.userData=JSON.parse(JSON.stringify(t.userData)),this.needsUpdate=!0,this}setValues(t){for(let e in t){let i=t[e];if(i===void 0){It(`Texture.setValues(): parameter '${e}' has value of undefined.`);continue}let s=this[e];if(s===void 0){It(`Texture.setValues(): property '${e}' does not exist.`);continue}s&&i&&s.isVector2&&i.isVector2||s&&i&&s.isVector3&&i.isVector3||s&&i&&s.isMatrix3&&i.isMatrix3?s.copy(i):this[e]=i}}toJSON(t){let e=t===void 0||typeof t=="string";if(!e&&t.textures[this.uuid]!==void 0)return t.textures[this.uuid];let i={metadata:{version:4.7,type:"Texture",generator:"Texture.toJSON"},uuid:this.uuid,name:this.name,image:this.source.toJSON(t).uuid,mapping:this.mapping,channel:this.channel,repeat:[this.repeat.x,this.repeat.y],offset:[this.offset.x,this.offset.y],center:[this.center.x,this.center.y],rotation:this.rotation,wrap:[this.wrapS,this.wrapT],format:this.format,internalFormat:this.internalFormat,type:this.type,normalized:this.normalized,colorSpace:this.colorSpace,minFilter:this.minFilter,magFilter:this.magFilter,anisotropy:this.anisotropy,flipY:this.flipY,generateMipmaps:this.generateMipmaps,mipmapsAutoUpdate:this.mipmapsAutoUpdate,premultiplyAlpha:this.premultiplyAlpha,unpackAlignment:this.unpackAlignment};return Object.keys(this.userData).length>0&&(i.userData=this.userData),e||(t.textures[this.uuid]=i),i}dispose(){this.dispatchEvent({type:"dispose"})}transformUv(t){if(this.mapping!==hh)return t;if(t.applyMatrix3(this.matrix),t.x<0||t.x>1)switch(this.wrapS){case cn:t.x=t.x-Math.floor(t.x);break;case Gi:t.x=t.x<0?0:1;break;case va:Math.abs(Math.floor(t.x)%2)===1?t.x=Math.ceil(t.x)-t.x:t.x=t.x-Math.floor(t.x);break}if(t.y<0||t.y>1)switch(this.wrapT){case cn:t.y=t.y-Math.floor(t.y);break;case Gi:t.y=t.y<0?0:1;break;case va:Math.abs(Math.floor(t.y)%2)===1?t.y=Math.ceil(t.y)-t.y:t.y=t.y-Math.floor(t.y);break}return this.flipY&&(t.y=1-t.y),t}set needsUpdate(t){t===!0&&(this.version++,this.source.needsUpdate=!0)}set needsPMREMUpdate(t){t===!0&&this.pmremVersion++}};We.DEFAULT_IMAGE=null;We.DEFAULT_MAPPING=hh;We.DEFAULT_ANISOTROPY=1;var ne=class{static{this.prototype.isVector4=!0}constructor(t=0,e=0,i=0,s=1){this.x=t,this.y=e,this.z=i,this.w=s}get width(){return this.z}set width(t){this.z=t}get height(){return this.w}set height(t){this.w=t}set(t,e,i,s){return this.x=t,this.y=e,this.z=i,this.w=s,this}setScalar(t){return this.x=t,this.y=t,this.z=t,this.w=t,this}setX(t){return this.x=t,this}setY(t){return this.y=t,this}setZ(t){return this.z=t,this}setW(t){return this.w=t,this}setComponent(t,e){switch(t){case 0:this.x=e;break;case 1:this.y=e;break;case 2:this.z=e;break;case 3:this.w=e;break;default:throw new Error("THREE.Vector4: index is out of range: "+t)}return this}getComponent(t){switch(t){case 0:return this.x;case 1:return this.y;case 2:return this.z;case 3:return this.w;default:throw new Error("THREE.Vector4: index is out of range: "+t)}}clone(){return new this.constructor(this.x,this.y,this.z,this.w)}copy(t){return this.x=t.x,this.y=t.y,this.z=t.z,this.w=t.w!==void 0?t.w:1,this}add(t){return this.x+=t.x,this.y+=t.y,this.z+=t.z,this.w+=t.w,this}addScalar(t){return this.x+=t,this.y+=t,this.z+=t,this.w+=t,this}addVectors(t,e){return this.x=t.x+e.x,this.y=t.y+e.y,this.z=t.z+e.z,this.w=t.w+e.w,this}addScaledVector(t,e){return this.x+=t.x*e,this.y+=t.y*e,this.z+=t.z*e,this.w+=t.w*e,this}sub(t){return this.x-=t.x,this.y-=t.y,this.z-=t.z,this.w-=t.w,this}subScalar(t){return this.x-=t,this.y-=t,this.z-=t,this.w-=t,this}subVectors(t,e){return this.x=t.x-e.x,this.y=t.y-e.y,this.z=t.z-e.z,this.w=t.w-e.w,this}multiply(t){return this.x*=t.x,this.y*=t.y,this.z*=t.z,this.w*=t.w,this}multiplyScalar(t){return this.x*=t,this.y*=t,this.z*=t,this.w*=t,this}applyMatrix4(t){let e=this.x,i=this.y,s=this.z,r=this.w,o=t.elements;return this.x=o[0]*e+o[4]*i+o[8]*s+o[12]*r,this.y=o[1]*e+o[5]*i+o[9]*s+o[13]*r,this.z=o[2]*e+o[6]*i+o[10]*s+o[14]*r,this.w=o[3]*e+o[7]*i+o[11]*s+o[15]*r,this}divide(t){return this.x/=t.x,this.y/=t.y,this.z/=t.z,this.w/=t.w,this}divideScalar(t){return this.multiplyScalar(1/t)}setAxisAngleFromQuaternion(t){this.w=2*Math.acos(t.w);let e=Math.sqrt(1-t.w*t.w);return e<1e-4?(this.x=1,this.y=0,this.z=0):(this.x=t.x/e,this.y=t.y/e,this.z=t.z/e),this}setAxisAngleFromRotationMatrix(t){let e,i,s,r,l=t.elements,c=l[0],h=l[4],u=l[8],d=l[1],f=l[5],p=l[9],x=l[2],m=l[6],g=l[10];if(Math.abs(h-d)<.01&&Math.abs(u-x)<.01&&Math.abs(p-m)<.01){if(Math.abs(h+d)<.1&&Math.abs(u+x)<.1&&Math.abs(p+m)<.1&&Math.abs(c+f+g-3)<.1)return this.set(1,0,0,0),this;e=Math.PI;let y=(c+1)/2,S=(f+1)/2,T=(g+1)/2,w=(h+d)/4,v=(u+x)/4,M=(p+m)/4;return y>S&&y>T?y<.01?(i=0,s=.707106781,r=.707106781):(i=Math.sqrt(y),s=w/i,r=v/i):S>T?S<.01?(i=.707106781,s=0,r=.707106781):(s=Math.sqrt(S),i=w/s,r=M/s):T<.01?(i=.707106781,s=.707106781,r=0):(r=Math.sqrt(T),i=v/r,s=M/r),this.set(i,s,r,e),this}let _=Math.sqrt((m-p)*(m-p)+(u-x)*(u-x)+(d-h)*(d-h));return Math.abs(_)<.001&&(_=1),this.x=(m-p)/_,this.y=(u-x)/_,this.z=(d-h)/_,this.w=Math.acos((c+f+g-1)/2),this}setFromMatrixPosition(t){let e=t.elements;return this.x=e[12],this.y=e[13],this.z=e[14],this.w=e[15],this}min(t){return this.x=Math.min(this.x,t.x),this.y=Math.min(this.y,t.y),this.z=Math.min(this.z,t.z),this.w=Math.min(this.w,t.w),this}max(t){return this.x=Math.max(this.x,t.x),this.y=Math.max(this.y,t.y),this.z=Math.max(this.z,t.z),this.w=Math.max(this.w,t.w),this}clamp(t,e){return this.x=kt(this.x,t.x,e.x),this.y=kt(this.y,t.y,e.y),this.z=kt(this.z,t.z,e.z),this.w=kt(this.w,t.w,e.w),this}clampScalar(t,e){return this.x=kt(this.x,t,e),this.y=kt(this.y,t,e),this.z=kt(this.z,t,e),this.w=kt(this.w,t,e),this}clampLength(t,e){let i=this.length();return this.divideScalar(i||1).multiplyScalar(kt(i,t,e))}floor(){return this.x=Math.floor(this.x),this.y=Math.floor(this.y),this.z=Math.floor(this.z),this.w=Math.floor(this.w),this}ceil(){return this.x=Math.ceil(this.x),this.y=Math.ceil(this.y),this.z=Math.ceil(this.z),this.w=Math.ceil(this.w),this}round(){return this.x=Math.round(this.x),this.y=Math.round(this.y),this.z=Math.round(this.z),this.w=Math.round(this.w),this}roundToZero(){return this.x=Math.trunc(this.x),this.y=Math.trunc(this.y),this.z=Math.trunc(this.z),this.w=Math.trunc(this.w),this}negate(){return this.x=-this.x,this.y=-this.y,this.z=-this.z,this.w=-this.w,this}dot(t){return this.x*t.x+this.y*t.y+this.z*t.z+this.w*t.w}lengthSq(){return this.x*this.x+this.y*this.y+this.z*this.z+this.w*this.w}length(){return Math.sqrt(this.x*this.x+this.y*this.y+this.z*this.z+this.w*this.w)}manhattanLength(){return Math.abs(this.x)+Math.abs(this.y)+Math.abs(this.z)+Math.abs(this.w)}normalize(){return this.divideScalar(this.length()||1)}setLength(t){return this.normalize().multiplyScalar(t)}lerp(t,e){return this.x+=(t.x-this.x)*e,this.y+=(t.y-this.y)*e,this.z+=(t.z-this.z)*e,this.w+=(t.w-this.w)*e,this}lerpVectors(t,e,i){return this.x=t.x+(e.x-t.x)*i,this.y=t.y+(e.y-t.y)*i,this.z=t.z+(e.z-t.z)*i,this.w=t.w+(e.w-t.w)*i,this}equals(t){return t.x===this.x&&t.y===this.y&&t.z===this.z&&t.w===this.w}fromArray(t,e=0){return this.x=t[e],this.y=t[e+1],this.z=t[e+2],this.w=t[e+3],this}toArray(t=[],e=0){return t[e]=this.x,t[e+1]=this.y,t[e+2]=this.z,t[e+3]=this.w,t}fromBufferAttribute(t,e){return this.x=t.getX(e),this.y=t.getY(e),this.z=t.getZ(e),this.w=t.getW(e),this}random(){return this.x=Math.random(),this.y=Math.random(),this.z=Math.random(),this.w=Math.random(),this}*[Symbol.iterator](){yield this.x,yield this.y,yield this.z,yield this.w}},Sa=class extends Li{constructor(t=1,e=1,i={}){super(),i=Object.assign({generateMipmaps:!1,internalFormat:null,minFilter:oe,depthBuffer:!0,stencilBuffer:!1,resolveColorBuffer:!0,resolveDepthBuffer:!0,resolveStencilBuffer:!0,storeMultisampledColorBuffer:!0,storeMultisampledDepthBuffer:!0,storeMultisampledStencilBuffer:!0,depthTexture:null,samples:0,count:1,depth:1,multiview:!1,useArrayDepthTexture:!1},i),this.isRenderTarget=!0,this.width=t,this.height=e,this.depth=i.depth,this.scissor=new ne(0,0,t,e),this.scissorTest=!1,this.viewport=new ne(0,0,t,e),this.textures=[];let s=i.count;for(let r=0;r<s;r++)this.textures[r]=new We({width:t,height:e,depth:i.depth}),this.textures[r].isRenderTargetTexture=!0,this.textures[r].renderTarget=this;this._setTextureOptions(i),this.depthBuffer=i.depthBuffer,this.stencilBuffer=i.stencilBuffer,this.resolveColorBuffer=i.resolveColorBuffer,this.resolveDepthBuffer=i.resolveDepthBuffer,this.resolveStencilBuffer=i.resolveStencilBuffer,this.storeMultisampledColorBuffer=i.storeMultisampledColorBuffer,this.storeMultisampledDepthBuffer=i.storeMultisampledDepthBuffer,this.storeMultisampledStencilBuffer=i.storeMultisampledStencilBuffer,this._depthTexture=null,this.depthTexture=i.depthTexture,this.samples=i.samples,this.multiview=i.multiview,this.useArrayDepthTexture=i.useArrayDepthTexture}_setTextureOptions(t={}){let e={minFilter:oe,generateMipmaps:!1,flipY:!1,internalFormat:null};t.mapping!==void 0&&(e.mapping=t.mapping),t.wrapS!==void 0&&(e.wrapS=t.wrapS),t.wrapT!==void 0&&(e.wrapT=t.wrapT),t.wrapR!==void 0&&(e.wrapR=t.wrapR),t.magFilter!==void 0&&(e.magFilter=t.magFilter),t.minFilter!==void 0&&(e.minFilter=t.minFilter),t.format!==void 0&&(e.format=t.format),t.type!==void 0&&(e.type=t.type),t.anisotropy!==void 0&&(e.anisotropy=t.anisotropy),t.colorSpace!==void 0&&(e.colorSpace=t.colorSpace),t.flipY!==void 0&&(e.flipY=t.flipY),t.generateMipmaps!==void 0&&(e.generateMipmaps=t.generateMipmaps),t.mipmapsAutoUpdate!==void 0&&(e.mipmapsAutoUpdate=t.mipmapsAutoUpdate),t.internalFormat!==void 0&&(e.internalFormat=t.internalFormat);for(let i=0;i<this.textures.length;i++)this.textures[i].setValues(e)}get texture(){return this.textures[0]}set texture(t){this.textures[0]=t}set depthTexture(t){this._depthTexture!==null&&this._depthTexture.renderTarget===this&&(this._depthTexture.renderTarget=null),t!==null&&t.renderTarget===null&&(t.renderTarget=this),this._depthTexture=t}get depthTexture(){return this._depthTexture}setSize(t,e,i=1){if(this.width!==t||this.height!==e||this.depth!==i){this.width=t,this.height=e,this.depth=i;for(let s=0,r=this.textures.length;s<r;s++)this.textures[s].image.width=t,this.textures[s].image.height=e,this.textures[s].image.depth=i,this.textures[s].isData3DTexture!==!0&&(this.textures[s].isArrayTexture=this.textures[s].image.depth>1);this.dispose()}this.viewport.set(0,0,t,e),this.scissor.set(0,0,t,e)}clone(){return new this.constructor().copy(this)}copy(t){this.width=t.width,this.height=t.height,this.depth=t.depth,this.scissor.copy(t.scissor),this.scissorTest=t.scissorTest,this.viewport.copy(t.viewport),this.textures.length=0;for(let e=0,i=t.textures.length;e<i;e++){this.textures[e]=t.textures[e].clone(),this.textures[e].isRenderTargetTexture=!0,this.textures[e].renderTarget=this;let s=Object.assign({},t.textures[e].image);this.textures[e].source=new Os(s)}if(this.depthBuffer=t.depthBuffer,this.stencilBuffer=t.stencilBuffer,this.resolveColorBuffer=t.resolveColorBuffer,this.resolveDepthBuffer=t.resolveDepthBuffer,this.resolveStencilBuffer=t.resolveStencilBuffer,this.storeMultisampledColorBuffer=t.storeMultisampledColorBuffer,this.storeMultisampledDepthBuffer=t.storeMultisampledDepthBuffer,this.storeMultisampledStencilBuffer=t.storeMultisampledStencilBuffer,t.depthTexture!==null)if(t.depthTexture.renderTarget===t){let e=t.depthTexture.clone();e.renderTarget=null,this.depthTexture=e}else this.depthTexture=t.depthTexture;else this.depthTexture=null;return this.samples=t.samples,this.multiview=t.multiview,this.useArrayDepthTexture=t.useArrayDepthTexture,this}dispose(){this.dispatchEvent({type:"dispose"})}},he=class extends Sa{constructor(t=1,e=1,i={}){super(t,e,i),this.isWebGLRenderTarget=!0}},Ir=class extends We{constructor(t=null,e=1,i=1,s=1){super(null),this.isDataArrayTexture=!0,this.image={data:t,width:e,height:i,depth:s},this.magFilter=Ce,this.minFilter=Ce,this.wrapR=Gi,this.generateMipmaps=!1,this.flipY=!1,this.unpackAlignment=1,this.layerUpdates=new Set}copy(t){return super.copy(t),this.wrapR=t.wrapR,this}addLayerUpdate(t){this.layerUpdates.add(t)}clearLayerUpdates(){this.layerUpdates.clear()}};var Bs=class extends We{constructor(t=null,e=1,i=1,s=1){super(null),this.isData3DTexture=!0,this.image={data:t,width:e,height:i,depth:s},this.magFilter=Ce,this.minFilter=Ce,this.wrapR=Gi,this.generateMipmaps=!1,this.flipY=!1,this.unpackAlignment=1}copy(t){return super.copy(t),this.wrapR=t.wrapR,this}};var Gt=class n{static{this.prototype.isMatrix4=!0}constructor(t,e,i,s,r,o,a,l,c,h,u,d,f,p,x,m){this.elements=[1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1],t!==void 0&&this.set(t,e,i,s,r,o,a,l,c,h,u,d,f,p,x,m)}set(t,e,i,s,r,o,a,l,c,h,u,d,f,p,x,m){let g=this.elements;return g[0]=t,g[4]=e,g[8]=i,g[12]=s,g[1]=r,g[5]=o,g[9]=a,g[13]=l,g[2]=c,g[6]=h,g[10]=u,g[14]=d,g[3]=f,g[7]=p,g[11]=x,g[15]=m,this}identity(){return this.set(1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1),this}clone(){return new n().fromArray(this.elements)}copy(t){let e=this.elements,i=t.elements;return e[0]=i[0],e[1]=i[1],e[2]=i[2],e[3]=i[3],e[4]=i[4],e[5]=i[5],e[6]=i[6],e[7]=i[7],e[8]=i[8],e[9]=i[9],e[10]=i[10],e[11]=i[11],e[12]=i[12],e[13]=i[13],e[14]=i[14],e[15]=i[15],this}copyPosition(t){let e=this.elements,i=t.elements;return e[12]=i[12],e[13]=i[13],e[14]=i[14],this}setFromMatrix3(t){let e=t.elements;return this.set(e[0],e[3],e[6],0,e[1],e[4],e[7],0,e[2],e[5],e[8],0,0,0,0,1),this}extractBasis(t,e,i){return this.determinantAffine()===0?(t.set(1,0,0),e.set(0,1,0),i.set(0,0,1),this):(t.setFromMatrixColumn(this,0),e.setFromMatrixColumn(this,1),i.setFromMatrixColumn(this,2),this)}makeBasis(t,e,i){return this.set(t.x,e.x,i.x,0,t.y,e.y,i.y,0,t.z,e.z,i.z,0,0,0,0,1),this}extractRotation(t){if(t.determinantAffine()===0)return this.identity();let e=this.elements,i=t.elements,s=1/vs.setFromMatrixColumn(t,0).length(),r=1/vs.setFromMatrixColumn(t,1).length(),o=1/vs.setFromMatrixColumn(t,2).length();return e[0]=i[0]*s,e[1]=i[1]*s,e[2]=i[2]*s,e[3]=0,e[4]=i[4]*r,e[5]=i[5]*r,e[6]=i[6]*r,e[7]=0,e[8]=i[8]*o,e[9]=i[9]*o,e[10]=i[10]*o,e[11]=0,e[12]=0,e[13]=0,e[14]=0,e[15]=1,this}makeRotationFromEuler(t){let e=this.elements,i=t.x,s=t.y,r=t.z,o=Math.cos(i),a=Math.sin(i),l=Math.cos(s),c=Math.sin(s),h=Math.cos(r),u=Math.sin(r);if(t.order==="XYZ"){let d=o*h,f=o*u,p=a*h,x=a*u;e[0]=l*h,e[4]=-l*u,e[8]=c,e[1]=f+p*c,e[5]=d-x*c,e[9]=-a*l,e[2]=x-d*c,e[6]=p+f*c,e[10]=o*l}else if(t.order==="YXZ"){let d=l*h,f=l*u,p=c*h,x=c*u;e[0]=d+x*a,e[4]=p*a-f,e[8]=o*c,e[1]=o*u,e[5]=o*h,e[9]=-a,e[2]=f*a-p,e[6]=x+d*a,e[10]=o*l}else if(t.order==="ZXY"){let d=l*h,f=l*u,p=c*h,x=c*u;e[0]=d-x*a,e[4]=-o*u,e[8]=p+f*a,e[1]=f+p*a,e[5]=o*h,e[9]=x-d*a,e[2]=-o*c,e[6]=a,e[10]=o*l}else if(t.order==="ZYX"){let d=o*h,f=o*u,p=a*h,x=a*u;e[0]=l*h,e[4]=p*c-f,e[8]=d*c+x,e[1]=l*u,e[5]=x*c+d,e[9]=f*c-p,e[2]=-c,e[6]=a*l,e[10]=o*l}else if(t.order==="YZX"){let d=o*l,f=o*c,p=a*l,x=a*c;e[0]=l*h,e[4]=x-d*u,e[8]=p*u+f,e[1]=u,e[5]=o*h,e[9]=-a*h,e[2]=-c*h,e[6]=f*u+p,e[10]=d-x*u}else if(t.order==="XZY"){let d=o*l,f=o*c,p=a*l,x=a*c;e[0]=l*h,e[4]=-u,e[8]=c*h,e[1]=d*u+x,e[5]=o*h,e[9]=f*u-p,e[2]=p*u-f,e[6]=a*h,e[10]=x*u+d}return e[3]=0,e[7]=0,e[11]=0,e[12]=0,e[13]=0,e[14]=0,e[15]=1,this}makeRotationFromQuaternion(t){return this.compose(nm,t,sm)}lookAt(t,e,i){let s=this.elements;return pi.subVectors(t,e),pi.lengthSq()===0&&(pi.z=1),pi.normalize(),wn.crossVectors(i,pi),wn.lengthSq()===0&&(Math.abs(i.z)===1?pi.x+=1e-4:pi.z+=1e-4,pi.normalize(),wn.crossVectors(i,pi)),wn.normalize(),Fo.crossVectors(pi,wn),s[0]=wn.x,s[4]=Fo.x,s[8]=pi.x,s[1]=wn.y,s[5]=Fo.y,s[9]=pi.y,s[2]=wn.z,s[6]=Fo.z,s[10]=pi.z,this}multiply(t){return this.multiplyMatrices(this,t)}premultiply(t){return this.multiplyMatrices(t,this)}multiplyMatrices(t,e){let i=t.elements,s=e.elements,r=this.elements,o=i[0],a=i[4],l=i[8],c=i[12],h=i[1],u=i[5],d=i[9],f=i[13],p=i[2],x=i[6],m=i[10],g=i[14],_=i[3],y=i[7],S=i[11],T=i[15],w=s[0],v=s[4],M=s[8],D=s[12],P=s[1],A=s[5],C=s[9],U=s[13],L=s[2],B=s[6],G=s[10],z=s[14],tt=s[3],W=s[7],J=s[11],j=s[15];return r[0]=o*w+a*P+l*L+c*tt,r[4]=o*v+a*A+l*B+c*W,r[8]=o*M+a*C+l*G+c*J,r[12]=o*D+a*U+l*z+c*j,r[1]=h*w+u*P+d*L+f*tt,r[5]=h*v+u*A+d*B+f*W,r[9]=h*M+u*C+d*G+f*J,r[13]=h*D+u*U+d*z+f*j,r[2]=p*w+x*P+m*L+g*tt,r[6]=p*v+x*A+m*B+g*W,r[10]=p*M+x*C+m*G+g*J,r[14]=p*D+x*U+m*z+g*j,r[3]=_*w+y*P+S*L+T*tt,r[7]=_*v+y*A+S*B+T*W,r[11]=_*M+y*C+S*G+T*J,r[15]=_*D+y*U+S*z+T*j,this}multiplyScalar(t){let e=this.elements;return e[0]*=t,e[4]*=t,e[8]*=t,e[12]*=t,e[1]*=t,e[5]*=t,e[9]*=t,e[13]*=t,e[2]*=t,e[6]*=t,e[10]*=t,e[14]*=t,e[3]*=t,e[7]*=t,e[11]*=t,e[15]*=t,this}determinant(){let t=this.elements,e=t[0],i=t[4],s=t[8],r=t[12],o=t[1],a=t[5],l=t[9],c=t[13],h=t[2],u=t[6],d=t[10],f=t[14],p=t[3],x=t[7],m=t[11],g=t[15],_=l*f-c*d,y=a*f-c*u,S=a*d-l*u,T=o*f-c*h,w=o*d-l*h,v=o*u-a*h;return e*(x*_-m*y+g*S)-i*(p*_-m*T+g*w)+s*(p*y-x*T+g*v)-r*(p*S-x*w+m*v)}determinantAffine(){let t=this.elements,e=t[0],i=t[4],s=t[8],r=t[1],o=t[5],a=t[9],l=t[2],c=t[6],h=t[10];return e*(o*h-a*c)-i*(r*h-a*l)+s*(r*c-o*l)}transpose(){let t=this.elements,e;return e=t[1],t[1]=t[4],t[4]=e,e=t[2],t[2]=t[8],t[8]=e,e=t[6],t[6]=t[9],t[9]=e,e=t[3],t[3]=t[12],t[12]=e,e=t[7],t[7]=t[13],t[13]=e,e=t[11],t[11]=t[14],t[14]=e,this}setPosition(t,e,i){let s=this.elements;return t.isVector3?(s[12]=t.x,s[13]=t.y,s[14]=t.z):(s[12]=t,s[13]=e,s[14]=i),this}invert(){let t=this.elements,e=t[0],i=t[1],s=t[2],r=t[3],o=t[4],a=t[5],l=t[6],c=t[7],h=t[8],u=t[9],d=t[10],f=t[11],p=t[12],x=t[13],m=t[14],g=t[15],_=e*a-i*o,y=e*l-s*o,S=e*c-r*o,T=i*l-s*a,w=i*c-r*a,v=s*c-r*l,M=h*x-u*p,D=h*m-d*p,P=h*g-f*p,A=u*m-d*x,C=u*g-f*x,U=d*g-f*m,L=_*U-y*C+S*A+T*P-w*D+v*M;if(L===0)return this.set(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0);let B=1/L;return t[0]=(a*U-l*C+c*A)*B,t[1]=(s*C-i*U-r*A)*B,t[2]=(x*v-m*w+g*T)*B,t[3]=(d*w-u*v-f*T)*B,t[4]=(l*P-o*U-c*D)*B,t[5]=(e*U-s*P+r*D)*B,t[6]=(m*S-p*v-g*y)*B,t[7]=(h*v-d*S+f*y)*B,t[8]=(o*C-a*P+c*M)*B,t[9]=(i*P-e*C-r*M)*B,t[10]=(p*w-x*S+g*_)*B,t[11]=(u*S-h*w-f*_)*B,t[12]=(a*D-o*A-l*M)*B,t[13]=(e*A-i*D+s*M)*B,t[14]=(x*y-p*T-m*_)*B,t[15]=(h*T-u*y+d*_)*B,this}scale(t){let e=this.elements,i=t.x,s=t.y,r=t.z;return e[0]*=i,e[4]*=s,e[8]*=r,e[1]*=i,e[5]*=s,e[9]*=r,e[2]*=i,e[6]*=s,e[10]*=r,e[3]*=i,e[7]*=s,e[11]*=r,this}getMaxScaleOnAxis(){let t=this.elements,e=t[0]*t[0]+t[1]*t[1]+t[2]*t[2],i=t[4]*t[4]+t[5]*t[5]+t[6]*t[6],s=t[8]*t[8]+t[9]*t[9]+t[10]*t[10];return Math.sqrt(Math.max(e,i,s))}makeTranslation(t,e,i){return t.isVector3?this.set(1,0,0,t.x,0,1,0,t.y,0,0,1,t.z,0,0,0,1):this.set(1,0,0,t,0,1,0,e,0,0,1,i,0,0,0,1),this}makeRotationX(t){let e=Math.cos(t),i=Math.sin(t);return this.set(1,0,0,0,0,e,-i,0,0,i,e,0,0,0,0,1),this}makeRotationY(t){let e=Math.cos(t),i=Math.sin(t);return this.set(e,0,i,0,0,1,0,0,-i,0,e,0,0,0,0,1),this}makeRotationZ(t){let e=Math.cos(t),i=Math.sin(t);return this.set(e,-i,0,0,i,e,0,0,0,0,1,0,0,0,0,1),this}makeRotationAxis(t,e){let i=Math.cos(e),s=Math.sin(e),r=1-i,o=t.x,a=t.y,l=t.z,c=r*o,h=r*a;return this.set(c*o+i,c*a-s*l,c*l+s*a,0,c*a+s*l,h*a+i,h*l-s*o,0,c*l-s*a,h*l+s*o,r*l*l+i,0,0,0,0,1),this}makeScale(t,e,i){return this.set(t,0,0,0,0,e,0,0,0,0,i,0,0,0,0,1),this}makeShear(t,e,i,s,r,o){return this.set(1,i,r,0,t,1,o,0,e,s,1,0,0,0,0,1),this}compose(t,e,i){let s=this.elements,r=e._x,o=e._y,a=e._z,l=e._w,c=r+r,h=o+o,u=a+a,d=r*c,f=r*h,p=r*u,x=o*h,m=o*u,g=a*u,_=l*c,y=l*h,S=l*u,T=i.x,w=i.y,v=i.z;return s[0]=(1-(x+g))*T,s[1]=(f+S)*T,s[2]=(p-y)*T,s[3]=0,s[4]=(f-S)*w,s[5]=(1-(d+g))*w,s[6]=(m+_)*w,s[7]=0,s[8]=(p+y)*v,s[9]=(m-_)*v,s[10]=(1-(d+x))*v,s[11]=0,s[12]=t.x,s[13]=t.y,s[14]=t.z,s[15]=1,this}decompose(t,e,i){let s=this.elements;t.x=s[12],t.y=s[13],t.z=s[14];let r=this.determinantAffine();if(r===0)return i.set(1,1,1),e.identity(),this;let o=vs.set(s[0],s[1],s[2]).length(),a=vs.set(s[4],s[5],s[6]).length(),l=vs.set(s[8],s[9],s[10]).length();r<0&&(o=-o),Ci.copy(this);let c=1/o,h=1/a,u=1/l;return Ci.elements[0]*=c,Ci.elements[1]*=c,Ci.elements[2]*=c,Ci.elements[4]*=h,Ci.elements[5]*=h,Ci.elements[6]*=h,Ci.elements[8]*=u,Ci.elements[9]*=u,Ci.elements[10]*=u,e.setFromRotationMatrix(Ci),i.x=o,i.y=a,i.z=l,this}makePerspective(t,e,i,s,r,o,a=Ei,l=!1){let c=this.elements,h=2*r/(e-t),u=2*r/(i-s),d=(e+t)/(e-t),f=(i+s)/(i-s),p,x;if(l)p=r/(o-r),x=o*r/(o-r);else if(a===Ei)p=-(o+r)/(o-r),x=-2*o*r/(o-r);else if(a===Us)p=-o/(o-r),x=-o*r/(o-r);else throw new Error("THREE.Matrix4.makePerspective(): Invalid coordinate system: "+a);return c[0]=h,c[4]=0,c[8]=d,c[12]=0,c[1]=0,c[5]=u,c[9]=f,c[13]=0,c[2]=0,c[6]=0,c[10]=p,c[14]=x,c[3]=0,c[7]=0,c[11]=-1,c[15]=0,this}makeOrthographic(t,e,i,s,r,o,a=Ei,l=!1){let c=this.elements,h=2/(e-t),u=2/(i-s),d=-(e+t)/(e-t),f=-(i+s)/(i-s),p,x;if(l)p=1/(o-r),x=o/(o-r);else if(a===Ei)p=-2/(o-r),x=-(o+r)/(o-r);else if(a===Us)p=-1/(o-r),x=-r/(o-r);else throw new Error("THREE.Matrix4.makeOrthographic(): Invalid coordinate system: "+a);return c[0]=h,c[4]=0,c[8]=0,c[12]=d,c[1]=0,c[5]=u,c[9]=0,c[13]=f,c[2]=0,c[6]=0,c[10]=p,c[14]=x,c[3]=0,c[7]=0,c[11]=0,c[15]=1,this}equals(t){let e=this.elements,i=t.elements;for(let s=0;s<16;s++)if(e[s]!==i[s])return!1;return!0}fromArray(t,e=0){for(let i=0;i<16;i++)this.elements[i]=t[i+e];return this}toArray(t=[],e=0){let i=this.elements;return t[e]=i[0],t[e+1]=i[1],t[e+2]=i[2],t[e+3]=i[3],t[e+4]=i[4],t[e+5]=i[5],t[e+6]=i[6],t[e+7]=i[7],t[e+8]=i[8],t[e+9]=i[9],t[e+10]=i[10],t[e+11]=i[11],t[e+12]=i[12],t[e+13]=i[13],t[e+14]=i[14],t[e+15]=i[15],t}},vs=new R,Ci=new Gt,nm=new R(0,0,0),sm=new R(1,1,1),wn=new R,Fo=new R,pi=new R,Pu=new Gt,Iu=new Qe,Ti=class n{constructor(t=0,e=0,i=0,s=n.DEFAULT_ORDER){this.isEuler=!0,this._x=t,this._y=e,this._z=i,this._order=s}get x(){return this._x}set x(t){this._x=t,this._onChangeCallback()}get y(){return this._y}set y(t){this._y=t,this._onChangeCallback()}get z(){return this._z}set z(t){this._z=t,this._onChangeCallback()}get order(){return this._order}set order(t){this._order=t,this._onChangeCallback()}set(t,e,i,s=this._order){return this._x=t,this._y=e,this._z=i,this._order=s,this._onChangeCallback(),this}clone(){return new this.constructor(this._x,this._y,this._z,this._order)}copy(t){return this._x=t._x,this._y=t._y,this._z=t._z,this._order=t._order,this._onChangeCallback(),this}setFromRotationMatrix(t,e=this._order,i=!0){let s=t.elements,r=s[0],o=s[4],a=s[8],l=s[1],c=s[5],h=s[9],u=s[2],d=s[6],f=s[10];switch(e){case"XYZ":this._y=Math.asin(kt(a,-1,1)),Math.abs(a)<.9999999?(this._x=Math.atan2(-h,f),this._z=Math.atan2(-o,r)):(this._x=Math.atan2(d,c),this._z=0);break;case"YXZ":this._x=Math.asin(-kt(h,-1,1)),Math.abs(h)<.9999999?(this._y=Math.atan2(a,f),this._z=Math.atan2(l,c)):(this._y=Math.atan2(-u,r),this._z=0);break;case"ZXY":this._x=Math.asin(kt(d,-1,1)),Math.abs(d)<.9999999?(this._y=Math.atan2(-u,f),this._z=Math.atan2(-o,c)):(this._y=0,this._z=Math.atan2(l,r));break;case"ZYX":this._y=Math.asin(-kt(u,-1,1)),Math.abs(u)<.9999999?(this._x=Math.atan2(d,f),this._z=Math.atan2(l,r)):(this._x=0,this._z=Math.atan2(-o,c));break;case"YZX":this._z=Math.asin(kt(l,-1,1)),Math.abs(l)<.9999999?(this._x=Math.atan2(-h,c),this._y=Math.atan2(-u,r)):(this._x=0,this._y=Math.atan2(a,f));break;case"XZY":this._z=Math.asin(-kt(o,-1,1)),Math.abs(o)<.9999999?(this._x=Math.atan2(d,c),this._y=Math.atan2(a,r)):(this._x=Math.atan2(-h,f),this._y=0);break;default:It("Euler: .setFromRotationMatrix() encountered an unknown order: "+e)}return this._order=e,i===!0&&this._onChangeCallback(),this}setFromQuaternion(t,e,i){return Pu.makeRotationFromQuaternion(t),this.setFromRotationMatrix(Pu,e,i)}setFromVector3(t,e=this._order){return this.set(t.x,t.y,t.z,e)}reorder(t){return Iu.setFromEuler(this),this.setFromQuaternion(Iu,t)}equals(t){return t._x===this._x&&t._y===this._y&&t._z===this._z&&t._order===this._order}fromArray(t){return this._x=t[0],this._y=t[1],this._z=t[2],t[3]!==void 0&&(this._order=t[3]),this._onChangeCallback(),this}toArray(t=[],e=0){return t[e]=this._x,t[e+1]=this._y,t[e+2]=this._z,t[e+3]=this._order,t}_onChange(t){return this._onChangeCallback=t,this}_onChangeCallback(){}*[Symbol.iterator](){yield this._x,yield this._y,yield this._z,yield this._order}};Ti.DEFAULT_ORDER="XYZ";var zs=class{constructor(){this.mask=1}set(t){this.mask=(1<<t|0)>>>0}enable(t){this.mask|=1<<t|0}enableAll(){this.mask=-1}toggle(t){this.mask^=1<<t|0}disable(t){this.mask&=~(1<<t|0)}disableAll(){this.mask=0}test(t){return(this.mask&t.mask)!==0}isEnabled(t){return(this.mask&(1<<t|0))!==0}},rm=0,Du=new R,ys=new Qe,tn=new Gt,Oo=new R,_r=new R,om=new R,am=new Qe,Lu=new R(1,0,0),Nu=new R(0,1,0),Uu=new R(0,0,1),Fu={type:"added"},lm={type:"removed"},Ms={type:"childadded",child:null},Tc={type:"childremoved",child:null},ve=class n extends Li{constructor(){super(),this.isObject3D=!0,Object.defineProperty(this,"id",{value:rm++}),this.uuid=ir(),this.name="",this.type="Object3D",this.parent=null,this.children=[],this.up=n.DEFAULT_UP.clone();let t=new R,e=new Ti,i=new Qe,s=new R(1,1,1);function r(){i.setFromEuler(e,!1)}function o(){e.setFromQuaternion(i,void 0,!1)}e._onChange(r),i._onChange(o),Object.defineProperties(this,{position:{configurable:!0,enumerable:!0,value:t},rotation:{configurable:!0,enumerable:!0,value:e},quaternion:{configurable:!0,enumerable:!0,value:i},scale:{configurable:!0,enumerable:!0,value:s},modelViewMatrix:{value:new Gt},normalMatrix:{value:new Ft}}),this.matrix=new Gt,this.matrixWorld=new Gt,this.matrixAutoUpdate=n.DEFAULT_MATRIX_AUTO_UPDATE,this.matrixWorldAutoUpdate=n.DEFAULT_MATRIX_WORLD_AUTO_UPDATE,this.matrixWorldNeedsUpdate=!1,this.layers=new zs,this.visible=!0,this.castShadow=!1,this.receiveShadow=!1,this.frustumCulled=!0,this.renderOrder=0,this.animations=[],this.customDepthMaterial=void 0,this.customDistanceMaterial=void 0,this.static=!1,this.userData={},this.pivot=null}onBeforeShadow(){}onAfterShadow(){}onBeforeRender(){}onAfterRender(){}applyMatrix4(t){this.matrixAutoUpdate&&this.updateMatrix(),this.matrix.premultiply(t),this.matrix.decompose(this.position,this.quaternion,this.scale),this.pivot!==null&&this.position.copy(this.pivot).applyMatrix4(this.matrix).sub(this.pivot)}applyQuaternion(t){return this.quaternion.premultiply(t),this}setRotationFromAxisAngle(t,e){this.quaternion.setFromAxisAngle(t,e)}setRotationFromEuler(t){this.quaternion.setFromEuler(t,!0)}setRotationFromMatrix(t){this.quaternion.setFromRotationMatrix(t)}setRotationFromQuaternion(t){this.quaternion.copy(t)}rotateOnAxis(t,e){return ys.setFromAxisAngle(t,e),this.quaternion.multiply(ys),this}rotateOnWorldAxis(t,e){return ys.setFromAxisAngle(t,e),this.quaternion.premultiply(ys),this}rotateX(t){return this.rotateOnAxis(Lu,t)}rotateY(t){return this.rotateOnAxis(Nu,t)}rotateZ(t){return this.rotateOnAxis(Uu,t)}translateOnAxis(t,e){return Du.copy(t).applyQuaternion(this.quaternion),this.position.add(Du.multiplyScalar(e)),this}translateX(t){return this.translateOnAxis(Lu,t)}translateY(t){return this.translateOnAxis(Nu,t)}translateZ(t){return this.translateOnAxis(Uu,t)}localToWorld(t){return this.updateWorldMatrix(!0,!1),t.applyMatrix4(this.matrixWorld)}worldToLocal(t){return this.updateWorldMatrix(!0,!1),t.applyMatrix4(tn.copy(this.matrixWorld).invert())}lookAt(t,e,i){t.isVector3?Oo.copy(t):Oo.set(t,e,i);let s=this.parent;this.updateWorldMatrix(!0,!1),_r.setFromMatrixPosition(this.matrixWorld),this.isCamera||this.isLight?tn.lookAt(_r,Oo,this.up):tn.lookAt(Oo,_r,this.up),this.quaternion.setFromRotationMatrix(tn),s&&(tn.extractRotation(s.matrixWorld),ys.setFromRotationMatrix(tn),this.quaternion.premultiply(ys.invert()))}add(t){if(arguments.length>1){for(let e=0;e<arguments.length;e++)this.add(arguments[e]);return this}return t===this?(Ut("Object3D.add: object can't be added as a child of itself.",t),this):(t&&t.isObject3D?(t.removeFromParent(),t.parent=this,this.children.push(t),t.dispatchEvent(Fu),Ms.child=t,this.dispatchEvent(Ms),Ms.child=null):Ut("Object3D.add: object not an instance of THREE.Object3D.",t),this)}remove(t){if(arguments.length>1){for(let i=0;i<arguments.length;i++)this.remove(arguments[i]);return this}let e=this.children.indexOf(t);return e!==-1&&(t.parent=null,this.children.splice(e,1),t.dispatchEvent(lm),Tc.child=t,this.dispatchEvent(Tc),Tc.child=null),this}removeFromParent(){let t=this.parent;return t!==null&&t.remove(this),this}clear(){return this.remove(...this.children)}attach(t){return this.updateWorldMatrix(!0,!1),tn.copy(this.matrixWorld).invert(),t.parent!==null&&(t.parent.updateWorldMatrix(!0,!1),tn.multiply(t.parent.matrixWorld)),t.applyMatrix4(tn),t.removeFromParent(),t.parent=this,this.children.push(t),t.updateWorldMatrix(!1,!0),t.dispatchEvent(Fu),Ms.child=t,this.dispatchEvent(Ms),Ms.child=null,this}getObjectById(t){return this.getObjectByProperty("id",t)}getObjectByName(t){return this.getObjectByProperty("name",t)}getObjectByProperty(t,e){if(this[t]===e)return this;for(let i=0,s=this.children.length;i<s;i++){let o=this.children[i].getObjectByProperty(t,e);if(o!==void 0)return o}}getObjectsByProperty(t,e,i=[]){this[t]===e&&i.push(this);let s=this.children;for(let r=0,o=s.length;r<o;r++)s[r].getObjectsByProperty(t,e,i);return i}getWorldPosition(t){return this.updateWorldMatrix(!0,!1),t.setFromMatrixPosition(this.matrixWorld)}getWorldQuaternion(t){return this.updateWorldMatrix(!0,!1),this.matrixWorld.decompose(_r,t,om),t}getWorldScale(t){return this.updateWorldMatrix(!0,!1),this.matrixWorld.decompose(_r,am,t),t}getWorldDirection(t){this.updateWorldMatrix(!0,!1);let e=this.matrixWorld.elements;return t.set(e[8],e[9],e[10]).normalize()}raycast(){}intersectsFrustum(){return!0}traverse(t){t(this);let e=this.children;for(let i=0,s=e.length;i<s;i++)e[i].traverse(t)}traverseVisible(t){if(this.visible===!1)return;t(this);let e=this.children;for(let i=0,s=e.length;i<s;i++)e[i].traverseVisible(t)}traverseAncestors(t){let e=this.parent;e!==null&&(t(e),e.traverseAncestors(t))}updateMatrix(){this.matrix.compose(this.position,this.quaternion,this.scale);let t=this.pivot;if(t!==null){let e=t.x,i=t.y,s=t.z,r=this.matrix.elements;r[12]+=e-r[0]*e-r[4]*i-r[8]*s,r[13]+=i-r[1]*e-r[5]*i-r[9]*s,r[14]+=s-r[2]*e-r[6]*i-r[10]*s}this.matrixWorldNeedsUpdate=!0}updateMatrixWorld(t){this.matrixAutoUpdate&&this.updateMatrix(),(this.matrixWorldNeedsUpdate||t)&&(this.matrixWorldAutoUpdate===!0&&(this.parent===null?this.matrixWorld.copy(this.matrix):this.matrixWorld.multiplyMatrices(this.parent.matrixWorld,this.matrix)),this.matrixWorldNeedsUpdate=!1,t=!0);let e=this.children;for(let i=0,s=e.length;i<s;i++)e[i].updateMatrixWorld(t)}updateWorldMatrix(t,e,i=!1){let s=this.parent;if(t===!0&&s!==null&&s.updateWorldMatrix(!0,!1),this.matrixAutoUpdate&&this.updateMatrix(),(this.matrixWorldNeedsUpdate||i)&&(this.matrixWorldAutoUpdate===!0&&(this.parent===null?this.matrixWorld.copy(this.matrix):this.matrixWorld.multiplyMatrices(this.parent.matrixWorld,this.matrix)),this.matrixWorldNeedsUpdate=!1,i=!0),e===!0){let r=this.children;for(let o=0,a=r.length;o<a;o++)r[o].updateWorldMatrix(!1,!0,i)}}toJSON(t){let e=t===void 0||typeof t=="string",i={};e&&(t={geometries:{},materials:{},textures:{},images:{},shapes:{},skeletons:{},animations:{},nodes:{}},i.metadata={version:4.7,type:"Object",generator:"Object3D.toJSON"});let s={};s.uuid=this.uuid,s.type=this.type,s.name=this.name,s.castShadow=this.castShadow,s.receiveShadow=this.receiveShadow,s.visible=this.visible,s.frustumCulled=this.frustumCulled,s.renderOrder=this.renderOrder,s.static=this.static,s.matrixAutoUpdate=this.matrixAutoUpdate,Object.keys(this.userData).length>0&&(s.userData=this.userData),s.layers=this.layers.mask,s.matrix=this.matrix.toArray(),s.up=this.up.toArray(),this.pivot!==null&&(s.pivot=this.pivot.toArray()),this.morphTargetDictionary!==void 0&&(s.morphTargetDictionary=Object.assign({},this.morphTargetDictionary)),this.morphTargetInfluences!==void 0&&(s.morphTargetInfluences=this.morphTargetInfluences.slice()),this.isInstancedMesh&&(s.type="InstancedMesh",s.count=this.count,s.instanceMatrix=this.instanceMatrix.toJSON(),this.instanceColor!==null&&(s.instanceColor=this.instanceColor.toJSON())),this.isBatchedMesh&&(s.type="BatchedMesh",s.perObjectFrustumCulled=this.perObjectFrustumCulled,s.sortObjects=this.sortObjects,s.drawRanges=this._drawRanges,s.reservedRanges=this._reservedRanges,s.geometryInfo=this._geometryInfo.map(a=>({...a,boundingBox:a.boundingBox?a.boundingBox.toJSON():void 0,boundingSphere:a.boundingSphere?a.boundingSphere.toJSON():void 0})),s.instanceInfo=this._instanceInfo.map(a=>({...a})),s.availableInstanceIds=this._availableInstanceIds.slice(),s.availableGeometryIds=this._availableGeometryIds.slice(),s.nextIndexStart=this._nextIndexStart,s.nextVertexStart=this._nextVertexStart,s.geometryCount=this._geometryCount,s.maxInstanceCount=this._maxInstanceCount,s.maxVertexCount=this._maxVertexCount,s.maxIndexCount=this._maxIndexCount,s.geometryInitialized=this._geometryInitialized,s.matricesTexture=this._matricesTexture.toJSON(t),s.indirectTexture=this._indirectTexture.toJSON(t),this._colorsTexture!==null&&(s.colorsTexture=this._colorsTexture.toJSON(t)),this.boundingSphere!==null&&(s.boundingSphere=this.boundingSphere.toJSON()),this.boundingBox!==null&&(s.boundingBox=this.boundingBox.toJSON()));function r(a,l){return a[l.uuid]===void 0&&(a[l.uuid]=l.toJSON(t)),l.uuid}if(this.isScene)this.background&&(this.background.isColor?s.background=this.background.toJSON():this.background.isTexture&&(s.background=this.background.toJSON(t).uuid)),this.environment&&this.environment.isTexture&&this.environment.isRenderTargetTexture!==!0&&(s.environment=this.environment.toJSON(t).uuid);else if(this.isMesh||this.isLine||this.isPoints){s.geometry=r(t.geometries,this.geometry);let a=this.geometry.parameters;if(a!==void 0&&a.shapes!==void 0){let l=a.shapes;if(Array.isArray(l))for(let c=0,h=l.length;c<h;c++){let u=l[c];r(t.shapes,u)}else r(t.shapes,l)}}if(this.isSkinnedMesh&&(s.bindMode=this.bindMode,s.bindMatrix=this.bindMatrix.toArray(),this.skeleton!==void 0&&(r(t.skeletons,this.skeleton),s.skeleton=this.skeleton.uuid)),this.material!==void 0)if(Array.isArray(this.material)){let a=[];for(let l=0,c=this.material.length;l<c;l++)a.push(r(t.materials,this.material[l]));s.material=a}else s.material=r(t.materials,this.material);if(this.children.length>0){s.children=[];for(let a=0;a<this.children.length;a++)s.children.push(this.children[a].toJSON(t).object)}if(this.animations.length>0){s.animations=[];for(let a=0;a<this.animations.length;a++){let l=this.animations[a];s.animations.push(r(t.animations,l))}}if(e){let a=o(t.geometries),l=o(t.materials),c=o(t.textures),h=o(t.images),u=o(t.shapes),d=o(t.skeletons),f=o(t.animations),p=o(t.nodes);a.length>0&&(i.geometries=a),l.length>0&&(i.materials=l),c.length>0&&(i.textures=c),h.length>0&&(i.images=h),u.length>0&&(i.shapes=u),d.length>0&&(i.skeletons=d),f.length>0&&(i.animations=f),p.length>0&&(i.nodes=p)}return i.object=s,i;function o(a){let l=[];for(let c in a){let h=a[c];delete h.metadata,l.push(h)}return l}}clone(t){return new this.constructor().copy(this,t)}copy(t,e=!0){if(this.name=t.name,this.up.copy(t.up),this.position.copy(t.position),this.rotation.order=t.rotation.order,this.quaternion.copy(t.quaternion),this.scale.copy(t.scale),this.pivot=t.pivot!==null?t.pivot.clone():null,this.matrix.copy(t.matrix),this.matrixWorld.copy(t.matrixWorld),this.matrixAutoUpdate=t.matrixAutoUpdate,this.matrixWorldAutoUpdate=t.matrixWorldAutoUpdate,this.matrixWorldNeedsUpdate=t.matrixWorldNeedsUpdate,this.layers.mask=t.layers.mask,this.visible=t.visible,this.castShadow=t.castShadow,this.receiveShadow=t.receiveShadow,this.frustumCulled=t.frustumCulled,this.renderOrder=t.renderOrder,this.static=t.static,this.animations=t.animations.slice(),this.userData=JSON.parse(JSON.stringify(t.userData)),e===!0)for(let i=0;i<t.children.length;i++){let s=t.children[i];this.add(s.clone())}return this}dispose(){this.dispatchEvent({type:"dispose"})}};ve.DEFAULT_UP=new R(0,1,0);ve.DEFAULT_MATRIX_AUTO_UPDATE=!0;ve.DEFAULT_MATRIX_WORLD_AUTO_UPDATE=!0;var ge=class extends ve{constructor(){super(),this.isGroup=!0,this.type="Group"}},cm={type:"move"},ks=class{constructor(){this._targetRay=null,this._grip=null,this._hand=null}getHandSpace(){return this._hand===null&&(this._hand=new ge,this._hand.matrixAutoUpdate=!1,this._hand.visible=!1,this._hand.joints={},this._hand.inputState={pinching:!1}),this._hand}getTargetRaySpace(){return this._targetRay===null&&(this._targetRay=new ge,this._targetRay.matrixAutoUpdate=!1,this._targetRay.visible=!1,this._targetRay.hasLinearVelocity=!1,this._targetRay.linearVelocity=new R,this._targetRay.hasAngularVelocity=!1,this._targetRay.angularVelocity=new R),this._targetRay}getGripSpace(){return this._grip===null&&(this._grip=new ge,this._grip.matrixAutoUpdate=!1,this._grip.visible=!1,this._grip.hasLinearVelocity=!1,this._grip.linearVelocity=new R,this._grip.hasAngularVelocity=!1,this._grip.angularVelocity=new R,this._grip.eventsEnabled=!1),this._grip}dispatchEvent(t){return this._targetRay!==null&&this._targetRay.dispatchEvent(t),this._grip!==null&&this._grip.dispatchEvent(t),this._hand!==null&&this._hand.dispatchEvent(t),this}connect(t){if(t&&t.hand){let e=this._hand;if(e)for(let i of t.hand.values())this._getHandJoint(e,i)}return this.dispatchEvent({type:"connected",data:t}),this}disconnect(t){return this.dispatchEvent({type:"disconnected",data:t}),this._targetRay!==null&&(this._targetRay.visible=!1),this._grip!==null&&(this._grip.visible=!1),this._hand!==null&&(this._hand.visible=!1),this}update(t,e,i){let s=null,r=null,o=null,a=this._targetRay,l=this._grip,c=this._hand;if(t&&e.session.visibilityState!=="visible-blurred"){if(c&&t.hand){o=!0;for(let x of t.hand.values()){let m=e.getJointPose(x,i),g=this._getHandJoint(c,x);m!==null&&(g.matrix.fromArray(m.transform.matrix),g.matrix.decompose(g.position,g.quaternion,g.scale),g.matrixWorldNeedsUpdate=!0,g.jointRadius=m.radius),g.visible=m!==null}let h=c.joints["index-finger-tip"],u=c.joints["thumb-tip"],d=h.position.distanceTo(u.position),f=.02,p=.005;c.inputState.pinching&&d>f+p?(c.inputState.pinching=!1,this.dispatchEvent({type:"pinchend",handedness:t.handedness,target:this})):!c.inputState.pinching&&d<=f-p&&(c.inputState.pinching=!0,this.dispatchEvent({type:"pinchstart",handedness:t.handedness,target:this}))}else l!==null&&t.gripSpace&&(r=e.getPose(t.gripSpace,i),r!==null&&(l.matrix.fromArray(r.transform.matrix),l.matrix.decompose(l.position,l.quaternion,l.scale),l.matrixWorldNeedsUpdate=!0,r.linearVelocity?(l.hasLinearVelocity=!0,l.linearVelocity.copy(r.linearVelocity)):l.hasLinearVelocity=!1,r.angularVelocity?(l.hasAngularVelocity=!0,l.angularVelocity.copy(r.angularVelocity)):l.hasAngularVelocity=!1,l.eventsEnabled&&l.dispatchEvent({type:"gripUpdated",data:t,target:this})));a!==null&&(s=e.getPose(t.targetRaySpace,i),s===null&&r!==null&&(s=r),s!==null&&(a.matrix.fromArray(s.transform.matrix),a.matrix.decompose(a.position,a.quaternion,a.scale),a.matrixWorldNeedsUpdate=!0,s.linearVelocity?(a.hasLinearVelocity=!0,a.linearVelocity.copy(s.linearVelocity)):a.hasLinearVelocity=!1,s.angularVelocity?(a.hasAngularVelocity=!0,a.angularVelocity.copy(s.angularVelocity)):a.hasAngularVelocity=!1,this.dispatchEvent(cm)))}return a!==null&&(a.visible=s!==null),l!==null&&(l.visible=r!==null),c!==null&&(c.visible=o!==null),this}_getHandJoint(t,e){if(t.joints[e.jointName]===void 0){let i=new ge;i.matrixAutoUpdate=!1,i.visible=!1,t.joints[e.jointName]=i,t.add(i)}return t.joints[e.jointName]}},Wd={aliceblue:15792383,antiquewhite:16444375,aqua:65535,aquamarine:8388564,azure:15794175,beige:16119260,bisque:16770244,black:0,blanchedalmond:16772045,blue:255,blueviolet:9055202,brown:10824234,burlywood:14596231,cadetblue:6266528,chartreuse:8388352,chocolate:13789470,coral:16744272,cornflowerblue:6591981,cornsilk:16775388,crimson:14423100,cyan:65535,darkblue:139,darkcyan:35723,darkgoldenrod:12092939,darkgray:11119017,darkgreen:25600,darkgrey:11119017,darkkhaki:12433259,darkmagenta:9109643,darkolivegreen:5597999,darkorange:16747520,darkorchid:10040012,darkred:9109504,darksalmon:15308410,darkseagreen:9419919,darkslateblue:4734347,darkslategray:3100495,darkslategrey:3100495,darkturquoise:52945,darkviolet:9699539,deeppink:16716947,deepskyblue:49151,dimgray:6908265,dimgrey:6908265,dodgerblue:2003199,firebrick:11674146,floralwhite:16775920,forestgreen:2263842,fuchsia:16711935,gainsboro:14474460,ghostwhite:16316671,gold:16766720,goldenrod:14329120,gray:8421504,green:32768,greenyellow:11403055,grey:8421504,honeydew:15794160,hotpink:16738740,indianred:13458524,indigo:4915330,ivory:16777200,khaki:15787660,lavender:15132410,lavenderblush:16773365,lawngreen:8190976,lemonchiffon:16775885,lightblue:11393254,lightcoral:15761536,lightcyan:14745599,lightgoldenrodyellow:16448210,lightgray:13882323,lightgreen:9498256,lightgrey:13882323,lightpink:16758465,lightsalmon:16752762,lightseagreen:2142890,lightskyblue:8900346,lightslategray:7833753,lightslategrey:7833753,lightsteelblue:11584734,lightyellow:16777184,lime:65280,limegreen:3329330,linen:16445670,magenta:16711935,maroon:8388608,mediumaquamarine:6737322,mediumblue:205,mediumorchid:12211667,mediumpurple:9662683,mediumseagreen:3978097,mediumslateblue:8087790,mediumspringgreen:64154,mediumturquoise:4772300,mediumvioletred:13047173,midnightblue:1644912,mintcream:16121850,mistyrose:16770273,moccasin:16770229,navajowhite:16768685,navy:128,oldlace:16643558,olive:8421376,olivedrab:7048739,orange:16753920,orangered:16729344,orchid:14315734,palegoldenrod:15657130,palegreen:10025880,paleturquoise:11529966,palevioletred:14381203,papayawhip:16773077,peachpuff:16767673,peru:13468991,pink:16761035,plum:14524637,powderblue:11591910,purple:8388736,rebeccapurple:6697881,red:16711680,rosybrown:12357519,royalblue:4286945,saddlebrown:9127187,salmon:16416882,sandybrown:16032864,seagreen:3050327,seashell:16774638,sienna:10506797,silver:12632256,skyblue:8900331,slateblue:6970061,slategray:7372944,slategrey:7372944,snow:16775930,springgreen:65407,steelblue:4620980,tan:13808780,teal:32896,thistle:14204888,tomato:16737095,turquoise:4251856,violet:15631086,wheat:16113331,white:16777215,whitesmoke:16119285,yellow:16776960,yellowgreen:10145074},En={h:0,s:0,l:0},Bo={h:0,s:0,l:0};function Ac(n,t,e){return e<0&&(e+=1),e>1&&(e-=1),e<1/6?n+(t-n)*6*e:e<1/2?t:e<2/3?n+(t-n)*6*(2/3-e):n}var St=class{constructor(t,e,i){return this.isColor=!0,this.r=1,this.g=1,this.b=1,this.set(t,e,i)}set(t,e,i){if(e===void 0&&i===void 0){let s=t;s&&s.isColor?this.copy(s):typeof s=="number"?this.setHex(s):typeof s=="string"&&this.setStyle(s)}else this.setRGB(t,e,i);return this}setScalar(t){return this.r=t,this.g=t,this.b=t,this}setHex(t,e=Re){return t=Math.floor(t),this.r=(t>>16&255)/255,this.g=(t>>8&255)/255,this.b=(t&255)/255,qt.colorSpaceToWorking(this,e),this}setRGB(t,e,i,s=qt.workingColorSpace){return this.r=t,this.g=e,this.b=i,qt.colorSpaceToWorking(this,s),this}setHSL(t,e,i,s=qt.workingColorSpace){if(t=yh(t,1),e=kt(e,0,1),i=kt(i,0,1),e===0)this.r=this.g=this.b=i;else{let r=i<=.5?i*(1+e):i+e-i*e,o=2*i-r;this.r=Ac(o,r,t+1/3),this.g=Ac(o,r,t),this.b=Ac(o,r,t-1/3)}return qt.colorSpaceToWorking(this,s),this}setStyle(t,e=Re){function i(r){r!==void 0&&parseFloat(r)<1&&It("Color: Alpha component of "+t+" will be ignored.")}let s;if(s=/^(\w+)\(([^\)]*)\)/.exec(t)){let r,o=s[1],a=s[2];switch(o){case"rgb":case"rgba":if(r=/^\s*(\d+)\s*,\s*(\d+)\s*,\s*(\d+)\s*(?:,\s*(\d+(?:\.\d+)?|\.\d+)\s*)?$/.exec(a))return i(r[4]),this.setRGB(Math.min(255,parseInt(r[1],10))/255,Math.min(255,parseInt(r[2],10))/255,Math.min(255,parseInt(r[3],10))/255,e);if(r=/^\s*(\d+(?:\.\d+)?|\.\d+)\%\s*,\s*(\d+(?:\.\d+)?|\.\d+)\%\s*,\s*(\d+(?:\.\d+)?|\.\d+)\%\s*(?:,\s*(\d+(?:\.\d+)?|\.\d+)\s*)?$/.exec(a))return i(r[4]),this.setRGB(Math.min(100,parseFloat(r[1]))/100,Math.min(100,parseFloat(r[2]))/100,Math.min(100,parseFloat(r[3]))/100,e);break;case"hsl":case"hsla":if(r=/^\s*([+-]?(?:\d+(?:\.\d+)?|\.\d+))\s*,\s*(\d+(?:\.\d+)?|\.\d+)\%\s*,\s*(\d+(?:\.\d+)?|\.\d+)\%\s*(?:,\s*(\d+(?:\.\d+)?|\.\d+)\s*)?$/.exec(a))return i(r[4]),this.setHSL(parseFloat(r[1])/360,parseFloat(r[2])/100,parseFloat(r[3])/100,e);break;default:It("Color: Unknown color model "+t)}}else if(s=/^\#([A-Fa-f\d]+)$/.exec(t)){let r=s[1],o=r.length;if(o===3)return this.setRGB(parseInt(r.charAt(0),16)/15,parseInt(r.charAt(1),16)/15,parseInt(r.charAt(2),16)/15,e);if(o===6)return this.setHex(parseInt(r,16),e);It("Color: Invalid hex color "+t)}else if(t&&t.length>0)return this.setColorName(t,e);return this}setColorName(t,e=Re){let i=Wd[t.toLowerCase()];return i!==void 0?this.setHex(i,e):It("Color: Unknown color "+t),this}clone(){return new this.constructor(this.r,this.g,this.b)}copy(t){return this.r=t.r,this.g=t.g,this.b=t.b,this}copySRGBToLinear(t){return this.r=ln(t.r),this.g=ln(t.g),this.b=ln(t.b),this}copyLinearToSRGB(t){return this.r=Ls(t.r),this.g=Ls(t.g),this.b=Ls(t.b),this}convertSRGBToLinear(){return this.copySRGBToLinear(this),this}convertLinearToSRGB(){return this.copyLinearToSRGB(this),this}getHex(t=Re){return qt.workingToColorSpace(je.copy(this),t),Math.round(kt(je.r*255,0,255))*65536+Math.round(kt(je.g*255,0,255))*256+Math.round(kt(je.b*255,0,255))}getHexString(t=Re){return("000000"+this.getHex(t).toString(16)).slice(-6)}getHSL(t,e=qt.workingColorSpace){qt.workingToColorSpace(je.copy(this),e);let i=je.r,s=je.g,r=je.b,o=Math.max(i,s,r),a=Math.min(i,s,r),l,c,h=(a+o)/2;if(a===o)l=0,c=0;else{let u=o-a;switch(c=h<=.5?u/(o+a):u/(2-o-a),o){case i:l=(s-r)/u+(s<r?6:0);break;case s:l=(r-i)/u+2;break;case r:l=(i-s)/u+4;break}l/=6}return t.h=l,t.s=c,t.l=h,t}getRGB(t,e=qt.workingColorSpace){return qt.workingToColorSpace(je.copy(this),e),t.r=je.r,t.g=je.g,t.b=je.b,t}getStyle(t=Re){qt.workingToColorSpace(je.copy(this),t);let e=je.r,i=je.g,s=je.b;return t!==Re?`color(${t} ${e.toFixed(3)} ${i.toFixed(3)} ${s.toFixed(3)})`:`rgb(${Math.round(e*255)},${Math.round(i*255)},${Math.round(s*255)})`}offsetHSL(t,e,i){return this.getHSL(En),this.setHSL(En.h+t,En.s+e,En.l+i)}add(t){return this.r+=t.r,this.g+=t.g,this.b+=t.b,this}addColors(t,e){return this.r=t.r+e.r,this.g=t.g+e.g,this.b=t.b+e.b,this}addScalar(t){return this.r+=t,this.g+=t,this.b+=t,this}sub(t){return this.r=Math.max(0,this.r-t.r),this.g=Math.max(0,this.g-t.g),this.b=Math.max(0,this.b-t.b),this}multiply(t){return this.r*=t.r,this.g*=t.g,this.b*=t.b,this}multiplyScalar(t){return this.r*=t,this.g*=t,this.b*=t,this}lerp(t,e){return this.r+=(t.r-this.r)*e,this.g+=(t.g-this.g)*e,this.b+=(t.b-this.b)*e,this}lerpColors(t,e,i){return this.r=t.r+(e.r-t.r)*i,this.g=t.g+(e.g-t.g)*i,this.b=t.b+(e.b-t.b)*i,this}lerpHSL(t,e){this.getHSL(En),t.getHSL(Bo);let i=Er(En.h,Bo.h,e),s=Er(En.s,Bo.s,e),r=Er(En.l,Bo.l,e);return this.setHSL(i,s,r),this}setFromVector3(t){return this.r=t.x,this.g=t.y,this.b=t.z,this}applyMatrix3(t){let e=this.r,i=this.g,s=this.b,r=t.elements;return this.r=r[0]*e+r[3]*i+r[6]*s,this.g=r[1]*e+r[4]*i+r[7]*s,this.b=r[2]*e+r[5]*i+r[8]*s,this}equals(t){return t.r===this.r&&t.g===this.g&&t.b===this.b}fromArray(t,e=0){return this.r=t[e],this.g=t[e+1],this.b=t[e+2],this}toArray(t=[],e=0){return t[e]=this.r,t[e+1]=this.g,t[e+2]=this.b,t}fromBufferAttribute(t,e){return this.r=t.getX(e),this.g=t.getY(e),this.b=t.getZ(e),this}toJSON(){return this.getHex()}*[Symbol.iterator](){yield this.r,yield this.g,yield this.b}},je=new St;St.NAMES=Wd;var Xe=class extends ve{constructor(){super(),this.isScene=!0,this.type="Scene",this.background=null,this.environment=null,this.fog=null,this.backgroundBlurriness=0,this.backgroundIntensity=1,this.backgroundRotation=new Ti,this.environmentIntensity=1,this.environmentRotation=new Ti,this.overrideMaterial=null,typeof __THREE_DEVTOOLS__<"u"&&__THREE_DEVTOOLS__.dispatchEvent(new CustomEvent("observe",{detail:this}))}copy(t,e){return super.copy(t,e),this.background=t.background!==null?t.background.clone():null,this.environment=t.environment!==null?t.environment.clone():null,this.fog=t.fog!==null?t.fog.clone():null,this.backgroundBlurriness=t.backgroundBlurriness,this.backgroundIntensity=t.backgroundIntensity,this.backgroundRotation.copy(t.backgroundRotation),this.environmentIntensity=t.environmentIntensity,this.environmentRotation.copy(t.environmentRotation),this.overrideMaterial=t.overrideMaterial!==null?t.overrideMaterial.clone():null,this.matrixAutoUpdate=t.matrixAutoUpdate,this}toJSON(t){let e=super.toJSON(t);return this.fog!==null&&(e.object.fog=this.fog.toJSON()),e.object.backgroundBlurriness=this.backgroundBlurriness,e.object.backgroundIntensity=this.backgroundIntensity,e.object.backgroundRotation=this.backgroundRotation.toArray(),e.object.environmentIntensity=this.environmentIntensity,e.object.environmentRotation=this.environmentRotation.toArray(),e}},Pi=new R,en=new R,Rc=new R,nn=new R,Ss=new R,bs=new R,Ou=new R,Cc=new R,Pc=new R,Ic=new R,Dc=new ne,Lc=new ne,Nc=new ne,on=class n{constructor(t=new R,e=new R,i=new R){this.a=t,this.b=e,this.c=i}static getNormal(t,e,i,s){s.subVectors(i,e),Pi.subVectors(t,e),s.cross(Pi);let r=s.lengthSq();return r>0?s.multiplyScalar(1/Math.sqrt(r)):s.set(0,0,0)}static getBarycoord(t,e,i,s,r){Pi.subVectors(s,e),en.subVectors(i,e),Rc.subVectors(t,e);let o=Pi.dot(Pi),a=Pi.dot(en),l=Pi.dot(Rc),c=en.dot(en),h=en.dot(Rc),u=o*c-a*a;if(u===0)return r.set(0,0,0),null;let d=1/u,f=(c*l-a*h)*d,p=(o*h-a*l)*d;return r.set(1-f-p,p,f)}static containsPoint(t,e,i,s){return this.getBarycoord(t,e,i,s,nn)===null?!1:nn.x>=0&&nn.y>=0&&nn.x+nn.y<=1}static getInterpolation(t,e,i,s,r,o,a,l){return this.getBarycoord(t,e,i,s,nn)===null?(l.x=0,l.y=0,"z"in l&&(l.z=0),"w"in l&&(l.w=0),null):(l.setScalar(0),l.addScaledVector(r,nn.x),l.addScaledVector(o,nn.y),l.addScaledVector(a,nn.z),l)}static getInterpolatedAttribute(t,e,i,s,r,o){return Dc.setScalar(0),Lc.setScalar(0),Nc.setScalar(0),Dc.fromBufferAttribute(t,e),Lc.fromBufferAttribute(t,i),Nc.fromBufferAttribute(t,s),o.setScalar(0),o.addScaledVector(Dc,r.x),o.addScaledVector(Lc,r.y),o.addScaledVector(Nc,r.z),o}static isFrontFacing(t,e,i,s){return Pi.subVectors(i,e),en.subVectors(t,e),Pi.cross(en).dot(s)<0}set(t,e,i){return this.a.copy(t),this.b.copy(e),this.c.copy(i),this}setFromPointsAndIndices(t,e,i,s){return this.a.copy(t[e]),this.b.copy(t[i]),this.c.copy(t[s]),this}setFromAttributeAndIndices(t,e,i,s){return this.a.fromBufferAttribute(t,e),this.b.fromBufferAttribute(t,i),this.c.fromBufferAttribute(t,s),this}clone(){return new this.constructor().copy(this)}copy(t){return this.a.copy(t.a),this.b.copy(t.b),this.c.copy(t.c),this}getArea(){return Pi.subVectors(this.c,this.b),en.subVectors(this.a,this.b),Pi.cross(en).length()*.5}getMidpoint(t){return t.addVectors(this.a,this.b).add(this.c).multiplyScalar(1/3)}getNormal(t){return n.getNormal(this.a,this.b,this.c,t)}getPlane(t){return t.setFromCoplanarPoints(this.a,this.b,this.c)}getBarycoord(t,e){return n.getBarycoord(t,this.a,this.b,this.c,e)}getInterpolation(t,e,i,s,r){return n.getInterpolation(t,this.a,this.b,this.c,e,i,s,r)}containsPoint(t){return n.containsPoint(t,this.a,this.b,this.c)}isFrontFacing(t){return n.isFrontFacing(this.a,this.b,this.c,t)}intersectsBox(t){return t.intersectsTriangle(this)}closestPointToPoint(t,e){let i=this.a,s=this.b,r=this.c,o,a;Ss.subVectors(s,i),bs.subVectors(r,i),Cc.subVectors(t,i);let l=Ss.dot(Cc),c=bs.dot(Cc);if(l<=0&&c<=0)return e.copy(i);Pc.subVectors(t,s);let h=Ss.dot(Pc),u=bs.dot(Pc);if(h>=0&&u<=h)return e.copy(s);let d=l*u-h*c;if(d<=0&&l>=0&&h<=0&&l-h>0)return o=l/(l-h),e.copy(i).addScaledVector(Ss,o);Ic.subVectors(t,r);let f=Ss.dot(Ic),p=bs.dot(Ic);if(p>=0&&f<=p)return e.copy(r);let x=f*c-l*p;if(x<=0&&c>=0&&p<=0)return a=c/(c-p),e.copy(i).addScaledVector(bs,a);let m=h*p-f*u;if(m<=0&&u-h>=0&&f-p>=0)return Ou.subVectors(r,s),a=(u-h)/(u-h+(f-p)),e.copy(s).addScaledVector(Ou,a);let g=1/(m+x+d);return o=x*g,a=d*g,e.copy(i).addScaledVector(Ss,o).addScaledVector(bs,a)}equals(t){return t.a.equals(this.a)&&t.b.equals(this.b)&&t.c.equals(this.c)}},Wi=class{constructor(t=new R(1/0,1/0,1/0),e=new R(-1/0,-1/0,-1/0)){this.isBox3=!0,this.min=t,this.max=e}set(t,e){return this.min.copy(t),this.max.copy(e),this}setFromArray(t){this.makeEmpty();for(let e=0,i=t.length;e<i;e+=3)this.expandByPoint(Ii.fromArray(t,e));return this}setFromBufferAttribute(t){this.makeEmpty();for(let e=0,i=t.count;e<i;e++)this.expandByPoint(Ii.fromBufferAttribute(t,e));return this}setFromPoints(t){this.makeEmpty();for(let e=0,i=t.length;e<i;e++)this.expandByPoint(t[e]);return this}setFromCenterAndSize(t,e){let i=Ii.copy(e).multiplyScalar(.5);return this.min.copy(t).sub(i),this.max.copy(t).add(i),this}setFromObject(t,e=!1){return this.makeEmpty(),this.expandByObject(t,e)}clone(){return new this.constructor().copy(this)}copy(t){return this.min.copy(t.min),this.max.copy(t.max),this}makeEmpty(){return this.min.x=this.min.y=this.min.z=1/0,this.max.x=this.max.y=this.max.z=-1/0,this}isEmpty(){return this.max.x<this.min.x||this.max.y<this.min.y||this.max.z<this.min.z}getCenter(t){return this.isEmpty()?t.set(0,0,0):t.addVectors(this.min,this.max).multiplyScalar(.5)}getSize(t){return this.isEmpty()?t.set(0,0,0):t.subVectors(this.max,this.min)}expandByPoint(t){return this.min.min(t),this.max.max(t),this}expandByVector(t){return this.min.sub(t),this.max.add(t),this}expandByScalar(t){return this.min.addScalar(-t),this.max.addScalar(t),this}expandByObject(t,e=!1){t.updateWorldMatrix(!1,!1);let i=t.geometry;if(i!==void 0){let r=i.getAttribute("position");if(e===!0&&r!==void 0&&t.isInstancedMesh!==!0&&t.isBatchedMesh!==!0)for(let o=0,a=r.count;o<a;o++)t.isMesh===!0?t.getVertexPosition(o,Ii):Ii.fromBufferAttribute(r,o),Ii.applyMatrix4(t.matrixWorld),this.expandByPoint(Ii);else t.boundingBox!==void 0?(t.boundingBox===null&&t.computeBoundingBox(),zo.copy(t.boundingBox)):(i.boundingBox===null&&i.computeBoundingBox(),zo.copy(i.boundingBox)),zo.applyMatrix4(t.matrixWorld),this.union(zo)}let s=t.children;for(let r=0,o=s.length;r<o;r++)this.expandByObject(s[r],e);return this}containsPoint(t){return t.x>=this.min.x&&t.x<=this.max.x&&t.y>=this.min.y&&t.y<=this.max.y&&t.z>=this.min.z&&t.z<=this.max.z}containsBox(t){return this.min.x<=t.min.x&&t.max.x<=this.max.x&&this.min.y<=t.min.y&&t.max.y<=this.max.y&&this.min.z<=t.min.z&&t.max.z<=this.max.z}getParameter(t,e){return e.set((t.x-this.min.x)/(this.max.x-this.min.x),(t.y-this.min.y)/(this.max.y-this.min.y),(t.z-this.min.z)/(this.max.z-this.min.z))}intersectsBox(t){return t.max.x>=this.min.x&&t.min.x<=this.max.x&&t.max.y>=this.min.y&&t.min.y<=this.max.y&&t.max.z>=this.min.z&&t.min.z<=this.max.z}intersectsSphere(t){return this.clampPoint(t.center,Ii),Ii.distanceToSquared(t.center)<=t.radius*t.radius}intersectsPlane(t){let e,i;return t.normal.x>0?(e=t.normal.x*this.min.x,i=t.normal.x*this.max.x):(e=t.normal.x*this.max.x,i=t.normal.x*this.min.x),t.normal.y>0?(e+=t.normal.y*this.min.y,i+=t.normal.y*this.max.y):(e+=t.normal.y*this.max.y,i+=t.normal.y*this.min.y),t.normal.z>0?(e+=t.normal.z*this.min.z,i+=t.normal.z*this.max.z):(e+=t.normal.z*this.max.z,i+=t.normal.z*this.min.z),e<=-t.constant&&i>=-t.constant}intersectsTriangle(t){if(this.isEmpty())return!1;this.getCenter(vr),ko.subVectors(this.max,vr),ws.subVectors(t.a,vr),Es.subVectors(t.b,vr),Ts.subVectors(t.c,vr),Tn.subVectors(Es,ws),An.subVectors(Ts,Es),Xn.subVectors(ws,Ts);let e=[0,-Tn.z,Tn.y,0,-An.z,An.y,0,-Xn.z,Xn.y,Tn.z,0,-Tn.x,An.z,0,-An.x,Xn.z,0,-Xn.x,-Tn.y,Tn.x,0,-An.y,An.x,0,-Xn.y,Xn.x,0];return!Uc(e,ws,Es,Ts,ko)||(e=[1,0,0,0,1,0,0,0,1],!Uc(e,ws,Es,Ts,ko))?!1:(Ho.crossVectors(Tn,An),e=[Ho.x,Ho.y,Ho.z],Uc(e,ws,Es,Ts,ko))}clampPoint(t,e){return e.copy(t).clamp(this.min,this.max)}distanceToPoint(t){return this.clampPoint(t,Ii).distanceTo(t)}getBoundingSphere(t){return this.isEmpty()?t.makeEmpty():(this.getCenter(t.center),t.radius=this.getSize(Ii).length()*.5),t}intersect(t){return this.min.max(t.min),this.max.min(t.max),this.isEmpty()&&this.makeEmpty(),this}union(t){return this.min.min(t.min),this.max.max(t.max),this}applyMatrix4(t){return this.isEmpty()?this:(sn[0].set(this.min.x,this.min.y,this.min.z).applyMatrix4(t),sn[1].set(this.min.x,this.min.y,this.max.z).applyMatrix4(t),sn[2].set(this.min.x,this.max.y,this.min.z).applyMatrix4(t),sn[3].set(this.min.x,this.max.y,this.max.z).applyMatrix4(t),sn[4].set(this.max.x,this.min.y,this.min.z).applyMatrix4(t),sn[5].set(this.max.x,this.min.y,this.max.z).applyMatrix4(t),sn[6].set(this.max.x,this.max.y,this.min.z).applyMatrix4(t),sn[7].set(this.max.x,this.max.y,this.max.z).applyMatrix4(t),this.setFromPoints(sn),this)}translate(t){return this.min.add(t),this.max.add(t),this}equals(t){return t.min.equals(this.min)&&t.max.equals(this.max)}toJSON(){return{min:this.min.toArray(),max:this.max.toArray()}}fromJSON(t){return this.min.fromArray(t.min),this.max.fromArray(t.max),this}},sn=[new R,new R,new R,new R,new R,new R,new R,new R],Ii=new R,zo=new Wi,ws=new R,Es=new R,Ts=new R,Tn=new R,An=new R,Xn=new R,vr=new R,ko=new R,Ho=new R,qn=new R;function Uc(n,t,e,i,s){for(let r=0,o=n.length-3;r<=o;r+=3){qn.fromArray(n,r);let a=s.x*Math.abs(qn.x)+s.y*Math.abs(qn.y)+s.z*Math.abs(qn.z),l=t.dot(qn),c=e.dot(qn),h=i.dot(qn);if(Math.max(-Math.max(l,c,h),Math.min(l,c,h))>a)return!1}return!0}var an=hm();function hm(){let n=new ArrayBuffer(4),t=new Float32Array(n),e=new Uint32Array(n),i=new Uint32Array(512),s=new Uint32Array(512);for(let l=0;l<256;++l){let c=l-127;c<-27?(i[l]=0,i[l|256]=32768,s[l]=24,s[l|256]=24):c<-14?(i[l]=1024>>-c-14,i[l|256]=1024>>-c-14|32768,s[l]=-c-1,s[l|256]=-c-1):c<=15?(i[l]=c+15<<10,i[l|256]=c+15<<10|32768,s[l]=13,s[l|256]=13):c<128?(i[l]=31744,i[l|256]=64512,s[l]=24,s[l|256]=24):(i[l]=31744,i[l|256]=64512,s[l]=13,s[l|256]=13)}let r=new Uint32Array(2048),o=new Uint32Array(64),a=new Uint32Array(64);for(let l=1;l<1024;++l){let c=l<<13,h=0;for(;(c&8388608)===0;)c<<=1,h-=8388608;c&=-8388609,h+=947912704,r[l]=c|h}for(let l=1024;l<2048;++l)r[l]=939524096+(l-1024<<13);for(let l=1;l<31;++l)o[l]=l<<23;o[31]=1199570944,o[32]=2147483648;for(let l=33;l<63;++l)o[l]=2147483648+(l-32<<23);o[63]=3347054592;for(let l=1;l<64;++l)l!==32&&(a[l]=1024);return{floatView:t,uint32View:e,baseTable:i,shiftTable:s,mantissaTable:r,exponentTable:o,offsetTable:a}}function um(n){Math.abs(n)>65504&&It("DataUtils.toHalfFloat(): Value out of range."),n=kt(n,-65504,65504),an.floatView[0]=n;let t=an.uint32View[0],e=t>>23&511;return an.baseTable[e]+((t&8388607)>>an.shiftTable[e])}function dm(n){let t=n>>10;return an.uint32View[0]=an.mantissaTable[an.offsetTable[t]+(n&1023)]+an.exponentTable[t],an.floatView[0]}var Hs=class{static toHalfFloat(t){return um(t)}static fromHalfFloat(t){return dm(t)}},Ae=new R,Vo=new et,fm=0,re=class extends Li{constructor(t,e,i=!1){if(super(),Array.isArray(t))throw new TypeError("THREE.BufferAttribute: array should be a Typed Array.");this.isBufferAttribute=!0,Object.defineProperty(this,"id",{value:fm++}),this.name="",this.array=t,this.itemSize=e,this.count=t!==void 0?t.length/e:0,this.normalized=i,this.usage=Od,this.updateRanges=[],this.gpuType=ui,this.version=0,this._normalizationScale=t!==void 0?Sh(t):void 0}onUploadCallback(){}set needsUpdate(t){t===!0&&this.version++}setUsage(t){return this.usage=t,this}addUpdateRange(t,e){this.updateRanges.push({start:t,count:e})}clearUpdateRanges(){this.updateRanges.length=0}copy(t){return this.name=t.name,this.array=new t.array.constructor(t.array),this.itemSize=t.itemSize,this.count=t.count,this.normalized=t.normalized,this.usage=t.usage,this.gpuType=t.gpuType,this._normalizationScale=t._normalizationScale,this}copyAt(t,e,i){t*=this.itemSize,i*=e.itemSize;for(let s=0,r=this.itemSize;s<r;s++)this.array[t+s]=e.array[i+s];return this}copyArray(t){return this.array.set(t),this}applyMatrix3(t){if(this.itemSize===2)for(let e=0,i=this.count;e<i;e++)Vo.fromBufferAttribute(this,e),Vo.applyMatrix3(t),this.setXY(e,Vo.x,Vo.y);else if(this.itemSize===3)for(let e=0,i=this.count;e<i;e++)Ae.fromBufferAttribute(this,e),Ae.applyMatrix3(t),this.setXYZ(e,Ae.x,Ae.y,Ae.z);return this}applyMatrix4(t){for(let e=0,i=this.count;e<i;e++)Ae.fromBufferAttribute(this,e),Ae.applyMatrix4(t),this.setXYZ(e,Ae.x,Ae.y,Ae.z);return this}applyNormalMatrix(t){for(let e=0,i=this.count;e<i;e++)Ae.fromBufferAttribute(this,e),Ae.applyNormalMatrix(t),this.setXYZ(e,Ae.x,Ae.y,Ae.z);return this}transformDirection(t){for(let e=0,i=this.count;e<i;e++)Ae.fromBufferAttribute(this,e),Ae.transformDirection(t),this.setXYZ(e,Ae.x,Ae.y,Ae.z);return this}set(t,e=0){return this.array.set(t,e),this}getComponent(t,e){let i=this.array[t*this.itemSize+e];return this.normalized&&(i=this._denormalize(i)),i}setComponent(t,e,i){return this.normalized&&(i=this._normalize(i)),this.array[t*this.itemSize+e]=i,this}getX(t){let e=this.array[t*this.itemSize];return this.normalized&&(e=this._denormalize(e)),e}setX(t,e){return this.normalized&&(e=this._normalize(e)),this.array[t*this.itemSize]=e,this}getY(t){let e=this.array[t*this.itemSize+1];return this.normalized&&(e=this._denormalize(e)),e}setY(t,e){return this.normalized&&(e=this._normalize(e)),this.array[t*this.itemSize+1]=e,this}getZ(t){let e=this.array[t*this.itemSize+2];return this.normalized&&(e=this._denormalize(e)),e}setZ(t,e){return this.normalized&&(e=this._normalize(e)),this.array[t*this.itemSize+2]=e,this}getW(t){let e=this.array[t*this.itemSize+3];return this.normalized&&(e=this._denormalize(e)),e}setW(t,e){return this.normalized&&(e=this._normalize(e)),this.array[t*this.itemSize+3]=e,this}setXY(t,e,i){return t*=this.itemSize,this.normalized&&(e=this._normalize(e),i=this._normalize(i)),this.array[t+0]=e,this.array[t+1]=i,this}setXYZ(t,e,i,s){return t*=this.itemSize,this.normalized&&(e=this._normalize(e),i=this._normalize(i),s=this._normalize(s)),this.array[t+0]=e,this.array[t+1]=i,this.array[t+2]=s,this}setXYZW(t,e,i,s,r){return t*=this.itemSize,this.normalized&&(e=this._normalize(e),i=this._normalize(i),s=this._normalize(s),r=this._normalize(r)),this.array[t+0]=e,this.array[t+1]=i,this.array[t+2]=s,this.array[t+3]=r,this}onUpload(t){return this.onUploadCallback=t,this}clone(){return new this.constructor(this.array,this.itemSize).copy(this)}toJSON(){let t={itemSize:this.itemSize,type:this.array.constructor.name,array:Array.from(this.array),normalized:this.normalized};return t.name=this.name,t.usage=this.usage,t.gpuType=this.gpuType,t}dispose(){this.dispatchEvent({type:"dispose"})}_denormalize(t){let e=this._normalizationScale;return e!==void 0?Math.max(t/e,-1):Vd(t,this.array)}_normalize(t){let e=this._normalizationScale;return e!==void 0?Math.round(t*e):Gd(t,this.array)}};var Dr=class extends re{constructor(t,e,i){super(new Uint16Array(t),e,i)}};var Lr=class extends re{constructor(t,e,i){super(new Uint32Array(t),e,i)}};var jt=class extends re{constructor(t,e,i){super(new Float32Array(t),e,i)}},pm=new Wi,yr=new R,Fc=new R,ri=class{constructor(t=new R,e=-1){this.isSphere=!0,this.center=t,this.radius=e}set(t,e){return this.center.copy(t),this.radius=e,this}setFromPoints(t,e){let i=this.center;e!==void 0?i.copy(e):pm.setFromPoints(t).getCenter(i);let s=0;for(let r=0,o=t.length;r<o;r++)s=Math.max(s,i.distanceToSquared(t[r]));return this.radius=Math.sqrt(s),this}copy(t){return this.center.copy(t.center),this.radius=t.radius,this}isEmpty(){return this.radius<0}makeEmpty(){return this.center.set(0,0,0),this.radius=-1,this}containsPoint(t){return t.distanceToSquared(this.center)<=this.radius*this.radius}distanceToPoint(t){return t.distanceTo(this.center)-this.radius}intersectsSphere(t){let e=this.radius+t.radius;return t.center.distanceToSquared(this.center)<=e*e}intersectsBox(t){return t.intersectsSphere(this)}intersectsPlane(t){return Math.abs(t.distanceToPoint(this.center))<=this.radius}clampPoint(t,e){let i=this.center.distanceToSquared(t);return e.copy(t),i>this.radius*this.radius&&(e.sub(this.center).normalize(),e.multiplyScalar(this.radius).add(this.center)),e}getBoundingBox(t){return this.isEmpty()?(t.makeEmpty(),t):(t.set(this.center,this.center),t.expandByScalar(this.radius),t)}applyMatrix4(t){return this.center.applyMatrix4(t),this.radius=this.radius*t.getMaxScaleOnAxis(),this}translate(t){return this.center.add(t),this}expandByPoint(t){if(this.isEmpty())return this.center.copy(t),this.radius=0,this;yr.subVectors(t,this.center);let e=yr.lengthSq();if(e>this.radius*this.radius){let i=Math.sqrt(e),s=(i-this.radius)*.5;this.center.addScaledVector(yr,s/i),this.radius+=s}return this}union(t){return t.isEmpty()?this:this.isEmpty()?(this.copy(t),this):(this.center.equals(t.center)===!0?this.radius=Math.max(this.radius,t.radius):(Fc.subVectors(t.center,this.center).setLength(t.radius),this.expandByPoint(yr.copy(t.center).add(Fc)),this.expandByPoint(yr.copy(t.center).sub(Fc))),this)}equals(t){return t.center.equals(this.center)&&t.radius===this.radius}clone(){return new this.constructor().copy(this)}toJSON(){return{radius:this.radius,center:this.center.toArray()}}fromJSON(t){return this.radius=t.radius,this.center.fromArray(t.center),this}},mm=0,wi=new Gt,Oc=new ve,As=new R,mi=new Wi,Mr=new Wi,ke=new R,Yt=class n extends Li{constructor(){super(),this.isBufferGeometry=!0,Object.defineProperty(this,"id",{value:mm++}),this.uuid=ir(),this.name="",this.type="BufferGeometry",this.index=null,this.indirect=null,this.indirectOffset=0,this.attributes={},this.morphAttributes={},this.morphTargetsRelative=!1,this.groups=[],this.boundingBox=null,this.boundingSphere=null,this.drawRange={start:0,count:1/0},this.userData={},this._transformed=!1}getIndex(){return this.index}setIndex(t){return Array.isArray(t)?this.index=new(Op(t)?Lr:Dr)(t,1):this.index=t,this}setIndirect(t,e=0){return this.indirect=t,this.indirectOffset=e,this}getIndirect(){return this.indirect}getAttribute(t){return this.attributes[t]}setAttribute(t,e){return this.attributes[t]=e,this}deleteAttribute(t){return delete this.attributes[t],this}hasAttribute(t){return this.attributes[t]!==void 0}addGroup(t,e,i=0){this.groups.push({start:t,count:e,materialIndex:i})}clearGroups(){this.groups=[]}setDrawRange(t,e){this.drawRange.start=t,this.drawRange.count=e}applyMatrix4(t){let e=this.attributes.position;e!==void 0&&(e.applyMatrix4(t),e.needsUpdate=!0);let i=this.attributes.normal;if(i!==void 0){let r=new Ft().getNormalMatrix(t);i.applyNormalMatrix(r),i.needsUpdate=!0}let s=this.attributes.tangent;return s!==void 0&&(s.transformDirection(t),s.needsUpdate=!0),this.boundingBox!==null&&this.computeBoundingBox(),this.boundingSphere!==null&&this.computeBoundingSphere(),this._transformed=!0,this}applyQuaternion(t){return wi.makeRotationFromQuaternion(t),this.applyMatrix4(wi),this}rotateX(t){return wi.makeRotationX(t),this.applyMatrix4(wi),this}rotateY(t){return wi.makeRotationY(t),this.applyMatrix4(wi),this}rotateZ(t){return wi.makeRotationZ(t),this.applyMatrix4(wi),this}translate(t,e,i){return wi.makeTranslation(t,e,i),this.applyMatrix4(wi),this}scale(t,e,i){return wi.makeScale(t,e,i),this.applyMatrix4(wi),this}lookAt(t){return Oc.lookAt(t),Oc.updateMatrix(),this.applyMatrix4(Oc.matrix),this}center(){return this.computeBoundingBox(),this.boundingBox.getCenter(As).negate(),this.translate(As.x,As.y,As.z),this}setFromPoints(t){let e=this.getAttribute("position");if(e===void 0){let i=[];for(let s=0,r=t.length;s<r;s++){let o=t[s];i.push(o.x,o.y,o.z||0)}this.setAttribute("position",new jt(i,3))}else{let i=Math.min(t.length,e.count);for(let s=0;s<i;s++){let r=t[s];e.setXYZ(s,r.x,r.y,r.z||0)}t.length>e.count&&It("BufferGeometry: Buffer size too small for points data. Use .dispose() and create a new geometry."),e.needsUpdate=!0}return this}computeBoundingBox(){this.boundingBox===null&&(this.boundingBox=new Wi);let t=this.attributes.position,e=this.morphAttributes.position;if(t&&t.isGLBufferAttribute){Ut("BufferGeometry.computeBoundingBox(): GLBufferAttribute requires a manual bounding box.",this),this.boundingBox.set(new R(-1/0,-1/0,-1/0),new R(1/0,1/0,1/0));return}if(t!==void 0){if(this.boundingBox.setFromBufferAttribute(t),e)for(let i=0,s=e.length;i<s;i++){let r=e[i];mi.setFromBufferAttribute(r),this.morphTargetsRelative?(ke.addVectors(this.boundingBox.min,mi.min),this.boundingBox.expandByPoint(ke),ke.addVectors(this.boundingBox.max,mi.max),this.boundingBox.expandByPoint(ke)):(this.boundingBox.expandByPoint(mi.min),this.boundingBox.expandByPoint(mi.max))}}else this.boundingBox.makeEmpty();(isNaN(this.boundingBox.min.x)||isNaN(this.boundingBox.min.y)||isNaN(this.boundingBox.min.z))&&Ut('BufferGeometry.computeBoundingBox(): Computed min/max have NaN values. The "position" attribute is likely to have NaN values.',this)}computeBoundingSphere(){this.boundingSphere===null&&(this.boundingSphere=new ri);let t=this.attributes.position,e=this.morphAttributes.position;if(t&&t.isGLBufferAttribute){Ut("BufferGeometry.computeBoundingSphere(): GLBufferAttribute requires a manual bounding sphere.",this),this.boundingSphere.set(new R,1/0);return}if(t){let i=this.boundingSphere.center;if(mi.setFromBufferAttribute(t),e)for(let r=0,o=e.length;r<o;r++){let a=e[r];Mr.setFromBufferAttribute(a),this.morphTargetsRelative?(ke.addVectors(mi.min,Mr.min),mi.expandByPoint(ke),ke.addVectors(mi.max,Mr.max),mi.expandByPoint(ke)):(mi.expandByPoint(Mr.min),mi.expandByPoint(Mr.max))}mi.getCenter(i);let s=0;for(let r=0,o=t.count;r<o;r++)ke.fromBufferAttribute(t,r),s=Math.max(s,i.distanceToSquared(ke));if(e)for(let r=0,o=e.length;r<o;r++){let a=e[r],l=this.morphTargetsRelative;for(let c=0,h=a.count;c<h;c++)ke.fromBufferAttribute(a,c),l&&(As.fromBufferAttribute(t,c),ke.add(As)),s=Math.max(s,i.distanceToSquared(ke))}this.boundingSphere.radius=Math.sqrt(s),isNaN(this.boundingSphere.radius)&&Ut('BufferGeometry.computeBoundingSphere(): Computed radius is NaN. The "position" attribute is likely to have NaN values.',this)}}computeTangents(){let t=this.index,e=this.attributes;if(t===null||e.position===void 0||e.normal===void 0||e.uv===void 0){Ut("BufferGeometry: .computeTangents() failed. Missing required attributes (index, position, normal or uv)");return}let i=e.position,s=e.normal,r=e.uv,o=this.getAttribute("tangent");(o===void 0||o.count!==i.count)&&(o=new re(new Float32Array(4*i.count),4),this.setAttribute("tangent",o));let a=[],l=[],c=new Uint8Array(i.count);for(let D=0;D<i.count;D++)a[D]=new R,l[D]=new R;let h=new R,u=new R,d=new R,f=new et,p=new et,x=new et,m=new R,g=new R;function _(D,P,A){c[D]=c[P]=c[A]=1,h.fromBufferAttribute(i,D),u.fromBufferAttribute(i,P),d.fromBufferAttribute(i,A),f.fromBufferAttribute(r,D),p.fromBufferAttribute(r,P),x.fromBufferAttribute(r,A),u.sub(h),d.sub(h),p.sub(f),x.sub(f);let C=1/(p.x*x.y-x.x*p.y);isFinite(C)&&(m.copy(u).multiplyScalar(x.y).addScaledVector(d,-p.y).multiplyScalar(C),g.copy(d).multiplyScalar(p.x).addScaledVector(u,-x.x).multiplyScalar(C),a[D].add(m),a[P].add(m),a[A].add(m),l[D].add(g),l[P].add(g),l[A].add(g))}let y=this.groups;y.length===0&&(y=[{start:0,count:t.count}]);for(let D=0,P=y.length;D<P;++D){let A=y[D],C=A.start,U=A.count;for(let L=C,B=C+U;L<B;L+=3)_(t.getX(L+0),t.getX(L+1),t.getX(L+2))}let S=new R,T=new R,w=new R,v=new R;function M(D){w.fromBufferAttribute(s,D),v.copy(w);let P=a[D];S.copy(P),S.sub(w.multiplyScalar(w.dot(P))).normalize(),T.crossVectors(v,P);let C=T.dot(l[D])<0?-1:1;o.setXYZW(D,S.x,S.y,S.z,C)}for(let D=0,P=i.count;D<P;++D)c[D]&&M(D);this._transformed=!0}computeVertexNormals(){let t=this.index,e=this.getAttribute("position");if(e!==void 0){let i=this.getAttribute("normal");if(i===void 0||i.count!==e.count)i=new re(new Float32Array(e.count*3),3),this.setAttribute("normal",i);else for(let d=0,f=i.count;d<f;d++)i.setXYZ(d,0,0,0);let s=new R,r=new R,o=new R,a=new R,l=new R,c=new R,h=new R,u=new R;if(t)for(let d=0,f=t.count;d<f;d+=3){let p=t.getX(d+0),x=t.getX(d+1),m=t.getX(d+2);s.fromBufferAttribute(e,p),r.fromBufferAttribute(e,x),o.fromBufferAttribute(e,m),h.subVectors(o,r),u.subVectors(s,r),h.cross(u),a.fromBufferAttribute(i,p),l.fromBufferAttribute(i,x),c.fromBufferAttribute(i,m),a.add(h),l.add(h),c.add(h),i.setXYZ(p,a.x,a.y,a.z),i.setXYZ(x,l.x,l.y,l.z),i.setXYZ(m,c.x,c.y,c.z)}else for(let d=0,f=e.count;d<f;d+=3)s.fromBufferAttribute(e,d+0),r.fromBufferAttribute(e,d+1),o.fromBufferAttribute(e,d+2),h.subVectors(o,r),u.subVectors(s,r),h.cross(u),i.setXYZ(d+0,h.x,h.y,h.z),i.setXYZ(d+1,h.x,h.y,h.z),i.setXYZ(d+2,h.x,h.y,h.z);this.normalizeNormals(),i.needsUpdate=!0}}normalizeNormals(){let t=this.attributes.normal;for(let e=0,i=t.count;e<i;e++)ke.fromBufferAttribute(t,e),ke.normalize(),t.setXYZ(e,ke.x,ke.y,ke.z)}toNonIndexed(){function t(a,l){let c=a.array,h=a.itemSize,u=a.normalized,d=new c.constructor(l.length*h),f=0,p=0;for(let x=0,m=l.length;x<m;x++){a.isInterleavedBufferAttribute?f=l[x]*a.data.stride+a.offset:f=l[x]*h;for(let g=0;g<h;g++)d[p++]=c[f++]}return new re(d,h,u)}if(this.index===null)return It("BufferGeometry.toNonIndexed(): BufferGeometry is already non-indexed."),this;let e=new n,i=this.index.array,s=this.attributes;for(let a in s){let l=s[a],c=t(l,i);e.setAttribute(a,c)}let r=this.morphAttributes;for(let a in r){let l=[],c=r[a];for(let h=0,u=c.length;h<u;h++){let d=c[h],f=t(d,i);l.push(f)}e.morphAttributes[a]=l}e.morphTargetsRelative=this.morphTargetsRelative;let o=this.groups;for(let a=0,l=o.length;a<l;a++){let c=o[a];e.addGroup(c.start,c.count,c.materialIndex)}return e}toJSON(){let t={metadata:{version:4.7,type:"BufferGeometry",generator:"BufferGeometry.toJSON"}};if(t.uuid=this.uuid,t.type=this.parameters!==void 0&&this._transformed===!0?"BufferGeometry":this.type,t.name=this.name,Object.keys(this.userData).length>0&&(t.userData=this.userData),this.parameters!==void 0&&this._transformed!==!0){let l=this.parameters;for(let c in l)l[c]!==void 0&&(t[c]=l[c]);return t}t.data={attributes:{}};let e=this.index;e!==null&&(t.data.index={type:e.array.constructor.name,array:Array.prototype.slice.call(e.array)});let i=this.attributes;for(let l in i){let c=i[l];t.data.attributes[l]=c.toJSON(t.data)}let s={},r=!1;for(let l in this.morphAttributes){let c=this.morphAttributes[l],h=[];for(let u=0,d=c.length;u<d;u++){let f=c[u];h.push(f.toJSON(t.data))}h.length>0&&(s[l]=h,r=!0)}r&&(t.data.morphAttributes=s,t.data.morphTargetsRelative=this.morphTargetsRelative);let o=this.groups;o.length>0&&(t.data.groups=JSON.parse(JSON.stringify(o)));let a=this.boundingSphere;return a!==null&&(t.data.boundingSphere=a.toJSON()),t}clone(){return new this.constructor().copy(this)}copy(t){this.index=null,this.attributes={},this.morphAttributes={},this.groups=[],this.boundingBox=null,this.boundingSphere=null;let e={};this.name=t.name;let i=t.index;i!==null&&this.setIndex(i.clone());let s=t.attributes;for(let c in s){let h=s[c];this.setAttribute(c,h.clone(e))}let r=t.morphAttributes;for(let c in r){let h=[],u=r[c];for(let d=0,f=u.length;d<f;d++)h.push(u[d].clone(e));this.morphAttributes[c]=h}this.morphTargetsRelative=t.morphTargetsRelative;let o=t.groups;for(let c=0,h=o.length;c<h;c++){let u=o[c];this.addGroup(u.start,u.count,u.materialIndex)}let a=t.boundingBox;a!==null&&(this.boundingBox=a.clone());let l=t.boundingSphere;return l!==null&&(this.boundingSphere=l.clone()),this.drawRange.start=t.drawRange.start,this.drawRange.count=t.drawRange.count,this.userData=t.userData,this._transformed=t._transformed,this}dispose(){this.dispatchEvent({type:"dispose"})}};var Bc=new R,gm=new R,xm=new Ft,si=class{constructor(t=new R(1,0,0),e=0){this.isPlane=!0,this.normal=t,this.constant=e}set(t,e){return this.normal.copy(t),this.constant=e,this}setComponents(t,e,i,s){return this.normal.set(t,e,i),this.constant=s,this}setFromNormalAndCoplanarPoint(t,e){return this.normal.copy(t),this.constant=-e.dot(this.normal),this}setFromCoplanarPoints(t,e,i){let s=Bc.subVectors(i,e).cross(gm.subVectors(t,e)).normalize();return this.setFromNormalAndCoplanarPoint(s,t),this}copy(t){return this.normal.copy(t.normal),this.constant=t.constant,this}normalize(){let t=1/this.normal.length();return this.normal.multiplyScalar(t),this.constant*=t,this}negate(){return this.constant*=-1,this.normal.negate(),this}distanceToPoint(t){return this.normal.dot(t)+this.constant}distanceToSphere(t){return this.distanceToPoint(t.center)-t.radius}projectPoint(t,e){return e.copy(t).addScaledVector(this.normal,-this.distanceToPoint(t))}intersectLine(t,e,i=!0){let s=t.delta(Bc),r=this.normal.dot(s);if(r===0)return this.distanceToPoint(t.start)===0?e.copy(t.start):null;let o=-(t.start.dot(this.normal)+this.constant)/r;return i===!0&&(o<0||o>1)?null:e.copy(t.start).addScaledVector(s,o)}intersectsLine(t){let e=this.distanceToPoint(t.start),i=this.distanceToPoint(t.end);return e<0&&i>0||i<0&&e>0}intersectsBox(t){return t.intersectsPlane(this)}intersectsSphere(t){return t.intersectsPlane(this)}coplanarPoint(t){return t.copy(this.normal).multiplyScalar(-this.constant)}applyMatrix4(t,e){let i=e||xm.getNormalMatrix(t),s=this.coplanarPoint(Bc).applyMatrix4(t),r=this.normal.applyMatrix3(i).normalize();return this.constant=-s.dot(r),this}translate(t){return this.constant-=t.dot(this.normal),this}equals(t){return t.normal.equals(this.normal)&&t.constant===this.constant}clone(){return new this.constructor().copy(this)}toJSON(){return{normal:this.normal.toArray(),constant:this.constant}}fromJSON(t){return this.normal.fromArray(t.normal),this.constant=t.constant,this}},_m=0,Ni=class extends Li{constructor(){super(),this.isMaterial=!0,Object.defineProperty(this,"id",{value:_m++}),this.uuid=ir(),this.name="",this.type="Material",this.blending=$s,this.side=Fn,this.vertexColors=!1,this.opacity=1,this.transparent=!1,this.alphaHash=!1,this.blendSrc=lh,this.blendDst=ch,this.blendEquation=pn,this.blendSrcAlpha=null,this.blendDstAlpha=null,this.blendEquationAlpha=null,this.blendColor=new St(0,0,0),this.blendAlpha=0,this.depthFunc=Ns,this.depthTest=!0,this.depthWrite=!0,this.stencilWriteMask=255,this.stencilFunc=Id,this.stencilRef=0,this.stencilFuncMask=255,this.stencilFail=ua,this.stencilZFail=ua,this.stencilZPass=ua,this.stencilWrite=!1,this.clippingPlanes=null,this.clipIntersection=!1,this.clipShadows=!1,this.shadowSide=null,this.colorWrite=!0,this.precision=null,this.polygonOffset=!1,this.polygonOffsetFactor=0,this.polygonOffsetUnits=0,this.dithering=!1,this.alphaToCoverage=!1,this.premultipliedAlpha=!1,this.forceSinglePass=!1,this.allowOverride=!0,this.visible=!0,this.toneMapped=!0,this.userData={},this.version=0,this._alphaTest=0}get alphaTest(){return this._alphaTest}set alphaTest(t){this._alphaTest>0!=t>0&&this.version++,this._alphaTest=t}onBeforeRender(){}onBeforeCompile(){}customProgramCacheKey(){return this.onBeforeCompile.toString()}setValues(t){if(t!==void 0)for(let e in t){let i=t[e];if(i===void 0){It(`Material: parameter '${e}' has value of undefined.`);continue}let s=this[e];if(s===void 0){It(`Material: '${e}' is not a property of THREE.${this.type}.`);continue}s&&s.isColor?s.set(i):s&&s.isVector2&&i&&i.isVector2||s&&s.isEuler&&i&&i.isEuler||s&&s.isVector3&&i&&i.isVector3?s.copy(i):this[e]=i}}toJSON(t){let e=t===void 0||typeof t=="string";e&&(t={textures:{},images:{}});let i={metadata:{version:4.7,type:"Material",generator:"Material.toJSON"}};i.uuid=this.uuid,i.type=this.type,i.blending=this.blending,i.side=this.side,i.shadowSide=this.shadowSide,i.vertexColors=this.vertexColors,i.opacity=this.opacity,i.transparent=this.transparent,i.blendSrc=this.blendSrc,i.blendDst=this.blendDst,i.blendEquation=this.blendEquation,i.blendSrcAlpha=this.blendSrcAlpha,i.blendDstAlpha=this.blendDstAlpha,i.blendEquationAlpha=this.blendEquationAlpha,i.blendColor=this.blendColor.getHex(),i.blendAlpha=this.blendAlpha,i.depthFunc=this.depthFunc,i.depthTest=this.depthTest,i.depthWrite=this.depthWrite,i.colorWrite=this.colorWrite,i.clipIntersection=this.clipIntersection,i.clipShadows=this.clipShadows,i.stencilWriteMask=this.stencilWriteMask,i.stencilFunc=this.stencilFunc,i.stencilRef=this.stencilRef,i.stencilFuncMask=this.stencilFuncMask,i.stencilFail=this.stencilFail,i.stencilZFail=this.stencilZFail,i.stencilZPass=this.stencilZPass,i.stencilWrite=this.stencilWrite,i.polygonOffset=this.polygonOffset,i.polygonOffsetFactor=this.polygonOffsetFactor,i.polygonOffsetUnits=this.polygonOffsetUnits,i.dithering=this.dithering,i.alphaTest=this.alphaTest,i.alphaHash=this.alphaHash,i.alphaToCoverage=this.alphaToCoverage,i.premultipliedAlpha=this.premultipliedAlpha,i.forceSinglePass=this.forceSinglePass,i.allowOverride=this.allowOverride,i.visible=this.visible,i.toneMapped=this.toneMapped,i.name=this.name,this.color&&this.color.isColor&&(i.color=this.color.getHex()),this.roughness!==void 0&&(i.roughness=this.roughness),this.metalness!==void 0&&(i.metalness=this.metalness),this.sheen!==void 0&&(i.sheen=this.sheen),this.sheenColor&&this.sheenColor.isColor&&(i.sheenColor=this.sheenColor.getHex()),this.sheenRoughness!==void 0&&(i.sheenRoughness=this.sheenRoughness),this.emissive&&this.emissive.isColor&&(i.emissive=this.emissive.getHex()),this.emissiveIntensity!==void 0&&(i.emissiveIntensity=this.emissiveIntensity),this.specular&&this.specular.isColor&&(i.specular=this.specular.getHex()),this.specularIntensity!==void 0&&(i.specularIntensity=this.specularIntensity),this.specularColor&&this.specularColor.isColor&&(i.specularColor=this.specularColor.getHex()),this.shininess!==void 0&&(i.shininess=this.shininess),this.clearcoat!==void 0&&(i.clearcoat=this.clearcoat),this.clearcoatRoughness!==void 0&&(i.clearcoatRoughness=this.clearcoatRoughness),this.diffuseRoughness!==void 0&&(i.diffuseRoughness=this.diffuseRoughness),this.diffuseRoughnessMap&&this.diffuseRoughnessMap.isTexture&&(i.diffuseRoughnessMap=this.diffuseRoughnessMap.toJSON(t).uuid),this.clearcoatMap&&this.clearcoatMap.isTexture&&(i.clearcoatMap=this.clearcoatMap.toJSON(t).uuid),this.clearcoatRoughnessMap&&this.clearcoatRoughnessMap.isTexture&&(i.clearcoatRoughnessMap=this.clearcoatRoughnessMap.toJSON(t).uuid),this.clearcoatNormalMap&&this.clearcoatNormalMap.isTexture&&(i.clearcoatNormalMap=this.clearcoatNormalMap.toJSON(t).uuid,i.clearcoatNormalScale=this.clearcoatNormalScale.toArray()),this.sheenColorMap&&this.sheenColorMap.isTexture&&(i.sheenColorMap=this.sheenColorMap.toJSON(t).uuid),this.sheenRoughnessMap&&this.sheenRoughnessMap.isTexture&&(i.sheenRoughnessMap=this.sheenRoughnessMap.toJSON(t).uuid),this.dispersion!==void 0&&(i.dispersion=this.dispersion),this.retroreflectivity!==void 0&&(i.retroreflectivity=this.retroreflectivity),this.iridescence!==void 0&&(i.iridescence=this.iridescence),this.iridescenceIOR!==void 0&&(i.iridescenceIOR=this.iridescenceIOR),this.iridescenceThicknessMinimum!==void 0&&(i.iridescenceThicknessMinimum=this.iridescenceThicknessMinimum),this.iridescenceThicknessMaximum!==void 0&&(i.iridescenceThicknessMaximum=this.iridescenceThicknessMaximum),this.iridescenceMap&&this.iridescenceMap.isTexture&&(i.iridescenceMap=this.iridescenceMap.toJSON(t).uuid),this.iridescenceThicknessMap&&this.iridescenceThicknessMap.isTexture&&(i.iridescenceThicknessMap=this.iridescenceThicknessMap.toJSON(t).uuid),this.anisotropy!==void 0&&(i.anisotropy=this.anisotropy),this.anisotropyRotation!==void 0&&(i.anisotropyRotation=this.anisotropyRotation),this.anisotropyMap&&this.anisotropyMap.isTexture&&(i.anisotropyMap=this.anisotropyMap.toJSON(t).uuid),this.map&&this.map.isTexture&&(i.map=this.map.toJSON(t).uuid),this.matcap&&this.matcap.isTexture&&(i.matcap=this.matcap.toJSON(t).uuid),this.alphaMap&&this.alphaMap.isTexture&&(i.alphaMap=this.alphaMap.toJSON(t).uuid),this.lightMap&&this.lightMap.isTexture&&(i.lightMap=this.lightMap.toJSON(t).uuid,i.lightMapIntensity=this.lightMapIntensity),this.aoMap&&this.aoMap.isTexture&&(i.aoMap=this.aoMap.toJSON(t).uuid,i.aoMapIntensity=this.aoMapIntensity),this.bumpMap&&this.bumpMap.isTexture&&(i.bumpMap=this.bumpMap.toJSON(t).uuid,i.bumpScale=this.bumpScale),this.normalMap&&this.normalMap.isTexture&&(i.normalMap=this.normalMap.toJSON(t).uuid,i.normalMapType=this.normalMapType,i.normalScale=this.normalScale.toArray()),this.displacementMap&&this.displacementMap.isTexture&&(i.displacementMap=this.displacementMap.toJSON(t).uuid,i.displacementScale=this.displacementScale,i.displacementBias=this.displacementBias),this.roughnessMap&&this.roughnessMap.isTexture&&(i.roughnessMap=this.roughnessMap.toJSON(t).uuid),this.metalnessMap&&this.metalnessMap.isTexture&&(i.metalnessMap=this.metalnessMap.toJSON(t).uuid),this.emissiveMap&&this.emissiveMap.isTexture&&(i.emissiveMap=this.emissiveMap.toJSON(t).uuid),this.specularMap&&this.specularMap.isTexture&&(i.specularMap=this.specularMap.toJSON(t).uuid),this.specularIntensityMap&&this.specularIntensityMap.isTexture&&(i.specularIntensityMap=this.specularIntensityMap.toJSON(t).uuid),this.specularColorMap&&this.specularColorMap.isTexture&&(i.specularColorMap=this.specularColorMap.toJSON(t).uuid),this.envMap&&this.envMap.isTexture&&(i.envMap=this.envMap.toJSON(t).uuid,this.combine!==void 0&&(i.combine=this.combine)),this.envMapRotation!==void 0&&(i.envMapRotation=this.envMapRotation.toArray()),this.envMapIntensity!==void 0&&(i.envMapIntensity=this.envMapIntensity),this.reflectivity!==void 0&&(i.reflectivity=this.reflectivity),this.refractionRatio!==void 0&&(i.refractionRatio=this.refractionRatio),this.gradientMap&&this.gradientMap.isTexture&&(i.gradientMap=this.gradientMap.toJSON(t).uuid),this.transmission!==void 0&&(i.transmission=this.transmission),this.transmissionMap&&this.transmissionMap.isTexture&&(i.transmissionMap=this.transmissionMap.toJSON(t).uuid),this.thickness!==void 0&&(i.thickness=this.thickness),this.thicknessMap&&this.thicknessMap.isTexture&&(i.thicknessMap=this.thicknessMap.toJSON(t).uuid),this.attenuationDistance!==void 0&&(i.attenuationDistance=this.attenuationDistance),this.attenuationColor!==void 0&&(i.attenuationColor=this.attenuationColor.getHex()),this.size!==void 0&&(i.size=this.size),this.sizeAttenuation!==void 0&&(i.sizeAttenuation=this.sizeAttenuation),Array.isArray(this.clippingPlanes)&&this.clippingPlanes.length>0&&(i.clippingPlanes=this.clippingPlanes.map(r=>r.toJSON())),this.rotation!==void 0&&(i.rotation=this.rotation),this.depthPacking!==void 0&&(i.depthPacking=this.depthPacking),this.linewidth!==void 0&&(i.linewidth=this.linewidth),this.linecap!==void 0&&(i.linecap=this.linecap),this.linejoin!==void 0&&(i.linejoin=this.linejoin),this.dashSize!==void 0&&(i.dashSize=this.dashSize),this.gapSize!==void 0&&(i.gapSize=this.gapSize),this.scale!==void 0&&(i.scale=this.scale),this.wireframe!==void 0&&(i.wireframe=this.wireframe),this.wireframeLinewidth!==void 0&&(i.wireframeLinewidth=this.wireframeLinewidth),this.wireframeLinecap!==void 0&&(i.wireframeLinecap=this.wireframeLinecap),this.wireframeLinejoin!==void 0&&(i.wireframeLinejoin=this.wireframeLinejoin),this.flatShading!==void 0&&(i.flatShading=this.flatShading),this.fog!==void 0&&(i.fog=this.fog),Object.keys(this.userData).length>0&&(i.userData=this.userData);function s(r){let o=[];for(let a in r){let l=r[a];delete l.metadata,o.push(l)}return o}if(e){let r=s(t.textures),o=s(t.images);r.length>0&&(i.textures=r),o.length>0&&(i.images=o)}return i}fromJSON(t,e){if(t.uuid!==void 0&&(this.uuid=t.uuid),t.name!==void 0&&(this.name=t.name),t.color!==void 0&&this.color!==void 0&&this.color.setHex(t.color),t.roughness!==void 0&&(this.roughness=t.roughness),t.metalness!==void 0&&(this.metalness=t.metalness),t.sheen!==void 0&&(this.sheen=t.sheen),t.sheenColor!==void 0&&(this.sheenColor=new St().setHex(t.sheenColor)),t.sheenRoughness!==void 0&&(this.sheenRoughness=t.sheenRoughness),t.emissive!==void 0&&this.emissive!==void 0&&this.emissive.setHex(t.emissive),t.specular!==void 0&&this.specular!==void 0&&this.specular.setHex(t.specular),t.specularIntensity!==void 0&&(this.specularIntensity=t.specularIntensity),t.specularColor!==void 0&&this.specularColor!==void 0&&this.specularColor.setHex(t.specularColor),t.shininess!==void 0&&(this.shininess=t.shininess),t.clearcoat!==void 0&&(this.clearcoat=t.clearcoat),t.clearcoatRoughness!==void 0&&(this.clearcoatRoughness=t.clearcoatRoughness),t.diffuseRoughness!==void 0&&(this.diffuseRoughness=t.diffuseRoughness),t.dispersion!==void 0&&(this.dispersion=t.dispersion),t.retroreflectivity!==void 0&&(this.retroreflectivity=t.retroreflectivity),t.iridescence!==void 0&&(this.iridescence=t.iridescence),t.iridescenceIOR!==void 0&&(this.iridescenceIOR=t.iridescenceIOR),t.iridescenceThicknessMinimum!==void 0&&(this.iridescenceThicknessMinimum=t.iridescenceThicknessMinimum),t.iridescenceThicknessMaximum!==void 0&&(this.iridescenceThicknessMaximum=t.iridescenceThicknessMaximum),t.iridescenceThicknessRange!==void 0&&(this.iridescenceThicknessMinimum=t.iridescenceThicknessRange[0],this.iridescenceThicknessMaximum=t.iridescenceThicknessRange[1]),t.transmission!==void 0&&(this.transmission=t.transmission),t.thickness!==void 0&&(this.thickness=t.thickness),t.attenuationDistance!==void 0&&(this.attenuationDistance=t.attenuationDistance),t.attenuationColor!==void 0&&this.attenuationColor!==void 0&&this.attenuationColor.setHex(t.attenuationColor),t.anisotropy!==void 0&&(this.anisotropy=t.anisotropy),t.anisotropyRotation!==void 0&&(this.anisotropyRotation=t.anisotropyRotation),t.fog!==void 0&&(this.fog=t.fog),t.flatShading!==void 0&&(this.flatShading=t.flatShading),t.blending!==void 0&&(this.blending=t.blending),t.combine!==void 0&&(this.combine=t.combine),t.side!==void 0&&(this.side=t.side),t.shadowSide!==void 0&&(this.shadowSide=t.shadowSide),t.opacity!==void 0&&(this.opacity=t.opacity),t.transparent!==void 0&&(this.transparent=t.transparent),t.alphaTest!==void 0&&(this.alphaTest=t.alphaTest),t.alphaHash!==void 0&&(this.alphaHash=t.alphaHash),t.depthFunc!==void 0&&(this.depthFunc=t.depthFunc),t.depthTest!==void 0&&(this.depthTest=t.depthTest),t.depthWrite!==void 0&&(this.depthWrite=t.depthWrite),t.colorWrite!==void 0&&(this.colorWrite=t.colorWrite),t.clippingPlanes!==void 0&&(this.clippingPlanes=t.clippingPlanes.map(i=>new si().fromJSON(i))),t.clipIntersection!==void 0&&(this.clipIntersection=t.clipIntersection),t.clipShadows!==void 0&&(this.clipShadows=t.clipShadows),t.depthPacking!==void 0&&(this.depthPacking=t.depthPacking),t.blendSrc!==void 0&&(this.blendSrc=t.blendSrc),t.blendDst!==void 0&&(this.blendDst=t.blendDst),t.blendEquation!==void 0&&(this.blendEquation=t.blendEquation),t.blendSrcAlpha!==void 0&&(this.blendSrcAlpha=t.blendSrcAlpha),t.blendDstAlpha!==void 0&&(this.blendDstAlpha=t.blendDstAlpha),t.blendEquationAlpha!==void 0&&(this.blendEquationAlpha=t.blendEquationAlpha),t.blendColor!==void 0&&this.blendColor!==void 0&&this.blendColor.setHex(t.blendColor),t.blendAlpha!==void 0&&(this.blendAlpha=t.blendAlpha),t.stencilWriteMask!==void 0&&(this.stencilWriteMask=t.stencilWriteMask),t.stencilFunc!==void 0&&(this.stencilFunc=t.stencilFunc),t.stencilRef!==void 0&&(this.stencilRef=t.stencilRef),t.stencilFuncMask!==void 0&&(this.stencilFuncMask=t.stencilFuncMask),t.stencilFail!==void 0&&(this.stencilFail=t.stencilFail),t.stencilZFail!==void 0&&(this.stencilZFail=t.stencilZFail),t.stencilZPass!==void 0&&(this.stencilZPass=t.stencilZPass),t.stencilWrite!==void 0&&(this.stencilWrite=t.stencilWrite),t.wireframe!==void 0&&(this.wireframe=t.wireframe),t.wireframeLinewidth!==void 0&&(this.wireframeLinewidth=t.wireframeLinewidth),t.wireframeLinecap!==void 0&&(this.wireframeLinecap=t.wireframeLinecap),t.wireframeLinejoin!==void 0&&(this.wireframeLinejoin=t.wireframeLinejoin),t.rotation!==void 0&&(this.rotation=t.rotation),t.linewidth!==void 0&&(this.linewidth=t.linewidth),t.linecap!==void 0&&(this.linecap=t.linecap),t.linejoin!==void 0&&(this.linejoin=t.linejoin),t.dashSize!==void 0&&(this.dashSize=t.dashSize),t.gapSize!==void 0&&(this.gapSize=t.gapSize),t.scale!==void 0&&(this.scale=t.scale),t.polygonOffset!==void 0&&(this.polygonOffset=t.polygonOffset),t.polygonOffsetFactor!==void 0&&(this.polygonOffsetFactor=t.polygonOffsetFactor),t.polygonOffsetUnits!==void 0&&(this.polygonOffsetUnits=t.polygonOffsetUnits),t.dithering!==void 0&&(this.dithering=t.dithering),t.alphaToCoverage!==void 0&&(this.alphaToCoverage=t.alphaToCoverage),t.premultipliedAlpha!==void 0&&(this.premultipliedAlpha=t.premultipliedAlpha),t.forceSinglePass!==void 0&&(this.forceSinglePass=t.forceSinglePass),t.allowOverride!==void 0&&(this.allowOverride=t.allowOverride),t.visible!==void 0&&(this.visible=t.visible),t.toneMapped!==void 0&&(this.toneMapped=t.toneMapped),t.userData!==void 0&&(this.userData=t.userData),t.vertexColors!==void 0&&(typeof t.vertexColors=="number"?this.vertexColors=t.vertexColors>0:this.vertexColors=t.vertexColors),t.size!==void 0&&(this.size=t.size),t.sizeAttenuation!==void 0&&(this.sizeAttenuation=t.sizeAttenuation),t.map!==void 0&&(this.map=e[t.map]||null),t.matcap!==void 0&&(this.matcap=e[t.matcap]||null),t.alphaMap!==void 0&&(this.alphaMap=e[t.alphaMap]||null),t.bumpMap!==void 0&&(this.bumpMap=e[t.bumpMap]||null),t.bumpScale!==void 0&&(this.bumpScale=t.bumpScale),t.normalMap!==void 0&&(this.normalMap=e[t.normalMap]||null),t.normalMapType!==void 0&&(this.normalMapType=t.normalMapType),t.normalScale!==void 0){let i=t.normalScale;Array.isArray(i)===!1&&(i=[i,i]),this.normalScale=new et().fromArray(i)}return t.displacementMap!==void 0&&(this.displacementMap=e[t.displacementMap]||null),t.displacementScale!==void 0&&(this.displacementScale=t.displacementScale),t.displacementBias!==void 0&&(this.displacementBias=t.displacementBias),t.roughnessMap!==void 0&&(this.roughnessMap=e[t.roughnessMap]||null),t.metalnessMap!==void 0&&(this.metalnessMap=e[t.metalnessMap]||null),t.emissiveMap!==void 0&&(this.emissiveMap=e[t.emissiveMap]||null),t.emissiveIntensity!==void 0&&(this.emissiveIntensity=t.emissiveIntensity),t.specularMap!==void 0&&(this.specularMap=e[t.specularMap]||null),t.specularIntensityMap!==void 0&&(this.specularIntensityMap=e[t.specularIntensityMap]||null),t.specularColorMap!==void 0&&(this.specularColorMap=e[t.specularColorMap]||null),t.envMap!==void 0&&(this.envMap=e[t.envMap]||null),t.envMapRotation!==void 0&&this.envMapRotation.fromArray(t.envMapRotation),t.envMapIntensity!==void 0&&(this.envMapIntensity=t.envMapIntensity),t.reflectivity!==void 0&&(this.reflectivity=t.reflectivity),t.refractionRatio!==void 0&&(this.refractionRatio=t.refractionRatio),t.lightMap!==void 0&&(this.lightMap=e[t.lightMap]||null),t.lightMapIntensity!==void 0&&(this.lightMapIntensity=t.lightMapIntensity),t.aoMap!==void 0&&(this.aoMap=e[t.aoMap]||null),t.aoMapIntensity!==void 0&&(this.aoMapIntensity=t.aoMapIntensity),t.gradientMap!==void 0&&(this.gradientMap=e[t.gradientMap]||null),t.clearcoatMap!==void 0&&(this.clearcoatMap=e[t.clearcoatMap]||null),t.clearcoatRoughnessMap!==void 0&&(this.clearcoatRoughnessMap=e[t.clearcoatRoughnessMap]||null),t.clearcoatNormalMap!==void 0&&(this.clearcoatNormalMap=e[t.clearcoatNormalMap]||null),t.clearcoatNormalScale!==void 0&&(this.clearcoatNormalScale=new et().fromArray(t.clearcoatNormalScale)),t.diffuseRoughnessMap!==void 0&&(this.diffuseRoughnessMap=e[t.diffuseRoughnessMap]||null),t.iridescenceMap!==void 0&&(this.iridescenceMap=e[t.iridescenceMap]||null),t.iridescenceThicknessMap!==void 0&&(this.iridescenceThicknessMap=e[t.iridescenceThicknessMap]||null),t.transmissionMap!==void 0&&(this.transmissionMap=e[t.transmissionMap]||null),t.thicknessMap!==void 0&&(this.thicknessMap=e[t.thicknessMap]||null),t.anisotropyMap!==void 0&&(this.anisotropyMap=e[t.anisotropyMap]||null),t.sheenColorMap!==void 0&&(this.sheenColorMap=e[t.sheenColorMap]||null),t.sheenRoughnessMap!==void 0&&(this.sheenRoughnessMap=e[t.sheenRoughnessMap]||null),this}clone(){return new this.constructor().copy(this)}copy(t){this.name=t.name,this.blending=t.blending,this.side=t.side,this.vertexColors=t.vertexColors,this.opacity=t.opacity,this.transparent=t.transparent,this.blendSrc=t.blendSrc,this.blendDst=t.blendDst,this.blendEquation=t.blendEquation,this.blendSrcAlpha=t.blendSrcAlpha,this.blendDstAlpha=t.blendDstAlpha,this.blendEquationAlpha=t.blendEquationAlpha,this.blendColor.copy(t.blendColor),this.blendAlpha=t.blendAlpha,this.depthFunc=t.depthFunc,this.depthTest=t.depthTest,this.depthWrite=t.depthWrite,this.stencilWriteMask=t.stencilWriteMask,this.stencilFunc=t.stencilFunc,this.stencilRef=t.stencilRef,this.stencilFuncMask=t.stencilFuncMask,this.stencilFail=t.stencilFail,this.stencilZFail=t.stencilZFail,this.stencilZPass=t.stencilZPass,this.stencilWrite=t.stencilWrite;let e=t.clippingPlanes,i=null;if(e!==null){let s=e.length;i=new Array(s);for(let r=0;r!==s;++r)i[r]=e[r].clone()}return this.clippingPlanes=i,this.clipIntersection=t.clipIntersection,this.clipShadows=t.clipShadows,this.shadowSide=t.shadowSide,this.colorWrite=t.colorWrite,this.precision=t.precision,this.polygonOffset=t.polygonOffset,this.polygonOffsetFactor=t.polygonOffsetFactor,this.polygonOffsetUnits=t.polygonOffsetUnits,this.dithering=t.dithering,this.alphaTest=t.alphaTest,this.alphaHash=t.alphaHash,this.alphaToCoverage=t.alphaToCoverage,this.premultipliedAlpha=t.premultipliedAlpha,this.forceSinglePass=t.forceSinglePass,this.allowOverride=t.allowOverride,this.visible=t.visible,this.toneMapped=t.toneMapped,this.userData=JSON.parse(JSON.stringify(t.userData)),this}dispose(){this.dispatchEvent({type:"dispose"})}set needsUpdate(t){t===!0&&this.version++}};var rn=new R,zc=new R,Go=new R,Wo=new R,hn=class{constructor(t=new R,e=new R(0,0,-1)){this.origin=t,this.direction=e}set(t,e){return this.origin.copy(t),this.direction.copy(e),this}copy(t){return this.origin.copy(t.origin),this.direction.copy(t.direction),this}at(t,e){return e.copy(this.origin).addScaledVector(this.direction,t)}lookAt(t){return this.direction.copy(t).sub(this.origin).normalize(),this}recast(t){return this.origin.copy(this.at(t,rn)),this}closestPointToPoint(t,e){e.subVectors(t,this.origin);let i=e.dot(this.direction);return i<0?e.copy(this.origin):e.copy(this.origin).addScaledVector(this.direction,i)}distanceToPoint(t){return Math.sqrt(this.distanceSqToPoint(t))}distanceSqToPoint(t){let e=rn.subVectors(t,this.origin).dot(this.direction);return e<0?this.origin.distanceToSquared(t):(rn.copy(this.origin).addScaledVector(this.direction,e),rn.distanceToSquared(t))}distanceSqToSegment(t,e,i,s){zc.copy(t).add(e).multiplyScalar(.5),Go.copy(e).sub(t).normalize(),Wo.copy(this.origin).sub(zc);let r=t.distanceTo(e)*.5,o=-this.direction.dot(Go),a=Wo.dot(this.direction),l=-Wo.dot(Go),c=Wo.lengthSq(),h=Math.abs(1-o*o),u,d,f,p;if(h>0)if(u=o*l-a,d=o*a-l,p=r*h,u>=0)if(d>=-p)if(d<=p){let x=1/h;u*=x,d*=x,f=u*(u+o*d+2*a)+d*(o*u+d+2*l)+c}else d=r,u=Math.max(0,-(o*d+a)),f=-u*u+d*(d+2*l)+c;else d=-r,u=Math.max(0,-(o*d+a)),f=-u*u+d*(d+2*l)+c;else d<=-p?(u=Math.max(0,-(-o*r+a)),d=u>0?-r:Math.min(Math.max(-r,-l),r),f=-u*u+d*(d+2*l)+c):d<=p?(u=0,d=Math.min(Math.max(-r,-l),r),f=d*(d+2*l)+c):(u=Math.max(0,-(o*r+a)),d=u>0?r:Math.min(Math.max(-r,-l),r),f=-u*u+d*(d+2*l)+c);else d=o>0?-r:r,u=Math.max(0,-(o*d+a)),f=-u*u+d*(d+2*l)+c;return i&&i.copy(this.origin).addScaledVector(this.direction,u),s&&s.copy(zc).addScaledVector(Go,d),f}intersectSphere(t,e){if(t.radius<0)return null;rn.subVectors(t.center,this.origin);let i=rn.dot(this.direction),s=rn.dot(rn)-i*i,r=t.radius*t.radius;if(s>r)return null;let o=Math.sqrt(r-s),a=i-o,l=i+o;return l<0?null:a<0?this.at(l,e):this.at(a,e)}intersectsSphere(t){return t.radius<0?!1:this.distanceSqToPoint(t.center)<=t.radius*t.radius}distanceToPlane(t){let e=t.normal.dot(this.direction);if(e===0)return t.distanceToPoint(this.origin)===0?0:null;let i=-(this.origin.dot(t.normal)+t.constant)/e;return i>=0?i:null}intersectPlane(t,e){let i=this.distanceToPlane(t);return i===null?null:this.at(i,e)}intersectsPlane(t){let e=t.distanceToPoint(this.origin);return e===0||t.normal.dot(this.direction)*e<0}intersectBox(t,e){let i,s,r,o,a,l,c=1/this.direction.x,h=1/this.direction.y,u=1/this.direction.z,d=this.origin;return c>=0?(i=(t.min.x-d.x)*c,s=(t.max.x-d.x)*c):(i=(t.max.x-d.x)*c,s=(t.min.x-d.x)*c),h>=0?(r=(t.min.y-d.y)*h,o=(t.max.y-d.y)*h):(r=(t.max.y-d.y)*h,o=(t.min.y-d.y)*h),i>o||r>s||((r>i||isNaN(i))&&(i=r),(o<s||isNaN(s))&&(s=o),u>=0?(a=(t.min.z-d.z)*u,l=(t.max.z-d.z)*u):(a=(t.max.z-d.z)*u,l=(t.min.z-d.z)*u),i>l||a>s)||((a>i||i!==i)&&(i=a),(l<s||s!==s)&&(s=l),s<0)?null:this.at(i>=0?i:s,e)}intersectsBox(t){return this.intersectBox(t,rn)!==null}intersectTriangle(t,e,i,s,r){let o=this.origin,a=this.direction,l=a.x,c=a.y,h=a.z,u=t.x-o.x,d=t.y-o.y,f=t.z-o.z,p=e.x-o.x,x=e.y-o.y,m=e.z-o.z,g=i.x-o.x,_=i.y-o.y,y=i.z-o.z,S=Math.abs(l),T=Math.abs(c),w=Math.abs(h),v,M,D,P,A,C,U,L,B,G,z,tt;if(S>=T&&S>=w?(D=l,C=u,B=p,tt=g,l>=0?(v=c,M=h,P=d,A=f,U=x,L=m,G=_,z=y):(v=h,M=c,P=f,A=d,U=m,L=x,G=y,z=_)):T>=w?(D=c,C=d,B=x,tt=_,c>=0?(v=h,M=l,P=f,A=u,U=m,L=p,G=y,z=g):(v=l,M=h,P=u,A=f,U=p,L=m,G=g,z=y)):(D=h,C=f,B=m,tt=y,h>=0?(v=l,M=c,P=u,A=d,U=p,L=x,G=g,z=_):(v=c,M=l,P=d,A=u,U=x,L=p,G=_,z=g)),D===0)return null;let W=v/D,J=M/D,j=1/D,xt=P-W*C,Mt=A-J*C,q=U-W*B,ot=L-J*B,gt=G-W*tt,wt=z-J*tt,rt=gt*ot-wt*q,Dt=xt*wt-Mt*gt,ie=q*Mt-ot*xt;if(s){if(rt<0||Dt<0||ie<0)return null}else if((rt<0||Dt<0||ie<0)&&(rt>0||Dt>0||ie>0))return null;let bt=rt+Dt+ie;if(bt===0)return null;let Bt=j*(rt*C+Dt*B+ie*tt);return(bt>0?Bt<0:Bt>0)?null:this.at(Bt/bt,r)}applyMatrix4(t){return this.origin.applyMatrix4(t),this.direction.transformDirection(t),this}equals(t){return t.origin.equals(this.origin)&&t.direction.equals(this.direction)}clone(){return new this.constructor().copy(this)}},ci=class extends Ni{constructor(t){super(),this.isMeshBasicMaterial=!0,this.type="MeshBasicMaterial",this.color=new St(16777215),this.map=null,this.lightMap=null,this.lightMapIntensity=1,this.aoMap=null,this.aoMapIntensity=1,this.specularMap=null,this.alphaMap=null,this.envMap=null,this.envMapRotation=new Ti,this.combine=Ya,this.reflectivity=1,this.refractionRatio=.98,this.wireframe=!1,this.wireframeLinewidth=1,this.wireframeLinecap="round",this.wireframeLinejoin="round",this.fog=!0,this.setValues(t)}copy(t){return super.copy(t),this.color.copy(t.color),this.map=t.map,this.lightMap=t.lightMap,this.lightMapIntensity=t.lightMapIntensity,this.aoMap=t.aoMap,this.aoMapIntensity=t.aoMapIntensity,this.specularMap=t.specularMap,this.alphaMap=t.alphaMap,this.envMap=t.envMap,this.envMapRotation.copy(t.envMapRotation),this.combine=t.combine,this.reflectivity=t.reflectivity,this.refractionRatio=t.refractionRatio,this.wireframe=t.wireframe,this.wireframeLinewidth=t.wireframeLinewidth,this.wireframeLinecap=t.wireframeLinecap,this.wireframeLinejoin=t.wireframeLinejoin,this.fog=t.fog,this}},Bu=new Gt,Yn=new hn,Xo=new ri,zu=new R,qo=new R,Yo=new R,Zo=new R,kc=new R,Ko=new R,ku=new R,Jo=new R,Lt=class extends ve{constructor(t=new Yt,e=new ci){super(),this.isMesh=!0,this.type="Mesh",this.geometry=t,this.material=e,this.morphTargetDictionary=void 0,this.morphTargetInfluences=void 0,this.count=1,this.updateMorphTargets()}copy(t,e){return super.copy(t,e),t.morphTargetInfluences!==void 0?this.morphTargetInfluences=t.morphTargetInfluences.slice():this.morphTargetInfluences=void 0,t.morphTargetDictionary!==void 0?this.morphTargetDictionary=Object.assign({},t.morphTargetDictionary):this.morphTargetDictionary=void 0,this.material=Array.isArray(t.material)?t.material.slice():t.material,this.geometry=t.geometry,this}updateMorphTargets(){let e=this.geometry.morphAttributes,i=Object.keys(e);if(i.length>0){let s=e[i[0]];if(s!==void 0){this.morphTargetInfluences=[],this.morphTargetDictionary={};for(let r=0,o=s.length;r<o;r++){let a=s[r].name||String(r);this.morphTargetInfluences.push(0),this.morphTargetDictionary[a]=r}}}}getVertexPosition(t,e){let i=this.geometry,s=i.attributes.position,r=i.morphAttributes.position,o=i.morphTargetsRelative;e.fromBufferAttribute(s,t);let a=this.morphTargetInfluences;if(r&&a){Ko.set(0,0,0);for(let l=0,c=r.length;l<c;l++){let h=a[l],u=r[l];h!==0&&(kc.fromBufferAttribute(u,t),o?Ko.addScaledVector(kc,h):Ko.addScaledVector(kc.sub(e),h))}e.add(Ko)}return e}intersectsFrustum(t){return t.intersectsObject(this)}raycast(t,e){let i=this.geometry,s=this.material,r=this.matrixWorld;s!==void 0&&(i.boundingSphere===null&&i.computeBoundingSphere(),Xo.copy(i.boundingSphere),Xo.applyMatrix4(r),Yn.copy(t.ray).recast(t.near),!(Xo.containsPoint(Yn.origin)===!1&&(Yn.intersectSphere(Xo,zu)===null||Yn.origin.distanceToSquared(zu)>(t.far-t.near)**2))&&(Bu.copy(r).invert(),Yn.copy(t.ray).applyMatrix4(Bu),!(i.boundingBox!==null&&Yn.intersectsBox(i.boundingBox)===!1)&&this._computeIntersections(t,e,Yn)))}_computeIntersections(t,e,i){let s,r=this.geometry,o=this.material,a=r.index,l=r.attributes.position,c=r.attributes.uv,h=r.attributes.uv1,u=r.attributes.normal,d=r.groups,f=r.drawRange;if(a!==null)if(Array.isArray(o))for(let p=0,x=d.length;p<x;p++){let m=d[p],g=o[m.materialIndex],_=Math.max(m.start,f.start),y=Math.min(a.count,Math.min(m.start+m.count,f.start+f.count));for(let S=_,T=y;S<T;S+=3){let w=a.getX(S),v=a.getX(S+1),M=a.getX(S+2);s=$o(this,g,t,i,c,h,u,w,v,M),s&&(s.faceIndex=Math.floor(S/3),s.face.materialIndex=m.materialIndex,e.push(s))}}else{let p=Math.max(0,f.start),x=Math.min(a.count,f.start+f.count);for(let m=p,g=x;m<g;m+=3){let _=a.getX(m),y=a.getX(m+1),S=a.getX(m+2);s=$o(this,o,t,i,c,h,u,_,y,S),s&&(s.faceIndex=Math.floor(m/3),e.push(s))}}else if(l!==void 0)if(Array.isArray(o))for(let p=0,x=d.length;p<x;p++){let m=d[p],g=o[m.materialIndex],_=Math.max(m.start,f.start),y=Math.min(l.count,Math.min(m.start+m.count,f.start+f.count));for(let S=_,T=y;S<T;S+=3){let w=S,v=S+1,M=S+2;s=$o(this,g,t,i,c,h,u,w,v,M),s&&(s.faceIndex=Math.floor(S/3),s.face.materialIndex=m.materialIndex,e.push(s))}}else{let p=Math.max(0,f.start),x=Math.min(l.count,f.start+f.count);for(let m=p,g=x;m<g;m+=3){let _=m,y=m+1,S=m+2;s=$o(this,o,t,i,c,h,u,_,y,S),s&&(s.faceIndex=Math.floor(m/3),e.push(s))}}}};function vm(n,t,e,i,s,r,o,a){let l;if(t.side===qe?l=i.intersectTriangle(o,r,s,!0,a):l=i.intersectTriangle(s,r,o,t.side===Fn,a),l===null)return null;Jo.copy(a),Jo.applyMatrix4(n.matrixWorld);let c=e.ray.origin.distanceTo(Jo);return c<e.near||c>e.far?null:{distance:c,point:Jo.clone(),object:n}}function $o(n,t,e,i,s,r,o,a,l,c){n.getVertexPosition(a,qo),n.getVertexPosition(l,Yo),n.getVertexPosition(c,Zo);let h=vm(n,t,e,i,qo,Yo,Zo,ku);if(h){let u=new R;on.getBarycoord(ku,qo,Yo,Zo,u),s&&(h.uv=on.getInterpolatedAttribute(s,a,l,c,u,new et)),r&&(h.uv1=on.getInterpolatedAttribute(r,a,l,c,u,new et)),o&&(h.normal=on.getInterpolatedAttribute(o,a,l,c,u,new R),h.normal.dot(i.direction)>0&&h.normal.multiplyScalar(-1));let d={a,b:l,c,normal:new R,materialIndex:0};on.getNormal(qo,Yo,Zo,d.normal),h.face=d,h.barycoord=u}return h}var Ui=class extends We{constructor(t=null,e=1,i=1,s,r,o,a,l,c=Ce,h=Ce,u,d){super(null,o,a,l,c,h,s,r,u,d),this.isDataTexture=!0,this.image={data:t,width:e,height:i},this.generateMipmaps=!1,this.flipY=!1,this.unpackAlignment=1}};var $n=class extends re{constructor(t,e,i,s=1){super(t,e,i),this.isInstancedBufferAttribute=!0,this.meshPerAttribute=s}copy(t){return super.copy(t),this.meshPerAttribute=t.meshPerAttribute,this}toJSON(){let t=super.toJSON();return t.meshPerAttribute=this.meshPerAttribute,t.isInstancedBufferAttribute=!0,t}},Rs=new Gt,Hu=new Gt,jo=[],Vu=new Wi,ym=new Gt,Sr=new Lt,br=new ri,jn=class extends Lt{constructor(t,e,i){super(t,e),this.isInstancedMesh=!0,this.instanceMatrix=new $n(new Float32Array(i*16),16),this.instanceColor=null,this.morphTexture=null,this.count=i,this.boundingBox=null,this.boundingSphere=null;for(let s=0;s<i;s++)this.setMatrixAt(s,ym)}computeBoundingBox(){let t=this.geometry,e=this.count;this.boundingBox===null&&(this.boundingBox=new Wi),t.boundingBox===null&&t.computeBoundingBox(),this.boundingBox.makeEmpty();for(let i=0;i<e;i++)this.getMatrixAt(i,Rs),Vu.copy(t.boundingBox).applyMatrix4(Rs),this.boundingBox.union(Vu)}computeBoundingSphere(){let t=this.geometry,e=this.count;this.boundingSphere===null&&(this.boundingSphere=new ri),t.boundingSphere===null&&t.computeBoundingSphere(),this.boundingSphere.makeEmpty();for(let i=0;i<e;i++)this.getMatrixAt(i,Rs),br.copy(t.boundingSphere).applyMatrix4(Rs),this.boundingSphere.union(br)}copy(t,e){return super.copy(t,e),this.instanceMatrix.copy(t.instanceMatrix),this.morphTexture=t.morphTexture!==null?t.morphTexture.clone():null,this.instanceColor=t.instanceColor!==null?t.instanceColor.clone():null,this.count=t.count,this.boundingBox=t.boundingBox!==null?t.boundingBox.clone():null,this.boundingSphere=t.boundingSphere!==null?t.boundingSphere.clone():null,this}getColorAt(t,e){return this.instanceColor===null?e.setRGB(1,1,1):e.fromArray(this.instanceColor.array,t*3)}getMatrixAt(t,e){return e.fromArray(this.instanceMatrix.array,t*16)}getMorphAt(t,e){let i=e.morphTargetInfluences,s=this.morphTexture.source.data.data,r=i.length+1,o=t*r+1;for(let a=0;a<i.length;a++)i[a]=s[o+a]}raycast(t,e){let i=this.matrixWorld,s=this.count;if(Sr.geometry=this.geometry,Sr.material=this.material,Sr.material!==void 0&&(this.boundingSphere===null&&this.computeBoundingSphere(),br.copy(this.boundingSphere),br.applyMatrix4(i),t.ray.intersectsSphere(br)!==!1))for(let r=0;r<s;r++){this.getMatrixAt(r,Rs),Hu.multiplyMatrices(i,Rs),Sr.matrixWorld=Hu,Sr.raycast(t,jo);for(let o=0,a=jo.length;o<a;o++){let l=jo[o];l.instanceId=r,l.object=this,e.push(l)}jo.length=0}}setColorAt(t,e){return this.instanceColor===null&&(this.instanceColor=new $n(new Float32Array(this.instanceMatrix.count*3).fill(1),3)),e.toArray(this.instanceColor.array,t*3),this}setMatrixAt(t,e){return e.toArray(this.instanceMatrix.array,t*16),this}setMorphAt(t,e){let i=e.morphTargetInfluences,s=i.length+1;this.morphTexture===null&&(this.morphTexture=new Ui(new Float32Array(s*this.count),s,this.count,On,ui));let r=this.morphTexture.source.data.data,o=0;for(let c=0;c<i.length;c++)o+=i[c];let a=this.geometry.morphTargetsRelative?1:1-o,l=s*t;return r[l]=a,r.set(i,l+1),this}updateMorphTargets(){}dispose(){super.dispose(),this.instanceMatrix.dispose(),this.instanceColor!==null&&this.instanceColor.dispose(),this.morphTexture!==null&&(this.morphTexture.dispose(),this.morphTexture=null)}},Zn=new ri,Mm=new et(.5,.5),Qo=new R,Pn=class{constructor(t=new si,e=new si,i=new si,s=new si,r=new si,o=new si){this.planes=[t,e,i,s,r,o]}set(t,e,i,s,r,o){let a=this.planes;return a[0].copy(t),a[1].copy(e),a[2].copy(i),a[3].copy(s),a[4].copy(r),a[5].copy(o),this}copy(t){let e=this.planes;for(let i=0;i<6;i++)e[i].copy(t.planes[i]);return this}setFromProjectionMatrix(t,e=Ei,i=!1){let s=this.planes,r=t.elements,o=r[0],a=r[1],l=r[2],c=r[3],h=r[4],u=r[5],d=r[6],f=r[7],p=r[8],x=r[9],m=r[10],g=r[11],_=r[12],y=r[13],S=r[14],T=r[15];if(s[0].setComponents(c-o,f-h,g-p,T-_).normalize(),s[1].setComponents(c+o,f+h,g+p,T+_).normalize(),s[2].setComponents(c+a,f+u,g+x,T+y).normalize(),s[3].setComponents(c-a,f-u,g-x,T-y).normalize(),i)s[4].setComponents(l,d,m,S).normalize(),s[5].setComponents(c-l,f-d,g-m,T-S).normalize();else if(s[4].setComponents(c-l,f-d,g-m,T-S).normalize(),e===Ei)s[5].setComponents(c+l,f+d,g+m,T+S).normalize();else if(e===Us)s[5].setComponents(l,d,m,S).normalize();else throw new Error("THREE.Frustum.setFromProjectionMatrix(): Invalid coordinate system: "+e);return this}intersectsObject(t){if(t.boundingSphere!==void 0)t.boundingSphere===null&&t.computeBoundingSphere(),Zn.copy(t.boundingSphere).applyMatrix4(t.matrixWorld);else{let e=t.geometry;e.boundingSphere===null&&e.computeBoundingSphere(),Zn.copy(e.boundingSphere).applyMatrix4(t.matrixWorld)}return this.intersectsSphere(Zn)}intersectsSprite(t){Zn.center.set(0,0,0);let e=Mm.distanceTo(t.center);return Zn.radius=.7071067811865476+e,Zn.applyMatrix4(t.matrixWorld),this.intersectsSphere(Zn)}intersectsSphere(t){let e=this.planes,i=t.center,s=-t.radius;for(let r=0;r<6;r++)if(e[r].distanceToPoint(i)<s)return!1;return!0}intersectsBox(t){let e=this.planes;for(let i=0;i<6;i++){let s=e[i];if(Qo.x=s.normal.x>0?t.max.x:t.min.x,Qo.y=s.normal.y>0?t.max.y:t.min.y,Qo.z=s.normal.z>0?t.max.z:t.min.z,s.distanceToPoint(Qo)<0)return!1}return!0}containsPoint(t){let e=this.planes;for(let i=0;i<6;i++)if(e[i].distanceToPoint(t)<0)return!1;return!0}clone(){return new this.constructor().copy(this)}},Gu=new Gt,ba=class n{constructor(){this.coordinateSystem=Ei,this._frustums=[],this._count=0}setFromArrayCamera(t){let e=t.cameras,i=this._frustums;for(let s=0;s<e.length;s++){let r=e[s];Gu.multiplyMatrices(r.projectionMatrix,r.matrixWorldInverse),i[s]===void 0&&(i[s]=new Pn),i[s].setFromProjectionMatrix(Gu,r.coordinateSystem,r.reversedDepth)}return this._count=e.length,this}getFrustum(t){return this._frustums[t]}intersectsObject(t){let e=this._frustums;for(let i=0;i<this._count;i++)if(e[i].intersectsObject(t))return!0;return!1}intersectsSprite(t){let e=this._frustums;for(let i=0;i<this._count;i++)if(e[i].intersectsSprite(t))return!0;return!1}intersectsSphere(t){let e=this._frustums;for(let i=0;i<this._count;i++)if(e[i].intersectsSphere(t))return!0;return!1}intersectsBox(t){let e=this._frustums;for(let i=0;i<this._count;i++)if(e[i].intersectsBox(t))return!0;return!1}containsPoint(t){let e=this._frustums;for(let i=0;i<this._count;i++)if(e[i].containsPoint(t))return!0;return!1}copy(t){this.coordinateSystem=t.coordinateSystem;let e=this._frustums,i=t._frustums;for(let s=0;s<t._count;s++)e[s]===void 0&&(e[s]=new Pn),e[s].copy(i[s]);return this._count=t._count,this}clone(){return new n().copy(this)}};var gi=class extends Ni{constructor(t){super(),this.isLineBasicMaterial=!0,this.type="LineBasicMaterial",this.color=new St(16777215),this.map=null,this.linewidth=1,this.linecap="round",this.linejoin="round",this.fog=!0,this.setValues(t)}copy(t){return super.copy(t),this.color.copy(t.color),this.map=t.map,this.linewidth=t.linewidth,this.linecap=t.linecap,this.linejoin=t.linejoin,this.fog=t.fog,this}},wa=new R,Ea=new R,Wu=new Gt,wr=new hn,ta=new ri,Hc=new R,Xu=new R,Qn=class extends ve{constructor(t=new Yt,e=new gi){super(),this.isLine=!0,this.type="Line",this.geometry=t,this.material=e,this.morphTargetDictionary=void 0,this.morphTargetInfluences=void 0,this.updateMorphTargets()}copy(t,e){return super.copy(t,e),t.morphTargetInfluences!==void 0?this.morphTargetInfluences=t.morphTargetInfluences.slice():this.morphTargetInfluences=void 0,t.morphTargetDictionary!==void 0?this.morphTargetDictionary=Object.assign({},t.morphTargetDictionary):this.morphTargetDictionary=void 0,this.material=Array.isArray(t.material)?t.material.slice():t.material,this.geometry=t.geometry,this}computeLineDistances(){let t=this.geometry;if(t.index===null){let e=t.attributes.position,i=[0];for(let s=1,r=e.count;s<r;s++)wa.fromBufferAttribute(e,s-1),Ea.fromBufferAttribute(e,s),i[s]=i[s-1],i[s]+=wa.distanceTo(Ea);t.setAttribute("lineDistance",new jt(i,1))}else It("Line.computeLineDistances(): Computation only possible with non-indexed BufferGeometry.");return this}intersectsFrustum(t){return t.intersectsObject(this)}raycast(t,e){let i=this.geometry,s=this.matrixWorld,r=t.params.Line.threshold,o=i.drawRange;if(i.boundingSphere===null&&i.computeBoundingSphere(),ta.copy(i.boundingSphere),ta.applyMatrix4(s),ta.radius+=r,t.ray.intersectsSphere(ta)===!1)return;Wu.copy(s).invert(),wr.copy(t.ray).applyMatrix4(Wu);let a=r/((this.scale.x+this.scale.y+this.scale.z)/3),l=a*a,c=this.isLineSegments?2:1,h=i.index,d=i.attributes.position;if(h!==null){let f=Math.max(0,o.start),p=Math.min(h.count,o.start+o.count);for(let x=f,m=p-1;x<m;x+=c){let g=h.getX(x),_=h.getX(x+1),y=ea(this,t,wr,l,g,_,x);y&&e.push(y)}if(this.isLineLoop){let x=h.getX(p-1),m=h.getX(f),g=ea(this,t,wr,l,x,m,p-1);g&&e.push(g)}}else{let f=Math.max(0,o.start),p=Math.min(d.count,o.start+o.count);for(let x=f,m=p-1;x<m;x+=c){let g=ea(this,t,wr,l,x,x+1,x);g&&e.push(g)}if(this.isLineLoop){let x=ea(this,t,wr,l,p-1,f,p-1);x&&e.push(x)}}}updateMorphTargets(){let e=this.geometry.morphAttributes,i=Object.keys(e);if(i.length>0){let s=e[i[0]];if(s!==void 0){this.morphTargetInfluences=[],this.morphTargetDictionary={};for(let r=0,o=s.length;r<o;r++){let a=s[r].name||String(r);this.morphTargetInfluences.push(0),this.morphTargetDictionary[a]=r}}}}};function ea(n,t,e,i,s,r,o){let a=n.geometry.attributes.position;if(wa.fromBufferAttribute(a,s),Ea.fromBufferAttribute(a,r),e.distanceSqToSegment(wa,Ea,Hc,Xu)>i)return;Hc.applyMatrix4(n.matrixWorld);let c=t.ray.origin.distanceTo(Hc);if(!(c<t.near||c>t.far))return{distance:c,point:Xu.clone().applyMatrix4(n.matrixWorld),index:o,face:null,faceIndex:null,barycoord:null,object:n}}var qu=new R,Yu=new R,un=class extends Qn{constructor(t,e){super(t,e),this.isLineSegments=!0,this.type="LineSegments"}computeLineDistances(){let t=this.geometry;if(t.index===null){let e=t.attributes.position,i=[];for(let s=0,r=e.count;s<r;s+=2)qu.fromBufferAttribute(e,s),Yu.fromBufferAttribute(e,s+1),i[s]=s===0?0:i[s-1],i[s+1]=i[s]+qu.distanceTo(Yu);t.setAttribute("lineDistance",new jt(i,1))}else It("LineSegments.computeLineDistances(): Computation only possible with non-indexed BufferGeometry.");return this}},Nr=class extends Qn{constructor(t,e){super(t,e),this.isLineLoop=!0,this.type="LineLoop"}},Vs=class extends Ni{constructor(t){super(),this.isPointsMaterial=!0,this.type="PointsMaterial",this.color=new St(16777215),this.map=null,this.alphaMap=null,this.size=1,this.sizeAttenuation=!0,this.fog=!0,this.setValues(t)}copy(t){return super.copy(t),this.color.copy(t.color),this.map=t.map,this.alphaMap=t.alphaMap,this.size=t.size,this.sizeAttenuation=t.sizeAttenuation,this.fog=t.fog,this}},Zu=new Gt,$c=new hn,ia=new ri,na=new R,ts=class extends ve{constructor(t=new Yt,e=new Vs){super(),this.isPoints=!0,this.type="Points",this.geometry=t,this.material=e,this.morphTargetDictionary=void 0,this.morphTargetInfluences=void 0,this.updateMorphTargets()}copy(t,e){return super.copy(t,e),t.morphTargetInfluences!==void 0?this.morphTargetInfluences=t.morphTargetInfluences.slice():this.morphTargetInfluences=void 0,t.morphTargetDictionary!==void 0?this.morphTargetDictionary=Object.assign({},t.morphTargetDictionary):this.morphTargetDictionary=void 0,this.material=Array.isArray(t.material)?t.material.slice():t.material,this.geometry=t.geometry,this}intersectsFrustum(t){return t.intersectsObject(this)}raycast(t,e){let i=this.geometry,s=this.matrixWorld,r=t.params.Points.threshold,o=i.drawRange;if(i.boundingSphere===null&&i.computeBoundingSphere(),ia.copy(i.boundingSphere),ia.applyMatrix4(s),ia.radius+=r,t.ray.intersectsSphere(ia)===!1)return;Zu.copy(s).invert(),$c.copy(t.ray).applyMatrix4(Zu);let a=r/((this.scale.x+this.scale.y+this.scale.z)/3),l=a*a,c=i.index,u=i.attributes.position;if(c!==null){let d=Math.max(0,o.start),f=Math.min(c.count,o.start+o.count);for(let p=d,x=f;p<x;p++){let m=c.getX(p);na.fromBufferAttribute(u,m),Ku(na,m,l,s,t,e,this)}}else{let d=Math.max(0,o.start),f=Math.min(u.count,o.start+o.count);for(let p=d,x=f;p<x;p++)na.fromBufferAttribute(u,p),Ku(na,p,l,s,t,e,this)}}updateMorphTargets(){let e=this.geometry.morphAttributes,i=Object.keys(e);if(i.length>0){let s=e[i[0]];if(s!==void 0){this.morphTargetInfluences=[],this.morphTargetDictionary={};for(let r=0,o=s.length;r<o;r++){let a=s[r].name||String(r);this.morphTargetInfluences.push(0),this.morphTargetDictionary[a]=r}}}}};function Ku(n,t,e,i,s,r,o){let a=$c.distanceSqToPoint(n);if(a<e){let l=new R;$c.closestPointToPoint(n,l),l.applyMatrix4(i);let c=s.ray.origin.distanceTo(l);if(c<s.near||c>s.far)return;r.push({distance:c,distanceToRay:Math.sqrt(a),point:l,index:t,face:null,faceIndex:null,barycoord:null,object:o})}}var Ur=class extends We{constructor(t=[],e=ls,i,s,r,o,a,l,c,h){super(t,e,i,s,r,o,a,l,c,h),this.isCubeTexture=!0,this.flipY=!1}get images(){return this.image}set images(t){this.image=t}},Gs=class extends We{constructor(t,e,i,s,r,o,a,l,c){super(t,e,i,s,r,o,a,l,c),this.isCanvasTexture=!0,this.needsUpdate=!0}};var Fi=class extends We{constructor(t,e,i=oi,s,r,o,a=Ce,l=Ce,c,h=Di,u=1){if(h!==Di&&h!==mn)throw new Error("THREE.DepthTexture: format must be either THREE.DepthFormat or THREE.DepthStencilFormat");let d={width:t,height:e,depth:u};super(d,s,r,o,a,l,h,i,c),this.isDepthTexture=!0,this.flipY=!1,this.generateMipmaps=!1,this.compareFunction=null}copy(t){return super.copy(t),this.source=new Os(Object.assign({},t.image)),this.compareFunction=t.compareFunction,this}toJSON(t){let e=super.toJSON(t);return e.compareFunction=this.compareFunction,e}},Fr=class extends Fi{constructor(t,e=oi,i=ls,s,r,o=Ce,a=Ce,l,c=Di){let h={width:t,height:t,depth:1},u=[h,h,h,h,h,h];super(t,t,e,i,s,r,o,a,l,c),this.image=u,this.isCubeDepthTexture=!0,this.isCubeTexture=!0}get images(){return this.image}set images(t){this.image=t}},Or=class extends We{constructor(t=null){super(),this.sourceTexture=t,this.isExternalTexture=!0}copy(t){return super.copy(t),this.sourceTexture=t.sourceTexture,this}},be=class n extends Yt{constructor(t=1,e=1,i=1,s=1,r=1,o=1){super(),this.type="BoxGeometry",this.parameters={width:t,height:e,depth:i,widthSegments:s,heightSegments:r,depthSegments:o};let a=this;s=Math.floor(s),r=Math.floor(r),o=Math.floor(o);let l=[],c=[],h=[],u=[],d=0,f=0;p("z","y","x",-1,-1,i,e,t,o,r,0),p("z","y","x",1,-1,i,e,-t,o,r,1),p("x","z","y",1,1,t,i,e,s,o,2),p("x","z","y",1,-1,t,i,-e,s,o,3),p("x","y","z",1,-1,t,e,i,s,r,4),p("x","y","z",-1,-1,t,e,-i,s,r,5),this.setIndex(l),this.setAttribute("position",new jt(c,3)),this.setAttribute("normal",new jt(h,3)),this.setAttribute("uv",new jt(u,2));function p(x,m,g,_,y,S,T,w,v,M,D){let P=S/v,A=T/M,C=S/2,U=T/2,L=w/2,B=v+1,G=M+1,z=0,tt=0,W=new R;for(let J=0;J<G;J++){let j=J*A-U;for(let xt=0;xt<B;xt++){let Mt=xt*P-C;W[x]=Mt*_,W[m]=j*y,W[g]=L,c.push(W.x,W.y,W.z),W[x]=0,W[m]=0,W[g]=w>0?1:-1,h.push(W.x,W.y,W.z),u.push(xt/v),u.push(1-J/M),z+=1}}for(let J=0;J<M;J++)for(let j=0;j<v;j++){let xt=d+j+B*J,Mt=d+j+B*(J+1),q=d+(j+1)+B*(J+1),ot=d+(j+1)+B*J;l.push(xt,Mt,ot),l.push(Mt,q,ot),tt+=6}a.addGroup(f,tt,D),f+=tt,d+=z}}copy(t){return super.copy(t),this.parameters=Object.assign({},t.parameters),this}static fromJSON(t){return new n(t.width,t.height,t.depth,t.widthSegments,t.heightSegments,t.depthSegments)}};var Br=class n extends Yt{constructor(t=1,e=32,i=0,s=Math.PI*2){super(),this.type="CircleGeometry",this.parameters={radius:t,segments:e,thetaStart:i,thetaLength:s},e=Math.max(3,e);let r=[],o=[],a=[],l=[],c=new R,h=new et;o.push(0,0,0),a.push(0,0,1),l.push(.5,.5);for(let u=0,d=3;u<=e;u++,d+=3){let f=i+u/e*s;c.x=t*Math.cos(f),c.y=t*Math.sin(f),o.push(c.x,c.y,c.z),a.push(0,0,1),h.x=(o[d]/t+1)/2,h.y=(o[d+1]/t+1)/2,l.push(h.x,h.y)}for(let u=1;u<=e;u++)r.push(u,u+1,0);this.setIndex(r),this.setAttribute("position",new jt(o,3)),this.setAttribute("normal",new jt(a,3)),this.setAttribute("uv",new jt(l,2))}copy(t){return super.copy(t),this.parameters=Object.assign({},t.parameters),this}static fromJSON(t){return new n(t.radius,t.segments,t.thetaStart,t.thetaLength)}},hi=class n extends Yt{constructor(t=1,e=1,i=1,s=32,r=1,o=!1,a=0,l=Math.PI*2){super(),this.type="CylinderGeometry",this.parameters={radiusTop:t,radiusBottom:e,height:i,radialSegments:s,heightSegments:r,openEnded:o,thetaStart:a,thetaLength:l};let c=this;s=Math.floor(s),r=Math.floor(r);let h=[],u=[],d=[],f=[],p=0,x=[],m=i/2,g=0;_(),o===!1&&(t>0&&y(!0),e>0&&y(!1)),this.setIndex(h),this.setAttribute("position",new jt(u,3)),this.setAttribute("normal",new jt(d,3)),this.setAttribute("uv",new jt(f,2));function _(){let S=new R,T=new R,w=0,v=(e-t)/i;for(let M=0;M<=r;M++){let D=[],P=M/r,A=P*(e-t)+t;for(let C=0;C<=s;C++){let U=C/s,L=U*l+a,B=Math.sin(L),G=Math.cos(L);T.x=A*B,T.y=-P*i+m,T.z=A*G,u.push(T.x,T.y,T.z),S.set(B,v,G).normalize(),d.push(S.x,S.y,S.z),f.push(U,1-P),D.push(p++)}x.push(D)}for(let M=0;M<s;M++)for(let D=0;D<r;D++){let P=x[D][M],A=x[D+1][M],C=x[D+1][M+1],U=x[D][M+1];(t>0||D!==0)&&(h.push(P,A,U),w+=3),(e>0||D!==r-1)&&(h.push(A,C,U),w+=3)}c.addGroup(g,w,0),g+=w}function y(S){let T=p,w=new et,v=new R,M=0,D=S===!0?t:e,P=S===!0?1:-1;for(let C=1;C<=s;C++)u.push(0,m*P,0),d.push(0,P,0),f.push(.5,.5),p++;let A=p;for(let C=0;C<=s;C++){let L=C/s*l+a,B=Math.cos(L),G=Math.sin(L);v.x=D*G,v.y=m*P,v.z=D*B,u.push(v.x,v.y,v.z),d.push(0,P,0),w.x=B*.5+.5,w.y=G*.5*P+.5,f.push(w.x,w.y),p++}for(let C=0;C<s;C++){let U=T+C,L=A+C;S===!0?h.push(L,L+1,U):h.push(L+1,L,U),M+=3}c.addGroup(g,M,S===!0?1:2),g+=M}}copy(t){return super.copy(t),this.parameters=Object.assign({},t.parameters),this}static fromJSON(t){return new n(t.radiusTop,t.radiusBottom,t.height,t.radialSegments,t.heightSegments,t.openEnded,t.thetaStart,t.thetaLength)}};var sa=new R,ra=new R,Vc=new R,oa=new on,es=class extends Yt{constructor(t=null,e=1){if(super(),this.type="EdgesGeometry",this.parameters={geometry:t,thresholdAngle:e},t!==null){let s=Math.pow(10,4),r=Math.cos(Ds*e),o=t.getIndex(),a=t.getAttribute("position"),l=o?o.count:a.count,c=[0,0,0],h=["a","b","c"],u=new Array(3),d=new Map,f=[];for(let p=0;p<l;p+=3){o?(c[0]=o.getX(p),c[1]=o.getX(p+1),c[2]=o.getX(p+2)):(c[0]=p,c[1]=p+1,c[2]=p+2);let{a:x,b:m,c:g}=oa;if(x.fromBufferAttribute(a,c[0]),m.fromBufferAttribute(a,c[1]),g.fromBufferAttribute(a,c[2]),oa.getNormal(Vc),u[0]=`${Math.round(x.x*s)},${Math.round(x.y*s)},${Math.round(x.z*s)}`,u[1]=`${Math.round(m.x*s)},${Math.round(m.y*s)},${Math.round(m.z*s)}`,u[2]=`${Math.round(g.x*s)},${Math.round(g.y*s)},${Math.round(g.z*s)}`,!(u[0]===u[1]||u[1]===u[2]||u[2]===u[0]))for(let _=0;_<3;_++){let y=(_+1)%3,S=u[_],T=u[y],w=oa[h[_]],v=oa[h[y]],M=`${S}_${T}`,D=`${T}_${S}`,P=d.get(D);P?(Vc.dot(P.normal)<=r&&(f.push(w.x,w.y,w.z),f.push(v.x,v.y,v.z)),d.set(D,null)):d.has(M)||d.set(M,{index0:c[_],index1:c[y],normal:Vc.clone()})}}for(let p of d.values())if(p){let{index0:x,index1:m}=p;sa.fromBufferAttribute(a,x),ra.fromBufferAttribute(a,m),f.push(sa.x,sa.y,sa.z),f.push(ra.x,ra.y,ra.z)}this.setAttribute("position",new jt(f,3))}}copy(t){return super.copy(t),this.parameters=Object.assign({},t.parameters),this}},xi=class{constructor(){this.type="Curve",this.arcLengthDivisions=200,this.needsUpdate=!1,this.cacheArcLengths=null}getPoint(){It("Curve: .getPoint() not implemented.")}getPointAt(t,e){let i=this.getUtoTmapping(t);return this.getPoint(i,e)}getPoints(t=5){let e=[];for(let i=0;i<=t;i++)e.push(this.getPoint(i/t));return e}getSpacedPoints(t=5){let e=[];for(let i=0;i<=t;i++)e.push(this.getPointAt(i/t));return e}getLength(){let t=this.getLengths();return t[t.length-1]}getLengths(t=this.arcLengthDivisions){if(this.cacheArcLengths&&this.cacheArcLengths.length===t+1&&!this.needsUpdate)return this.cacheArcLengths;this.needsUpdate=!1;let e=[],i,s=this.getPoint(0),r=0;e.push(0);for(let o=1;o<=t;o++)i=this.getPoint(o/t),r+=i.distanceTo(s),e.push(r),s=i;return this.cacheArcLengths=e,e}updateArcLengths(){this.needsUpdate=!0,this.getLengths()}getUtoTmapping(t,e=null){let i=this.getLengths(),s=0,r=i.length,o;e?o=e:o=t*i[r-1];let a=0,l=r-1,c;for(;a<=l;)if(s=Math.floor(a+(l-a)/2),c=i[s]-o,c<0)a=s+1;else if(c>0)l=s-1;else{l=s;break}if(s=l,i[s]===o)return s/(r-1);let h=i[s],d=i[s+1]-h,f=(o-h)/d;return(s+f)/(r-1)}getTangent(t,e){let s=t-1e-4,r=t+1e-4;s<0&&(s=0),r>1&&(r=1);let o=this.getPoint(s),a=this.getPoint(r),l=e||(o.isVector2?new et:new R);return l.copy(a).sub(o).normalize(),l}getTangentAt(t,e){let i=this.getUtoTmapping(t);return this.getTangent(i,e)}computeFrenetFrames(t,e=!1){let i=new R,s=[],r=[],o=[],a=new R,l=new Gt;for(let f=0;f<=t;f++){let p=f/t;s[f]=this.getTangentAt(p,new R)}r[0]=new R,o[0]=new R;let c=Number.MAX_VALUE,h=Math.abs(s[0].x),u=Math.abs(s[0].y),d=Math.abs(s[0].z);h<=c&&(c=h,i.set(1,0,0)),u<=c&&(c=u,i.set(0,1,0)),d<=c&&i.set(0,0,1),a.crossVectors(s[0],i).normalize(),r[0].crossVectors(s[0],a),o[0].crossVectors(s[0],r[0]);for(let f=1;f<=t;f++){if(r[f]=r[f-1].clone(),o[f]=o[f-1].clone(),a.crossVectors(s[f-1],s[f]),a.length()>Number.EPSILON){a.normalize();let p=Math.acos(kt(s[f-1].dot(s[f]),-1,1));r[f].applyMatrix4(l.makeRotationAxis(a,p))}o[f].crossVectors(s[f],r[f])}if(e===!0){let f=Math.acos(kt(r[0].dot(r[t]),-1,1));f/=t,s[0].dot(a.crossVectors(r[0],r[t]))>0&&(f=-f);for(let p=1;p<=t;p++)r[p].applyMatrix4(l.makeRotationAxis(s[p],f*p)),o[p].crossVectors(s[p],r[p])}return{tangents:s,normals:r,binormals:o}}clone(){return new this.constructor().copy(this)}copy(t){return this.arcLengthDivisions=t.arcLengthDivisions,this.cacheArcLengths=null,this}toJSON(){let t={metadata:{version:4.7,type:"Curve",generator:"Curve.toJSON"}};return t.arcLengthDivisions=this.arcLengthDivisions,t.type=this.type,t}fromJSON(t){return this.arcLengthDivisions=t.arcLengthDivisions,this.cacheArcLengths=null,this}},zr=class extends xi{constructor(t=0,e=0,i=1,s=1,r=0,o=Math.PI*2,a=!1,l=0){super(),this.isEllipseCurve=!0,this.type="EllipseCurve",this.aX=t,this.aY=e,this.xRadius=i,this.yRadius=s,this.aStartAngle=r,this.aEndAngle=o,this.aClockwise=a,this.aRotation=l}getPoint(t,e=new et){let i=e,s=Math.PI*2,r=this.aEndAngle-this.aStartAngle,o=Math.abs(r)<Number.EPSILON;for(;r<0;)r+=s;for(;r>s;)r-=s;r<Number.EPSILON&&(o?r=0:r=s),this.aClockwise===!0&&!o&&(r===s?r=-s:r=r-s);let a=this.aStartAngle+t*r,l=this.aX+this.xRadius*Math.cos(a),c=this.aY+this.yRadius*Math.sin(a);if(this.aRotation!==0){let h=Math.cos(this.aRotation),u=Math.sin(this.aRotation),d=l-this.aX,f=c-this.aY;l=d*h-f*u+this.aX,c=d*u+f*h+this.aY}return i.set(l,c)}copy(t){return super.copy(t),this.aX=t.aX,this.aY=t.aY,this.xRadius=t.xRadius,this.yRadius=t.yRadius,this.aStartAngle=t.aStartAngle,this.aEndAngle=t.aEndAngle,this.aClockwise=t.aClockwise,this.aRotation=t.aRotation,this}toJSON(){let t=super.toJSON();return t.aX=this.aX,t.aY=this.aY,t.xRadius=this.xRadius,t.yRadius=this.yRadius,t.aStartAngle=this.aStartAngle,t.aEndAngle=this.aEndAngle,t.aClockwise=this.aClockwise,t.aRotation=this.aRotation,t}fromJSON(t){return super.fromJSON(t),this.aX=t.aX,this.aY=t.aY,this.xRadius=t.xRadius,this.yRadius=t.yRadius,this.aStartAngle=t.aStartAngle,this.aEndAngle=t.aEndAngle,this.aClockwise=t.aClockwise,this.aRotation=t.aRotation,this}},Ta=class extends zr{constructor(t,e,i,s,r,o){super(t,e,i,i,s,r,o),this.isArcCurve=!0,this.type="ArcCurve"}};function bh(){let n=0,t=0,e=0,i=0;function s(r,o,a,l){n=r,t=a,e=-3*r+3*o-2*a-l,i=2*r-2*o+a+l}return{initCatmullRom:function(r,o,a,l,c){s(o,a,c*(a-r),c*(l-o))},initNonuniformCatmullRom:function(r,o,a,l,c,h,u){let d=(o-r)/c-(a-r)/(c+h)+(a-o)/h,f=(a-o)/h-(l-o)/(h+u)+(l-a)/u;d*=h,f*=h,s(o,a,d,f)},calc:function(r){let o=r*r,a=o*r;return n+t*r+e*o+i*a}}}var Ju=new R,$u=new R,Gc=new bh,Wc=new bh,Xc=new bh,Ws=class extends xi{constructor(t=[],e=!1,i="centripetal",s=.5){super(),this.isCatmullRomCurve3=!0,this.type="CatmullRomCurve3",this.points=t,this.closed=e,this.curveType=i,this.tension=s}getPoint(t,e=new R){let i=e,s=this.points,r=s.length,o=(r-(this.closed?0:1))*t,a=Math.floor(o),l=o-a;this.closed?a+=a>0?0:(Math.floor(Math.abs(a)/r)+1)*r:l===0&&a===r-1&&(a=r-2,l=1);let c,h;this.closed||a>0?c=s[(a-1)%r]:($u.subVectors(s[0],s[1]).add(s[0]),c=$u);let u=s[a%r],d=s[(a+1)%r];if(this.closed||a+2<r?h=s[(a+2)%r]:(Ju.subVectors(s[r-1],s[r-2]).add(s[r-1]),h=Ju),this.curveType==="centripetal"||this.curveType==="chordal"){let f=this.curveType==="chordal"?.5:.25,p=Math.pow(c.distanceToSquared(u),f),x=Math.pow(u.distanceToSquared(d),f),m=Math.pow(d.distanceToSquared(h),f);x<1e-4&&(x=1),p<1e-4&&(p=x),m<1e-4&&(m=x),Gc.initNonuniformCatmullRom(c.x,u.x,d.x,h.x,p,x,m),Wc.initNonuniformCatmullRom(c.y,u.y,d.y,h.y,p,x,m),Xc.initNonuniformCatmullRom(c.z,u.z,d.z,h.z,p,x,m)}else this.curveType==="catmullrom"&&(Gc.initCatmullRom(c.x,u.x,d.x,h.x,this.tension),Wc.initCatmullRom(c.y,u.y,d.y,h.y,this.tension),Xc.initCatmullRom(c.z,u.z,d.z,h.z,this.tension));return i.set(Gc.calc(l),Wc.calc(l),Xc.calc(l)),i}copy(t){super.copy(t),this.points=[];for(let e=0,i=t.points.length;e<i;e++){let s=t.points[e];this.points.push(s.clone())}return this.closed=t.closed,this.curveType=t.curveType,this.tension=t.tension,this}toJSON(){let t=super.toJSON();t.points=[];for(let e=0,i=this.points.length;e<i;e++){let s=this.points[e];t.points.push(s.toArray())}return t.closed=this.closed,t.curveType=this.curveType,t.tension=this.tension,t}fromJSON(t){super.fromJSON(t),this.points=[];for(let e=0,i=t.points.length;e<i;e++){let s=t.points[e];this.points.push(new R().fromArray(s))}return this.closed=t.closed,this.curveType=t.curveType,this.tension=t.tension,this}};function ju(n,t,e,i,s){let r=(i-t)*.5,o=(s-e)*.5,a=n*n,l=n*a;return(2*e-2*i+r+o)*l+(-3*e+3*i-2*r-o)*a+r*n+e}function Sm(n,t){let e=1-n;return e*e*t}function bm(n,t){return 2*(1-n)*n*t}function wm(n,t){return n*n*t}function Tr(n,t,e,i){return Sm(n,t)+bm(n,e)+wm(n,i)}function Em(n,t){let e=1-n;return e*e*e*t}function Tm(n,t){let e=1-n;return 3*e*e*n*t}function Am(n,t){return 3*(1-n)*n*n*t}function Rm(n,t){return n*n*n*t}function Ar(n,t,e,i,s){return Em(n,t)+Tm(n,e)+Am(n,i)+Rm(n,s)}var Aa=class extends xi{constructor(t=new et,e=new et,i=new et,s=new et){super(),this.isCubicBezierCurve=!0,this.type="CubicBezierCurve",this.v0=t,this.v1=e,this.v2=i,this.v3=s}getPoint(t,e=new et){let i=e,s=this.v0,r=this.v1,o=this.v2,a=this.v3;return i.set(Ar(t,s.x,r.x,o.x,a.x),Ar(t,s.y,r.y,o.y,a.y)),i}copy(t){return super.copy(t),this.v0.copy(t.v0),this.v1.copy(t.v1),this.v2.copy(t.v2),this.v3.copy(t.v3),this}toJSON(){let t=super.toJSON();return t.v0=this.v0.toArray(),t.v1=this.v1.toArray(),t.v2=this.v2.toArray(),t.v3=this.v3.toArray(),t}fromJSON(t){return super.fromJSON(t),this.v0.fromArray(t.v0),this.v1.fromArray(t.v1),this.v2.fromArray(t.v2),this.v3.fromArray(t.v3),this}},Ra=class extends xi{constructor(t=new R,e=new R,i=new R,s=new R){super(),this.isCubicBezierCurve3=!0,this.type="CubicBezierCurve3",this.v0=t,this.v1=e,this.v2=i,this.v3=s}getPoint(t,e=new R){let i=e,s=this.v0,r=this.v1,o=this.v2,a=this.v3;return i.set(Ar(t,s.x,r.x,o.x,a.x),Ar(t,s.y,r.y,o.y,a.y),Ar(t,s.z,r.z,o.z,a.z)),i}copy(t){return super.copy(t),this.v0.copy(t.v0),this.v1.copy(t.v1),this.v2.copy(t.v2),this.v3.copy(t.v3),this}toJSON(){let t=super.toJSON();return t.v0=this.v0.toArray(),t.v1=this.v1.toArray(),t.v2=this.v2.toArray(),t.v3=this.v3.toArray(),t}fromJSON(t){return super.fromJSON(t),this.v0.fromArray(t.v0),this.v1.fromArray(t.v1),this.v2.fromArray(t.v2),this.v3.fromArray(t.v3),this}},Ca=class extends xi{constructor(t=new et,e=new et){super(),this.isLineCurve=!0,this.type="LineCurve",this.v1=t,this.v2=e}getPoint(t,e=new et){let i=e;return t===1?i.copy(this.v2):(i.copy(this.v2).sub(this.v1),i.multiplyScalar(t).add(this.v1)),i}getPointAt(t,e){return this.getPoint(t,e)}getTangent(t,e=new et){return e.subVectors(this.v2,this.v1).normalize()}getTangentAt(t,e){return this.getTangent(t,e)}copy(t){return super.copy(t),this.v1.copy(t.v1),this.v2.copy(t.v2),this}toJSON(){let t=super.toJSON();return t.v1=this.v1.toArray(),t.v2=this.v2.toArray(),t}fromJSON(t){return super.fromJSON(t),this.v1.fromArray(t.v1),this.v2.fromArray(t.v2),this}},Xs=class extends xi{constructor(t=new R,e=new R){super(),this.isLineCurve3=!0,this.type="LineCurve3",this.v1=t,this.v2=e}getPoint(t,e=new R){let i=e;return t===1?i.copy(this.v2):(i.copy(this.v2).sub(this.v1),i.multiplyScalar(t).add(this.v1)),i}getPointAt(t,e){return this.getPoint(t,e)}getTangent(t,e=new R){return e.subVectors(this.v2,this.v1).normalize()}getTangentAt(t,e){return this.getTangent(t,e)}copy(t){return super.copy(t),this.v1.copy(t.v1),this.v2.copy(t.v2),this}toJSON(){let t=super.toJSON();return t.v1=this.v1.toArray(),t.v2=this.v2.toArray(),t}fromJSON(t){return super.fromJSON(t),this.v1.fromArray(t.v1),this.v2.fromArray(t.v2),this}},Pa=class extends xi{constructor(t=new et,e=new et,i=new et){super(),this.isQuadraticBezierCurve=!0,this.type="QuadraticBezierCurve",this.v0=t,this.v1=e,this.v2=i}getPoint(t,e=new et){let i=e,s=this.v0,r=this.v1,o=this.v2;return i.set(Tr(t,s.x,r.x,o.x),Tr(t,s.y,r.y,o.y)),i}copy(t){return super.copy(t),this.v0.copy(t.v0),this.v1.copy(t.v1),this.v2.copy(t.v2),this}toJSON(){let t=super.toJSON();return t.v0=this.v0.toArray(),t.v1=this.v1.toArray(),t.v2=this.v2.toArray(),t}fromJSON(t){return super.fromJSON(t),this.v0.fromArray(t.v0),this.v1.fromArray(t.v1),this.v2.fromArray(t.v2),this}},kr=class extends xi{constructor(t=new R,e=new R,i=new R){super(),this.isQuadraticBezierCurve3=!0,this.type="QuadraticBezierCurve3",this.v0=t,this.v1=e,this.v2=i}getPoint(t,e=new R){let i=e,s=this.v0,r=this.v1,o=this.v2;return i.set(Tr(t,s.x,r.x,o.x),Tr(t,s.y,r.y,o.y),Tr(t,s.z,r.z,o.z)),i}copy(t){return super.copy(t),this.v0.copy(t.v0),this.v1.copy(t.v1),this.v2.copy(t.v2),this}toJSON(){let t=super.toJSON();return t.v0=this.v0.toArray(),t.v1=this.v1.toArray(),t.v2=this.v2.toArray(),t}fromJSON(t){return super.fromJSON(t),this.v0.fromArray(t.v0),this.v1.fromArray(t.v1),this.v2.fromArray(t.v2),this}},Ia=class extends xi{constructor(t=[]){super(),this.isSplineCurve=!0,this.type="SplineCurve",this.points=t}getPoint(t,e=new et){let i=e,s=this.points,r=(s.length-1)*t,o=Math.floor(r),a=r-o,l=s[o===0?o:o-1],c=s[o],h=s[o>s.length-2?s.length-1:o+1],u=s[o>s.length-3?s.length-1:o+2];return i.set(ju(a,l.x,c.x,h.x,u.x),ju(a,l.y,c.y,h.y,u.y)),i}copy(t){super.copy(t),this.points=[];for(let e=0,i=t.points.length;e<i;e++){let s=t.points[e];this.points.push(s.clone())}return this}toJSON(){let t=super.toJSON();t.points=[];for(let e=0,i=this.points.length;e<i;e++){let s=this.points[e];t.points.push(s.toArray())}return t}fromJSON(t){super.fromJSON(t),this.points=[];for(let e=0,i=t.points.length;e<i;e++){let s=t.points[e];this.points.push(new et().fromArray(s))}return this}},jc=Object.freeze({__proto__:null,ArcCurve:Ta,CatmullRomCurve3:Ws,CubicBezierCurve:Aa,CubicBezierCurve3:Ra,EllipseCurve:zr,LineCurve:Ca,LineCurve3:Xs,QuadraticBezierCurve:Pa,QuadraticBezierCurve3:kr,SplineCurve:Ia}),Hr=class extends xi{constructor(){super(),this.type="CurvePath",this.curves=[],this.autoClose=!1,this.cacheLengths=null}add(t){this.curves.push(t)}closePath(){let t=this.curves[0].getPoint(0),e=this.curves[this.curves.length-1].getPoint(1);if(!t.equals(e)){let i=t.isVector2===!0?"LineCurve":"LineCurve3";this.curves.push(new jc[i](e,t))}return this}getPoint(t,e){let i=t*this.getLength(),s=this.getCurveLengths(),r=0;for(;r<s.length;){if(s[r]>=i){let o=s[r]-i,a=this.curves[r],l=a.getLength(),c=l===0?0:1-o/l;return a.getPointAt(c,e)}r++}return null}getLength(){let t=this.getCurveLengths();return t[t.length-1]}updateArcLengths(){this.needsUpdate=!0,this.cacheLengths=null,this.getCurveLengths()}getCurveLengths(){if(this.cacheLengths&&this.cacheLengths.length===this.curves.length)return this.cacheLengths;let t=[],e=0;for(let i=0,s=this.curves.length;i<s;i++)e+=this.curves[i].getLength(),t.push(e);return this.cacheLengths=t,t}getSpacedPoints(t=40){let e=[];for(let i=0;i<=t;i++)e.push(this.getPoint(i/t));return this.autoClose&&e.push(e[0]),e}getPoints(t=12){let e=[],i;for(let s=0,r=this.curves;s<r.length;s++){let o=r[s],a=o.isEllipseCurve?t*2:o.isLineCurve||o.isLineCurve3?1:o.isSplineCurve?t*o.points.length:t,l=o.getPoints(a);for(let c=0;c<l.length;c++){let h=l[c];i&&i.equals(h)||(e.push(h),i=h)}}return this.autoClose&&e.length>1&&!e[e.length-1].equals(e[0])&&e.push(e[0]),e}copy(t){super.copy(t),this.curves=[];for(let e=0,i=t.curves.length;e<i;e++){let s=t.curves[e];this.curves.push(s.clone())}return this.autoClose=t.autoClose,this.cacheLengths=null,this}toJSON(){let t=super.toJSON();t.autoClose=this.autoClose,t.curves=[];for(let e=0,i=this.curves.length;e<i;e++){let s=this.curves[e];t.curves.push(s.toJSON())}return t}fromJSON(t){super.fromJSON(t),this.autoClose=t.autoClose,this.curves=[];for(let e=0,i=t.curves.length;e<i;e++){let s=t.curves[e];this.curves.push(new jc[s.type]().fromJSON(s))}return this.cacheLengths=null,this}};var _i=class n extends Yt{constructor(t=1,e=1,i=1,s=1){super(),this.type="PlaneGeometry",this.parameters={width:t,height:e,widthSegments:i,heightSegments:s};let r=t/2,o=e/2,a=Math.floor(i),l=Math.floor(s),c=a+1,h=l+1,u=t/a,d=e/l,f=[],p=[],x=[],m=[];for(let g=0;g<h;g++){let _=g*d-o;for(let y=0;y<c;y++){let S=y*u-r;p.push(S,-_,0),x.push(0,0,1),m.push(y/a),m.push(1-g/l)}}for(let g=0;g<l;g++)for(let _=0;_<a;_++){let y=_+c*g,S=_+c*(g+1),T=_+1+c*(g+1),w=_+1+c*g;f.push(y,S,w),f.push(S,T,w)}this.setIndex(f),this.setAttribute("position",new jt(p,3)),this.setAttribute("normal",new jt(x,3)),this.setAttribute("uv",new jt(m,2))}copy(t){return super.copy(t),this.parameters=Object.assign({},t.parameters),this}static fromJSON(t){return new n(t.width,t.height,t.widthSegments,t.heightSegments)}};var dn=class n extends Yt{constructor(t=1,e=32,i=16,s=0,r=Math.PI*2,o=0,a=Math.PI){super(),this.type="SphereGeometry",this.parameters={radius:t,widthSegments:e,heightSegments:i,phiStart:s,phiLength:r,thetaStart:o,thetaLength:a},e=Math.max(3,Math.floor(e)),i=Math.max(2,Math.floor(i));let l=Math.min(o+a,Math.PI),c=0,h=[],u=new R,d=new R,f=[],p=[],x=[],m=[];for(let g=0;g<=i;g++){let _=[],y=g/i,S=o+y*a,T=t*Math.cos(S),w=Math.sqrt(t*t-T*T),v=0;g===0&&o===0?v=.5/e:g===i&&l===Math.PI&&(v=-.5/e);for(let M=0;M<=e;M++){let D=M/e,P=s+D*r;u.x=-w*Math.cos(P),u.y=T,u.z=w*Math.sin(P),p.push(u.x,u.y,u.z),d.copy(u).normalize(),x.push(d.x,d.y,d.z),m.push(D+v,1-y),_.push(c++)}h.push(_)}for(let g=0;g<i;g++)for(let _=0;_<e;_++){let y=h[g][_+1],S=h[g][_],T=h[g+1][_],w=h[g+1][_+1];(g!==0||o>0)&&f.push(y,S,w),(g!==i-1||l<Math.PI)&&f.push(S,T,w)}this.setIndex(f),this.setAttribute("position",new jt(p,3)),this.setAttribute("normal",new jt(x,3)),this.setAttribute("uv",new jt(m,2))}copy(t){return super.copy(t),this.parameters=Object.assign({},t.parameters),this}static fromJSON(t){return new n(t.radius,t.widthSegments,t.heightSegments,t.phiStart,t.phiLength,t.thetaStart,t.thetaLength)}};var Vr=class n extends Yt{constructor(t=1,e=.4,i=12,s=48,r=Math.PI*2,o=0,a=Math.PI*2){super(),this.type="TorusGeometry",this.parameters={radius:t,tube:e,radialSegments:i,tubularSegments:s,arc:r,thetaStart:o,thetaLength:a},i=Math.floor(i),s=Math.floor(s);let l=[],c=[],h=[],u=[],d=new R,f=new R,p=new R;for(let x=0;x<=i;x++){let m=o+x/i*a;for(let g=0;g<=s;g++){let _=g/s*r;f.x=(t+e*Math.cos(m))*Math.cos(_),f.y=(t+e*Math.cos(m))*Math.sin(_),f.z=e*Math.sin(m),c.push(f.x,f.y,f.z),d.x=t*Math.cos(_),d.y=t*Math.sin(_),p.subVectors(f,d).normalize(),h.push(p.x,p.y,p.z),u.push(g/s),u.push(x/i)}}for(let x=1;x<=i;x++)for(let m=1;m<=s;m++){let g=(s+1)*x+m-1,_=(s+1)*(x-1)+m-1,y=(s+1)*(x-1)+m,S=(s+1)*x+m;l.push(g,_,S),l.push(_,y,S)}this.setIndex(l),this.setAttribute("position",new jt(c,3)),this.setAttribute("normal",new jt(h,3)),this.setAttribute("uv",new jt(u,2))}copy(t){return super.copy(t),this.parameters=Object.assign({},t.parameters),this}static fromJSON(t){return new n(t.radius,t.tube,t.radialSegments,t.tubularSegments,t.arc,t.thetaStart,t.thetaLength)}};var is=class n extends Yt{constructor(t=new kr(new R(-1,-1,0),new R(-1,1,0),new R(1,1,0)),e=64,i=1,s=8,r=!1){super(),this.type="TubeGeometry",this.parameters={path:t,tubularSegments:e,radius:i,radialSegments:s,closed:r};let o=t.computeFrenetFrames(e,r);this.tangents=o.tangents,this.normals=o.normals,this.binormals=o.binormals;let a=new R,l=new R,c=new et,h=new R,u=[],d=[],f=[],p=[];x(),this.setIndex(p),this.setAttribute("position",new jt(u,3)),this.setAttribute("normal",new jt(d,3)),this.setAttribute("uv",new jt(f,2));function x(){for(let y=0;y<e;y++)m(y);m(r===!1?e:0),_(),g()}function m(y){h=t.getPointAt(y/e,h);let S=o.normals[y],T=o.binormals[y];for(let w=0;w<=s;w++){let v=w/s*Math.PI*2,M=Math.sin(v),D=-Math.cos(v);l.x=D*S.x+M*T.x,l.y=D*S.y+M*T.y,l.z=D*S.z+M*T.z,l.normalize(),d.push(l.x,l.y,l.z),a.x=h.x+i*l.x,a.y=h.y+i*l.y,a.z=h.z+i*l.z,u.push(a.x,a.y,a.z)}}function g(){for(let y=1;y<=e;y++)for(let S=1;S<=s;S++){let T=(s+1)*(y-1)+(S-1),w=(s+1)*y+(S-1),v=(s+1)*y+S,M=(s+1)*(y-1)+S;p.push(T,w,M),p.push(w,v,M)}}function _(){for(let y=0;y<=e;y++)for(let S=0;S<=s;S++)c.x=y/e,c.y=S/s,f.push(c.x,c.y)}}copy(t){return super.copy(t),this.parameters=Object.assign({},t.parameters),this}toJSON(){let t=super.toJSON();return t.path=this.parameters.path.toJSON(),t}static fromJSON(t){return new n(new jc[t.path.type]().fromJSON(t.path),t.tubularSegments,t.radius,t.radialSegments,t.closed)}};function hs(n){let t={};for(let e in n){t[e]={};for(let i in n[e]){let s=n[e][i];if(Qu(s))s.isRenderTargetTexture?(It("UniformsUtils: Textures of render targets cannot be cloned via cloneUniforms() or mergeUniforms()."),t[e][i]=null):t[e][i]=s.clone();else if(Array.isArray(s))if(Qu(s[0])){let r=[];for(let o=0,a=s.length;o<a;o++)r[o]=s[o].clone();t[e][i]=r}else t[e][i]=s.slice();else t[e][i]=s}}return t}function ei(n){let t={};for(let e=0;e<n.length;e++){let i=hs(n[e]);for(let s in i)t[s]=i[s]}return t}function Qu(n){return n&&(n.isColor||n.isMatrix3||n.isMatrix4||n.isVector2||n.isVector3||n.isVector4||n.isTexture||n.isQuaternion)}function Cm(n){let t=[];for(let e=0;e<n.length;e++)t.push(n[e].clone());return t}function wh(n){let t=n.getRenderTarget();return t===null?n.outputColorSpace:t.isXRRenderTarget===!0?t.texture.colorSpace:qt.workingColorSpace}var ki={clone:hs,merge:ei},Pm=`void main() {
	gl_Position = projectionMatrix * modelViewMatrix * vec4( position, 1.0 );
}`,Im=`void main() {
	gl_FragColor = vec4( 1.0, 0.0, 0.0, 1.0 );
}`,ae=class extends Ni{constructor(t){super(),this.isShaderMaterial=!0,this.type="ShaderMaterial",this.defines={},this.uniforms={},this.uniformsGroups=[],this.vertexShader=Pm,this.fragmentShader=Im,this.linewidth=1,this.wireframe=!1,this.wireframeLinewidth=1,this.fog=!1,this.lights=!1,this.clipping=!1,this.forceSinglePass=!0,this.extensions={clipCullDistance:!1,multiDraw:!1},this.defaultAttributeValues={color:[1,1,1],uv:[0,0],uv1:[0,0]},this.index0AttributeName=void 0,this.uniformsNeedUpdate=!1,this.glslVersion=null,t!==void 0&&this.setValues(t)}copy(t){return super.copy(t),this.fragmentShader=t.fragmentShader,this.vertexShader=t.vertexShader,this.uniforms=hs(t.uniforms),this.uniformsGroups=Cm(t.uniformsGroups),this.defines=Object.assign({},t.defines),this.wireframe=t.wireframe,this.wireframeLinewidth=t.wireframeLinewidth,this.fog=t.fog,this.lights=t.lights,this.clipping=t.clipping,this.extensions=Object.assign({},t.extensions),this.glslVersion=t.glslVersion,this.defaultAttributeValues=Object.assign({},t.defaultAttributeValues),this.index0AttributeName=t.index0AttributeName,this.uniformsNeedUpdate=t.uniformsNeedUpdate,this}toJSON(t){let e=super.toJSON(t);e.glslVersion=this.glslVersion,e.uniforms={};for(let s in this.uniforms){let o=this.uniforms[s].value;o&&o.isTexture?e.uniforms[s]={type:"t",value:o.toJSON(t).uuid}:o&&o.isColor?e.uniforms[s]={type:"c",value:o.getHex()}:o&&o.isVector2?e.uniforms[s]={type:"v2",value:o.toArray()}:o&&o.isVector3?e.uniforms[s]={type:"v3",value:o.toArray()}:o&&o.isVector4?e.uniforms[s]={type:"v4",value:o.toArray()}:o&&o.isMatrix3?e.uniforms[s]={type:"m3",value:o.toArray()}:o&&o.isMatrix4?e.uniforms[s]={type:"m4",value:o.toArray()}:e.uniforms[s]={value:o}}Object.keys(this.defines).length>0&&(e.defines=this.defines),e.vertexShader=this.vertexShader,e.fragmentShader=this.fragmentShader,e.lights=this.lights,e.clipping=this.clipping;let i={};for(let s in this.extensions)this.extensions[s]===!0&&(i[s]=!0);return Object.keys(i).length>0&&(e.extensions=i),e}fromJSON(t,e){if(super.fromJSON(t,e),t.uniforms!==void 0)for(let i in t.uniforms){let s=t.uniforms[i];switch(this.uniforms[i]={},s.type){case"t":this.uniforms[i].value=e[s.value]||null;break;case"c":this.uniforms[i].value=new St().setHex(s.value);break;case"v2":this.uniforms[i].value=new et().fromArray(s.value);break;case"v3":this.uniforms[i].value=new R().fromArray(s.value);break;case"v4":this.uniforms[i].value=new ne().fromArray(s.value);break;case"m3":this.uniforms[i].value=new Ft().fromArray(s.value);break;case"m4":this.uniforms[i].value=new Gt().fromArray(s.value);break;default:this.uniforms[i].value=s.value}}if(t.defines!==void 0&&(this.defines=t.defines),t.vertexShader!==void 0&&(this.vertexShader=t.vertexShader),t.fragmentShader!==void 0&&(this.fragmentShader=t.fragmentShader),t.glslVersion!==void 0&&(this.glslVersion=t.glslVersion),t.extensions!==void 0)for(let i in t.extensions)this.extensions[i]=t.extensions[i];return t.lights!==void 0&&(this.lights=t.lights),t.clipping!==void 0&&(this.clipping=t.clipping),this}},In=class extends ae{constructor(t){super(t),this.isRawShaderMaterial=!0,this.type="RawShaderMaterial"}},Pe=class extends Ni{constructor(t){super(),this.isMeshStandardMaterial=!0,this.type="MeshStandardMaterial",this.defines={STANDARD:""},this.color=new St(16777215),this.roughness=1,this.metalness=0,this.map=null,this.lightMap=null,this.lightMapIntensity=1,this.aoMap=null,this.aoMapIntensity=1,this.emissive=new St(0),this.emissiveIntensity=1,this.emissiveMap=null,this.bumpMap=null,this.bumpScale=1,this.normalMap=null,this.normalMapType=uo,this.normalScale=new et(1,1),this.displacementMap=null,this.displacementScale=1,this.displacementBias=0,this.roughnessMap=null,this.metalnessMap=null,this.alphaMap=null,this.envMap=null,this.envMapRotation=new Ti,this.envMapIntensity=1,this.wireframe=!1,this.wireframeLinewidth=1,this.wireframeLinecap="round",this.wireframeLinejoin="round",this.flatShading=!1,this.fog=!0,this.setValues(t)}copy(t){return super.copy(t),this.defines={STANDARD:""},this.color.copy(t.color),this.roughness=t.roughness,this.metalness=t.metalness,this.map=t.map,this.lightMap=t.lightMap,this.lightMapIntensity=t.lightMapIntensity,this.aoMap=t.aoMap,this.aoMapIntensity=t.aoMapIntensity,this.emissive.copy(t.emissive),this.emissiveMap=t.emissiveMap,this.emissiveIntensity=t.emissiveIntensity,this.bumpMap=t.bumpMap,this.bumpScale=t.bumpScale,this.normalMap=t.normalMap,this.normalMapType=t.normalMapType,this.normalScale.copy(t.normalScale),this.displacementMap=t.displacementMap,this.displacementScale=t.displacementScale,this.displacementBias=t.displacementBias,this.roughnessMap=t.roughnessMap,this.metalnessMap=t.metalnessMap,this.alphaMap=t.alphaMap,this.envMap=t.envMap,this.envMapRotation.copy(t.envMapRotation),this.envMapIntensity=t.envMapIntensity,this.wireframe=t.wireframe,this.wireframeLinewidth=t.wireframeLinewidth,this.wireframeLinecap=t.wireframeLinecap,this.wireframeLinejoin=t.wireframeLinejoin,this.flatShading=t.flatShading,this.fog=t.fog,this}},Gr=class extends Pe{constructor(t){super(),this.isMeshPhysicalMaterial=!0,this.defines={STANDARD:"",PHYSICAL:""},this.type="MeshPhysicalMaterial",this.anisotropyRotation=0,this.anisotropyMap=null,this.clearcoatMap=null,this.clearcoatRoughness=0,this.clearcoatRoughnessMap=null,this.clearcoatNormalScale=new et(1,1),this.clearcoatNormalMap=null,this.diffuseRoughnessMap=null,this.ior=1.5,Object.defineProperty(this,"reflectivity",{get:function(){return kt(2.5*(this.ior-1)/(this.ior+1),0,1)},set:function(e){this.ior=(1+.4*e)/(1-.4*e)}}),this.iridescenceMap=null,this.iridescenceIOR=1.3,this.iridescenceThicknessMinimum=100,this.iridescenceThicknessMaximum=400,this.iridescenceThicknessMap=null,this.sheenColor=new St(0),this.sheenColorMap=null,this.sheenRoughness=1,this.sheenRoughnessMap=null,this.transmissionMap=null,this.thickness=0,this.thicknessMap=null,this.attenuationDistance=1/0,this.attenuationColor=new St(1,1,1),this.specularIntensity=1,this.specularIntensityMap=null,this.specularColor=new St(1,1,1),this.specularColorMap=null,this._anisotropy=0,this._clearcoat=0,this._diffuseRoughness=0,this._dispersion=0,this._iridescence=0,this._retroreflectivity=0,this._sheen=0,this._transmission=0,this.setValues(t)}get anisotropy(){return this._anisotropy}set anisotropy(t){this._anisotropy>0!=t>0&&this.version++,this._anisotropy=t}get clearcoat(){return this._clearcoat}set clearcoat(t){this._clearcoat>0!=t>0&&this.version++,this._clearcoat=t}get diffuseRoughness(){return this._diffuseRoughness}set diffuseRoughness(t){this._diffuseRoughness>0!=t>0&&this.version++,this._diffuseRoughness=t}get iridescence(){return this._iridescence}set iridescence(t){this._iridescence>0!=t>0&&this.version++,this._iridescence=t}get dispersion(){return this._dispersion}set dispersion(t){this._dispersion>0!=t>0&&this.version++,this._dispersion=t}get retroreflectivity(){return this._retroreflectivity}set retroreflectivity(t){this._retroreflectivity>0!=t>0&&this.version++,this._retroreflectivity=t}get sheen(){return this._sheen}set sheen(t){this._sheen>0!=t>0&&this.version++,this._sheen=t}get transmission(){return this._transmission}set transmission(t){this._transmission>0!=t>0&&this.version++,this._transmission=t}get iridescenceThicknessRange(){return Kn('MeshPhysicalMaterial: The "iridescenceThicknessRange" property has been replaced by "iridescenceThicknessMinimum" and "iridescenceThicknessMaximum".'),[this.iridescenceThicknessMinimum,this.iridescenceThicknessMaximum]}set iridescenceThicknessRange(t){Kn('MeshPhysicalMaterial: The "iridescenceThicknessRange" property has been replaced by "iridescenceThicknessMinimum" and "iridescenceThicknessMaximum".'),this.iridescenceThicknessMinimum=t[0],this.iridescenceThicknessMaximum=t[1]}copy(t){return super.copy(t),this.defines={STANDARD:"",PHYSICAL:""},this.anisotropy=t.anisotropy,this.anisotropyRotation=t.anisotropyRotation,this.anisotropyMap=t.anisotropyMap,this.clearcoat=t.clearcoat,this.clearcoatMap=t.clearcoatMap,this.clearcoatRoughness=t.clearcoatRoughness,this.clearcoatRoughnessMap=t.clearcoatRoughnessMap,this.clearcoatNormalMap=t.clearcoatNormalMap,this.clearcoatNormalScale.copy(t.clearcoatNormalScale),this.diffuseRoughness=t.diffuseRoughness,this.diffuseRoughnessMap=t.diffuseRoughnessMap,this.dispersion=t.dispersion,this.ior=t.ior,this.iridescence=t.iridescence,this.iridescenceMap=t.iridescenceMap,this.iridescenceIOR=t.iridescenceIOR,this.iridescenceThicknessMinimum=t.iridescenceThicknessMinimum,this.iridescenceThicknessMaximum=t.iridescenceThicknessMaximum,this.iridescenceThicknessMap=t.iridescenceThicknessMap,this.retroreflectivity=t.retroreflectivity,this.sheen=t.sheen,this.sheenColor.copy(t.sheenColor),this.sheenColorMap=t.sheenColorMap,this.sheenRoughness=t.sheenRoughness,this.sheenRoughnessMap=t.sheenRoughnessMap,this.transmission=t.transmission,this.transmissionMap=t.transmissionMap,this.thickness=t.thickness,this.thicknessMap=t.thicknessMap,this.attenuationDistance=t.attenuationDistance,this.attenuationColor.copy(t.attenuationColor),this.specularIntensity=t.specularIntensity,this.specularIntensityMap=t.specularIntensityMap,this.specularColor.copy(t.specularColor),this.specularColorMap=t.specularColorMap,this}};var Wr=class extends Ni{constructor(t){super(),this.isMeshLambertMaterial=!0,this.type="MeshLambertMaterial",this.color=new St(16777215),this.map=null,this.lightMap=null,this.lightMapIntensity=1,this.aoMap=null,this.aoMapIntensity=1,this.emissive=new St(0),this.emissiveIntensity=1,this.emissiveMap=null,this.bumpMap=null,this.bumpScale=1,this.normalMap=null,this.normalMapType=uo,this.normalScale=new et(1,1),this.displacementMap=null,this.displacementScale=1,this.displacementBias=0,this.specularMap=null,this.alphaMap=null,this.envMap=null,this.envMapRotation=new Ti,this.combine=Ya,this.reflectivity=1,this.envMapIntensity=1,this.refractionRatio=.98,this.wireframe=!1,this.wireframeLinewidth=1,this.wireframeLinecap="round",this.wireframeLinejoin="round",this.flatShading=!1,this.fog=!0,this.setValues(t)}copy(t){return super.copy(t),this.color.copy(t.color),this.map=t.map,this.lightMap=t.lightMap,this.lightMapIntensity=t.lightMapIntensity,this.aoMap=t.aoMap,this.aoMapIntensity=t.aoMapIntensity,this.emissive.copy(t.emissive),this.emissiveMap=t.emissiveMap,this.emissiveIntensity=t.emissiveIntensity,this.bumpMap=t.bumpMap,this.bumpScale=t.bumpScale,this.normalMap=t.normalMap,this.normalMapType=t.normalMapType,this.normalScale.copy(t.normalScale),this.displacementMap=t.displacementMap,this.displacementScale=t.displacementScale,this.displacementBias=t.displacementBias,this.specularMap=t.specularMap,this.alphaMap=t.alphaMap,this.envMap=t.envMap,this.envMapRotation.copy(t.envMapRotation),this.combine=t.combine,this.reflectivity=t.reflectivity,this.envMapIntensity=t.envMapIntensity,this.refractionRatio=t.refractionRatio,this.wireframe=t.wireframe,this.wireframeLinewidth=t.wireframeLinewidth,this.wireframeLinecap=t.wireframeLinecap,this.wireframeLinejoin=t.wireframeLinejoin,this.flatShading=t.flatShading,this.fog=t.fog,this}},Da=class extends Ni{constructor(t){super(),this.isMeshDepthMaterial=!0,this.type="MeshDepthMaterial",this.depthPacking=Cd,this.map=null,this.alphaMap=null,this.displacementMap=null,this.displacementScale=1,this.displacementBias=0,this.wireframe=!1,this.wireframeLinewidth=1,this.setValues(t)}copy(t){return super.copy(t),this.depthPacking=t.depthPacking,this.map=t.map,this.alphaMap=t.alphaMap,this.displacementMap=t.displacementMap,this.displacementScale=t.displacementScale,this.displacementBias=t.displacementBias,this.wireframe=t.wireframe,this.wireframeLinewidth=t.wireframeLinewidth,this}},La=class extends Ni{constructor(t){super(),this.isMeshDistanceMaterial=!0,this.type="MeshDistanceMaterial",this.map=null,this.alphaMap=null,this.displacementMap=null,this.displacementScale=1,this.displacementBias=0,this.setValues(t)}copy(t){return super.copy(t),this.map=t.map,this.alphaMap=t.alphaMap,this.displacementMap=t.displacementMap,this.displacementScale=t.displacementScale,this.displacementBias=t.displacementBias,this}};function Cs(n,t){return!n||n.constructor===t?n:typeof t.BYTES_PER_ELEMENT=="number"?new t(n):Array.prototype.slice.call(n)}function aa(n){return n!==void 0&&n.inTangents!==void 0&&n.outTangents!==void 0}var Dn=class{constructor(t,e,i,s){this.parameterPositions=t,this._cachedIndex=0,this.resultBuffer=s!==void 0?s:new e.constructor(i),this.sampleValues=e,this.valueSize=i,this.settings=null,this.DefaultSettings_={}}evaluate(t){let e=this.parameterPositions,i=this._cachedIndex,s=e[i],r=e[i-1];i:{t:{let o;e:{n:if(!(t<s)){for(let a=i+2;;){if(s===void 0){if(t<r)break n;return i=e.length,this._cachedIndex=i,this.copySampleValue_(i-1)}if(i===a)break;if(r=s,s=e[++i],t<s)break t}o=e.length;break e}if(!(t>=r)){let a=e[1];t<a&&(i=2,r=a);for(let l=i-2;;){if(r===void 0)return this._cachedIndex=0,this.copySampleValue_(0);if(i===l)break;if(s=r,r=e[--i-1],t>=r)break t}o=i,i=0;break e}break i}for(;i<o;){let a=i+o>>>1;t<e[a]?o=a:i=a+1}if(s=e[i],r=e[i-1],r===void 0)return this._cachedIndex=0,this.copySampleValue_(0);if(s===void 0)return i=e.length,this._cachedIndex=i,this.copySampleValue_(i-1)}this._cachedIndex=i,this.intervalChanged_(i,r,s)}return this.interpolate_(i,r,t,s)}getSettings_(){return this.settings||this.DefaultSettings_}copySampleValue_(t){let e=this.resultBuffer,i=this.sampleValues,s=this.valueSize,r=t*s;for(let o=0;o!==s;++o)e[o]=i[r+o];return e}interpolate_(){throw new Error("THREE.Interpolant: Call to abstract method.")}intervalChanged_(){}},Na=class extends Dn{constructor(t,e,i,s){super(t,e,i,s),this._weightPrev=-0,this._offsetPrev=-0,this._weightNext=-0,this._offsetNext=-0,this.DefaultSettings_={endingStart:Zc,endingEnd:Zc}}intervalChanged_(t,e,i){let s=this.parameterPositions,r=t-2,o=t+1,a=s[r],l=s[o];if(a===void 0)switch(this.getSettings_().endingStart){case Kc:r=t,a=2*e-i;break;case Jc:r=s.length-2,a=e+s[r]-s[r+1];break;default:r=t,a=i}if(l===void 0)switch(this.getSettings_().endingEnd){case Kc:o=t,l=2*i-e;break;case Jc:o=1,l=i+s[1]-s[0];break;default:o=t-1,l=e}let c=(i-e)*.5,h=this.valueSize;this._weightPrev=c/(e-a),this._weightNext=c/(l-i),this._offsetPrev=r*h,this._offsetNext=o*h}interpolate_(t,e,i,s){let r=this.resultBuffer,o=this.sampleValues,a=this.valueSize,l=t*a,c=l-a,h=this._offsetPrev,u=this._offsetNext,d=this._weightPrev,f=this._weightNext,p=(i-e)/(s-e),x=p*p,m=x*p,g=-d*m+2*d*x-d*p,_=(1+d)*m+(-1.5-2*d)*x+(-.5+d)*p+1,y=(-1-f)*m+(1.5+f)*x+.5*p,S=f*m-f*x;for(let T=0;T!==a;++T)r[T]=g*o[h+T]+_*o[c+T]+y*o[l+T]+S*o[u+T];return r}},Ua=class extends Dn{constructor(t,e,i,s){super(t,e,i,s)}interpolate_(t,e,i,s){let r=this.resultBuffer,o=this.sampleValues,a=this.valueSize,l=t*a,c=l-a,h=(i-e)/(s-e),u=1-h;for(let d=0;d!==a;++d)r[d]=o[c+d]*u+o[l+d]*h;return r}},Fa=class extends Dn{constructor(t,e,i,s){super(t,e,i,s)}interpolate_(t){return this.copySampleValue_(t-1)}},Oa=class extends Dn{interpolate_(t,e,i,s){let r=this.resultBuffer,o=this.sampleValues,a=this.valueSize,l=t*a,c=l-a,h=this.inTangents,u=this.outTangents;if(!h||!u){let p=(i-e)/(s-e),x=1-p;for(let m=0;m!==a;++m)r[m]=o[c+m]*x+o[l+m]*p;return r}let d=a*2,f=t-1;for(let p=0;p!==a;++p){let x=o[c+p],m=o[l+p],g=f*d+p*2,_=u[g],y=u[g+1],S=t*d+p*2,T=h[S],w=h[S+1],v=Lm(i,e,_,T,s);r[p]=Xd(v,x,y,w,m)}return r}};function Xd(n,t,e,i,s){let r=1-n;return r*r*r*t+3*r*r*n*e+3*r*n*n*i+n*n*n*s}function Dm(n,t,e,i,s){let r=1-n;return 3*r*r*(e-t)+6*r*n*(i-e)+3*n*n*(s-i)}function Lm(n,t,e,i,s){let r=(n-t)/(s-t);for(let o=0;o<8;o++){let a=Xd(r,t,e,i,s)-n;if(Math.abs(a)<1e-10)break;let l=Dm(r,t,e,i,s);if(Math.abs(l)<1e-10)break;r=Math.max(0,Math.min(1,r-a/l))}return r}var vi=class{constructor(t,e,i,s){if(t===void 0)throw new Error("THREE.KeyframeTrack: track name is undefined");if(e===void 0||e.length===0)throw new Error("THREE.KeyframeTrack: no keyframes in track named "+t);this.name=t,this.times=Cs(e,this.TimeBufferType),this.values=Cs(i,this.ValueBufferType),this.setInterpolation(s||this.DefaultInterpolation)}static toJSON(t){let e=t.constructor,i;if(e.toJSON!==this.toJSON)i=e.toJSON(t);else{i={name:t.name,times:Cs(t.times,Array),values:Cs(t.values,Array)};let s=t.getInterpolation();s!==t.DefaultInterpolation&&(i.interpolation=s),aa(t.settings)&&(i.settings={inTangents:Cs(t.settings.inTangents,Array),outTangents:Cs(t.settings.outTangents,Array)})}return i.type=t.ValueTypeName,i}InterpolantFactoryMethodDiscrete(t){return new Fa(this.times,this.values,this.getValueSize(),t)}InterpolantFactoryMethodLinear(t){return new Ua(this.times,this.values,this.getValueSize(),t)}InterpolantFactoryMethodSmooth(t){return new Na(this.times,this.values,this.getValueSize(),t)}InterpolantFactoryMethodBezier(t){let e=new Oa(this.times,this.values,this.getValueSize(),t);return this.settings&&(e.inTangents=this.settings.inTangents,e.outTangents=this.settings.outTangents),e}setInterpolation(t){let e;switch(t){case Rr:e=this.InterpolantFactoryMethodDiscrete;break;case ya:e=this.InterpolantFactoryMethodLinear;break;case ha:e=this.InterpolantFactoryMethodSmooth;break;case Yc:e=this.InterpolantFactoryMethodBezier;break}if(e===void 0){let i="unsupported interpolation for "+this.ValueTypeName+" keyframe track named "+this.name;if(this.createInterpolant===void 0)if(t!==this.DefaultInterpolation)this.setInterpolation(this.DefaultInterpolation);else throw new Error(i);return It("KeyframeTrack:",i),this}return this.createInterpolant=e,this}getInterpolation(){switch(this.createInterpolant){case this.InterpolantFactoryMethodDiscrete:return Rr;case this.InterpolantFactoryMethodLinear:return ya;case this.InterpolantFactoryMethodSmooth:return ha;case this.InterpolantFactoryMethodBezier:return Yc}}getValueSize(){return this.values.length/this.times.length}shift(t){if(t!==0){let e=this.times;for(let i=0,s=e.length;i!==s;++i)e[i]+=t;aa(this.settings)&&(td(this.settings.inTangents,t),td(this.settings.outTangents,t))}return this}scale(t){if(t!==1){let e=this.times;for(let i=0,s=e.length;i!==s;++i)e[i]*=t;aa(this.settings)&&(ed(this.settings.inTangents,t),ed(this.settings.outTangents,t))}return this}trim(t,e){let i=this.times,s=i.length,r=0,o=s-1;for(;r!==s&&i[r]<t;)++r;for(;o!==-1&&i[o]>e;)--o;if(++o,r!==0||o!==s){r>=o&&(o=Math.max(o,1),r=o-1);let a=this.getValueSize();this.times=i.slice(r,o),this.values=this.values.slice(r*a,o*a)}return this}validate(){let t=!0,e=this.getValueSize();e-Math.floor(e)!==0&&(Ut("KeyframeTrack: Invalid value size in track.",this),t=!1);let i=this.times,s=this.values,r=i.length;r===0&&(Ut("KeyframeTrack: Track is empty.",this),t=!1);let o=null;for(let a=0;a!==r;a++){let l=i[a];if(typeof l=="number"&&isNaN(l)){Ut("KeyframeTrack: Time is not a valid number.",this,a,l),t=!1;break}if(o!==null&&o>l){Ut("KeyframeTrack: Out of order keys.",this,a,l,o),t=!1;break}o=l}if(s!==void 0&&Bp(s))for(let a=0,l=s.length;a!==l;++a){let c=s[a];if(isNaN(c)){Ut("KeyframeTrack: Value is not a valid number.",this,a,c),t=!1;break}}return t}optimize(){let t=this.times.slice(),e=this.values.slice(),i=this.getValueSize(),s=this.getInterpolation()===ha,r=t.length-1,o=1;for(let a=1;a<r;++a){let l=!1,c=t[a],h=t[a+1];if(c!==h&&(a!==1||c!==t[0]))if(s)l=!0;else{let u=a*i,d=u-i,f=u+i;for(let p=0;p!==i;++p){let x=e[u+p];if(x!==e[d+p]||x!==e[f+p]){l=!0;break}}}if(l){if(a!==o){t[o]=t[a];let u=a*i,d=o*i;for(let f=0;f!==i;++f)e[d+f]=e[u+f]}++o}}if(r>0){t[o]=t[r];for(let a=r*i,l=o*i,c=0;c!==i;++c)e[l+c]=e[a+c];++o}return o!==t.length?(this.times=t.slice(0,o),this.values=e.slice(0,o*i)):(this.times=t,this.values=e),this}clone(){let t=this.times.slice(),e=this.values.slice(),i=this.constructor,s=new i(this.name,t,e);return s.createInterpolant=this.createInterpolant,aa(this.settings)&&(s.settings={inTangents:this.settings.inTangents.slice(),outTangents:this.settings.outTangents.slice()}),s}};function td(n,t){for(let e=0,i=n.length;e!==i;e+=2)n[e]+=t}function ed(n,t){for(let e=0,i=n.length;e!==i;e+=2)n[e]*=t}vi.prototype.ValueTypeName="";vi.prototype.TimeBufferType=Float32Array;vi.prototype.ValueBufferType=Float32Array;vi.prototype.DefaultInterpolation=ya;var Ln=class extends vi{constructor(t,e,i){super(t,e,i)}};Ln.prototype.ValueTypeName="bool";Ln.prototype.ValueBufferType=Array;Ln.prototype.DefaultInterpolation=Rr;Ln.prototype.InterpolantFactoryMethodLinear=void 0;Ln.prototype.InterpolantFactoryMethodSmooth=void 0;var Ba=class extends vi{constructor(t,e,i,s){super(t,e,i,s)}};Ba.prototype.ValueTypeName="color";var za=class extends vi{constructor(t,e,i,s){super(t,e,i,s)}};za.prototype.ValueTypeName="number";var ka=class extends Dn{constructor(t,e,i,s){super(t,e,i,s)}interpolate_(t,e,i,s){let r=this.resultBuffer,o=this.sampleValues,a=this.valueSize,l=(i-e)/(s-e),c=t*a;for(let h=c+a;c!==h;c+=4)Qe.slerpFlat(r,0,o,c-a,o,c,l);return r}},Xr=class extends vi{constructor(t,e,i,s){super(t,e,i,s)}InterpolantFactoryMethodLinear(t){return new ka(this.times,this.values,this.getValueSize(),t)}};Xr.prototype.ValueTypeName="quaternion";Xr.prototype.InterpolantFactoryMethodSmooth=void 0;var Nn=class extends vi{constructor(t,e,i){super(t,e,i)}};Nn.prototype.ValueTypeName="string";Nn.prototype.ValueBufferType=Array;Nn.prototype.DefaultInterpolation=Rr;Nn.prototype.InterpolantFactoryMethodLinear=void 0;Nn.prototype.InterpolantFactoryMethodSmooth=void 0;var Ha=class extends vi{constructor(t,e,i,s){super(t,e,i,s)}};Ha.prototype.ValueTypeName="vector";var Va=class{constructor(t,e,i){let s=this,r=!1,o=0,a=0,l,c=[];this.onStart=void 0,this.onLoad=t,this.onProgress=e,this.onError=i,this._abortController=null,this.itemStart=function(h){a++,r===!1&&s.onStart!==void 0&&s.onStart(h,o,a),r=!0},this.itemEnd=function(h){o++,s.onProgress!==void 0&&s.onProgress(h,o,a),o===a&&(r=!1,s.onLoad!==void 0&&s.onLoad())},this.itemError=function(h){s.onError!==void 0&&s.onError(h)},this.resolveURL=function(h){return h=h.normalize("NFC"),l?l(h):h},this.setURLModifier=function(h){return l=h,this},this.addHandler=function(h,u){return c.push(h,u),this},this.removeHandler=function(h){let u=c.indexOf(h);return u!==-1&&c.splice(u,2),this},this.getHandler=function(h){for(let u=0,d=c.length;u<d;u+=2){let f=c[u],p=c[u+1];if(f.global&&(f.lastIndex=0),f.test(h))return p}return null},this.abort=function(){return this.abortController.abort(),this._abortController=null,this}}get abortController(){return this._abortController||(this._abortController=new AbortController),this._abortController}},qd=new Va,Ga=class{constructor(t){this.manager=t!==void 0?t:qd,this.crossOrigin="anonymous",this.withCredentials=!1,this.path="",this.resourcePath="",this.requestHeader={},this.priority="auto",this.cache="default",typeof __THREE_DEVTOOLS__<"u"&&__THREE_DEVTOOLS__.dispatchEvent(new CustomEvent("observe",{detail:this}))}load(){}loadAsync(t,e){let i=this;return new Promise(function(s,r){i.load(t,s,e,r)})}parse(){}setCrossOrigin(t){return this.crossOrigin=t,this}setWithCredentials(t){return this.withCredentials=t,this}setPath(t){return this.path=t,this}setResourcePath(t){return this.resourcePath=t,this}setRequestHeader(t){return this.requestHeader=t,this}setPriority(t){return this.priority=t,this}setCache(t){return this.cache=t,this}abort(){return this}};Ga.DEFAULT_MATERIAL_NAME="__DEFAULT";var ns=class extends ve{static registerNode(t){this.prototype._lightNode=t}constructor(t,e=1){super(),this.isLight=!0,this.type="Light",this.color=new St(t),this.intensity=e}copy(t,e){return super.copy(t,e),this.color.copy(t.color),this.intensity=t.intensity,this}toJSON(t){let e=super.toJSON(t);return e.object.color=this.color.getHex(),e.object.intensity=this.intensity,e}},qs=class extends ns{constructor(t,e,i){super(t,i),this.isHemisphereLight=!0,this.type="HemisphereLight",this.position.copy(ve.DEFAULT_UP),this.updateMatrix(),this.groundColor=new St(e)}copy(t,e){return super.copy(t,e),this.groundColor.copy(t.groundColor),this}toJSON(t){let e=super.toJSON(t);return e.object.groundColor=this.groundColor.getHex(),e}},qc=new Gt,id=new R,nd=new R,Ys=class{constructor(t){this.camera=t,this.intensity=1,this.bias=0,this.biasNode=null,this.normalBias=0,this.radius=1,this.blurSamples=8,this.mapSize=new et(512,512),this.mapType=ti,this.map=null,this.mapPass=null,this.matrix=new Gt,this.autoUpdate=!0,this.needsUpdate=!1,this._frustum=new Pn,this._frameExtents=new et(1,1),this._viewportCount=1,this._viewports=[new ne(0,0,1,1)]}getViewportCount(){return this._viewportCount}getCamera(){return this.camera}getFrustum(){return this._frustum}updateMatrices(t){let e=this.camera;id.setFromMatrixPosition(t.matrixWorld),e.position.copy(id),nd.setFromMatrixPosition(t.target.matrixWorld),e.lookAt(nd),e.updateMatrixWorld(),this._updateMatrix(e,this.matrix,this._frustum)}_updateMatrix(t,e,i,s){qc.multiplyMatrices(t.projectionMatrix,t.matrixWorldInverse),i.setFromProjectionMatrix(qc,t.coordinateSystem,t.reversedDepth);let r=this._frameExtents,o=s?s.z/r.x:1,a=s?s.w/r.y:1,l=s?s.x/r.x:0,c=s?s.y/r.y:0;t.coordinateSystem===Us||t.reversedDepth?e.set(.5*o,0,0,.5*o+l,0,.5*a,0,.5*a+c,0,0,1,0,0,0,0,1):e.set(.5*o,0,0,.5*o+l,0,.5*a,0,.5*a+c,0,0,.5,.5,0,0,0,1),e.multiply(qc)}getViewport(t){return this._viewports[t]}getFrameExtents(){return this._frameExtents}dispose(){this.map&&this.map.dispose(),this.mapPass&&this.mapPass.dispose()}copy(t){return this.camera=t.camera.clone(),this.intensity=t.intensity,this.bias=t.bias,this.radius=t.radius,this.autoUpdate=t.autoUpdate,this.needsUpdate=t.needsUpdate,this.normalBias=t.normalBias,this.blurSamples=t.blurSamples,this.mapSize.copy(t.mapSize),this.mapType=t.mapType,this.biasNode=t.biasNode,this}clone(){return new this.constructor().copy(this)}toJSON(){let t={};return t.intensity=this.intensity,t.bias=this.bias,t.normalBias=this.normalBias,t.radius=this.radius,t.blurSamples=this.blurSamples,t.mapSize=this.mapSize.toArray(),t.mapType=this.mapType,t.camera=this.camera.toJSON(!1).object,delete t.camera.matrix,t}},la=new R,ca=new Qe,Vi=new R,fn=class extends ve{constructor(){super(),this.isCamera=!0,this.type="Camera",this.matrixWorldInverse=new Gt,this.projectionMatrix=new Gt,this.projectionMatrixInverse=new Gt,this.coordinateSystem=Ei,this._reversedDepth=!1}get reversedDepth(){return this._reversedDepth}copy(t,e){return super.copy(t,e),this.matrixWorldInverse.copy(t.matrixWorldInverse),this.projectionMatrix.copy(t.projectionMatrix),this.projectionMatrixInverse.copy(t.projectionMatrixInverse),this.coordinateSystem=t.coordinateSystem,this}getWorldDirection(t){return super.getWorldDirection(t).negate()}updateMatrixWorld(t){super.updateMatrixWorld(t),this.matrixWorld.decompose(la,ca,Vi),Vi.x===1&&Vi.y===1&&Vi.z===1?this.matrixWorldInverse.copy(this.matrixWorld).invert():this.matrixWorldInverse.compose(la,ca,Vi.set(1,1,1)).invert()}updateWorldMatrix(t,e,i=!1){super.updateWorldMatrix(t,e,i),this.matrixWorld.decompose(la,ca,Vi),Vi.x===1&&Vi.y===1&&Vi.z===1?this.matrixWorldInverse.copy(this.matrixWorld).invert():this.matrixWorldInverse.compose(la,ca,Vi.set(1,1,1)).invert()}clone(){return new this.constructor().copy(this)}},Rn=new R,sd=new et,rd=new et,Ge=class extends fn{constructor(t=50,e=1,i=.1,s=2e3){super(),this.isPerspectiveCamera=!0,this.type="PerspectiveCamera",this.fov=t,this.zoom=1,this.near=i,this.far=s,this.focus=10,this.aspect=e,this.view=null,this.filmGauge=35,this.filmOffset=0,this.updateProjectionMatrix()}copy(t,e){return super.copy(t,e),this.fov=t.fov,this.zoom=t.zoom,this.near=t.near,this.far=t.far,this.focus=t.focus,this.aspect=t.aspect,this.view=t.view===null?null:Object.assign({},t.view),this.filmGauge=t.filmGauge,this.filmOffset=t.filmOffset,this}setFocalLength(t){let e=.5*this.getFilmHeight()/t;this.fov=Jn*2*Math.atan(e),this.updateProjectionMatrix()}getFocalLength(){let t=Math.tan(Ds*.5*this.fov);return .5*this.getFilmHeight()/t}getEffectiveFOV(){return Jn*2*Math.atan(Math.tan(Ds*.5*this.fov)/this.zoom)}getFilmWidth(){return this.filmGauge*Math.min(this.aspect,1)}getFilmHeight(){return this.filmGauge/Math.max(this.aspect,1)}getViewBounds(t,e,i){Rn.set(-1,-1,.5).applyMatrix4(this.projectionMatrixInverse),e.set(Rn.x,Rn.y).multiplyScalar(-t/Rn.z),Rn.set(1,1,.5).applyMatrix4(this.projectionMatrixInverse),i.set(Rn.x,Rn.y).multiplyScalar(-t/Rn.z)}getViewSize(t,e){return this.getViewBounds(t,sd,rd),e.subVectors(rd,sd)}setViewOffset(t,e,i,s,r,o){this.aspect=t/e,this.view===null&&(this.view={enabled:!0,fullWidth:1,fullHeight:1,offsetX:0,offsetY:0,width:1,height:1}),this.view.enabled=!0,this.view.fullWidth=t,this.view.fullHeight=e,this.view.offsetX=i,this.view.offsetY=s,this.view.width=r,this.view.height=o,this.updateProjectionMatrix()}clearViewOffset(){this.view!==null&&(this.view.enabled=!1),this.updateProjectionMatrix()}updateProjectionMatrix(){let t=this.near,e=t*Math.tan(Ds*.5*this.fov)/this.zoom,i=2*e,s=this.aspect*i,r=-.5*s,o=this.view;if(this.view!==null&&this.view.enabled){let l=o.fullWidth,c=o.fullHeight;r+=o.offsetX*s/l,e-=o.offsetY*i/c,s*=o.width/l,i*=o.height/c}let a=this.filmOffset;a!==0&&(r+=t*a/this.getFilmWidth()),this.projectionMatrix.makePerspective(r,r+s,e,e-i,t,this.far,this.coordinateSystem,this.reversedDepth),this.projectionMatrixInverse.copy(this.projectionMatrix).invert()}toJSON(t){let e=super.toJSON(t);return e.object.fov=this.fov,e.object.zoom=this.zoom,e.object.near=this.near,e.object.far=this.far,e.object.focus=this.focus,e.object.aspect=this.aspect,this.view!==null&&(e.object.view=Object.assign({},this.view)),e.object.filmGauge=this.filmGauge,e.object.filmOffset=this.filmOffset,e}},Qc=class extends Ys{constructor(){super(new Ge(50,1,.5,500)),this.isSpotLightShadow=!0,this.focus=1,this.aspect=1}updateMatrices(t){let e=this.camera,i=Jn*2*t.angle*this.focus,s=this.mapSize.width/this.mapSize.height*this.aspect,r=t.distance||e.far;(i!==e.fov||s!==e.aspect||r!==e.far)&&(e.fov=i,e.aspect=s,e.far=r,e.updateProjectionMatrix()),super.updateMatrices(t)}copy(t){return super.copy(t),this.focus=t.focus,this.aspect=t.aspect,this}toJSON(){let t=super.toJSON();return t.focus=this.focus,t.aspect=this.aspect,t}},ss=class extends ns{constructor(t,e,i=0,s=Math.PI/3,r=0,o=2){super(t,e),this.isSpotLight=!0,this.type="SpotLight",this.position.copy(ve.DEFAULT_UP),this.updateMatrix(),this.target=new ve,this.distance=i,this.angle=s,this.penumbra=r,this.decay=o,this.map=null,this.shadow=new Qc}get power(){return this.intensity*Math.PI}set power(t){this.intensity=t/Math.PI}dispose(){super.dispose(),this.shadow.dispose()}copy(t,e){return super.copy(t,e),this.distance=t.distance,this.angle=t.angle,this.penumbra=t.penumbra,this.decay=t.decay,this.target=t.target.clone(),this.map=t.map,this.shadow=t.shadow.clone(),this}toJSON(t){let e=super.toJSON(t);return e.object.distance=this.distance,e.object.angle=this.angle,e.object.decay=this.decay,e.object.penumbra=this.penumbra,e.object.target=this.target.uuid,this.map&&this.map.isTexture&&(e.object.map=this.map.toJSON(t).uuid),e.object.shadow=this.shadow.toJSON(),e}},th=class extends Ys{constructor(){super(new Ge(90,1,.5,500)),this.isPointLightShadow=!0}},qr=class extends ns{constructor(t,e,i=0,s=2){super(t,e),this.isPointLight=!0,this.type="PointLight",this.distance=i,this.decay=s,this.shadow=new th}get power(){return this.intensity*4*Math.PI}set power(t){this.intensity=t/(4*Math.PI)}dispose(){super.dispose(),this.shadow.dispose()}copy(t,e){return super.copy(t,e),this.distance=t.distance,this.decay=t.decay,this.shadow=t.shadow.clone(),this}toJSON(t){let e=super.toJSON(t);return e.object.distance=this.distance,e.object.decay=this.decay,e.object.shadow=this.shadow.toJSON(),e}},rs=class extends fn{constructor(t=-1,e=1,i=1,s=-1,r=.1,o=2e3){super(),this.isOrthographicCamera=!0,this.type="OrthographicCamera",this.zoom=1,this.view=null,this.left=t,this.right=e,this.top=i,this.bottom=s,this.near=r,this.far=o,this.updateProjectionMatrix()}copy(t,e){return super.copy(t,e),this.left=t.left,this.right=t.right,this.top=t.top,this.bottom=t.bottom,this.near=t.near,this.far=t.far,this.zoom=t.zoom,this.view=t.view===null?null:Object.assign({},t.view),this}setViewOffset(t,e,i,s,r,o){this.view===null&&(this.view={enabled:!0,fullWidth:1,fullHeight:1,offsetX:0,offsetY:0,width:1,height:1}),this.view.enabled=!0,this.view.fullWidth=t,this.view.fullHeight=e,this.view.offsetX=i,this.view.offsetY=s,this.view.width=r,this.view.height=o,this.updateProjectionMatrix()}clearViewOffset(){this.view!==null&&(this.view.enabled=!1),this.updateProjectionMatrix()}updateProjectionMatrix(){let t=(this.right-this.left)/(2*this.zoom),e=(this.top-this.bottom)/(2*this.zoom),i=(this.right+this.left)/2,s=(this.top+this.bottom)/2,r=i-t,o=i+t,a=s+e,l=s-e;if(this.view!==null&&this.view.enabled){let c=(this.right-this.left)/this.view.fullWidth/this.zoom,h=(this.top-this.bottom)/this.view.fullHeight/this.zoom;r+=c*this.view.offsetX,o=r+c*this.view.width,a-=h*this.view.offsetY,l=a-h*this.view.height}this.projectionMatrix.makeOrthographic(r,o,a,l,this.near,this.far,this.coordinateSystem,this.reversedDepth),this.projectionMatrixInverse.copy(this.projectionMatrix).invert()}toJSON(t){let e=super.toJSON(t);return e.object.zoom=this.zoom,e.object.left=this.left,e.object.right=this.right,e.object.top=this.top,e.object.bottom=this.bottom,e.object.near=this.near,e.object.far=this.far,this.view!==null&&(e.object.view=Object.assign({},this.view)),e}},eh=class extends Ys{constructor(){super(new rs(-5,5,5,-5,.5,500)),this.isDirectionalLightShadow=!0}},Yr=class extends ns{constructor(t,e){super(t,e),this.isDirectionalLight=!0,this.type="DirectionalLight",this.position.copy(ve.DEFAULT_UP),this.updateMatrix(),this.target=new ve,this.shadow=new eh}dispose(){super.dispose(),this.shadow.dispose()}copy(t){return super.copy(t),this.target=t.target.clone(),this.shadow=t.shadow.clone(),this}toJSON(t){let e=super.toJSON(t);return e.object.shadow=this.shadow.toJSON(),e.object.target=this.target.uuid,e}};var Zr=class extends Yt{constructor(){super(),this.isInstancedBufferGeometry=!0,this.type="InstancedBufferGeometry",this.instanceCount=1/0}copy(t){return super.copy(t),this.instanceCount=t.instanceCount,this}toJSON(){let t=super.toJSON();return t.instanceCount=this.instanceCount,t.isInstancedBufferGeometry=!0,t}};var Ps=-90,Is=1,Zs=class extends ve{constructor(t,e,i){super(),this.type="CubeCamera",this.renderTarget=i,this.coordinateSystem=null,this.activeMipmapLevel=0;let s=new Ge(Ps,Is,t,e);s.layers=this.layers,this.add(s);let r=new Ge(Ps,Is,t,e);r.layers=this.layers,this.add(r);let o=new Ge(Ps,Is,t,e);o.layers=this.layers,this.add(o);let a=new Ge(Ps,Is,t,e);a.layers=this.layers,this.add(a);let l=new Ge(Ps,Is,t,e);l.layers=this.layers,this.add(l);let c=new Ge(Ps,Is,t,e);c.layers=this.layers,this.add(c)}updateCoordinateSystem(){let t=this.coordinateSystem,e=this.children.concat(),[i,s,r,o,a,l]=e;for(let c of e)this.remove(c);if(t===Ei)i.up.set(0,1,0),i.lookAt(1,0,0),s.up.set(0,1,0),s.lookAt(-1,0,0),r.up.set(0,0,-1),r.lookAt(0,1,0),o.up.set(0,0,1),o.lookAt(0,-1,0),a.up.set(0,1,0),a.lookAt(0,0,1),l.up.set(0,1,0),l.lookAt(0,0,-1);else if(t===Us)i.up.set(0,-1,0),i.lookAt(-1,0,0),s.up.set(0,-1,0),s.lookAt(1,0,0),r.up.set(0,0,1),r.lookAt(0,1,0),o.up.set(0,0,-1),o.lookAt(0,-1,0),a.up.set(0,-1,0),a.lookAt(0,0,1),l.up.set(0,-1,0),l.lookAt(0,0,-1);else throw new Error("THREE.CubeCamera.updateCoordinateSystem(): Invalid coordinate system: "+t);for(let c of e)this.add(c),c.updateMatrixWorld()}update(t,e){this.parent===null&&this.updateMatrixWorld();let{renderTarget:i,activeMipmapLevel:s}=this;this.coordinateSystem!==t.coordinateSystem&&(this.coordinateSystem=t.coordinateSystem,this.updateCoordinateSystem());let[r,o,a,l,c,h]=this.children,u=t.getRenderTarget(),d=t.getActiveCubeFace(),f=t.getActiveMipmapLevel(),p=t.xr.enabled;t.xr.enabled=!1;let x=i.texture.mipmapsAutoUpdate;i.texture.mipmapsAutoUpdate=!1;let m=!1;t.isWebGLRenderer===!0?m=t.state.buffers.depth.getReversed():m=t.reversedDepthBuffer,t.setRenderTarget(i,0,s),m&&t.autoClear===!1&&t.clearDepth(),t.render(e,r),t.setRenderTarget(i,1,s),m&&t.autoClear===!1&&t.clearDepth(),t.render(e,o),t.setRenderTarget(i,2,s),m&&t.autoClear===!1&&t.clearDepth(),t.render(e,a),t.setRenderTarget(i,3,s),m&&t.autoClear===!1&&t.clearDepth(),t.render(e,l),t.setRenderTarget(i,4,s),m&&t.autoClear===!1&&t.clearDepth(),t.render(e,c),i.texture.mipmapsAutoUpdate=x,t.setRenderTarget(i,5,s),m&&t.autoClear===!1&&t.clearDepth(),t.render(e,h),t.setRenderTarget(u,d,f),t.xr.enabled=p,i.texture.needsPMREMUpdate=!0}},Wa=class extends Ge{constructor(t=[]){super(),this.isArrayCamera=!0,this.isMultiViewCamera=!1,this.cameras=t,this.perCameraFrustumCulled=!1}},Kr=class{constructor(){this._previousTime=0,this._currentTime=0,this._startTime=performance.now(),this._delta=0,this._elapsed=0,this._timescale=1,this._document=null,this._pageVisibilityHandler=null}connect(t){this._document=t,t.hidden!==void 0&&(this._pageVisibilityHandler=Nm.bind(this),t.addEventListener("visibilitychange",this._pageVisibilityHandler,!1))}disconnect(){this._pageVisibilityHandler!==null&&(this._document.removeEventListener("visibilitychange",this._pageVisibilityHandler),this._pageVisibilityHandler=null),this._document=null}getDelta(){return this._delta/1e3}getElapsed(){return this._elapsed/1e3}getTimescale(){return this._timescale}setTimescale(t){return this._timescale=t,this}reset(){return this._currentTime=performance.now()-this._startTime,this}dispose(){this.disconnect()}update(t){return this._pageVisibilityHandler!==null&&this._document.hidden===!0?this._delta=0:(this._previousTime=this._currentTime,this._currentTime=(t!==void 0?t:performance.now())-this._startTime,this._delta=(this._currentTime-this._previousTime)*this._timescale,this._elapsed+=this._delta),this}};function Nm(){this._document.hidden===!1&&this.reset()}var Eh="\\[\\]\\.:\\/",Um=new RegExp("["+Eh+"]","g"),Th="[^"+Eh+"]",Fm="[^"+Eh.replace("\\.","")+"]",Om=/((?:WC+[\/:])*)/.source.replace("WC",Th),Bm=/(WCOD+)?/.source.replace("WCOD",Fm),zm=/(?:\.(WC+)(?:\[(.+)\])?)?/.source.replace("WC",Th),km=/\.(WC+)(?:\[(.+)\])?/.source.replace("WC",Th),Hm=new RegExp("^"+Om+Bm+zm+km+"$"),Vm=["material","materials","bones","map"],ih=class{constructor(t,e,i){let s=i||me.parseTrackName(e);this._targetGroup=t,this._bindings=t.subscribe_(e,s)}getValue(t,e){this.bind();let i=this._targetGroup.nCachedObjects_,s=this._bindings[i];s!==void 0&&s.getValue(t,e)}setValue(t,e){let i=this._bindings;for(let s=this._targetGroup.nCachedObjects_,r=i.length;s!==r;++s)i[s].setValue(t,e)}bind(){let t=this._bindings;for(let e=this._targetGroup.nCachedObjects_,i=t.length;e!==i;++e)t[e].bind()}unbind(){let t=this._bindings;for(let e=this._targetGroup.nCachedObjects_,i=t.length;e!==i;++e)t[e].unbind()}},me=class n{constructor(t,e,i){this.path=e,this.parsedPath=i||n.parseTrackName(e),this.node=n.findNode(t,this.parsedPath.nodeName),this.rootNode=t,this.getValue=this._getValue_unbound,this.setValue=this._setValue_unbound}static create(t,e,i){return t&&t.isAnimationObjectGroup?new n.Composite(t,e,i):new n(t,e,i)}static sanitizeNodeName(t){return t.replace(/\s/g,"_").replace(Um,"")}static parseTrackName(t){let e=Hm.exec(t);if(e===null)throw new Error("THREE.PropertyBinding: Cannot parse trackName: "+t);let i={nodeName:e[2],objectName:e[3],objectIndex:e[4],propertyName:e[5],propertyIndex:e[6]},s=i.nodeName&&i.nodeName.lastIndexOf(".");if(s!==void 0&&s!==-1){let r=i.nodeName.substring(s+1);Vm.indexOf(r)!==-1&&(i.nodeName=i.nodeName.substring(0,s),i.objectName=r)}if(i.propertyName===null||i.propertyName.length===0)throw new Error("THREE.PropertyBinding: can not parse propertyName from trackName: "+t);return i}static findNode(t,e){if(e===void 0||e===""||e==="."||e===-1||e===t.name||e===t.uuid)return t;if(t.skeleton){let i=t.skeleton.getBoneByName(e);if(i!==void 0)return i}if(t.children){let i=function(r){for(let o=0;o<r.length;o++){let a=r[o];if(a.name===e||a.uuid===e)return a;let l=i(a.children);if(l)return l}return null},s=i(t.children);if(s)return s}return null}_getValue_unavailable(){}_setValue_unavailable(){}_getValue_direct(t,e){t[e]=this.targetObject[this.propertyName]}_getValue_array(t,e){let i=this.resolvedProperty;for(let s=0,r=i.length;s!==r;++s)t[e++]=i[s]}_getValue_arrayElement(t,e){t[e]=this.resolvedProperty[this.propertyIndex]}_getValue_toArray(t,e){this.resolvedProperty.toArray(t,e)}_setValue_direct(t,e){this.targetObject[this.propertyName]=t[e]}_setValue_direct_setNeedsUpdate(t,e){this.targetObject[this.propertyName]=t[e],this.targetObject.needsUpdate=!0}_setValue_direct_setMatrixWorldNeedsUpdate(t,e){this.targetObject[this.propertyName]=t[e],this.targetObject.matrixWorldNeedsUpdate=!0}_setValue_array(t,e){let i=this.resolvedProperty;for(let s=0,r=i.length;s!==r;++s)i[s]=t[e++]}_setValue_array_setNeedsUpdate(t,e){let i=this.resolvedProperty;for(let s=0,r=i.length;s!==r;++s)i[s]=t[e++];this.targetObject.needsUpdate=!0}_setValue_array_setMatrixWorldNeedsUpdate(t,e){let i=this.resolvedProperty;for(let s=0,r=i.length;s!==r;++s)i[s]=t[e++];this.targetObject.matrixWorldNeedsUpdate=!0}_setValue_arrayElement(t,e){this.resolvedProperty[this.propertyIndex]=t[e]}_setValue_arrayElement_setNeedsUpdate(t,e){this.resolvedProperty[this.propertyIndex]=t[e],this.targetObject.needsUpdate=!0}_setValue_arrayElement_setMatrixWorldNeedsUpdate(t,e){this.resolvedProperty[this.propertyIndex]=t[e],this.targetObject.matrixWorldNeedsUpdate=!0}_setValue_fromArray(t,e){this.resolvedProperty.fromArray(t,e)}_setValue_fromArray_setNeedsUpdate(t,e){this.resolvedProperty.fromArray(t,e),this.targetObject.needsUpdate=!0}_setValue_fromArray_setMatrixWorldNeedsUpdate(t,e){this.resolvedProperty.fromArray(t,e),this.targetObject.matrixWorldNeedsUpdate=!0}_getValue_unbound(t,e){this.bind(),this.getValue(t,e)}_setValue_unbound(t,e){this.bind(),this.setValue(t,e)}bind(){let t=this.node,e=this.parsedPath,i=e.objectName,s=e.propertyName,r=e.propertyIndex;if(t||(t=n.findNode(this.rootNode,e.nodeName),this.node=t),this.getValue=this._getValue_unavailable,this.setValue=this._setValue_unavailable,!t){It("PropertyBinding: No target node found for track: "+this.path+".");return}if(i){let c=e.objectIndex;switch(i){case"materials":if(!t.material){Ut("PropertyBinding: Can not bind to material as node does not have a material.",this);return}if(!t.material.materials){Ut("PropertyBinding: Can not bind to material.materials as node.material does not have a materials array.",this);return}t=t.material.materials;break;case"bones":if(!t.skeleton){Ut("PropertyBinding: Can not bind to bones as node does not have a skeleton.",this);return}t=t.skeleton.bones;for(let h=0;h<t.length;h++)if(t[h].name===c){c=h;break}break;case"map":if("map"in t){t=t.map;break}if(!t.material){Ut("PropertyBinding: Can not bind to material as node does not have a material.",this);return}if(!t.material.map){Ut("PropertyBinding: Can not bind to material.map as node.material does not have a map.",this);return}t=t.material.map;break;default:if(t[i]===void 0){Ut("PropertyBinding: Can not bind to objectName of node undefined.",this);return}t=t[i]}if(c!==void 0){if(t[c]===void 0){Ut("PropertyBinding: Trying to bind to objectIndex of objectName, but is undefined.",this,t);return}t=t[c]}}let o=t[s];if(o===void 0){let c=e.nodeName;Ut("PropertyBinding: Trying to update property for track: "+c+"."+s+" but it wasn't found.",t);return}let a=this.Versioning.None;this.targetObject=t,t.isMaterial===!0?a=this.Versioning.NeedsUpdate:t.isObject3D===!0&&(a=this.Versioning.MatrixWorldNeedsUpdate);let l=this.BindingType.Direct;if(r!==void 0){if(s==="morphTargetInfluences"){if(!t.geometry){Ut("PropertyBinding: Can not bind to morphTargetInfluences because node does not have a geometry.",this);return}if(!t.geometry.morphAttributes){Ut("PropertyBinding: Can not bind to morphTargetInfluences because node does not have a geometry.morphAttributes.",this);return}t.morphTargetDictionary[r]!==void 0&&(r=t.morphTargetDictionary[r])}l=this.BindingType.ArrayElement,this.resolvedProperty=o,this.propertyIndex=r}else o.fromArray!==void 0&&o.toArray!==void 0?(l=this.BindingType.HasFromToArray,this.resolvedProperty=o):Array.isArray(o)?(l=this.BindingType.EntireArray,this.resolvedProperty=o):this.propertyName=s;this.getValue=this.GetterByBindingType[l],this.setValue=this.SetterByBindingTypeAndVersioning[l][a]}unbind(){this.node=null,this.getValue=this._getValue_unbound,this.setValue=this._setValue_unbound}};me.Composite=ih;me.prototype.BindingType={Direct:0,EntireArray:1,ArrayElement:2,HasFromToArray:3};me.prototype.Versioning={None:0,NeedsUpdate:1,MatrixWorldNeedsUpdate:2};me.prototype.GetterByBindingType=[me.prototype._getValue_direct,me.prototype._getValue_array,me.prototype._getValue_arrayElement,me.prototype._getValue_toArray];me.prototype.SetterByBindingTypeAndVersioning=[[me.prototype._setValue_direct,me.prototype._setValue_direct_setNeedsUpdate,me.prototype._setValue_direct_setMatrixWorldNeedsUpdate],[me.prototype._setValue_array,me.prototype._setValue_array_setNeedsUpdate,me.prototype._setValue_array_setMatrixWorldNeedsUpdate],[me.prototype._setValue_arrayElement,me.prototype._setValue_arrayElement_setNeedsUpdate,me.prototype._setValue_arrayElement_setMatrixWorldNeedsUpdate],[me.prototype._setValue_fromArray,me.prototype._setValue_fromArray_setNeedsUpdate,me.prototype._setValue_fromArray_setMatrixWorldNeedsUpdate]];var iy=new Float32Array(1);var od=new Gt,Xi=class{constructor(t,e,i=0,s=1/0){this.ray=new hn(t,e),this.near=i,this.far=s,this.camera=null,this.layers=new zs,this.params={Mesh:{},Line:{threshold:1},LOD:{},Points:{threshold:1},Sprite:{}}}set(t,e){this.ray.set(t,e)}setFromCamera(t,e){e.isPerspectiveCamera?(this.ray.origin.setFromMatrixPosition(e.matrixWorld),this.ray.direction.set(t.x,t.y,.5).unproject(e).sub(this.ray.origin).normalize(),this.camera=e):e.isOrthographicCamera?(this.ray.origin.set(t.x,t.y,e.projectionMatrix.elements[14]).unproject(e),this.ray.direction.set(0,0,-1).transformDirection(e.matrixWorld),this.camera=e):Ut("Raycaster: Unsupported camera type: "+e.type)}setFromXRController(t){return od.identity().extractRotation(t.matrixWorld),this.ray.origin.setFromMatrixPosition(t.matrixWorld),this.ray.direction.set(0,0,-1).applyMatrix4(od),this}intersectObject(t,e=!0,i=[]){return nh(t,this,i,e),i.sort(ad),i}intersectObjects(t,e=!0,i=[]){for(let s=0,r=t.length;s<r;s++)nh(t[s],this,i,e);return i.sort(ad),i}};function ad(n,t){return n.distance-t.distance}function nh(n,t,e,i){let s=!0;if(n.layers.test(t.layers)&&n.raycast(t,e)===!1&&(s=!1),s===!0&&i===!0){let r=n.children;for(let o=0,a=r.length;o<a;o++)nh(r[o],t,e,!0)}}var Ks=class{constructor(t=1,e=0,i=0){this.radius=t,this.phi=e,this.theta=i}set(t,e,i){return this.radius=t,this.phi=e,this.theta=i,this}copy(t){return this.radius=t.radius,this.phi=t.phi,this.theta=t.theta,this}makeSafe(){return this.phi=kt(this.phi,1e-6,Math.PI-1e-6),this}setFromVector3(t){return this.setFromCartesianCoords(t.x,t.y,t.z)}setFromCartesianCoords(t,e,i){return this.radius=Math.sqrt(t*t+e*e+i*i),this.radius===0?(this.theta=0,this.phi=0):(this.theta=Math.atan2(t,i),this.phi=Math.acos(kt(e/this.radius,-1,1))),this}clone(){return new this.constructor().copy(this)}};var sh=class{static{this.prototype.isMatrix2=!0}constructor(t,e,i,s){this.elements=[1,0,0,1],t!==void 0&&this.set(t,e,i,s)}identity(){return this.set(1,0,0,1),this}fromArray(t,e=0){for(let i=0;i<4;i++)this.elements[i]=t[i+e];return this}set(t,e,i,s){let r=this.elements;return r[0]=t,r[2]=e,r[1]=i,r[3]=s,this}};var Jr=class extends Li{constructor(t,e=null){super(),this.object=t,this.domElement=e,this.enabled=!0,this.state=-1,this.keys={},this.mouseButtons={LEFT:null,MIDDLE:null,RIGHT:null},this.touches={ONE:null,TWO:null}}connect(t){this.domElement!==null&&this.disconnect(),this.domElement=t}disconnect(){}dispose(){}update(){}};function Ah(n,t,e,i){let s=Gm(i);switch(e){case mh:return n*t;case On:return n*t/s.components*s.byteLength;case tl:return n*t/s.components*s.byteLength;case Bn:return n*t*2/s.components*s.byteLength;case el:return n*t*2/s.components*s.byteLength;case gh:return n*t*3/s.components*s.byteLength;case ai:return n*t*4/s.components*s.byteLength;case il:return n*t*4/s.components*s.byteLength;case ro:case oo:return Math.floor((n+3)/4)*Math.floor((t+3)/4)*8;case ao:case lo:return Math.floor((n+3)/4)*Math.floor((t+3)/4)*16;case sl:case ol:return Math.max(n,16)*Math.max(t,8)/4;case nl:case rl:return Math.max(n,8)*Math.max(t,8)/2;case al:case ll:case hl:case ul:return Math.floor((n+3)/4)*Math.floor((t+3)/4)*8;case cl:case co:case dl:return Math.floor((n+3)/4)*Math.floor((t+3)/4)*16;case fl:return Math.floor((n+3)/4)*Math.floor((t+3)/4)*16;case pl:return Math.floor((n+4)/5)*Math.floor((t+3)/4)*16;case ml:return Math.floor((n+4)/5)*Math.floor((t+4)/5)*16;case gl:return Math.floor((n+5)/6)*Math.floor((t+4)/5)*16;case xl:return Math.floor((n+5)/6)*Math.floor((t+5)/6)*16;case _l:return Math.floor((n+7)/8)*Math.floor((t+4)/5)*16;case vl:return Math.floor((n+7)/8)*Math.floor((t+5)/6)*16;case yl:return Math.floor((n+7)/8)*Math.floor((t+7)/8)*16;case Ml:return Math.floor((n+9)/10)*Math.floor((t+4)/5)*16;case Sl:return Math.floor((n+9)/10)*Math.floor((t+5)/6)*16;case bl:return Math.floor((n+9)/10)*Math.floor((t+7)/8)*16;case wl:return Math.floor((n+9)/10)*Math.floor((t+9)/10)*16;case El:return Math.floor((n+11)/12)*Math.floor((t+9)/10)*16;case Tl:return Math.floor((n+11)/12)*Math.floor((t+11)/12)*16;case Al:case Rl:case Cl:return Math.ceil(n/4)*Math.ceil(t/4)*16;case Pl:case Il:return Math.ceil(n/4)*Math.ceil(t/4)*8;case ho:case Dl:return Math.ceil(n/4)*Math.ceil(t/4)*16}throw new Error(`Unable to determine texture byte length for ${e} format.`)}function Gm(n){switch(n){case ti:case uh:return{byteLength:1,components:1};case tr:case dh:case de:return{byteLength:2,components:1};case ja:case Qa:return{byteLength:2,components:4};case oi:case $a:case ui:return{byteLength:4,components:1};case fh:case ph:return{byteLength:4,components:3}}throw new Error(`THREE.TextureUtils: Unknown texture type ${n}.`)}typeof __THREE_DEVTOOLS__<"u"&&__THREE_DEVTOOLS__.dispatchEvent(new CustomEvent("register",{detail:{revision:"187"}}));typeof window<"u"&&(window.__THREE__?It("WARNING: Multiple instances of Three.js being imported."):window.__THREE__="187");function mf(){let n=null,t=!1,e=null,i=null;function s(r,o){i=n.requestAnimationFrame(s),e(r,o)}return{start:function(){t!==!0&&e!==null&&n!==null&&(i=n.requestAnimationFrame(s),t=!0)},stop:function(){n!==null&&n.cancelAnimationFrame(i),t=!1},setAnimationLoop:function(r){e=r},setContext:function(r){n=r}}}function Wm(n){let t=new WeakMap;function e(a,l){let c=a.array,h=a.usage,u=c.byteLength,d=n.createBuffer();n.bindBuffer(l,d),n.bufferData(l,c,h),a.onUploadCallback();let f;if(c instanceof Float32Array)f=n.FLOAT;else if(typeof Float16Array<"u"&&c instanceof Float16Array)f=n.HALF_FLOAT;else if(c instanceof Uint16Array)a.isFloat16BufferAttribute?f=n.HALF_FLOAT:f=n.UNSIGNED_SHORT;else if(c instanceof Int16Array)f=n.SHORT;else if(c instanceof Uint32Array)f=n.UNSIGNED_INT;else if(c instanceof Int32Array)f=n.INT;else if(c instanceof Int8Array)f=n.BYTE;else if(c instanceof Uint8Array)f=n.UNSIGNED_BYTE;else if(c instanceof Uint8ClampedArray)f=n.UNSIGNED_BYTE;else throw new Error("THREE.WebGLAttributes: Unsupported buffer data format: "+c);return{buffer:d,type:f,bytesPerElement:c.BYTES_PER_ELEMENT,version:a.version,size:u}}function i(a,l,c){let h=l.array,u=l.updateRanges;if(n.bindBuffer(c,a),u.length===0)n.bufferSubData(c,0,h);else{u.sort((f,p)=>f.start-p.start);let d=0;for(let f=1;f<u.length;f++){let p=u[d],x=u[f];x.start<=p.start+p.count+1?p.count=Math.max(p.count,x.start+x.count-p.start):(++d,u[d]=x)}u.length=d+1;for(let f=0,p=u.length;f<p;f++){let x=u[f];n.bufferSubData(c,x.start*h.BYTES_PER_ELEMENT,h,x.start,x.count)}l.clearUpdateRanges()}l.onUploadCallback()}function s(a){return a.isInterleavedBufferAttribute&&(a=a.data),t.get(a)}function r(a){a.isInterleavedBufferAttribute&&(a=a.data);let l=t.get(a);l&&(n.deleteBuffer(l.buffer),t.delete(a))}function o(a,l){if(a.isInterleavedBufferAttribute&&(a=a.data),a.isGLBufferAttribute){let h=t.get(a);(!h||h.version<a.version)&&t.set(a,{buffer:a.buffer,type:a.type,bytesPerElement:a.elementSize,version:a.version});return}let c=t.get(a);if(c===void 0)t.set(a,e(a,l));else if(c.version<a.version){if(c.size!==a.array.byteLength)throw new Error("THREE.WebGLAttributes: The size of the buffer attribute's array buffer does not match the original size. Resizing buffer attributes is not supported.");i(c.buffer,a,l),c.version=a.version}}return{get:s,remove:r,update:o}}var Xm=`#ifdef USE_ALPHAHASH
	if ( diffuseColor.a < getAlphaHashThreshold( vPosition ) ) discard;
#endif`,qm=`#ifdef USE_ALPHAHASH
	const float ALPHA_HASH_SCALE = 0.05;
	float hash2D( vec2 value ) {
		return fract( 1.0e4 * sin( 17.0 * value.x + 0.1 * value.y ) * ( 0.1 + abs( sin( 13.0 * value.y + value.x ) ) ) );
	}
	float hash3D( vec3 value ) {
		return hash2D( vec2( hash2D( value.xy ), value.z ) );
	}
	float getAlphaHashThreshold( vec3 position ) {
		float maxDeriv = max(
			length( dFdx( position.xyz ) ),
			length( dFdy( position.xyz ) )
		);
		float pixScale = 1.0 / ( ALPHA_HASH_SCALE * maxDeriv );
		vec2 pixScales = vec2(
			exp2( floor( log2( pixScale ) ) ),
			exp2( ceil( log2( pixScale ) ) )
		);
		vec2 alpha = vec2(
			hash3D( floor( pixScales.x * position.xyz ) ),
			hash3D( floor( pixScales.y * position.xyz ) )
		);
		float lerpFactor = fract( log2( pixScale ) );
		float x = ( 1.0 - lerpFactor ) * alpha.x + lerpFactor * alpha.y;
		float a = min( lerpFactor, 1.0 - lerpFactor );
		vec3 cases = vec3(
			x * x / ( 2.0 * a * ( 1.0 - a ) ),
			( x - 0.5 * a ) / ( 1.0 - a ),
			1.0 - ( ( 1.0 - x ) * ( 1.0 - x ) / ( 2.0 * a * ( 1.0 - a ) ) )
		);
		float threshold = ( x < ( 1.0 - a ) )
			? ( ( x < a ) ? cases.x : cases.y )
			: cases.z;
		return clamp( threshold , 1.0e-6, 1.0 );
	}
#endif`,Ym=`#ifdef USE_ALPHAMAP
	diffuseColor.a *= texture2D( alphaMap, vAlphaMapUv ).g;
#endif`,Zm=`#ifdef USE_ALPHAMAP
	uniform sampler2D alphaMap;
#endif`,Km=`#ifdef USE_ALPHATEST
	#ifdef ALPHA_TO_COVERAGE
	diffuseColor.a = smoothstep( alphaTest, alphaTest + fwidth( diffuseColor.a ), diffuseColor.a );
	if ( diffuseColor.a == 0.0 ) discard;
	#else
	if ( diffuseColor.a < alphaTest ) discard;
	#endif
#endif`,Jm=`#ifdef USE_ALPHATEST
	uniform float alphaTest;
#endif`,$m=`#ifdef USE_AOMAP
	float ambientOcclusion = ( texture2D( aoMap, vAoMapUv ).r - 1.0 ) * aoMapIntensity + 1.0;
	reflectedLight.indirectDiffuse *= ambientOcclusion;
	#if defined( USE_CLEARCOAT )
		clearcoatSpecularIndirect *= ambientOcclusion;
	#endif
	#if defined( USE_SHEEN )
		sheenSpecularIndirect *= ambientOcclusion;
	#endif
	#if defined( USE_ENVMAP ) && defined( STANDARD )
		float dotNV = saturate( dot( geometryNormal, geometryViewDir ) );
		reflectedLight.indirectSpecular *= computeSpecularOcclusion( dotNV, ambientOcclusion, material.roughness );
	#endif
#endif`,jm=`#ifdef USE_AOMAP
	uniform sampler2D aoMap;
	uniform float aoMapIntensity;
#endif`,Qm=`#ifdef USE_BATCHING
	#if ! defined( GL_ANGLE_multi_draw )
	#define gl_DrawID _gl_DrawID
	uniform int _gl_DrawID;
	#endif
	uniform highp sampler2D batchingTexture;
	uniform highp usampler2D batchingIdTexture;
	mat4 getBatchingMatrix( const in float i ) {
		int size = textureSize( batchingTexture, 0 ).x;
		int j = int( i ) * 4;
		int x = j % size;
		int y = j / size;
		vec4 v1 = texelFetch( batchingTexture, ivec2( x, y ), 0 );
		vec4 v2 = texelFetch( batchingTexture, ivec2( x + 1, y ), 0 );
		vec4 v3 = texelFetch( batchingTexture, ivec2( x + 2, y ), 0 );
		vec4 v4 = texelFetch( batchingTexture, ivec2( x + 3, y ), 0 );
		return mat4( v1, v2, v3, v4 );
	}
	float getIndirectIndex( const in int i ) {
		int size = textureSize( batchingIdTexture, 0 ).x;
		int x = i % size;
		int y = i / size;
		return float( texelFetch( batchingIdTexture, ivec2( x, y ), 0 ).r );
	}
#endif
#ifdef USE_BATCHING_COLOR
	uniform sampler2D batchingColorTexture;
	vec4 getBatchingColor( const in float i ) {
		int size = textureSize( batchingColorTexture, 0 ).x;
		int j = int( i );
		int x = j % size;
		int y = j / size;
		return texelFetch( batchingColorTexture, ivec2( x, y ), 0 );
	}
#endif`,tg=`#ifdef USE_BATCHING
	mat4 batchingMatrix = getBatchingMatrix( getIndirectIndex( gl_DrawID ) );
#endif`,eg=`vec3 transformed = vec3( position );
#ifdef USE_ALPHAHASH
	vPosition = vec3( position );
#endif`,ig=`vec3 objectNormal = vec3( normal );
#ifdef USE_TANGENT
	vec3 objectTangent = vec3( tangent.xyz );
#endif`,ng=`float G_BlinnPhong_Implicit( ) {
	return 0.25;
}
float D_BlinnPhong( const in float shininess, const in float dotNH ) {
	return RECIPROCAL_PI * ( shininess * 0.5 + 1.0 ) * pow( dotNH, shininess );
}
vec3 BRDF_BlinnPhong( const in vec3 lightDir, const in vec3 viewDir, const in vec3 normal, const in vec3 specularColor, const in float shininess ) {
	vec3 halfDir = normalize( lightDir + viewDir );
	float dotNH = saturate( dot( normal, halfDir ) );
	float dotVH = saturate( dot( viewDir, halfDir ) );
	vec3 F = F_Schlick( specularColor, 1.0, dotVH );
	float G = G_BlinnPhong_Implicit( );
	float D = D_BlinnPhong( shininess, dotNH );
	return F * ( G * D );
} // validated`,sg=`#ifdef USE_IRIDESCENCE
	const mat3 XYZ_TO_REC709 = mat3(
		 3.2404542, -0.9692660,  0.0556434,
		-1.5371385,  1.8760108, -0.2040259,
		-0.4985314,  0.0415560,  1.0572252
	);
	vec3 Fresnel0ToIor( vec3 fresnel0 ) {
		vec3 sqrtF0 = sqrt( fresnel0 );
		return ( vec3( 1.0 ) + sqrtF0 ) / ( vec3( 1.0 ) - sqrtF0 );
	}
	vec3 IorToFresnel0( vec3 transmittedIor, float incidentIor ) {
		return pow2( ( transmittedIor - vec3( incidentIor ) ) / ( transmittedIor + vec3( incidentIor ) ) );
	}
	float IorToFresnel0( float transmittedIor, float incidentIor ) {
		return pow2( ( transmittedIor - incidentIor ) / ( transmittedIor + incidentIor ));
	}
	vec3 evalSensitivity( float OPD, vec3 shift ) {
		float phase = 2.0 * PI * OPD * 1.0e-9;
		vec3 val = vec3( 5.4856e-13, 4.4201e-13, 5.2481e-13 );
		vec3 pos = vec3( 1.6810e+06, 1.7953e+06, 2.2084e+06 );
		vec3 var = vec3( 4.3278e+09, 9.3046e+09, 6.6121e+09 );
		vec3 xyz = val * sqrt( 2.0 * PI * var ) * cos( pos * phase + shift ) * exp( - pow2( phase ) * var );
		xyz.x += 9.7470e-14 * sqrt( 2.0 * PI * 4.5282e+09 ) * cos( 2.2399e+06 * phase + shift[ 0 ] ) * exp( - 4.5282e+09 * pow2( phase ) );
		xyz /= 1.0685e-7;
		vec3 rgb = XYZ_TO_REC709 * xyz;
		return rgb;
	}
	vec3 evalIridescence( float outsideIOR, float eta2, float cosTheta1, float thinFilmThickness, vec3 baseF0 ) {
		vec3 I;
		float iridescenceIOR = mix( outsideIOR, eta2, smoothstep( 0.0, 0.03, thinFilmThickness ) );
		float sinTheta2Sq = pow2( outsideIOR / iridescenceIOR ) * ( 1.0 - pow2( cosTheta1 ) );
		float cosTheta2Sq = 1.0 - sinTheta2Sq;
		if ( cosTheta2Sq < 0.0 ) {
			return vec3( 1.0 );
		}
		float cosTheta2 = sqrt( cosTheta2Sq );
		float R0 = IorToFresnel0( iridescenceIOR, outsideIOR );
		float R12 = F_Schlick( R0, 1.0, cosTheta1 );
		float T121 = 1.0 - R12;
		float phi12 = 0.0;
		if ( iridescenceIOR < outsideIOR ) phi12 = PI;
		float phi21 = PI - phi12;
		vec3 baseIOR = Fresnel0ToIor( clamp( baseF0, 0.0, 0.9999 ) );		vec3 R1 = IorToFresnel0( baseIOR, iridescenceIOR );
		vec3 R23 = F_Schlick( R1, 1.0, cosTheta2 );
		vec3 phi23 = vec3( 0.0 );
		if ( baseIOR[ 0 ] < iridescenceIOR ) phi23[ 0 ] = PI;
		if ( baseIOR[ 1 ] < iridescenceIOR ) phi23[ 1 ] = PI;
		if ( baseIOR[ 2 ] < iridescenceIOR ) phi23[ 2 ] = PI;
		float OPD = 2.0 * iridescenceIOR * thinFilmThickness * cosTheta2;
		vec3 phi = vec3( phi21 ) + phi23;
		vec3 R123 = clamp( R12 * R23, 1e-5, 0.9999 );
		vec3 r123 = sqrt( R123 );
		vec3 Rs = pow2( T121 ) * R23 / ( vec3( 1.0 ) - R123 );
		vec3 C0 = R12 + Rs;
		I = C0;
		vec3 Cm = Rs - T121;
		for ( int m = 1; m <= 2; ++ m ) {
			Cm *= r123;
			vec3 Sm = 2.0 * evalSensitivity( float( m ) * OPD, float( m ) * phi );
			I += Cm * Sm;
		}
		return max( I, vec3( 0.0 ) );
	}
#endif`,rg=`#ifdef USE_BUMPMAP
	uniform sampler2D bumpMap;
	uniform float bumpScale;
	vec2 dHdxy_fwd() {
		vec2 dSTdx = dFdx( vBumpMapUv );
		vec2 dSTdy = dFdy( vBumpMapUv );
		float Hll = bumpScale * texture2D( bumpMap, vBumpMapUv ).x;
		float dBx = bumpScale * texture2D( bumpMap, vBumpMapUv + dSTdx ).x - Hll;
		float dBy = bumpScale * texture2D( bumpMap, vBumpMapUv + dSTdy ).x - Hll;
		return vec2( dBx, dBy );
	}
	vec3 perturbNormalArb( vec3 surf_pos, vec3 surf_norm, vec2 dHdxy, float faceDirection ) {
		vec3 vSigmaX = normalize( dFdx( surf_pos.xyz ) );
		vec3 vSigmaY = normalize( dFdy( surf_pos.xyz ) );
		vec3 vN = surf_norm;
		vec3 R1 = cross( vSigmaY, vN );
		vec3 R2 = cross( vN, vSigmaX );
		float fDet = dot( vSigmaX, R1 ) * faceDirection;
		vec3 vGrad = sign( fDet ) * ( dHdxy.x * R1 + dHdxy.y * R2 );
		return normalize( abs( fDet ) * surf_norm - vGrad );
	}
#endif`,og=`#if NUM_CLIPPING_PLANES > 0
	vec4 plane;
	#ifdef ALPHA_TO_COVERAGE
		float distanceToPlane, distanceGradient;
		float clipOpacity = 1.0;
		#pragma unroll_loop_start
		for ( int i = 0; i < UNION_CLIPPING_PLANES; i ++ ) {
			plane = clippingPlanes[ i ];
			distanceToPlane = - dot( vClipPosition, plane.xyz ) + plane.w;
			distanceGradient = fwidth( distanceToPlane ) / 2.0;
			clipOpacity *= smoothstep( - distanceGradient, distanceGradient, distanceToPlane );
			if ( clipOpacity == 0.0 ) discard;
		}
		#pragma unroll_loop_end
		#if UNION_CLIPPING_PLANES < NUM_CLIPPING_PLANES
			float unionClipOpacity = 1.0;
			#pragma unroll_loop_start
			for ( int i = UNION_CLIPPING_PLANES; i < NUM_CLIPPING_PLANES; i ++ ) {
				plane = clippingPlanes[ i ];
				distanceToPlane = - dot( vClipPosition, plane.xyz ) + plane.w;
				distanceGradient = fwidth( distanceToPlane ) / 2.0;
				unionClipOpacity *= 1.0 - smoothstep( - distanceGradient, distanceGradient, distanceToPlane );
			}
			#pragma unroll_loop_end
			clipOpacity *= 1.0 - unionClipOpacity;
		#endif
		diffuseColor.a *= clipOpacity;
		if ( diffuseColor.a == 0.0 ) discard;
	#else
		#pragma unroll_loop_start
		for ( int i = 0; i < UNION_CLIPPING_PLANES; i ++ ) {
			plane = clippingPlanes[ i ];
			if ( dot( vClipPosition, plane.xyz ) > plane.w ) discard;
		}
		#pragma unroll_loop_end
		#if UNION_CLIPPING_PLANES < NUM_CLIPPING_PLANES
			bool clipped = true;
			#pragma unroll_loop_start
			for ( int i = UNION_CLIPPING_PLANES; i < NUM_CLIPPING_PLANES; i ++ ) {
				plane = clippingPlanes[ i ];
				clipped = ( dot( vClipPosition, plane.xyz ) > plane.w ) && clipped;
			}
			#pragma unroll_loop_end
			if ( clipped ) discard;
		#endif
	#endif
#endif`,ag=`#if NUM_CLIPPING_PLANES > 0
	varying vec3 vClipPosition;
	uniform vec4 clippingPlanes[ NUM_CLIPPING_PLANES ];
#endif`,lg=`#if NUM_CLIPPING_PLANES > 0
	varying vec3 vClipPosition;
#endif`,cg=`#if NUM_CLIPPING_PLANES > 0
	vClipPosition = - mvPosition.xyz;
#endif`,hg=`#if defined( USE_COLOR ) || defined( USE_COLOR_ALPHA )
	diffuseColor *= vColor;
#endif`,ug=`#if defined( USE_COLOR ) || defined( USE_COLOR_ALPHA )
	varying vec4 vColor;
#endif`,dg=`#if defined( USE_COLOR ) || defined( USE_COLOR_ALPHA ) || defined( USE_INSTANCING_COLOR ) || defined( USE_BATCHING_COLOR )
	varying vec4 vColor;
#endif`,fg=`#if defined( USE_COLOR ) || defined( USE_COLOR_ALPHA ) || defined( USE_INSTANCING_COLOR ) || defined( USE_BATCHING_COLOR )
	vColor = vec4( 1.0 );
#endif
#ifdef USE_COLOR_ALPHA
	vColor *= color;
#elif defined( USE_COLOR )
	vColor.rgb *= color;
#endif
#ifdef USE_INSTANCING_COLOR
	vColor.rgb *= instanceColor.rgb;
#endif
#ifdef USE_BATCHING_COLOR
	vColor *= getBatchingColor( getIndirectIndex( gl_DrawID ) );
#endif`,pg=`#define PI 3.141592653589793
#define PI2 6.283185307179586
#define PI_HALF 1.5707963267948966
#define RECIPROCAL_PI 0.3183098861837907
#define RECIPROCAL_PI2 0.15915494309189535
#define EPSILON 1e-6
#ifndef saturate
#define saturate( a ) clamp( a, 0.0, 1.0 )
#endif
#define whiteComplement( a ) ( 1.0 - saturate( a ) )
float pow2( const in float x ) { return x*x; }
vec3 pow2( const in vec3 x ) { return x*x; }
float pow3( const in float x ) { return x*x*x; }
float pow4( const in float x ) { float x2 = x*x; return x2*x2; }
float max3( const in vec3 v ) { return max( max( v.x, v.y ), v.z ); }
float average( const in vec3 v ) { return dot( v, vec3( 0.3333333 ) ); }
highp float rand( const in vec2 uv ) {
	const highp float a = 12.9898, b = 78.233, c = 43758.5453;
	highp float dt = dot( uv.xy, vec2( a,b ) ), sn = mod( dt, PI );
	return fract( sin( sn ) * c );
}
#ifdef HIGH_PRECISION
	float precisionSafeLength( vec3 v ) { return length( v ); }
#else
	float precisionSafeLength( vec3 v ) {
		float maxComponent = max3( abs( v ) );
		return length( v / maxComponent ) * maxComponent;
	}
#endif
struct IncidentLight {
	vec3 color;
	vec3 direction;
	bool visible;
};
struct ReflectedLight {
	vec3 directDiffuse;
	vec3 directSpecular;
	vec3 indirectDiffuse;
	vec3 indirectSpecular;
};
#ifdef USE_ALPHAHASH
	varying vec3 vPosition;
#endif
vec3 transformDirection( in vec3 dir, in mat4 matrix ) {
	return normalize( ( matrix * vec4( dir, 0.0 ) ).xyz );
}
#define inverseTransformDirection transformDirectionByInverseViewMatrix
vec3 transformNormalByInverseViewMatrix( in vec3 normal, in mat4 viewMatrix ) {
	return normalize( ( vec4( normal, 0.0 ) * viewMatrix ).xyz );
}
vec3 transformDirectionByInverseViewMatrix( in vec3 dir, in mat4 viewMatrix ) {
	return normalize( ( vec4( dir, 0.0 ) * viewMatrix ).xyz );
}
bool isPerspectiveMatrix( mat4 m ) {
	return m[ 2 ][ 3 ] == - 1.0;
}
vec2 equirectUv( in vec3 dir ) {
	float u = atan( dir.z, dir.x ) * RECIPROCAL_PI2 + 0.5;
	float v = asin( clamp( dir.y, - 1.0, 1.0 ) ) * RECIPROCAL_PI + 0.5;
	return vec2( u, v );
}
vec3 BRDF_Lambert( const in vec3 diffuseColor ) {
	return RECIPROCAL_PI * diffuseColor;
}
vec3 F_Schlick( const in vec3 f0, const in float f90, const in float dotVH ) {
	float fresnel = exp2( ( - 5.55473 * dotVH - 6.98316 ) * dotVH );
	return f0 * ( 1.0 - fresnel ) + ( f90 * fresnel );
}
float F_Schlick( const in float f0, const in float f90, const in float dotVH ) {
	float fresnel = exp2( ( - 5.55473 * dotVH - 6.98316 ) * dotVH );
	return f0 * ( 1.0 - fresnel ) + ( f90 * fresnel );
} // validated`,mg=`vec3 transformedNormal = objectNormal;
#ifdef USE_TANGENT
	vec3 transformedTangent = objectTangent;
#endif
#ifdef USE_BATCHING
	mat3 bm = mat3( batchingMatrix );
	transformedNormal /= vec3( dot( bm[ 0 ], bm[ 0 ] ), dot( bm[ 1 ], bm[ 1 ] ), dot( bm[ 2 ], bm[ 2 ] ) );
	transformedNormal = bm * transformedNormal;
	#ifdef USE_TANGENT
		transformedTangent = bm * transformedTangent;
	#endif
#endif
#ifdef USE_INSTANCING
	mat3 im = mat3( instanceMatrix );
	transformedNormal /= vec3( dot( im[ 0 ], im[ 0 ] ), dot( im[ 1 ], im[ 1 ] ), dot( im[ 2 ], im[ 2 ] ) );
	transformedNormal = im * transformedNormal;
	#ifdef USE_TANGENT
		transformedTangent = im * transformedTangent;
	#endif
#endif
transformedNormal = normalMatrix * transformedNormal;
#ifdef FLIP_SIDED
	transformedNormal = - transformedNormal;
#endif
#ifdef USE_TANGENT
	transformedTangent = ( modelViewMatrix * vec4( transformedTangent, 0.0 ) ).xyz;
#endif`,gg=`#ifdef USE_DISPLACEMENTMAP
	uniform sampler2D displacementMap;
	uniform float displacementScale;
	uniform float displacementBias;
#endif`,xg=`#ifdef USE_DISPLACEMENTMAP
	transformed += normalize( objectNormal ) * ( texture2D( displacementMap, vDisplacementMapUv ).x * displacementScale + displacementBias );
#endif`,_g=`#ifdef USE_EMISSIVEMAP
	vec4 emissiveColor = texture2D( emissiveMap, vEmissiveMapUv );
	#ifdef DECODE_VIDEO_TEXTURE_EMISSIVE
		emissiveColor = sRGBTransferEOTF( emissiveColor );
	#endif
	totalEmissiveRadiance *= emissiveColor.rgb;
#endif`,vg=`#ifdef USE_EMISSIVEMAP
	uniform sampler2D emissiveMap;
#endif`,yg="gl_FragColor = linearToOutputTexel( gl_FragColor );",Mg=`vec4 LinearTransferOETF( in vec4 value ) {
	return value;
}
vec4 sRGBTransferEOTF( in vec4 value ) {
	return vec4( mix( pow( value.rgb * 0.9478672986 + vec3( 0.0521327014 ), vec3( 2.4 ) ), value.rgb * 0.0773993808, vec3( lessThanEqual( value.rgb, vec3( 0.04045 ) ) ) ), value.a );
}
vec4 sRGBTransferOETF( in vec4 value ) {
	return vec4( mix( pow( value.rgb, vec3( 0.41666 ) ) * 1.055 - vec3( 0.055 ), value.rgb * 12.92, vec3( lessThanEqual( value.rgb, vec3( 0.0031308 ) ) ) ), value.a );
}`,Sg=`#ifdef USE_ENVMAP
	#ifdef ENV_WORLDPOS
		vec3 cameraToFrag;
		if ( isOrthographic ) {
			cameraToFrag = normalize( vec3( - viewMatrix[ 0 ][ 2 ], - viewMatrix[ 1 ][ 2 ], - viewMatrix[ 2 ][ 2 ] ) );
		} else {
			cameraToFrag = normalize( vWorldPosition - cameraPosition );
		}
		vec3 worldNormal = transformNormalByInverseViewMatrix( normal, viewMatrix );
		#ifdef ENVMAP_MODE_REFLECTION
			vec3 reflectVec = reflect( cameraToFrag, worldNormal );
		#else
			vec3 reflectVec = refract( cameraToFrag, worldNormal, refractionRatio );
		#endif
	#else
		vec3 reflectVec = vReflect;
	#endif
	#ifdef ENVMAP_TYPE_CUBE
		vec4 envColor = textureCube( envMap, envMapRotation * reflectVec );
		#ifdef ENVMAP_BLENDING_MULTIPLY
			outgoingLight = mix( outgoingLight, outgoingLight * envColor.xyz, specularStrength * reflectivity );
		#elif defined( ENVMAP_BLENDING_MIX )
			outgoingLight = mix( outgoingLight, envColor.xyz, specularStrength * reflectivity );
		#elif defined( ENVMAP_BLENDING_ADD )
			outgoingLight += envColor.xyz * specularStrength * reflectivity;
		#endif
	#endif
#endif`,bg=`#ifdef USE_ENVMAP
	uniform float envMapIntensity;
	uniform mat3 envMapRotation;
	uniform samplerCube envMap;
	#ifdef ENVMAP_TYPE_PMREM
		float roughnessToMip( const in float roughness ) {
			float r = clamp( roughness, 0.0, 1.0 );
			return ENVMAP_MAX_LOD * r * ( 2.0 - r );
		}
	#endif
#endif`,wg=`#ifdef USE_ENVMAP
	uniform float reflectivity;
	#if defined( USE_BUMPMAP ) || defined( USE_NORMALMAP ) || defined( PHONG ) || defined( LAMBERT )
		#define ENV_WORLDPOS
	#endif
	#ifdef ENV_WORLDPOS
		varying vec3 vWorldPosition;
		uniform float refractionRatio;
	#else
		varying vec3 vReflect;
	#endif
#endif`,Eg=`#ifdef USE_ENVMAP
	#if defined( USE_BUMPMAP ) || defined( USE_NORMALMAP ) || defined( PHONG ) || defined( LAMBERT )
		#define ENV_WORLDPOS
	#endif
	#ifdef ENV_WORLDPOS

		varying vec3 vWorldPosition;
	#else
		varying vec3 vReflect;
		uniform float refractionRatio;
	#endif
#endif`,Tg=`#ifdef USE_ENVMAP
	#ifdef ENV_WORLDPOS
		vWorldPosition = worldPosition.xyz;
	#else
		vec3 cameraToVertex;
		if ( isOrthographic ) {
			cameraToVertex = normalize( vec3( - viewMatrix[ 0 ][ 2 ], - viewMatrix[ 1 ][ 2 ], - viewMatrix[ 2 ][ 2 ] ) );
		} else {
			cameraToVertex = normalize( worldPosition.xyz - cameraPosition );
		}
		vec3 worldNormal = transformNormalByInverseViewMatrix( transformedNormal, viewMatrix );
		#ifdef ENVMAP_MODE_REFLECTION
			vReflect = reflect( cameraToVertex, worldNormal );
		#else
			vReflect = refract( cameraToVertex, worldNormal, refractionRatio );
		#endif
	#endif
#endif`,Ag=`#ifdef USE_FOG
	vFogDepth = - mvPosition.z;
#endif`,Rg=`#ifdef USE_FOG
	varying float vFogDepth;
#endif`,Cg=`#ifdef USE_FOG
	#ifdef FOG_EXP2
		float fogFactor = 1.0 - exp( - fogDensity * fogDensity * vFogDepth * vFogDepth );
	#else
		float fogFactor = smoothstep( fogNear, fogFar, vFogDepth );
	#endif
	gl_FragColor.rgb = mix( gl_FragColor.rgb, fogColor, fogFactor );
#endif`,Pg=`#ifdef USE_FOG
	uniform vec3 fogColor;
	varying float vFogDepth;
	#ifdef FOG_EXP2
		uniform float fogDensity;
	#else
		uniform float fogNear;
		uniform float fogFar;
	#endif
#endif`,Ig=`#ifdef USE_GRADIENTMAP
	uniform sampler2D gradientMap;
#endif
vec3 getGradientIrradiance( vec3 normal, vec3 lightDirection ) {
	float dotNL = dot( normal, lightDirection );
	vec2 coord = vec2( dotNL * 0.5 + 0.5, 0.0 );
	#ifdef USE_GRADIENTMAP
		return vec3( texture2D( gradientMap, coord ).r );
	#else
		vec2 fw = fwidth( coord ) * 0.5;
		return mix( vec3( 0.7 ), vec3( 1.0 ), smoothstep( 0.7 - fw.x, 0.7 + fw.x, coord.x ) );
	#endif
}`,Dg=`#ifdef USE_LIGHTMAP
	uniform sampler2D lightMap;
	uniform float lightMapIntensity;
#endif`,Lg=`LambertMaterial material;
material.diffuseColor = diffuseColor.rgb;
material.specularStrength = specularStrength;`,Ng=`varying vec3 vViewPosition;
struct LambertMaterial {
	vec3 diffuseColor;
	float specularStrength;
};
void RE_Direct_Lambert( const in IncidentLight directLight, const in vec3 geometryPosition, const in vec3 geometryNormal, const in vec3 geometryViewDir, const in vec3 geometryClearcoatNormal, const in LambertMaterial material, inout ReflectedLight reflectedLight ) {
	float dotNL = saturate( dot( geometryNormal, directLight.direction ) );
	vec3 irradiance = dotNL * directLight.color;
	reflectedLight.directDiffuse += irradiance * BRDF_Lambert( material.diffuseColor );
}
void RE_IndirectDiffuse_Lambert( const in vec3 irradiance, const in vec3 geometryPosition, const in vec3 geometryNormal, const in vec3 geometryViewDir, const in vec3 geometryClearcoatNormal, const in LambertMaterial material, inout ReflectedLight reflectedLight ) {
	reflectedLight.indirectDiffuse += irradiance * BRDF_Lambert( material.diffuseColor );
}
#define RE_Direct				RE_Direct_Lambert
#define RE_IndirectDiffuse		RE_IndirectDiffuse_Lambert`,Ug=`uniform bool receiveShadow;
uniform vec3 ambientLightColor;
#if defined( USE_LIGHT_PROBES )
	uniform vec3 lightProbe[ 9 ];
#endif
vec3 shGetIrradianceAt( in vec3 normal, in vec3 shCoefficients[ 9 ] ) {
	float x = normal.x, y = normal.y, z = normal.z;
	vec3 result = shCoefficients[ 0 ] * 0.886227;
	result += shCoefficients[ 1 ] * 2.0 * 0.511664 * y;
	result += shCoefficients[ 2 ] * 2.0 * 0.511664 * z;
	result += shCoefficients[ 3 ] * 2.0 * 0.511664 * x;
	result += shCoefficients[ 4 ] * 2.0 * 0.429043 * x * y;
	result += shCoefficients[ 5 ] * 2.0 * 0.429043 * y * z;
	result += shCoefficients[ 6 ] * ( 0.743125 * z * z - 0.247708 );
	result += shCoefficients[ 7 ] * 2.0 * 0.429043 * x * z;
	result += shCoefficients[ 8 ] * 0.429043 * ( x * x - y * y );
	return result;
}
vec3 getLightProbeIrradiance( const in vec3 lightProbe[ 9 ], const in vec3 normal ) {
	vec3 worldNormal = transformNormalByInverseViewMatrix( normal, viewMatrix );
	vec3 irradiance = shGetIrradianceAt( worldNormal, lightProbe );
	return irradiance;
}
vec3 getAmbientLightIrradiance( const in vec3 ambientLightColor ) {
	vec3 irradiance = ambientLightColor;
	return irradiance;
}
float getDistanceAttenuation( const in float lightDistance, const in float cutoffDistance, const in float decayExponent ) {
	float distanceFalloff = 1.0 / max( pow( lightDistance, decayExponent ), 0.01 );
	if ( cutoffDistance > 0.0 ) {
		distanceFalloff *= pow2( saturate( 1.0 - pow4( lightDistance / cutoffDistance ) ) );
	}
	return distanceFalloff;
}
float getSpotAttenuation( const in float coneCosine, const in float penumbraCosine, const in float angleCosine ) {
	return smoothstep( coneCosine, penumbraCosine, angleCosine );
}
#if NUM_SUN_LIGHTS > 0
	struct SunLight {
		vec3 direction;
		vec3 color;
	};
	uniform SunLight sunLights[ NUM_SUN_LIGHTS ];
	void getSunLightInfo( const in SunLight sunLight, out IncidentLight light ) {
		light.color = sunLight.color;
		light.direction = sunLight.direction;
		light.visible = true;
	}
#endif
#if NUM_DIR_LIGHTS > 0
	struct DirectionalLight {
		vec3 direction;
		vec3 color;
	};
	uniform DirectionalLight directionalLights[ NUM_DIR_LIGHTS ];
	void getDirectionalLightInfo( const in DirectionalLight directionalLight, out IncidentLight light ) {
		light.color = directionalLight.color;
		light.direction = directionalLight.direction;
		light.visible = true;
	}
#endif
#if NUM_POINT_LIGHTS > 0
	struct PointLight {
		vec3 position;
		vec3 color;
		float distance;
		float decay;
	};
	uniform PointLight pointLights[ NUM_POINT_LIGHTS ];
	void getPointLightInfo( const in PointLight pointLight, const in vec3 geometryPosition, out IncidentLight light ) {
		vec3 lVector = pointLight.position - geometryPosition;
		light.direction = normalize( lVector );
		float lightDistance = length( lVector );
		light.color = pointLight.color;
		light.color *= getDistanceAttenuation( lightDistance, pointLight.distance, pointLight.decay );
		light.visible = ( light.color != vec3( 0.0 ) );
	}
#endif
#if NUM_SPOT_LIGHTS > 0
	struct SpotLight {
		vec3 position;
		vec3 direction;
		vec3 color;
		float distance;
		float decay;
		float coneCos;
		float penumbraCos;
	};
	uniform SpotLight spotLights[ NUM_SPOT_LIGHTS ];
	void getSpotLightInfo( const in SpotLight spotLight, const in vec3 geometryPosition, out IncidentLight light ) {
		vec3 lVector = spotLight.position - geometryPosition;
		light.direction = normalize( lVector );
		float angleCos = dot( light.direction, spotLight.direction );
		float spotAttenuation = getSpotAttenuation( spotLight.coneCos, spotLight.penumbraCos, angleCos );
		if ( spotAttenuation > 0.0 ) {
			float lightDistance = length( lVector );
			light.color = spotLight.color * spotAttenuation;
			light.color *= getDistanceAttenuation( lightDistance, spotLight.distance, spotLight.decay );
			light.visible = ( light.color != vec3( 0.0 ) );
		} else {
			light.color = vec3( 0.0 );
			light.visible = false;
		}
	}
#endif
#if NUM_RECT_AREA_LIGHTS > 0
	struct RectAreaLight {
		vec3 color;
		vec3 position;
		vec3 halfWidth;
		vec3 halfHeight;
	};
	uniform sampler2D ltc_1;	uniform sampler2D ltc_2;
	uniform RectAreaLight rectAreaLights[ NUM_RECT_AREA_LIGHTS ];
#endif
#if NUM_HEMI_LIGHTS > 0
	struct HemisphereLight {
		vec3 direction;
		vec3 skyColor;
		vec3 groundColor;
	};
	uniform HemisphereLight hemisphereLights[ NUM_HEMI_LIGHTS ];
	vec3 getHemisphereLightIrradiance( const in HemisphereLight hemiLight, const in vec3 normal ) {
		float dotNL = dot( normal, hemiLight.direction );
		float hemiDiffuseWeight = 0.5 * dotNL + 0.5;
		vec3 irradiance = mix( hemiLight.groundColor, hemiLight.skyColor, hemiDiffuseWeight );
		return irradiance;
	}
#endif
#include <lightprobes_pars_fragment>`,Fg=`#ifdef USE_ENVMAP
	vec3 getIBLIrradiance( const in vec3 normal ) {
		#ifdef ENVMAP_TYPE_PMREM
			vec3 worldNormal = transformNormalByInverseViewMatrix( normal, viewMatrix );
			vec4 envMapColor = textureLod( envMap, envMapRotation * worldNormal, ENVMAP_MAX_LOD );
			return PI * envMapColor.rgb * envMapIntensity;
		#else
			return vec3( 0.0 );
		#endif
	}
	vec3 getIBLRadiance( const in vec3 viewDir, const in vec3 normal, const in float roughness ) {
		#ifdef ENVMAP_TYPE_PMREM
			vec3 reflectVec = reflect( - viewDir, normal );
			reflectVec = normalize( mix( reflectVec, normal, pow4( roughness ) ) );
			reflectVec = transformDirectionByInverseViewMatrix( reflectVec, viewMatrix );
			vec4 envMapColor = textureLod( envMap, envMapRotation * reflectVec, roughnessToMip( roughness ) );
			return envMapColor.rgb * envMapIntensity;
		#else
			return vec3( 0.0 );
		#endif
	}
	#ifdef USE_RETROREFLECTION
		vec3 getIBLRetroRadiance( const in vec3 viewDir, const in vec3 normal, const in float roughness ) {
			#ifdef ENVMAP_TYPE_PMREM
				vec3 retroVec = normalize( mix( viewDir, normal, pow4( roughness ) ) );
				retroVec = transformDirectionByInverseViewMatrix( retroVec, viewMatrix );
				vec4 envMapColor = textureLod( envMap, envMapRotation * retroVec, roughnessToMip( roughness ) );
				return envMapColor.rgb * envMapIntensity;
			#else
				return vec3( 0.0 );
			#endif
		}
	#endif
	#ifdef USE_ANISOTROPY
		vec3 getIBLAnisotropyRadiance( const in vec3 viewDir, const in vec3 normal, const in float roughness, const in vec3 bitangent, const in float anisotropy ) {
			#ifdef ENVMAP_TYPE_PMREM
				vec3 bentNormal = cross( bitangent, viewDir );
				bentNormal = normalize( cross( bentNormal, bitangent ) );
				bentNormal = normalize( mix( bentNormal, normal, pow2( pow2( 1.0 - anisotropy * ( 1.0 - roughness ) ) ) ) );
				return getIBLRadiance( viewDir, bentNormal, roughness );
			#else
				return vec3( 0.0 );
			#endif
		}
		#ifdef USE_RETROREFLECTION
			vec3 getIBLAnisotropyRetroRadiance( const in vec3 viewDir, const in vec3 normal, const in float roughness, const in vec3 bitangent, const in float anisotropy ) {
				#ifdef ENVMAP_TYPE_PMREM
					vec3 bentNormal = cross( bitangent, viewDir );
					bentNormal = normalize( cross( bentNormal, bitangent ) );
					bentNormal = normalize( mix( bentNormal, normal, pow2( pow2( 1.0 - anisotropy * ( 1.0 - roughness ) ) ) ) );
					return getIBLRetroRadiance( viewDir, bentNormal, roughness );
				#else
					return vec3( 0.0 );
				#endif
			}
		#endif
	#endif
#endif`,Og=`ToonMaterial material;
material.diffuseColor = diffuseColor.rgb;`,Bg=`varying vec3 vViewPosition;
struct ToonMaterial {
	vec3 diffuseColor;
};
void RE_Direct_Toon( const in IncidentLight directLight, const in vec3 geometryPosition, const in vec3 geometryNormal, const in vec3 geometryViewDir, const in vec3 geometryClearcoatNormal, const in ToonMaterial material, inout ReflectedLight reflectedLight ) {
	vec3 irradiance = getGradientIrradiance( geometryNormal, directLight.direction ) * directLight.color;
	reflectedLight.directDiffuse += irradiance * BRDF_Lambert( material.diffuseColor );
}
void RE_IndirectDiffuse_Toon( const in vec3 irradiance, const in vec3 geometryPosition, const in vec3 geometryNormal, const in vec3 geometryViewDir, const in vec3 geometryClearcoatNormal, const in ToonMaterial material, inout ReflectedLight reflectedLight ) {
	reflectedLight.indirectDiffuse += irradiance * BRDF_Lambert( material.diffuseColor );
}
#define RE_Direct				RE_Direct_Toon
#define RE_IndirectDiffuse		RE_IndirectDiffuse_Toon`,zg=`BlinnPhongMaterial material;
material.diffuseColor = diffuseColor.rgb;
material.specularColor = specular;
material.specularShininess = shininess;
material.specularStrength = specularStrength;`,kg=`varying vec3 vViewPosition;
struct BlinnPhongMaterial {
	vec3 diffuseColor;
	vec3 specularColor;
	float specularShininess;
	float specularStrength;
};
void RE_Direct_BlinnPhong( const in IncidentLight directLight, const in vec3 geometryPosition, const in vec3 geometryNormal, const in vec3 geometryViewDir, const in vec3 geometryClearcoatNormal, const in BlinnPhongMaterial material, inout ReflectedLight reflectedLight ) {
	float dotNL = saturate( dot( geometryNormal, directLight.direction ) );
	vec3 irradiance = dotNL * directLight.color;
	reflectedLight.directDiffuse += irradiance * BRDF_Lambert( material.diffuseColor );
	reflectedLight.directSpecular += irradiance * BRDF_BlinnPhong( directLight.direction, geometryViewDir, geometryNormal, material.specularColor, material.specularShininess ) * material.specularStrength;
}
void RE_IndirectDiffuse_BlinnPhong( const in vec3 irradiance, const in vec3 geometryPosition, const in vec3 geometryNormal, const in vec3 geometryViewDir, const in vec3 geometryClearcoatNormal, const in BlinnPhongMaterial material, inout ReflectedLight reflectedLight ) {
	reflectedLight.indirectDiffuse += irradiance * BRDF_Lambert( material.diffuseColor );
}
#define RE_Direct				RE_Direct_BlinnPhong
#define RE_IndirectDiffuse		RE_IndirectDiffuse_BlinnPhong`,Hg=`PhysicalMaterial material;
material.diffuseColor = diffuseColor.rgb;
material.diffuseContribution = diffuseColor.rgb * ( 1.0 - metalnessFactor );
material.metalness = metalnessFactor;
vec3 dxy = max( abs( dFdx( nonPerturbedNormal ) ), abs( dFdy( nonPerturbedNormal ) ) );
float geometryRoughness = max( max( dxy.x, dxy.y ), dxy.z );
material.roughness = max( roughnessFactor, 0.045 );
material.roughness += geometryRoughness;
material.roughness = min( material.roughness, 1.0 );
#ifdef USE_DIFFUSE_ROUGHNESS
	float diffuseRoughnessFactor = diffuseRoughness;
	#ifdef USE_DIFFUSE_ROUGHNESSMAP
		diffuseRoughnessFactor *= texture2D( diffuseRoughnessMap, vDiffuseRoughnessMapUv ).r;
	#endif
	material.diffuseRoughness = saturate( diffuseRoughnessFactor );
#endif
#ifdef IOR
	material.ior = ior;
	#ifdef USE_SPECULAR
		float specularIntensityFactor = specularIntensity;
		vec3 specularColorFactor = specularColor;
		#ifdef USE_SPECULAR_COLORMAP
			specularColorFactor *= texture2D( specularColorMap, vSpecularColorMapUv ).rgb;
		#endif
		#ifdef USE_SPECULAR_INTENSITYMAP
			specularIntensityFactor *= texture2D( specularIntensityMap, vSpecularIntensityMapUv ).a;
		#endif
		material.specularF90 = mix( specularIntensityFactor, 1.0, metalnessFactor );
	#else
		float specularIntensityFactor = 1.0;
		vec3 specularColorFactor = vec3( 1.0 );
		material.specularF90 = 1.0;
	#endif
	material.specularColor = min( pow2( ( material.ior - 1.0 ) / ( material.ior + 1.0 ) ) * specularColorFactor, vec3( 1.0 ) ) * specularIntensityFactor;
	material.specularColorBlended = mix( material.specularColor, diffuseColor.rgb, metalnessFactor );
#else
	material.specularColor = vec3( 0.04 );
	material.specularColorBlended = mix( material.specularColor, diffuseColor.rgb, metalnessFactor );
	material.specularF90 = 1.0;
#endif
#ifdef USE_CLEARCOAT
	material.clearcoat = clearcoat;
	material.clearcoatRoughness = clearcoatRoughness;
	material.clearcoatF0 = vec3( 0.04 );
	material.clearcoatF90 = 1.0;
	#ifdef USE_CLEARCOATMAP
		material.clearcoat *= texture2D( clearcoatMap, vClearcoatMapUv ).x;
	#endif
	#ifdef USE_CLEARCOAT_ROUGHNESSMAP
		material.clearcoatRoughness *= texture2D( clearcoatRoughnessMap, vClearcoatRoughnessMapUv ).y;
	#endif
	material.clearcoat = saturate( material.clearcoat );	material.clearcoatRoughness = max( material.clearcoatRoughness, 0.045 );
	material.clearcoatRoughness += geometryRoughness;
	material.clearcoatRoughness = min( material.clearcoatRoughness, 1.0 );
#endif
#ifdef USE_DISPERSION
	material.dispersion = dispersion;
#endif
#ifdef USE_RETROREFLECTION
	material.retroreflectivity = retroreflectivity;
#endif
#ifdef USE_IRIDESCENCE
	material.iridescence = iridescence;
	material.iridescenceIOR = iridescenceIOR;
	#ifdef USE_IRIDESCENCEMAP
		material.iridescence *= texture2D( iridescenceMap, vIridescenceMapUv ).r;
	#endif
	#ifdef USE_IRIDESCENCE_THICKNESSMAP
		material.iridescenceThickness = (iridescenceThicknessMaximum - iridescenceThicknessMinimum) * texture2D( iridescenceThicknessMap, vIridescenceThicknessMapUv ).g + iridescenceThicknessMinimum;
	#else
		material.iridescenceThickness = iridescenceThicknessMaximum;
	#endif
#endif
#ifdef USE_SHEEN
	material.sheenColor = sheenColor;
	#ifdef USE_SHEEN_COLORMAP
		material.sheenColor *= texture2D( sheenColorMap, vSheenColorMapUv ).rgb;
	#endif
	material.sheenRoughness = clamp( sheenRoughness, 0.0001, 1.0 );
	#ifdef USE_SHEEN_ROUGHNESSMAP
		material.sheenRoughness *= texture2D( sheenRoughnessMap, vSheenRoughnessMapUv ).a;
	#endif
#endif
#ifdef USE_ANISOTROPY
	#ifdef USE_ANISOTROPYMAP
		mat2 anisotropyMat = mat2( anisotropyVector.x, anisotropyVector.y, - anisotropyVector.y, anisotropyVector.x );
		vec3 anisotropyPolar = texture2D( anisotropyMap, vAnisotropyMapUv ).rgb;
		vec2 anisotropyV = anisotropyMat * normalize( 2.0 * anisotropyPolar.rg - vec2( 1.0 ) ) * anisotropyPolar.b;
	#else
		vec2 anisotropyV = anisotropyVector;
	#endif
	material.anisotropy = length( anisotropyV );
	if( material.anisotropy == 0.0 ) {
		anisotropyV = vec2( 1.0, 0.0 );
	} else {
		anisotropyV /= material.anisotropy;
		material.anisotropy = saturate( material.anisotropy );
	}
	material.alphaT = mix( pow2( material.roughness ), 1.0, pow2( material.anisotropy ) );
	material.anisotropyT = tbn[ 0 ] * anisotropyV.x + tbn[ 1 ] * anisotropyV.y;
	material.anisotropyB = tbn[ 1 ] * anisotropyV.x - tbn[ 0 ] * anisotropyV.y;
#endif`,Vg=`uniform sampler2D dfgLUT;
struct PhysicalMaterial {
	vec3 diffuseColor;
	vec3 diffuseContribution;
	vec3 specularColor;
	vec3 specularColorBlended;
	float roughness;
	float metalness;
	float specularF90;
	float dispersion;
	vec2 dfg;
	vec3 multiScatteringCompensation;
	#ifdef USE_DIFFUSE_ROUGHNESS
		float diffuseRoughness;
	#endif
	#ifdef USE_RETROREFLECTION
		float retroreflectivity;
	#endif
	#ifdef USE_CLEARCOAT
		float clearcoat;
		float clearcoatRoughness;
		vec3 clearcoatF0;
		float clearcoatF90;
	#endif
	#ifdef USE_IRIDESCENCE
		float iridescence;
		float iridescenceIOR;
		float iridescenceThickness;
		vec3 iridescenceFresnel;
		vec3 iridescenceF0Dielectric;
		vec3 iridescenceF0Metallic;
	#endif
	#ifdef USE_SHEEN
		vec3 sheenColor;
		float sheenRoughness;
	#endif
	#ifdef IOR
		float ior;
	#endif
	#ifdef USE_TRANSMISSION
		float transmission;
		float transmissionAlpha;
		float thickness;
		float attenuationDistance;
		vec3 attenuationColor;
	#endif
	#ifdef USE_ANISOTROPY
		float anisotropy;
		float alphaT;
		vec3 anisotropyT;
		vec3 anisotropyB;
	#endif
};
vec3 clearcoatSpecularDirect = vec3( 0.0 );
vec3 clearcoatSpecularIndirect = vec3( 0.0 );
vec3 sheenSpecularDirect = vec3( 0.0 );
vec3 sheenSpecularIndirect = vec3(0.0 );
vec3 Schlick_to_F0( const in vec3 f, const in float f90, const in float dotVH ) {
    float x = clamp( 1.0 - dotVH, 0.0, 1.0 );
    float x2 = x * x;
    float x5 = clamp( x * x2 * x2, 0.0, 0.9999 );
    return ( f - vec3( f90 ) * x5 ) / ( 1.0 - x5 );
}
#ifdef USE_DIFFUSE_ROUGHNESS
	const float EON_EPSILON = 1e-7;
	float FON_DirectionalAlbedo( const in float mu, const in float roughness, const in float A ) {
		float muComp = 1.0 - mu;
		float gOverPi = muComp * ( 0.0571085289 + muComp * ( 0.491881867 + muComp * ( - 0.332181442 + muComp * 0.0714429953 ) ) );
		return A * ( 1.0 + roughness * gOverPi );
	}
	vec3 BRDF_EON( const in vec3 lightDir, const in vec3 viewDir, const in vec3 normal, const in vec3 diffuseColor, const in float roughness ) {
		vec3 rho = saturate( diffuseColor );
		if ( roughness <= EON_EPSILON ) return BRDF_Lambert( rho );
		float dotNL = saturate( dot( normal, lightDir ) );
		float dotNV = saturate( dot( normal, viewDir ) );
		float s = dot( lightDir, viewDir ) - dotNL * dotNV;
		float sOverT = ( s > 0.0 ) ? s / max( max( dotNL, dotNV ), EON_EPSILON ) : s;
		float A = 1.0 / ( 1.0 + ( 0.5 - 2.0 / ( 3.0 * PI ) ) * roughness );
		vec3 singleScatter = rho * RECIPROCAL_PI * A * ( 1.0 + roughness * sOverT );
		float averageAlbedo = A * ( 1.0 + ( 2.0 / 3.0 - 28.0 / ( 15.0 * PI ) ) * roughness );
		float albedoV = FON_DirectionalAlbedo( dotNV, roughness, A );
		float albedoL = FON_DirectionalAlbedo( dotNL, roughness, A );
		vec3 rhoMultiScatter = rho * rho * averageAlbedo / max( vec3( EON_EPSILON ), vec3( 1.0 ) - rho * ( 1.0 - averageAlbedo ) );
		vec3 multiScatter = rhoMultiScatter * RECIPROCAL_PI
			* max( EON_EPSILON, 1.0 - albedoV )
			* max( EON_EPSILON, 1.0 - albedoL )
			/ max( EON_EPSILON, 1.0 - averageAlbedo );
		return singleScatter + multiScatter;
	}
	vec3 EON_DirectionalAlbedo( const in vec3 diffuseColor, const in float roughness, const in float dotNV ) {
		vec3 rho = saturate( diffuseColor );
		if ( roughness <= EON_EPSILON ) return rho;
		float A = 1.0 / ( 1.0 + ( 0.5 - 2.0 / ( 3.0 * PI ) ) * roughness );
		float directionalAlbedo = FON_DirectionalAlbedo( dotNV, roughness, A );
		float averageAlbedo = A * ( 1.0 + ( 2.0 / 3.0 - 28.0 / ( 15.0 * PI ) ) * roughness );
		vec3 rhoMultiScatter = rho * rho * averageAlbedo / max( vec3( EON_EPSILON ), vec3( 1.0 ) - rho * ( 1.0 - averageAlbedo ) );
		return rho * directionalAlbedo + rhoMultiScatter * ( 1.0 - directionalAlbedo );
	}
#endif
float V_GGX_SmithCorrelated( const in float alpha, const in float dotNL, const in float dotNV ) {
	float a2 = pow2( alpha );
	float gv = dotNL * sqrt( a2 + ( 1.0 - a2 ) * pow2( dotNV ) );
	float gl = dotNV * sqrt( a2 + ( 1.0 - a2 ) * pow2( dotNL ) );
	return 0.5 / max( gv + gl, EPSILON );
}
float D_GGX( const in float alpha, const in float dotNH ) {
	float a2 = pow2( alpha );
	float denom = pow2( dotNH ) * ( a2 - 1.0 ) + 1.0;
	return RECIPROCAL_PI * a2 / pow2( denom );
}
#ifdef USE_ANISOTROPY
	float V_GGX_SmithCorrelated_Anisotropic( const in float alphaT, const in float alphaB, const in float dotTV, const in float dotBV, const in float dotTL, const in float dotBL, const in float dotNV, const in float dotNL ) {
		float gv = dotNL * length( vec3( alphaT * dotTV, alphaB * dotBV, dotNV ) );
		float gl = dotNV * length( vec3( alphaT * dotTL, alphaB * dotBL, dotNL ) );
		return 0.5 / max( gv + gl, EPSILON );
	}
	float D_GGX_Anisotropic( const in float alphaT, const in float alphaB, const in float dotNH, const in float dotTH, const in float dotBH ) {
		float a2 = alphaT * alphaB;
		highp vec3 v = vec3( alphaB * dotTH, alphaT * dotBH, a2 * dotNH );
		highp float v2 = dot( v, v );
		float w2 = a2 / v2;
		return RECIPROCAL_PI * a2 * pow2 ( w2 );
	}
#endif
#ifdef USE_CLEARCOAT
	vec3 BRDF_GGX_Clearcoat( const in vec3 lightDir, const in vec3 viewDir, const in vec3 normal, const in PhysicalMaterial material) {
		vec3 f0 = material.clearcoatF0;
		float f90 = material.clearcoatF90;
		float roughness = max( material.clearcoatRoughness, 0.045 );
		float alpha = pow2( roughness );
		vec3 halfDir = normalize( lightDir + viewDir );
		float dotNL = saturate( dot( normal, lightDir ) );
		float dotNV = saturate( dot( normal, viewDir ) );
		float dotNH = saturate( dot( normal, halfDir ) );
		float dotVH = saturate( dot( viewDir, halfDir ) );
		vec3 F = F_Schlick( f0, f90, dotVH );
		float V = V_GGX_SmithCorrelated( alpha, dotNL, dotNV );
		float D = D_GGX( alpha, dotNH );
		return F * ( V * D );
	}
#endif
vec3 BRDF_GGX( const in vec3 lightDir, const in vec3 viewDir, const in vec3 normal, const in PhysicalMaterial material ) {
	vec3 f0 = material.specularColorBlended;
	float f90 = material.specularF90;
	float roughness = max( material.roughness, 0.045 );
	float alpha = pow2( roughness );
	vec3 halfDir = normalize( lightDir + viewDir );
	float dotNL = saturate( dot( normal, lightDir ) );
	float dotNV = saturate( dot( normal, viewDir ) );
	float dotNH = saturate( dot( normal, halfDir ) );
	float dotVH = saturate( dot( viewDir, halfDir ) );
	vec3 F = F_Schlick( f0, f90, dotVH );
	#ifdef USE_IRIDESCENCE
		F = mix( F, material.iridescenceFresnel, material.iridescence );
	#endif
	#ifdef USE_ANISOTROPY
		float dotTL = dot( material.anisotropyT, lightDir );
		float dotTV = dot( material.anisotropyT, viewDir );
		float dotTH = dot( material.anisotropyT, halfDir );
		float dotBL = dot( material.anisotropyB, lightDir );
		float dotBV = dot( material.anisotropyB, viewDir );
		float dotBH = dot( material.anisotropyB, halfDir );
		float alphaT = max( material.alphaT, alpha );
		float V = V_GGX_SmithCorrelated_Anisotropic( alphaT, alpha, dotTV, dotBV, dotTL, dotBL, dotNV, dotNL );
		float D = D_GGX_Anisotropic( alphaT, alpha, dotNH, dotTH, dotBH );
	#else
		float V = V_GGX_SmithCorrelated( alpha, dotNL, dotNV );
		float D = D_GGX( alpha, dotNH );
	#endif
	return F * ( V * D );
}
vec2 LTC_Uv( const in vec3 N, const in vec3 V, const in float roughness ) {
	const float LUT_SIZE = 64.0;
	const float LUT_SCALE = ( LUT_SIZE - 1.0 ) / LUT_SIZE;
	const float LUT_BIAS = 0.5 / LUT_SIZE;
	float dotNV = saturate( dot( N, V ) );
	vec2 uv = vec2( roughness, sqrt( 1.0 - dotNV ) );
	uv = uv * LUT_SCALE + LUT_BIAS;
	return uv;
}
float LTC_ClippedSphereFormFactor( const in vec3 f ) {
	float l = length( f );
	return max( ( l * l + f.z ) / ( l + 1.0 ), 0.0 );
}
vec3 LTC_EdgeVectorFormFactor( const in vec3 v1, const in vec3 v2 ) {
	float x = dot( v1, v2 );
	float y = abs( x );
	float a = 0.8543985 + ( 0.4965155 + 0.0145206 * y ) * y;
	float b = 3.4175940 + ( 4.1616724 + y ) * y;
	float v = a / b;
	float theta_sintheta = ( x > 0.0 ) ? v : 0.5 * inversesqrt( max( 1.0 - x * x, 1e-7 ) ) - v;
	return cross( v1, v2 ) * theta_sintheta;
}
vec3 LTC_Evaluate( const in vec3 N, const in vec3 V, const in vec3 P, const in mat3 mInv, const in vec3 rectCoords[ 4 ] ) {
	vec3 v1 = rectCoords[ 1 ] - rectCoords[ 0 ];
	vec3 v2 = rectCoords[ 3 ] - rectCoords[ 0 ];
	vec3 lightNormal = cross( v1, v2 );
	if( dot( lightNormal, P - rectCoords[ 0 ] ) < 0.0 ) return vec3( 0.0 );
	vec3 T1, T2;
	T1 = normalize( V - N * dot( V, N ) );
	T2 = - cross( N, T1 );
	mat3 mat = mInv * transpose( mat3( T1, T2, N ) );
	vec3 coords[ 4 ];
	coords[ 0 ] = mat * ( rectCoords[ 0 ] - P );
	coords[ 1 ] = mat * ( rectCoords[ 1 ] - P );
	coords[ 2 ] = mat * ( rectCoords[ 2 ] - P );
	coords[ 3 ] = mat * ( rectCoords[ 3 ] - P );
	coords[ 0 ] = normalize( coords[ 0 ] );
	coords[ 1 ] = normalize( coords[ 1 ] );
	coords[ 2 ] = normalize( coords[ 2 ] );
	coords[ 3 ] = normalize( coords[ 3 ] );
	vec3 vectorFormFactor = vec3( 0.0 );
	vectorFormFactor += LTC_EdgeVectorFormFactor( coords[ 0 ], coords[ 1 ] );
	vectorFormFactor += LTC_EdgeVectorFormFactor( coords[ 1 ], coords[ 2 ] );
	vectorFormFactor += LTC_EdgeVectorFormFactor( coords[ 2 ], coords[ 3 ] );
	vectorFormFactor += LTC_EdgeVectorFormFactor( coords[ 3 ], coords[ 0 ] );
	float result = LTC_ClippedSphereFormFactor( vectorFormFactor );
	return vec3( result );
}
#if defined( USE_SHEEN )
float D_Charlie( float roughness, float dotNH ) {
	float alpha = pow2( roughness );
	float invAlpha = 1.0 / alpha;
	float cos2h = dotNH * dotNH;
	float sin2h = max( 1.0 - cos2h, 0.0078125 );
	return ( 2.0 + invAlpha ) * pow( sin2h, invAlpha * 0.5 ) / ( 2.0 * PI );
}
float V_Neubelt( float dotNV, float dotNL ) {
	return saturate( 1.0 / ( 4.0 * ( dotNL + dotNV - dotNL * dotNV ) ) );
}
vec3 BRDF_Sheen( const in vec3 lightDir, const in vec3 viewDir, const in vec3 normal, vec3 sheenColor, const in float sheenRoughness ) {
	vec3 halfDir = normalize( lightDir + viewDir );
	float dotNL = saturate( dot( normal, lightDir ) );
	float dotNV = saturate( dot( normal, viewDir ) );
	float dotNH = saturate( dot( normal, halfDir ) );
	float D = D_Charlie( sheenRoughness, dotNH );
	float V = V_Neubelt( dotNV, dotNL );
	return sheenColor * ( D * V );
}
#endif
float IBLSheenBRDF( const in vec3 normal, const in vec3 viewDir, const in float roughness ) {
	float dotNV = saturate( dot( normal, viewDir ) );
	float r2 = roughness * roughness;
	float rInv = 1.0 / ( roughness + 0.1 );
	float a = -1.9362 + 1.0678 * roughness + 0.4573 * r2 - 0.8469 * rInv;
	float b = -0.6014 + 0.5538 * roughness - 0.4670 * r2 - 0.1255 * rInv;
	float DG = exp( a * dotNV + b );
	return saturate( DG );
}
vec3 EnvironmentBRDF( const in vec3 normal, const in vec3 viewDir, const in vec3 specularColor, const in float specularF90, const in float roughness ) {
	float dotNV = saturate( dot( normal, viewDir ) );
	vec2 fab = texture2D( dfgLUT, vec2( roughness, dotNV ) ).rg;
	return specularColor * fab.x + specularF90 * fab.y;
}
#ifdef USE_IRIDESCENCE
void computeMultiscatteringIridescence( const in vec2 fab, const in vec3 specularColor, const in float specularF90, const in float iridescence, const in vec3 iridescenceF0, inout vec3 singleScatter, inout vec3 multiScatter ) {
#else
void computeMultiscattering( const in vec2 fab, const in vec3 specularColor, const in float specularF90, inout vec3 singleScatter, inout vec3 multiScatter ) {
#endif
	#ifdef USE_IRIDESCENCE
		vec3 Fr = mix( specularColor, iridescenceF0, iridescence );
	#else
		vec3 Fr = specularColor;
	#endif
	vec3 FssEss = Fr * fab.x + specularF90 * fab.y;
	float Ess = fab.x + fab.y;
	float Ems = 1.0 - Ess;
	vec3 Favg = Fr + ( 1.0 - Fr ) * 0.047619;	vec3 Fms = FssEss * Favg / ( 1.0 - Ems * Favg );
	singleScatter += FssEss;
	multiScatter += Fms * Ems;
}
#if NUM_RECT_AREA_LIGHTS > 0
	void RE_Direct_RectArea_Physical( const in RectAreaLight rectAreaLight, const in vec3 geometryPosition, const in vec3 geometryNormal, const in vec3 geometryViewDir, const in vec3 geometryClearcoatNormal, const in PhysicalMaterial material, inout ReflectedLight reflectedLight ) {
		vec3 normal = geometryNormal;
		vec3 viewDir = geometryViewDir;
		vec3 position = geometryPosition;
		vec3 lightPos = rectAreaLight.position;
		vec3 halfWidth = rectAreaLight.halfWidth;
		vec3 halfHeight = rectAreaLight.halfHeight;
		vec3 lightColor = rectAreaLight.color;
		float roughness = material.roughness;
		vec3 rectCoords[ 4 ];
		rectCoords[ 0 ] = lightPos + halfWidth - halfHeight;		rectCoords[ 1 ] = lightPos - halfWidth - halfHeight;
		rectCoords[ 2 ] = lightPos - halfWidth + halfHeight;
		rectCoords[ 3 ] = lightPos + halfWidth + halfHeight;
		vec2 uv = LTC_Uv( normal, viewDir, roughness );
		vec4 t1 = texture2D( ltc_1, uv );
		vec4 t2 = texture2D( ltc_2, uv );
		mat3 mInv = mat3(
			vec3( t1.x, 0, t1.y ),
			vec3(    0, 1,    0 ),
			vec3( t1.z, 0, t1.w )
		);
		vec3 fresnel = ( material.specularColorBlended * t2.x + ( material.specularF90 - material.specularColorBlended ) * t2.y );
		reflectedLight.directSpecular += lightColor * fresnel * LTC_Evaluate( normal, viewDir, position, mInv, rectCoords );
		reflectedLight.directDiffuse += lightColor * material.diffuseContribution * LTC_Evaluate( normal, viewDir, position, mat3( 1.0 ), rectCoords );
		#ifdef USE_CLEARCOAT
			vec3 Ncc = geometryClearcoatNormal;
			vec2 uvClearcoat = LTC_Uv( Ncc, viewDir, material.clearcoatRoughness );
			vec4 t1Clearcoat = texture2D( ltc_1, uvClearcoat );
			vec4 t2Clearcoat = texture2D( ltc_2, uvClearcoat );
			mat3 mInvClearcoat = mat3(
				vec3( t1Clearcoat.x, 0, t1Clearcoat.y ),
				vec3(             0, 1,             0 ),
				vec3( t1Clearcoat.z, 0, t1Clearcoat.w )
			);
			vec3 fresnelClearcoat = material.clearcoatF0 * t2Clearcoat.x + ( material.clearcoatF90 - material.clearcoatF0 ) * t2Clearcoat.y;
			clearcoatSpecularDirect += lightColor * fresnelClearcoat * LTC_Evaluate( Ncc, viewDir, position, mInvClearcoat, rectCoords );
		#endif
	}
#endif
void RE_Direct_Physical( const in IncidentLight directLight, const in vec3 geometryPosition, const in vec3 geometryNormal, const in vec3 geometryViewDir, const in vec3 geometryClearcoatNormal, const in PhysicalMaterial material, inout ReflectedLight reflectedLight ) {
	float dotNL = saturate( dot( geometryNormal, directLight.direction ) );
	vec3 irradiance = dotNL * directLight.color;
	#ifdef USE_CLEARCOAT
		float dotNLcc = saturate( dot( geometryClearcoatNormal, directLight.direction ) );
		vec3 ccIrradiance = dotNLcc * directLight.color;
		clearcoatSpecularDirect += ccIrradiance * BRDF_GGX_Clearcoat( directLight.direction, geometryViewDir, geometryClearcoatNormal, material );
	#endif
	#ifdef USE_SHEEN

		sheenSpecularDirect += irradiance * BRDF_Sheen( directLight.direction, geometryViewDir, geometryNormal, material.sheenColor, material.sheenRoughness );

		float sheenAlbedoV = IBLSheenBRDF( geometryNormal, geometryViewDir, material.sheenRoughness );
		float sheenAlbedoL = IBLSheenBRDF( geometryNormal, directLight.direction, material.sheenRoughness );

		float sheenEnergyComp = 1.0 - max3( material.sheenColor ) * max( sheenAlbedoV, sheenAlbedoL );

		irradiance *= sheenEnergyComp;

	#endif
	vec3 specularBRDF = BRDF_GGX( directLight.direction, geometryViewDir, geometryNormal, material );
	#ifdef USE_RETROREFLECTION
		vec3 retroViewDir = reflect( - geometryViewDir, geometryNormal );
		vec3 retroSpecularBRDF = BRDF_GGX( directLight.direction, retroViewDir, geometryNormal, material );
		specularBRDF = mix( specularBRDF, retroSpecularBRDF, saturate( material.retroreflectivity ) );
	#endif
	reflectedLight.directSpecular += irradiance * specularBRDF * material.multiScatteringCompensation;
	vec3 halfDir = normalize( directLight.direction + geometryViewDir );
	float dotVH = saturate( dot( geometryViewDir, halfDir ) );
	vec3 F = F_Schlick( material.specularColor, material.specularF90, dotVH );
	#ifdef USE_RETROREFLECTION
		vec3 retroHalfDir = normalize( directLight.direction + retroViewDir );
		float dotRetroVH = saturate( dot( retroViewDir, retroHalfDir ) );
		vec3 retroF = F_Schlick( material.specularColor, material.specularF90, dotRetroVH );
		F = mix( F, retroF, saturate( material.retroreflectivity ) );
	#endif
	#ifdef USE_DIFFUSE_ROUGHNESS
		vec3 diffuseBRDF = BRDF_EON( directLight.direction, geometryViewDir, geometryNormal, material.diffuseColor, material.diffuseRoughness ) * ( 1.0 - material.metalness );
	#else
		vec3 diffuseBRDF = BRDF_Lambert( material.diffuseContribution );
	#endif
	reflectedLight.directDiffuse += irradiance * diffuseBRDF * ( 1.0 - F );
}
void RE_IndirectDiffuse_Physical( const in vec3 irradiance, const in vec3 geometryPosition, const in vec3 geometryNormal, const in vec3 geometryViewDir, const in vec3 geometryClearcoatNormal, const in PhysicalMaterial material, inout ReflectedLight reflectedLight ) {
	vec3 singleScattering = vec3( 0.0 );
	vec3 multiScattering = vec3( 0.0 );
	#ifdef USE_IRIDESCENCE
		computeMultiscatteringIridescence( material.dfg, material.specularColor, material.specularF90, material.iridescence, material.iridescenceF0Dielectric, singleScattering, multiScattering );
	#else
		computeMultiscattering( material.dfg, material.specularColor, material.specularF90, singleScattering, multiScattering );
	#endif
	#ifdef USE_DIFFUSE_ROUGHNESS
		float dotNV = saturate( dot( geometryNormal, geometryViewDir ) );
		vec3 diffuseAlbedo = EON_DirectionalAlbedo( material.diffuseColor, material.diffuseRoughness, dotNV ) * ( 1.0 - material.metalness );
		vec3 diffuse = irradiance * RECIPROCAL_PI * diffuseAlbedo * ( 1.0 - singleScattering - multiScattering );
	#else
		vec3 diffuse = irradiance * BRDF_Lambert( material.diffuseContribution ) * ( 1.0 - singleScattering - multiScattering );
	#endif
	#ifdef USE_SHEEN
		float sheenAlbedo = IBLSheenBRDF( geometryNormal, geometryViewDir, material.sheenRoughness );
		sheenSpecularIndirect += irradiance * material.sheenColor * sheenAlbedo * RECIPROCAL_PI;
		float sheenEnergyComp = 1.0 - max3( material.sheenColor ) * sheenAlbedo;
		diffuse *= sheenEnergyComp;
	#endif
	reflectedLight.indirectDiffuse += diffuse;
}
void RE_IndirectSpecular_Physical( const in vec3 radiance, const in vec3 irradiance, const in vec3 clearcoatRadiance, const in vec3 geometryPosition, const in vec3 geometryNormal, const in vec3 geometryViewDir, const in vec3 geometryClearcoatNormal, const in PhysicalMaterial material, inout ReflectedLight reflectedLight) {
	#ifdef USE_CLEARCOAT
		clearcoatSpecularIndirect += clearcoatRadiance * EnvironmentBRDF( geometryClearcoatNormal, geometryViewDir, material.clearcoatF0, material.clearcoatF90, material.clearcoatRoughness );
	#endif
	#ifdef USE_SHEEN
		sheenSpecularIndirect += irradiance * material.sheenColor * IBLSheenBRDF( geometryNormal, geometryViewDir, material.sheenRoughness ) * RECIPROCAL_PI;
	#endif
	vec3 singleScatteringDielectric = vec3( 0.0 );
	vec3 multiScatteringDielectric = vec3( 0.0 );
	vec3 singleScatteringMetallic = vec3( 0.0 );
	vec3 multiScatteringMetallic = vec3( 0.0 );
	#ifdef USE_IRIDESCENCE
		computeMultiscatteringIridescence( material.dfg, material.specularColor, material.specularF90, material.iridescence, material.iridescenceF0Dielectric, singleScatteringDielectric, multiScatteringDielectric );
		computeMultiscatteringIridescence( material.dfg, material.diffuseColor, material.specularF90, material.iridescence, material.iridescenceF0Metallic, singleScatteringMetallic, multiScatteringMetallic );
	#else
		computeMultiscattering( material.dfg, material.specularColor, material.specularF90, singleScatteringDielectric, multiScatteringDielectric );
		computeMultiscattering( material.dfg, material.diffuseColor, material.specularF90, singleScatteringMetallic, multiScatteringMetallic );
	#endif
	vec3 singleScattering = mix( singleScatteringDielectric, singleScatteringMetallic, material.metalness );
	vec3 multiScattering = mix( multiScatteringDielectric, multiScatteringMetallic, material.metalness );
	vec3 totalScatteringDielectric = singleScatteringDielectric + multiScatteringDielectric;
	#ifdef USE_DIFFUSE_ROUGHNESS
		float dotNV = saturate( dot( geometryNormal, geometryViewDir ) );
		vec3 diffuse = EON_DirectionalAlbedo( material.diffuseColor, material.diffuseRoughness, dotNV ) * ( 1.0 - material.metalness ) * ( 1.0 - totalScatteringDielectric );
	#else
		vec3 diffuse = material.diffuseContribution * ( 1.0 - totalScatteringDielectric );
	#endif
	vec3 cosineWeightedIrradiance = irradiance * RECIPROCAL_PI;
	vec3 indirectSpecular = radiance * singleScattering;
	indirectSpecular += multiScattering * cosineWeightedIrradiance;
	vec3 indirectDiffuse = diffuse * cosineWeightedIrradiance;
	#ifdef USE_SHEEN
		float sheenAlbedo = IBLSheenBRDF( geometryNormal, geometryViewDir, material.sheenRoughness );
		float sheenEnergyComp = 1.0 - max3( material.sheenColor ) * sheenAlbedo;
		indirectSpecular *= sheenEnergyComp;
		indirectDiffuse *= sheenEnergyComp;
	#endif
	reflectedLight.indirectSpecular += indirectSpecular;
	reflectedLight.indirectDiffuse += indirectDiffuse;
}
#define RE_Direct				RE_Direct_Physical
#define RE_Direct_RectArea		RE_Direct_RectArea_Physical
#define RE_IndirectDiffuse		RE_IndirectDiffuse_Physical
#define RE_IndirectSpecular		RE_IndirectSpecular_Physical
float computeSpecularOcclusion( const in float dotNV, const in float ambientOcclusion, const in float roughness ) {
	return saturate( pow( dotNV + ambientOcclusion, exp2( - 16.0 * roughness - 1.0 ) ) - 1.0 + ambientOcclusion );
}`,Gg=`
vec3 geometryPosition = - vViewPosition;
vec3 geometryNormal = normal;
vec3 geometryViewDir = ( isOrthographic ) ? vec3( 0, 0, 1 ) : normalize( vViewPosition );
vec3 geometryClearcoatNormal = vec3( 0.0 );
#ifdef USE_CLEARCOAT
	geometryClearcoatNormal = clearcoatNormal;
#endif
#ifdef USE_IRIDESCENCE
	float dotNVi = saturate( dot( normal, geometryViewDir ) );
	if ( material.iridescenceThickness == 0.0 ) {
		material.iridescence = 0.0;
	} else {
		material.iridescence = saturate( material.iridescence );
	}
	if ( material.iridescence > 0.0 ) {
		vec3 iridescenceFresnelDielectric = evalIridescence( 1.0, material.iridescenceIOR, dotNVi, material.iridescenceThickness, material.specularColor );
		vec3 iridescenceFresnelMetallic = evalIridescence( 1.0, material.iridescenceIOR, dotNVi, material.iridescenceThickness, material.diffuseColor );
		material.iridescenceFresnel = mix( iridescenceFresnelDielectric, iridescenceFresnelMetallic, material.metalness );
		material.iridescenceF0Dielectric = Schlick_to_F0( iridescenceFresnelDielectric, 1.0, dotNVi );
		material.iridescenceF0Metallic = Schlick_to_F0( iridescenceFresnelMetallic, 1.0, dotNVi );
	}
#endif
#ifdef STANDARD
	float dotNVms = saturate( dot( geometryNormal, geometryViewDir ) );
	material.dfg = texture2D( dfgLUT, vec2( material.roughness, dotNVms ) ).rg;
	#if ( NUM_SUN_LIGHTS > 0 || NUM_DIR_LIGHTS > 0 || NUM_POINT_LIGHTS > 0 || NUM_SPOT_LIGHTS > 0 )
		float EssMs = material.dfg.x + material.dfg.y;
		material.multiScatteringCompensation = 1.0 + material.specularColorBlended * ( 1.0 / EssMs - 1.0 );
	#endif
#endif
IncidentLight directLight;
#if ( NUM_POINT_LIGHTS > 0 ) && defined( RE_Direct )
	PointLight pointLight;
	#if defined( USE_SHADOWMAP ) && NUM_POINT_LIGHT_SHADOWS > 0
	PointLightShadow pointLightShadow;
	#endif
	#pragma unroll_loop_start
	for ( int i = 0; i < NUM_POINT_LIGHTS; i ++ ) {
		pointLight = pointLights[ i ];
		getPointLightInfo( pointLight, geometryPosition, directLight );
		#if defined( USE_SHADOWMAP ) && ( UNROLLED_LOOP_INDEX < NUM_POINT_LIGHT_SHADOWS ) && ( defined( SHADOWMAP_TYPE_PCF ) || defined( SHADOWMAP_TYPE_BASIC ) )
		pointLightShadow = pointLightShadows[ i ];
		directLight.color *= ( directLight.visible && receiveShadow ) ? getPointShadow( pointShadowMap[ i ], pointLightShadow.shadowMapSize, pointLightShadow.shadowIntensity, pointLightShadow.shadowBias, pointLightShadow.shadowRadius, vPointShadowCoord[ i ], pointLightShadow.shadowCameraNear, pointLightShadow.shadowCameraFar ) : 1.0;
		#endif
		RE_Direct( directLight, geometryPosition, geometryNormal, geometryViewDir, geometryClearcoatNormal, material, reflectedLight );
	}
	#pragma unroll_loop_end
#endif
#if ( NUM_SPOT_LIGHTS > 0 ) && defined( RE_Direct )
	SpotLight spotLight;
	vec4 spotColor;
	vec3 spotLightCoord;
	bool inSpotLightMap;
	#if defined( USE_SHADOWMAP ) && NUM_SPOT_LIGHT_SHADOWS > 0
	SpotLightShadow spotLightShadow;
	#endif
	#pragma unroll_loop_start
	for ( int i = 0; i < NUM_SPOT_LIGHTS; i ++ ) {
		spotLight = spotLights[ i ];
		getSpotLightInfo( spotLight, geometryPosition, directLight );
		#if ( UNROLLED_LOOP_INDEX < NUM_SPOT_LIGHT_SHADOWS_WITH_MAPS )
		#define SPOT_LIGHT_MAP_INDEX UNROLLED_LOOP_INDEX
		#elif ( UNROLLED_LOOP_INDEX < NUM_SPOT_LIGHT_SHADOWS )
		#define SPOT_LIGHT_MAP_INDEX NUM_SPOT_LIGHT_MAPS
		#else
		#define SPOT_LIGHT_MAP_INDEX ( UNROLLED_LOOP_INDEX - NUM_SPOT_LIGHT_SHADOWS + NUM_SPOT_LIGHT_SHADOWS_WITH_MAPS )
		#endif
		#if ( SPOT_LIGHT_MAP_INDEX < NUM_SPOT_LIGHT_MAPS )
			spotLightCoord = vSpotLightCoord[ i ].xyz / vSpotLightCoord[ i ].w;
			inSpotLightMap = all( lessThan( abs( spotLightCoord * 2. - 1. ), vec3( 1.0 ) ) );
			spotColor = texture2D( spotLightMap[ SPOT_LIGHT_MAP_INDEX ], spotLightCoord.xy );
			directLight.color = inSpotLightMap ? directLight.color * spotColor.rgb : directLight.color;
		#endif
		#undef SPOT_LIGHT_MAP_INDEX
		#if defined( USE_SHADOWMAP ) && ( UNROLLED_LOOP_INDEX < NUM_SPOT_LIGHT_SHADOWS )
		spotLightShadow = spotLightShadows[ i ];
		directLight.color *= ( directLight.visible && receiveShadow ) ? getShadow( spotShadowMap[ i ], spotLightShadow.shadowMapSize, spotLightShadow.shadowIntensity, spotLightShadow.shadowBias, spotLightShadow.shadowRadius, vSpotLightCoord[ i ] ) : 1.0;
		#endif
		RE_Direct( directLight, geometryPosition, geometryNormal, geometryViewDir, geometryClearcoatNormal, material, reflectedLight );
	}
	#pragma unroll_loop_end
#endif
#if ( NUM_SUN_LIGHTS > 0 ) && defined( RE_Direct )
	SunLight sunLight;
	#if defined( USE_SHADOWMAP ) && NUM_SUN_LIGHT_SHADOWS > 0
	SunLightShadow sunLightShadow;
	#endif
	#pragma unroll_loop_start
	for ( int i = 0; i < NUM_SUN_LIGHTS; i ++ ) {
		sunLight = sunLights[ i ];
		getSunLightInfo( sunLight, directLight );
		#if defined( USE_SHADOWMAP ) && ( UNROLLED_LOOP_INDEX < NUM_SUN_LIGHT_SHADOWS )
		sunLightShadow = sunLightShadows[ i ];
		directLight.color *= ( directLight.visible && receiveShadow ) ? getSunShadow( sunShadowMap[ i ], sunLightShadow, UNROLLED_LOOP_INDEX ) : 1.0;
		#endif
		RE_Direct( directLight, geometryPosition, geometryNormal, geometryViewDir, geometryClearcoatNormal, material, reflectedLight );
	}
	#pragma unroll_loop_end
#endif
#if ( NUM_DIR_LIGHTS > 0 ) && defined( RE_Direct )
	DirectionalLight directionalLight;
	#if defined( USE_SHADOWMAP ) && NUM_DIR_LIGHT_SHADOWS > 0
	DirectionalLightShadow directionalLightShadow;
	#endif
	#pragma unroll_loop_start
	for ( int i = 0; i < NUM_DIR_LIGHTS; i ++ ) {
		directionalLight = directionalLights[ i ];
		getDirectionalLightInfo( directionalLight, directLight );
		#if defined( USE_SHADOWMAP ) && ( UNROLLED_LOOP_INDEX < NUM_DIR_LIGHT_SHADOWS )
		directionalLightShadow = directionalLightShadows[ i ];
		directLight.color *= ( directLight.visible && receiveShadow ) ? getShadow( directionalShadowMap[ i ], directionalLightShadow.shadowMapSize, directionalLightShadow.shadowIntensity, directionalLightShadow.shadowBias, directionalLightShadow.shadowRadius, vDirectionalShadowCoord[ i ] ) : 1.0;
		#endif
		RE_Direct( directLight, geometryPosition, geometryNormal, geometryViewDir, geometryClearcoatNormal, material, reflectedLight );
	}
	#pragma unroll_loop_end
#endif
#if ( NUM_RECT_AREA_LIGHTS > 0 ) && defined( RE_Direct_RectArea )
	RectAreaLight rectAreaLight;
	#pragma unroll_loop_start
	for ( int i = 0; i < NUM_RECT_AREA_LIGHTS; i ++ ) {
		rectAreaLight = rectAreaLights[ i ];
		RE_Direct_RectArea( rectAreaLight, geometryPosition, geometryNormal, geometryViewDir, geometryClearcoatNormal, material, reflectedLight );
	}
	#pragma unroll_loop_end
#endif
#if defined( RE_IndirectDiffuse )
	vec3 iblIrradiance = vec3( 0.0 );
	vec3 irradiance = getAmbientLightIrradiance( ambientLightColor );
	#if defined( USE_LIGHT_PROBES )
		irradiance += getLightProbeIrradiance( lightProbe, geometryNormal );
	#endif
	#if ( NUM_HEMI_LIGHTS > 0 )
		#pragma unroll_loop_start
		for ( int i = 0; i < NUM_HEMI_LIGHTS; i ++ ) {
			irradiance += getHemisphereLightIrradiance( hemisphereLights[ i ], geometryNormal );
		}
		#pragma unroll_loop_end
	#endif
	#if NUM_LIGHT_PROBE_GRIDS > 0
		vec3 probeWorldPos = ( ( vec4( geometryPosition, 1.0 ) - viewMatrix[ 3 ] ) * viewMatrix ).xyz;
		vec3 probeWorldNormal = transformNormalByInverseViewMatrix( geometryNormal, viewMatrix );
		int probeGrid = - 1;
		#pragma unroll_loop_start
		for ( int i = 0; i < NUM_LIGHT_PROBE_GRIDS; i ++ ) {
			if ( isInsideLightProbeGrid( probeWorldPos, probeWorldNormal, probesMin[ i ], probesMax[ i ], probesResolution[ i ] ) ) probeGrid = UNROLLED_LOOP_INDEX;
		}
		#pragma unroll_loop_end
		#pragma unroll_loop_start
		for ( int i = 0; i < NUM_LIGHT_PROBE_GRIDS; i ++ ) {
			if ( probeGrid == UNROLLED_LOOP_INDEX ) irradiance += getLightProbeGridIrradiance( probesSH[ i ], probesMin[ i ], probesMax[ i ], probesResolution[ i ], probeWorldPos, probeWorldNormal );
		}
		#pragma unroll_loop_end
	#endif
#endif
#if defined( RE_IndirectSpecular )
	vec3 radiance = vec3( 0.0 );
	vec3 clearcoatRadiance = vec3( 0.0 );
#endif`,Wg=`#if defined( RE_IndirectDiffuse )
	#ifdef USE_LIGHTMAP
		vec4 lightMapTexel = texture2D( lightMap, vLightMapUv );
		vec3 lightMapIrradiance = lightMapTexel.rgb * lightMapIntensity;
		irradiance += lightMapIrradiance;
	#endif
	#if defined( USE_ENVMAP ) && defined( ENVMAP_TYPE_PMREM )
		#if defined( STANDARD ) || defined( LAMBERT ) || defined( PHONG )
			iblIrradiance += getIBLIrradiance( geometryNormal );
		#endif
	#endif
#endif
#if defined( USE_ENVMAP ) && defined( RE_IndirectSpecular )
	#ifdef USE_ANISOTROPY
		vec3 iblRadiance = getIBLAnisotropyRadiance( geometryViewDir, geometryNormal, material.roughness, material.anisotropyB, material.anisotropy );
	#else
		vec3 iblRadiance = getIBLRadiance( geometryViewDir, geometryNormal, material.roughness );
	#endif
	#ifdef USE_RETROREFLECTION
		#ifdef USE_ANISOTROPY
			vec3 retroIBLRadiance = getIBLAnisotropyRetroRadiance( geometryViewDir, geometryNormal, material.roughness, material.anisotropyB, material.anisotropy );
		#else
			vec3 retroIBLRadiance = getIBLRetroRadiance( geometryViewDir, geometryNormal, material.roughness );
		#endif
		iblRadiance = mix( iblRadiance, retroIBLRadiance, saturate( material.retroreflectivity ) );
	#endif
	radiance += iblRadiance;
	#ifdef USE_CLEARCOAT
		clearcoatRadiance += getIBLRadiance( geometryViewDir, geometryClearcoatNormal, material.clearcoatRoughness );
	#endif
#endif`,Xg=`#if defined( RE_IndirectDiffuse )
	#if defined( LAMBERT ) || defined( PHONG )
		irradiance += iblIrradiance;
	#endif
	RE_IndirectDiffuse( irradiance, geometryPosition, geometryNormal, geometryViewDir, geometryClearcoatNormal, material, reflectedLight );
#endif
#if defined( RE_IndirectSpecular )
	RE_IndirectSpecular( radiance, iblIrradiance, clearcoatRadiance, geometryPosition, geometryNormal, geometryViewDir, geometryClearcoatNormal, material, reflectedLight );
#endif`,qg=`#if NUM_LIGHT_PROBE_GRIDS > 0
uniform highp sampler3D probesSH[ NUM_LIGHT_PROBE_GRIDS ];
uniform vec3 probesMin[ NUM_LIGHT_PROBE_GRIDS ];
uniform vec3 probesMax[ NUM_LIGHT_PROBE_GRIDS ];
uniform vec3 probesResolution[ NUM_LIGHT_PROBE_GRIDS ];
bool isInsideLightProbeGrid( vec3 worldPos, vec3 worldNormal, vec3 probesMin, vec3 probesMax, vec3 probesResolution ) {
	vec3 spacing = ( probesMax - probesMin ) / probesResolution;
	vec3 samplePos = worldPos + worldNormal * spacing * 0.5;
	vec3 margin = spacing * 0.01;
	return all( greaterThanEqual( samplePos, probesMin - margin ) ) && all( lessThanEqual( samplePos, probesMax + margin ) );
}
vec3 getLightProbeGridIrradiance( highp sampler3D probesSH, vec3 probesMin, vec3 probesMax, vec3 probesResolution, vec3 worldPos, vec3 worldNormal ) {
	vec3 res = probesResolution;
	vec3 gridRange = probesMax - probesMin;
	vec3 probeSpacing = gridRange / res;
	vec3 samplePos = worldPos + worldNormal * probeSpacing * 0.5;
	vec3 uvw = clamp( ( samplePos - probesMin ) / gridRange, 0.5 / res, 1.0 - 0.5 / res );
	float nz          = res.z;
	float paddedSlices = nz + 2.0;
	float atlasDepth  = 7.0 * paddedSlices;
	float uvZBase     = uvw.z * nz + 1.0;
	vec4 s0 = texture( probesSH, vec3( uvw.xy, ( uvZBase                       ) / atlasDepth ) );
	vec4 s1 = texture( probesSH, vec3( uvw.xy, ( uvZBase +       paddedSlices   ) / atlasDepth ) );
	vec4 s2 = texture( probesSH, vec3( uvw.xy, ( uvZBase + 2.0 * paddedSlices   ) / atlasDepth ) );
	vec4 s3 = texture( probesSH, vec3( uvw.xy, ( uvZBase + 3.0 * paddedSlices   ) / atlasDepth ) );
	vec4 s4 = texture( probesSH, vec3( uvw.xy, ( uvZBase + 4.0 * paddedSlices   ) / atlasDepth ) );
	vec4 s5 = texture( probesSH, vec3( uvw.xy, ( uvZBase + 5.0 * paddedSlices   ) / atlasDepth ) );
	vec4 s6 = texture( probesSH, vec3( uvw.xy, ( uvZBase + 6.0 * paddedSlices   ) / atlasDepth ) );
	vec3 c0 = s0.xyz;
	vec3 c1 = vec3( s0.w, s1.xy );
	vec3 c2 = vec3( s1.zw, s2.x );
	vec3 c3 = s2.yzw;
	vec3 c4 = s3.xyz;
	vec3 c5 = vec3( s3.w, s4.xy );
	vec3 c6 = vec3( s4.zw, s5.x );
	vec3 c7 = s5.yzw;
	vec3 c8 = s6.xyz;
	float x = worldNormal.x, y = worldNormal.y, z = worldNormal.z;
	vec3 result = c0 * 0.886227;
	result += c1 * 2.0 * 0.511664 * y;
	result += c2 * 2.0 * 0.511664 * z;
	result += c3 * 2.0 * 0.511664 * x;
	result += c4 * 2.0 * 0.429043 * x * y;
	result += c5 * 2.0 * 0.429043 * y * z;
	result += c6 * ( 0.743125 * z * z - 0.247708 );
	result += c7 * 2.0 * 0.429043 * x * z;
	result += c8 * 0.429043 * ( x * x - y * y );
	return max( result, vec3( 0.0 ) );
}
#endif`,Yg=`#if defined( USE_LOGARITHMIC_DEPTH_BUFFER )
	gl_FragDepth = vIsPerspective == 0.0 ? gl_FragCoord.z : log2( vFragDepth ) * logDepthBufFC * 0.5;
#endif`,Zg=`#if defined( USE_LOGARITHMIC_DEPTH_BUFFER )
	uniform float logDepthBufFC;
	varying float vFragDepth;
	varying float vIsPerspective;
#endif`,Kg=`#ifdef USE_LOGARITHMIC_DEPTH_BUFFER
	varying float vFragDepth;
	varying float vIsPerspective;
#endif`,Jg=`#ifdef USE_LOGARITHMIC_DEPTH_BUFFER
	vFragDepth = 1.0 + gl_Position.w;
	vIsPerspective = float( isPerspectiveMatrix( projectionMatrix ) );
#endif`,$g=`#ifdef USE_MAP
	vec4 sampledDiffuseColor = texture2D( map, vMapUv );
	#ifdef DECODE_VIDEO_TEXTURE
		sampledDiffuseColor = sRGBTransferEOTF( sampledDiffuseColor );
	#endif
	diffuseColor *= sampledDiffuseColor;
#endif`,jg=`#ifdef USE_MAP
	uniform sampler2D map;
#endif`,Qg=`#if defined( USE_MAP ) || defined( USE_ALPHAMAP )
	#if defined( USE_POINTS_UV )
		vec2 uv = vUv;
	#else
		vec2 uv = ( uvTransform * vec3( gl_PointCoord.x, 1.0 - gl_PointCoord.y, 1 ) ).xy;
	#endif
#endif
#ifdef USE_MAP
	diffuseColor *= texture2D( map, uv );
#endif
#ifdef USE_ALPHAMAP
	diffuseColor.a *= texture2D( alphaMap, uv ).g;
#endif`,t0=`#if defined( USE_POINTS_UV )
	varying vec2 vUv;
#else
	#if defined( USE_MAP ) || defined( USE_ALPHAMAP )
		uniform mat3 uvTransform;
	#endif
#endif
#ifdef USE_MAP
	uniform sampler2D map;
#endif
#ifdef USE_ALPHAMAP
	uniform sampler2D alphaMap;
#endif`,e0=`float metalnessFactor = metalness;
#ifdef USE_METALNESSMAP
	vec4 texelMetalness = texture2D( metalnessMap, vMetalnessMapUv );
	metalnessFactor *= texelMetalness.b;
#endif`,i0=`#ifdef USE_METALNESSMAP
	uniform sampler2D metalnessMap;
#endif`,n0=`#ifdef USE_INSTANCING_MORPH
	float morphTargetInfluences[ MORPHTARGETS_COUNT ];
	float morphTargetBaseInfluence = texelFetch( morphTexture, ivec2( 0, gl_InstanceID ), 0 ).r;
	for ( int i = 0; i < MORPHTARGETS_COUNT; i ++ ) {
		morphTargetInfluences[i] =  texelFetch( morphTexture, ivec2( i + 1, gl_InstanceID ), 0 ).r;
	}
#endif`,s0=`#if defined( USE_MORPHCOLORS )
	#if defined( USE_COLOR_ALPHA )
		vColor *= morphTargetBaseInfluence;
	#elif defined( USE_COLOR )
		vColor.rgb *= morphTargetBaseInfluence;
	#endif
	for ( int i = 0; i < MORPHTARGETS_COUNT; i ++ ) {
		#if defined( USE_COLOR_ALPHA )
			if ( morphTargetInfluences[ i ] != 0.0 ) vColor += getMorph( gl_VertexID, i, 2 ) * morphTargetInfluences[ i ];
		#elif defined( USE_COLOR )
			if ( morphTargetInfluences[ i ] != 0.0 ) vColor.rgb += getMorph( gl_VertexID, i, 2 ).rgb * morphTargetInfluences[ i ];
		#endif
	}
#endif`,r0=`#ifdef USE_MORPHNORMALS
	objectNormal *= morphTargetBaseInfluence;
	for ( int i = 0; i < MORPHTARGETS_COUNT; i ++ ) {
		if ( morphTargetInfluences[ i ] != 0.0 ) objectNormal += getMorph( gl_VertexID, i, 1 ).xyz * morphTargetInfluences[ i ];
	}
#endif`,o0=`#ifdef USE_MORPHTARGETS
	#ifndef USE_INSTANCING_MORPH
		uniform float morphTargetBaseInfluence;
		uniform float morphTargetInfluences[ MORPHTARGETS_COUNT ];
	#endif
	uniform sampler2DArray morphTargetsTexture;
	uniform ivec2 morphTargetsTextureSize;
	vec4 getMorph( const in int vertexIndex, const in int morphTargetIndex, const in int offset ) {
		int texelIndex = vertexIndex * MORPHTARGETS_TEXTURE_STRIDE + offset;
		int y = texelIndex / morphTargetsTextureSize.x;
		int x = texelIndex - y * morphTargetsTextureSize.x;
		ivec3 morphUV = ivec3( x, y, morphTargetIndex );
		return texelFetch( morphTargetsTexture, morphUV, 0 );
	}
#endif`,a0=`#ifdef USE_MORPHTARGETS
	transformed *= morphTargetBaseInfluence;
	for ( int i = 0; i < MORPHTARGETS_COUNT; i ++ ) {
		if ( morphTargetInfluences[ i ] != 0.0 ) transformed += getMorph( gl_VertexID, i, 0 ).xyz * morphTargetInfluences[ i ];
	}
#endif`,l0=`float faceDirection = gl_FrontFacing ? 1.0 : - 1.0;
#ifdef FLAT_SHADED
	vec3 fdx = dFdx( vViewPosition );
	vec3 fdy = dFdy( vViewPosition );
	vec3 normal = normalize( cross( fdx, fdy ) );
#else
	vec3 normal = normalize( vNormal );
	#ifdef DOUBLE_SIDED
		normal *= faceDirection;
	#endif
#endif
#if defined( USE_NORMALMAP_TANGENTSPACE ) || defined( USE_CLEARCOAT_NORMALMAP ) || defined( USE_ANISOTROPY )
	#ifdef USE_TANGENT
		mat3 tbn = mat3( normalize( vTangent ), normalize( vBitangent ), normal );
	#else
		mat3 tbn = getTangentFrame( - vViewPosition, normal,
		#if defined( USE_NORMALMAP )
			vNormalMapUv
		#elif defined( USE_CLEARCOAT_NORMALMAP )
			vClearcoatNormalMapUv
		#else
			vUv
		#endif
		);
	#endif
	#ifdef DOUBLE_SIDED
		tbn[0] *= faceDirection;
		tbn[1] *= faceDirection;
	#endif
#endif
#ifdef USE_CLEARCOAT_NORMALMAP
	#ifdef USE_TANGENT
		mat3 tbn2 = mat3( normalize( vTangent ), normalize( vBitangent ), normal );
	#else
		mat3 tbn2 = getTangentFrame( - vViewPosition, normal, vClearcoatNormalMapUv );
	#endif
	#ifdef DOUBLE_SIDED
		tbn2[0] *= faceDirection;
		tbn2[1] *= faceDirection;
	#endif
#endif
vec3 nonPerturbedNormal = normal;`,c0=`#ifdef USE_NORMALMAP_OBJECTSPACE
	normal = texture2D( normalMap, vNormalMapUv ).xyz * 2.0 - 1.0;
	#ifdef FLIP_SIDED
		normal = - normal;
	#endif
	#ifdef DOUBLE_SIDED
		normal = normal * faceDirection;
	#endif
	normal = normalize( normalMatrix * normal );
#elif defined( USE_NORMALMAP_TANGENTSPACE )
	vec3 mapN = texture2D( normalMap, vNormalMapUv ).xyz * 2.0 - 1.0;
	#if defined( USE_PACKED_NORMALMAP )
		mapN = vec3( mapN.xy, sqrt( saturate( 1.0 - dot( mapN.xy, mapN.xy ) ) ) );
	#endif
	mapN.xy *= normalScale;
	normal = normalize( tbn * mapN );
#elif defined( USE_BUMPMAP )
	normal = perturbNormalArb( - vViewPosition, normal, dHdxy_fwd(), faceDirection );
#endif`,h0=`#ifndef FLAT_SHADED
	varying vec3 vNormal;
	#ifdef USE_TANGENT
		varying vec3 vTangent;
		varying vec3 vBitangent;
	#endif
#endif`,u0=`#ifndef FLAT_SHADED
	varying vec3 vNormal;
	#ifdef USE_TANGENT
		varying vec3 vTangent;
		varying vec3 vBitangent;
	#endif
#endif`,d0=`#ifndef FLAT_SHADED
	vNormal = normalize( transformedNormal );
	#ifdef USE_TANGENT
		vTangent = normalize( transformedTangent );
		vBitangent = normalize( cross( vNormal, vTangent ) * tangent.w );
		#ifdef FLIP_SIDED
			vBitangent = - vBitangent;
		#endif
	#endif
#endif`,f0=`#ifdef USE_NORMALMAP
	uniform sampler2D normalMap;
	uniform vec2 normalScale;
#endif
#ifdef USE_NORMALMAP_OBJECTSPACE
	uniform mat3 normalMatrix;
#endif
#if ! defined ( USE_TANGENT ) && ( defined ( USE_NORMALMAP_TANGENTSPACE ) || defined ( USE_CLEARCOAT_NORMALMAP ) || defined( USE_ANISOTROPY ) )
	mat3 getTangentFrame( vec3 eye_pos, vec3 surf_norm, vec2 uv ) {
		vec3 q0 = dFdx( eye_pos.xyz );
		vec3 q1 = dFdy( eye_pos.xyz );
		vec2 st0 = dFdx( uv.st );
		vec2 st1 = dFdy( uv.st );
		vec3 N = surf_norm;
		vec3 q1perp = cross( q1, N );
		vec3 q0perp = cross( N, q0 );
		vec3 T = q1perp * st0.x + q0perp * st1.x;
		vec3 B = q1perp * st0.y + q0perp * st1.y;
		float det = max( dot( T, T ), dot( B, B ) );
		float scale = ( det == 0.0 ) ? 0.0 : inversesqrt( det );
		return mat3( T * scale, B * scale, N );
	}
#endif`,p0=`#ifdef USE_CLEARCOAT
	vec3 clearcoatNormal = nonPerturbedNormal;
#endif`,m0=`#ifdef USE_CLEARCOAT_NORMALMAP
	vec3 clearcoatMapN = texture2D( clearcoatNormalMap, vClearcoatNormalMapUv ).xyz * 2.0 - 1.0;
	clearcoatMapN.xy *= clearcoatNormalScale;
	clearcoatNormal = normalize( tbn2 * clearcoatMapN );
#endif`,g0=`#ifdef USE_CLEARCOATMAP
	uniform sampler2D clearcoatMap;
#endif
#ifdef USE_CLEARCOAT_NORMALMAP
	uniform sampler2D clearcoatNormalMap;
	uniform vec2 clearcoatNormalScale;
#endif
#ifdef USE_CLEARCOAT_ROUGHNESSMAP
	uniform sampler2D clearcoatRoughnessMap;
#endif`,x0=`#ifdef USE_IRIDESCENCEMAP
	uniform sampler2D iridescenceMap;
#endif
#ifdef USE_IRIDESCENCE_THICKNESSMAP
	uniform sampler2D iridescenceThicknessMap;
#endif`,_0=`#ifdef OPAQUE
diffuseColor.a = 1.0;
#endif
#ifdef USE_TRANSMISSION
diffuseColor.a *= material.transmissionAlpha;
#endif
gl_FragColor = vec4( outgoingLight, diffuseColor.a );`,v0=`vec3 packNormalToRGB( const in vec3 normal ) {
	return normalize( normal ) * 0.5 + 0.5;
}
vec3 unpackRGBToNormal( const in vec3 rgb ) {
	return 2.0 * rgb.xyz - 1.0;
}
const float PackUpscale = 256. / 255.;const float UnpackDownscale = 255. / 256.;const float ShiftRight8 = 1. / 256.;
const float Inv255 = 1. / 255.;
const vec4 PackFactors = vec4( 1.0, 256.0, 256.0 * 256.0, 256.0 * 256.0 * 256.0 );
const vec2 UnpackFactors2 = vec2( UnpackDownscale, 1.0 / PackFactors.g );
const vec3 UnpackFactors3 = vec3( UnpackDownscale / PackFactors.rg, 1.0 / PackFactors.b );
const vec4 UnpackFactors4 = vec4( UnpackDownscale / PackFactors.rgb, 1.0 / PackFactors.a );
vec4 packDepthToRGBA( const in float v ) {
	if( v <= 0.0 )
		return vec4( 0., 0., 0., 0. );
	if( v >= 1.0 )
		return vec4( 1., 1., 1., 1. );
	float vuf;
	float af = modf( v * PackFactors.a, vuf );
	float bf = modf( vuf * ShiftRight8, vuf );
	float gf = modf( vuf * ShiftRight8, vuf );
	return vec4( vuf * Inv255, gf * PackUpscale, bf * PackUpscale, af );
}
vec3 packDepthToRGB( const in float v ) {
	if( v <= 0.0 )
		return vec3( 0., 0., 0. );
	if( v >= 1.0 )
		return vec3( 1., 1., 1. );
	float vuf;
	float bf = modf( v * PackFactors.b, vuf );
	float gf = modf( vuf * ShiftRight8, vuf );
	return vec3( vuf * Inv255, gf * PackUpscale, bf );
}
vec2 packDepthToRG( const in float v ) {
	if( v <= 0.0 )
		return vec2( 0., 0. );
	if( v >= 1.0 )
		return vec2( 1., 1. );
	float vuf;
	float gf = modf( v * 256., vuf );
	return vec2( vuf * Inv255, gf );
}
float unpackRGBAToDepth( const in vec4 v ) {
	return dot( v, UnpackFactors4 );
}
float unpackRGBToDepth( const in vec3 v ) {
	return dot( v, UnpackFactors3 );
}
float unpackRGToDepth( const in vec2 v ) {
	return v.r * UnpackFactors2.r + v.g * UnpackFactors2.g;
}
vec4 pack2HalfToRGBA( const in vec2 v ) {
	vec4 r = vec4( v.x, fract( v.x * 255.0 ), v.y, fract( v.y * 255.0 ) );
	return vec4( r.x - r.y / 255.0, r.y, r.z - r.w / 255.0, r.w );
}
vec2 unpackRGBATo2Half( const in vec4 v ) {
	return vec2( v.x + ( v.y / 255.0 ), v.z + ( v.w / 255.0 ) );
}
float viewZToOrthographicDepth( const in float viewZ, const in float near, const in float far ) {
	return ( viewZ + near ) / ( near - far );
}
float orthographicDepthToViewZ( const in float depth, const in float near, const in float far ) {
	#ifdef USE_REVERSED_DEPTH_BUFFER

		return depth * ( far - near ) - far;
	#else
		return depth * ( near - far ) - near;
	#endif
}
float viewZToPerspectiveDepth( const in float viewZ, const in float near, const in float far ) {
	return ( ( near + viewZ ) * far ) / ( ( far - near ) * viewZ );
}
float perspectiveDepthToViewZ( const in float depth, const in float near, const in float far ) {

	#ifdef USE_REVERSED_DEPTH_BUFFER
		return ( near * far ) / ( ( near - far ) * depth - near );
	#else
		return ( near * far ) / ( ( far - near ) * depth - far );
	#endif
}`,y0=`#ifdef PREMULTIPLIED_ALPHA
	gl_FragColor.rgb *= gl_FragColor.a;
#endif`,M0=`vec4 mvPosition = vec4( transformed, 1.0 );
#ifdef USE_BATCHING
	mvPosition = batchingMatrix * mvPosition;
#endif
#ifdef USE_INSTANCING
	mvPosition = instanceMatrix * mvPosition;
#endif
mvPosition = modelViewMatrix * mvPosition;
gl_Position = projectionMatrix * mvPosition;`,S0=`#ifdef DITHERING
	gl_FragColor.rgb = dithering( gl_FragColor.rgb );
#endif`,b0=`#ifdef DITHERING
	vec3 dithering( vec3 color ) {
		float grid_position = rand( gl_FragCoord.xy );
		vec3 dither_shift_RGB = vec3( 0.25 / 255.0, -0.25 / 255.0, 0.25 / 255.0 );
		dither_shift_RGB = mix( 2.0 * dither_shift_RGB, -2.0 * dither_shift_RGB, grid_position );
		return color + dither_shift_RGB;
	}
#endif`,w0=`float roughnessFactor = roughness;
#ifdef USE_ROUGHNESSMAP
	vec4 texelRoughness = texture2D( roughnessMap, vRoughnessMapUv );
	roughnessFactor *= texelRoughness.g;
#endif`,E0=`#ifdef USE_ROUGHNESSMAP
	uniform sampler2D roughnessMap;
#endif`,T0=`#if NUM_SPOT_LIGHT_COORDS > 0
	varying vec4 vSpotLightCoord[ NUM_SPOT_LIGHT_COORDS ];
#endif
#if NUM_SPOT_LIGHT_MAPS > 0
	uniform sampler2D spotLightMap[ NUM_SPOT_LIGHT_MAPS ];
#endif
#ifdef USE_SHADOWMAP
	#if NUM_SUN_LIGHT_SHADOWS > 0
		#define SUN_LIGHT_CASCADES 2
		#if defined( SHADOWMAP_TYPE_PCF )
			uniform sampler2DShadow sunShadowMap[ NUM_SUN_LIGHT_SHADOWS ];
		#else
			uniform sampler2D sunShadowMap[ NUM_SUN_LIGHT_SHADOWS ];
		#endif
		uniform mat4 sunShadowMatrix[ NUM_SUN_LIGHT_SHADOWS * SUN_LIGHT_CASCADES ];
		uniform vec4 sunShadowCascade[ NUM_SUN_LIGHT_SHADOWS * SUN_LIGHT_CASCADES ];
		varying vec4 vSunShadowWorldPosition;
		varying vec3 vSunShadowWorldNormal;
		struct SunLightShadow {
			float shadowIntensity;
			float shadowBias;
			float shadowNormalBias;
			float shadowRadius;
			vec2 shadowMapSize;
		};
		uniform SunLightShadow sunLightShadows[ NUM_SUN_LIGHT_SHADOWS ];
	#endif
	#if NUM_DIR_LIGHT_SHADOWS > 0
		#if defined( SHADOWMAP_TYPE_PCF )
			uniform sampler2DShadow directionalShadowMap[ NUM_DIR_LIGHT_SHADOWS ];
		#else
			uniform sampler2D directionalShadowMap[ NUM_DIR_LIGHT_SHADOWS ];
		#endif
		varying vec4 vDirectionalShadowCoord[ NUM_DIR_LIGHT_SHADOWS ];
		struct DirectionalLightShadow {
			float shadowIntensity;
			float shadowBias;
			float shadowNormalBias;
			float shadowRadius;
			vec2 shadowMapSize;
		};
		uniform DirectionalLightShadow directionalLightShadows[ NUM_DIR_LIGHT_SHADOWS ];
	#endif
	#if NUM_SPOT_LIGHT_SHADOWS > 0
		#if defined( SHADOWMAP_TYPE_PCF )
			uniform sampler2DShadow spotShadowMap[ NUM_SPOT_LIGHT_SHADOWS ];
		#else
			uniform sampler2D spotShadowMap[ NUM_SPOT_LIGHT_SHADOWS ];
		#endif
		struct SpotLightShadow {
			float shadowIntensity;
			float shadowBias;
			float shadowNormalBias;
			float shadowRadius;
			vec2 shadowMapSize;
		};
		uniform SpotLightShadow spotLightShadows[ NUM_SPOT_LIGHT_SHADOWS ];
	#endif
	#if NUM_POINT_LIGHT_SHADOWS > 0
		#if defined( SHADOWMAP_TYPE_PCF )
			uniform samplerCubeShadow pointShadowMap[ NUM_POINT_LIGHT_SHADOWS ];
		#elif defined( SHADOWMAP_TYPE_BASIC )
			uniform samplerCube pointShadowMap[ NUM_POINT_LIGHT_SHADOWS ];
		#endif
		varying vec4 vPointShadowCoord[ NUM_POINT_LIGHT_SHADOWS ];
		struct PointLightShadow {
			float shadowIntensity;
			float shadowBias;
			float shadowNormalBias;
			float shadowRadius;
			vec2 shadowMapSize;
			float shadowCameraNear;
			float shadowCameraFar;
		};
		uniform PointLightShadow pointLightShadows[ NUM_POINT_LIGHT_SHADOWS ];
	#endif
	#if defined( SHADOWMAP_TYPE_PCF )
		float interleavedGradientNoise( vec2 position ) {
			return fract( 52.9829189 * fract( dot( position, vec2( 0.06711056, 0.00583715 ) ) ) );
		}
		vec2 vogelDiskSample( int sampleIndex, int samplesCount, float phi ) {
			const float goldenAngle = 2.399963229728653;
			float r = sqrt( ( float( sampleIndex ) + 0.5 ) / float( samplesCount ) );
			float theta = float( sampleIndex ) * goldenAngle + phi;
			return vec2( cos( theta ), sin( theta ) ) * r;
		}
	#endif
	#if defined( SHADOWMAP_TYPE_PCF )
		float getShadow( sampler2DShadow shadowMap, vec2 shadowMapSize, float shadowIntensity, float shadowBias, float shadowRadius, vec4 shadowCoord ) {
			float shadow = 1.0;
			shadowCoord.xyz /= shadowCoord.w;
			#ifdef USE_REVERSED_DEPTH_BUFFER
				shadowCoord.z -= shadowBias;
			#else
				shadowCoord.z += shadowBias;
			#endif
			bool inFrustum = shadowCoord.x >= 0.0 && shadowCoord.x <= 1.0 && shadowCoord.y >= 0.0 && shadowCoord.y <= 1.0;
			bool frustumTest = inFrustum && shadowCoord.z <= 1.0;
			if ( frustumTest ) {
				vec2 texelSize = vec2( 1.0 ) / shadowMapSize;
				vec2 radius = shadowRadius * texelSize;
				float phi = interleavedGradientNoise( gl_FragCoord.xy ) * PI2;
				shadow = (
					texture( shadowMap, vec3( shadowCoord.xy + vogelDiskSample( 0, 5, phi ) * radius, shadowCoord.z ) ) +
					texture( shadowMap, vec3( shadowCoord.xy + vogelDiskSample( 1, 5, phi ) * radius, shadowCoord.z ) ) +
					texture( shadowMap, vec3( shadowCoord.xy + vogelDiskSample( 2, 5, phi ) * radius, shadowCoord.z ) ) +
					texture( shadowMap, vec3( shadowCoord.xy + vogelDiskSample( 3, 5, phi ) * radius, shadowCoord.z ) ) +
					texture( shadowMap, vec3( shadowCoord.xy + vogelDiskSample( 4, 5, phi ) * radius, shadowCoord.z ) )
				) * 0.2;
			}
			return mix( 1.0, shadow, shadowIntensity );
		}
	#elif defined( SHADOWMAP_TYPE_VSM )
		float getShadow( sampler2D shadowMap, vec2 shadowMapSize, float shadowIntensity, float shadowBias, float shadowRadius, vec4 shadowCoord ) {
			float shadow = 1.0;
			shadowCoord.xyz /= shadowCoord.w;
			#ifdef USE_REVERSED_DEPTH_BUFFER
				shadowCoord.z -= shadowBias;
			#else
				shadowCoord.z += shadowBias;
			#endif
			bool inFrustum = shadowCoord.x >= 0.0 && shadowCoord.x <= 1.0 && shadowCoord.y >= 0.0 && shadowCoord.y <= 1.0;
			bool frustumTest = inFrustum && shadowCoord.z <= 1.0;
			if ( frustumTest ) {
				vec2 distribution = texture2D( shadowMap, shadowCoord.xy ).rg;
				float mean = distribution.x;
				float variance = distribution.y * distribution.y;
				#ifdef USE_REVERSED_DEPTH_BUFFER
					float hard_shadow = step( mean, shadowCoord.z );
				#else
					float hard_shadow = step( shadowCoord.z, mean );
				#endif

				if ( hard_shadow == 1.0 ) {
					shadow = 1.0;
				} else {
					variance = max( variance, 0.0000001 );
					float d = shadowCoord.z - mean;
					float p_max = variance / ( variance + d * d );
					p_max = clamp( ( p_max - 0.3 ) / 0.65, 0.0, 1.0 );
					shadow = max( hard_shadow, p_max );
				}
			}
			return mix( 1.0, shadow, shadowIntensity );
		}
	#else
		float getShadow( sampler2D shadowMap, vec2 shadowMapSize, float shadowIntensity, float shadowBias, float shadowRadius, vec4 shadowCoord ) {
			float shadow = 1.0;
			shadowCoord.xyz /= shadowCoord.w;
			#ifdef USE_REVERSED_DEPTH_BUFFER
				shadowCoord.z -= shadowBias;
			#else
				shadowCoord.z += shadowBias;
			#endif
			bool inFrustum = shadowCoord.x >= 0.0 && shadowCoord.x <= 1.0 && shadowCoord.y >= 0.0 && shadowCoord.y <= 1.0;
			bool frustumTest = inFrustum && shadowCoord.z <= 1.0;
			if ( frustumTest ) {
				float depth = texture2D( shadowMap, shadowCoord.xy ).r;
				#ifdef USE_REVERSED_DEPTH_BUFFER
					shadow = step( depth, shadowCoord.z );
				#else
					shadow = step( shadowCoord.z, depth );
				#endif
			}
			return mix( 1.0, shadow, shadowIntensity );
		}
	#endif
	#if NUM_SUN_LIGHT_SHADOWS > 0
		float getSunShadow(
			#if defined( SHADOWMAP_TYPE_PCF )
				sampler2DShadow shadowMap,
			#else
				sampler2D shadowMap,
			#endif
			SunLightShadow sunLightShadow,
			int shadowIndex
		) {
			vec4 shadowWorldPosition = vec4( vSunShadowWorldPosition.xyz + vSunShadowWorldNormal * sunLightShadow.shadowNormalBias, 1.0 );
			float viewDepth = vSunShadowWorldPosition.w;
			int cascadeOffset = shadowIndex * SUN_LIGHT_CASCADES;
			float shadow = 1.0;
			for ( int i = SUN_LIGHT_CASCADES - 1; i >= 0; i -- ) {
				vec4 cascade = sunShadowCascade[ cascadeOffset + i ];
				if ( viewDepth >= cascade.x && viewDepth < cascade.y ) {
					float cascadeShadow = getShadow(
						shadowMap,
						sunLightShadow.shadowMapSize,
						sunLightShadow.shadowIntensity,
						sunLightShadow.shadowBias,
						sunLightShadow.shadowRadius,
						sunShadowMatrix[ cascadeOffset + i ] * shadowWorldPosition
					);
					shadow = mix( cascadeShadow, shadow, smoothstep( cascade.z, cascade.y, viewDepth ) );
				}
			}
			return shadow;
		}
	#endif
	#if NUM_POINT_LIGHT_SHADOWS > 0
	#if defined( SHADOWMAP_TYPE_PCF )
	float getPointShadow( samplerCubeShadow shadowMap, vec2 shadowMapSize, float shadowIntensity, float shadowBias, float shadowRadius, vec4 shadowCoord, float shadowCameraNear, float shadowCameraFar ) {
		float shadow = 1.0;
		vec3 lightToPosition = shadowCoord.xyz;
		vec3 bd3D = normalize( lightToPosition );
		vec3 absVec = abs( lightToPosition );
		float viewSpaceZ = max( max( absVec.x, absVec.y ), absVec.z );
		if ( viewSpaceZ - shadowCameraFar <= 0.0 && viewSpaceZ - shadowCameraNear >= 0.0 ) {
			float lightDistance = length( lightToPosition );
			float depthRange = shadowCameraFar - shadowCameraNear;
			#ifdef USE_REVERSED_DEPTH_BUFFER
				float depthA = - shadowCameraNear / depthRange - shadowBias;
				float depthB = shadowCameraNear * shadowCameraFar / ( depthRange * lightDistance );
			#else
				float depthA = shadowCameraFar / depthRange + shadowBias;
				float depthB = - shadowCameraNear * shadowCameraFar / ( depthRange * lightDistance );
			#endif
			float texelSize = shadowRadius / shadowMapSize.x;
			vec3 absDir = abs( bd3D );
			vec3 tangent = absDir.x > absDir.z ? vec3( 0.0, 1.0, 0.0 ) : vec3( 1.0, 0.0, 0.0 );
			tangent = normalize( cross( bd3D, tangent ) );
			vec3 bitangent = cross( bd3D, tangent );
			float phi = interleavedGradientNoise( gl_FragCoord.xy ) * PI2;
			shadow = 0.0;
			for ( int i = 0; i < 5; i ++ ) {
				vec2 offset = vogelDiskSample( i, 5, phi );
				vec3 direction = bd3D + ( tangent * offset.x + bitangent * offset.y ) * texelSize;
				vec3 absDirection = abs( direction );
				float dp = depthA + depthB / max( max( absDirection.x, absDirection.y ), absDirection.z );
				shadow += texture( shadowMap, vec4( direction, dp ) );
			}
			shadow *= 0.2;
		}
		return mix( 1.0, shadow, shadowIntensity );
	}
	#elif defined( SHADOWMAP_TYPE_BASIC )
	float getPointShadow( samplerCube shadowMap, vec2 shadowMapSize, float shadowIntensity, float shadowBias, float shadowRadius, vec4 shadowCoord, float shadowCameraNear, float shadowCameraFar ) {
		float shadow = 1.0;
		vec3 lightToPosition = shadowCoord.xyz;
		vec3 absVec = abs( lightToPosition );
		float viewSpaceZ = max( max( absVec.x, absVec.y ), absVec.z );
		if ( viewSpaceZ - shadowCameraFar <= 0.0 && viewSpaceZ - shadowCameraNear >= 0.0 ) {
			float dp = ( shadowCameraFar * ( viewSpaceZ - shadowCameraNear ) ) / ( viewSpaceZ * ( shadowCameraFar - shadowCameraNear ) );
			dp += shadowBias;
			vec3 bd3D = normalize( lightToPosition );
			float depth = textureCube( shadowMap, bd3D ).r;
			#ifdef USE_REVERSED_DEPTH_BUFFER
				depth = 1.0 - depth;
			#endif
			shadow = step( dp, depth );
		}
		return mix( 1.0, shadow, shadowIntensity );
	}
	#endif
	#endif
#endif`,A0=`#if NUM_SPOT_LIGHT_COORDS > 0
	uniform mat4 spotLightMatrix[ NUM_SPOT_LIGHT_COORDS ];
	varying vec4 vSpotLightCoord[ NUM_SPOT_LIGHT_COORDS ];
#endif
#ifdef USE_SHADOWMAP
	#if NUM_SUN_LIGHT_SHADOWS > 0
		varying vec4 vSunShadowWorldPosition;
		varying vec3 vSunShadowWorldNormal;
	#endif
	#if NUM_DIR_LIGHT_SHADOWS > 0
		uniform mat4 directionalShadowMatrix[ NUM_DIR_LIGHT_SHADOWS ];
		varying vec4 vDirectionalShadowCoord[ NUM_DIR_LIGHT_SHADOWS ];
		struct DirectionalLightShadow {
			float shadowIntensity;
			float shadowBias;
			float shadowNormalBias;
			float shadowRadius;
			vec2 shadowMapSize;
		};
		uniform DirectionalLightShadow directionalLightShadows[ NUM_DIR_LIGHT_SHADOWS ];
	#endif
	#if NUM_SPOT_LIGHT_SHADOWS > 0
		struct SpotLightShadow {
			float shadowIntensity;
			float shadowBias;
			float shadowNormalBias;
			float shadowRadius;
			vec2 shadowMapSize;
		};
		uniform SpotLightShadow spotLightShadows[ NUM_SPOT_LIGHT_SHADOWS ];
	#endif
	#if NUM_POINT_LIGHT_SHADOWS > 0
		uniform mat4 pointShadowMatrix[ NUM_POINT_LIGHT_SHADOWS ];
		varying vec4 vPointShadowCoord[ NUM_POINT_LIGHT_SHADOWS ];
		struct PointLightShadow {
			float shadowIntensity;
			float shadowBias;
			float shadowNormalBias;
			float shadowRadius;
			vec2 shadowMapSize;
			float shadowCameraNear;
			float shadowCameraFar;
		};
		uniform PointLightShadow pointLightShadows[ NUM_POINT_LIGHT_SHADOWS ];
	#endif
#endif`,R0=`#if ( defined( USE_SHADOWMAP ) && ( NUM_DIR_LIGHT_SHADOWS > 0 || NUM_SUN_LIGHT_SHADOWS > 0 || NUM_POINT_LIGHT_SHADOWS > 0 ) ) || ( NUM_SPOT_LIGHT_COORDS > 0 )
	#ifdef HAS_NORMAL
		vec3 shadowWorldNormal = transformNormalByInverseViewMatrix( transformedNormal, viewMatrix );
	#else
		vec3 shadowWorldNormal = vec3( 0.0 );
	#endif
	vec4 shadowWorldPosition;
#endif
#if defined( USE_SHADOWMAP )
	#if NUM_SUN_LIGHT_SHADOWS > 0
		vSunShadowWorldPosition = vec4( worldPosition.xyz, - mvPosition.z );
		vSunShadowWorldNormal = shadowWorldNormal;
	#endif
	#if NUM_DIR_LIGHT_SHADOWS > 0
		#pragma unroll_loop_start
		for ( int i = 0; i < NUM_DIR_LIGHT_SHADOWS; i ++ ) {
			shadowWorldPosition = worldPosition + vec4( shadowWorldNormal * directionalLightShadows[ i ].shadowNormalBias, 0 );
			vDirectionalShadowCoord[ i ] = directionalShadowMatrix[ i ] * shadowWorldPosition;
		}
		#pragma unroll_loop_end
	#endif
	#if NUM_POINT_LIGHT_SHADOWS > 0
		#pragma unroll_loop_start
		for ( int i = 0; i < NUM_POINT_LIGHT_SHADOWS; i ++ ) {
			shadowWorldPosition = worldPosition + vec4( shadowWorldNormal * pointLightShadows[ i ].shadowNormalBias, 0 );
			vPointShadowCoord[ i ] = pointShadowMatrix[ i ] * shadowWorldPosition;
		}
		#pragma unroll_loop_end
	#endif
#endif
#if NUM_SPOT_LIGHT_COORDS > 0
	#pragma unroll_loop_start
	for ( int i = 0; i < NUM_SPOT_LIGHT_COORDS; i ++ ) {
		shadowWorldPosition = worldPosition;
		#if ( defined( USE_SHADOWMAP ) && UNROLLED_LOOP_INDEX < NUM_SPOT_LIGHT_SHADOWS )
			shadowWorldPosition.xyz += shadowWorldNormal * spotLightShadows[ i ].shadowNormalBias;
		#endif
		vSpotLightCoord[ i ] = spotLightMatrix[ i ] * shadowWorldPosition;
	}
	#pragma unroll_loop_end
#endif`,C0=`float getShadowMask() {
	float shadow = 1.0;
	#ifdef USE_SHADOWMAP
	#if NUM_SUN_LIGHT_SHADOWS > 0
	SunLightShadow sunLight;
	#pragma unroll_loop_start
	for ( int i = 0; i < NUM_SUN_LIGHT_SHADOWS; i ++ ) {
		sunLight = sunLightShadows[ i ];
		shadow *= receiveShadow ? getSunShadow( sunShadowMap[ i ], sunLight, UNROLLED_LOOP_INDEX ) : 1.0;
	}
	#pragma unroll_loop_end
	#endif
	#if NUM_DIR_LIGHT_SHADOWS > 0
	DirectionalLightShadow directionalLight;
	#pragma unroll_loop_start
	for ( int i = 0; i < NUM_DIR_LIGHT_SHADOWS; i ++ ) {
		directionalLight = directionalLightShadows[ i ];
		shadow *= receiveShadow ? getShadow( directionalShadowMap[ i ], directionalLight.shadowMapSize, directionalLight.shadowIntensity, directionalLight.shadowBias, directionalLight.shadowRadius, vDirectionalShadowCoord[ i ] ) : 1.0;
	}
	#pragma unroll_loop_end
	#endif
	#if NUM_SPOT_LIGHT_SHADOWS > 0
	SpotLightShadow spotLight;
	#pragma unroll_loop_start
	for ( int i = 0; i < NUM_SPOT_LIGHT_SHADOWS; i ++ ) {
		spotLight = spotLightShadows[ i ];
		shadow *= receiveShadow ? getShadow( spotShadowMap[ i ], spotLight.shadowMapSize, spotLight.shadowIntensity, spotLight.shadowBias, spotLight.shadowRadius, vSpotLightCoord[ i ] ) : 1.0;
	}
	#pragma unroll_loop_end
	#endif
	#if NUM_POINT_LIGHT_SHADOWS > 0 && ( defined( SHADOWMAP_TYPE_PCF ) || defined( SHADOWMAP_TYPE_BASIC ) )
	PointLightShadow pointLight;
	#pragma unroll_loop_start
	for ( int i = 0; i < NUM_POINT_LIGHT_SHADOWS; i ++ ) {
		pointLight = pointLightShadows[ i ];
		shadow *= receiveShadow ? getPointShadow( pointShadowMap[ i ], pointLight.shadowMapSize, pointLight.shadowIntensity, pointLight.shadowBias, pointLight.shadowRadius, vPointShadowCoord[ i ], pointLight.shadowCameraNear, pointLight.shadowCameraFar ) : 1.0;
	}
	#pragma unroll_loop_end
	#endif
	#endif
	return shadow;
}`,P0=`#ifdef USE_SKINNING
	mat4 boneMatX = getBoneMatrix( skinIndex.x );
	mat4 boneMatY = getBoneMatrix( skinIndex.y );
	mat4 boneMatZ = getBoneMatrix( skinIndex.z );
	mat4 boneMatW = getBoneMatrix( skinIndex.w );
#endif`,I0=`#ifdef USE_SKINNING
	uniform mat4 bindMatrix;
	uniform mat4 bindMatrixInverse;
	uniform highp sampler2D boneTexture;
	mat4 getBoneMatrix( const in float i ) {
		int size = textureSize( boneTexture, 0 ).x;
		int j = int( i ) * 4;
		int x = j % size;
		int y = j / size;
		vec4 v1 = texelFetch( boneTexture, ivec2( x, y ), 0 );
		vec4 v2 = texelFetch( boneTexture, ivec2( x + 1, y ), 0 );
		vec4 v3 = texelFetch( boneTexture, ivec2( x + 2, y ), 0 );
		vec4 v4 = texelFetch( boneTexture, ivec2( x + 3, y ), 0 );
		return mat4( v1, v2, v3, v4 );
	}
#endif`,D0=`#ifdef USE_SKINNING
	vec4 skinVertex = bindMatrix * vec4( transformed, 1.0 );
	vec4 skinned = vec4( 0.0 );
	skinned += boneMatX * skinVertex * skinWeight.x;
	skinned += boneMatY * skinVertex * skinWeight.y;
	skinned += boneMatZ * skinVertex * skinWeight.z;
	skinned += boneMatW * skinVertex * skinWeight.w;
	transformed = ( bindMatrixInverse * skinned ).xyz;
#endif`,L0=`#ifdef USE_SKINNING
	mat4 skinMatrix = mat4( 0.0 );
	skinMatrix += skinWeight.x * boneMatX;
	skinMatrix += skinWeight.y * boneMatY;
	skinMatrix += skinWeight.z * boneMatZ;
	skinMatrix += skinWeight.w * boneMatW;
	skinMatrix = bindMatrixInverse * skinMatrix * bindMatrix;
	objectNormal = vec4( skinMatrix * vec4( objectNormal, 0.0 ) ).xyz;
	#ifdef USE_TANGENT
		objectTangent = vec4( skinMatrix * vec4( objectTangent, 0.0 ) ).xyz;
	#endif
#endif`,N0=`float specularStrength;
#ifdef USE_SPECULARMAP
	vec4 texelSpecular = texture2D( specularMap, vSpecularMapUv );
	specularStrength = texelSpecular.r;
#else
	specularStrength = 1.0;
#endif`,U0=`#ifdef USE_SPECULARMAP
	uniform sampler2D specularMap;
#endif`,F0=`#if defined( TONE_MAPPING )
	gl_FragColor.rgb = toneMapping( gl_FragColor.rgb );
#endif`,O0=`#ifndef saturate
#define saturate( a ) clamp( a, 0.0, 1.0 )
#endif
uniform float toneMappingExposure;
vec3 LinearToneMapping( vec3 color ) {
	return saturate( toneMappingExposure * color );
}
vec3 ReinhardToneMapping( vec3 color ) {
	color *= toneMappingExposure;
	return saturate( color / ( vec3( 1.0 ) + color ) );
}
vec3 CineonToneMapping( vec3 color ) {
	color *= toneMappingExposure;
	color = max( vec3( 0.0 ), color - 0.004 );
	return pow( ( color * ( 6.2 * color + 0.5 ) ) / ( color * ( 6.2 * color + 1.7 ) + 0.06 ), vec3( 2.2 ) );
}
vec3 RRTAndODTFit( vec3 v ) {
	vec3 a = v * ( v + 0.0245786 ) - 0.000090537;
	vec3 b = v * ( 0.983729 * v + 0.4329510 ) + 0.238081;
	return a / b;
}
vec3 ACESFilmicToneMapping( vec3 color ) {
	const mat3 ACESInputMat = mat3(
		vec3( 0.59719, 0.07600, 0.02840 ),		vec3( 0.35458, 0.90834, 0.13383 ),
		vec3( 0.04823, 0.01566, 0.83777 )
	);
	const mat3 ACESOutputMat = mat3(
		vec3(  1.60475, -0.10208, -0.00327 ),		vec3( -0.53108,  1.10813, -0.07276 ),
		vec3( -0.07367, -0.00605,  1.07602 )
	);
	color *= toneMappingExposure / 0.6;
	color = ACESInputMat * color;
	color = RRTAndODTFit( color );
	color = ACESOutputMat * color;
	return saturate( color );
}
const mat3 LINEAR_REC2020_TO_LINEAR_SRGB = mat3(
	vec3( 1.6605, - 0.1246, - 0.0182 ),
	vec3( - 0.5876, 1.1329, - 0.1006 ),
	vec3( - 0.0728, - 0.0083, 1.1187 )
);
const mat3 LINEAR_SRGB_TO_LINEAR_REC2020 = mat3(
	vec3( 0.6274, 0.0691, 0.0164 ),
	vec3( 0.3293, 0.9195, 0.0880 ),
	vec3( 0.0433, 0.0113, 0.8956 )
);
vec3 agxDefaultContrastApprox( vec3 x ) {
	vec3 x2 = x * x;
	vec3 x4 = x2 * x2;
	return + 15.5 * x4 * x2
		- 40.14 * x4 * x
		+ 31.96 * x4
		- 6.868 * x2 * x
		+ 0.4298 * x2
		+ 0.1191 * x
		- 0.00232;
}
vec3 AgXToneMapping( vec3 color ) {
	const mat3 AgXInsetMatrix = mat3(
		vec3( 0.856627153315983, 0.137318972929847, 0.11189821299995 ),
		vec3( 0.0951212405381588, 0.761241990602591, 0.0767994186031903 ),
		vec3( 0.0482516061458583, 0.101439036467562, 0.811302368396859 )
	);
	const mat3 AgXOutsetMatrix = mat3(
		vec3( 1.1271005818144368, - 0.1413297634984383, - 0.14132976349843826 ),
		vec3( - 0.11060664309660323, 1.157823702216272, - 0.11060664309660294 ),
		vec3( - 0.016493938717834573, - 0.016493938717834257, 1.2519364065950405 )
	);
	const float AgxMinEv = - 12.47393;	const float AgxMaxEv = 4.026069;
	color *= toneMappingExposure;
	color = LINEAR_SRGB_TO_LINEAR_REC2020 * color;
	color = AgXInsetMatrix * color;
	color = max( color, 1e-10 );	color = log2( color );
	color = ( color - AgxMinEv ) / ( AgxMaxEv - AgxMinEv );
	color = clamp( color, 0.0, 1.0 );
	color = agxDefaultContrastApprox( color );
	color = AgXOutsetMatrix * color;
	color = pow( max( vec3( 0.0 ), color ), vec3( 2.2 ) );
	color = LINEAR_REC2020_TO_LINEAR_SRGB * color;
	color = clamp( color, 0.0, 1.0 );
	return color;
}
vec3 NeutralToneMapping( vec3 color ) {
	const float StartCompression = 0.8 - 0.04;
	const float Desaturation = 0.15;
	color *= toneMappingExposure;
	float x = min( color.r, min( color.g, color.b ) );
	float offset = x < 0.08 ? x - 6.25 * x * x : 0.04;
	color -= offset;
	float peak = max( color.r, max( color.g, color.b ) );
	if ( peak < StartCompression ) return color;
	float d = 1. - StartCompression;
	float newPeak = 1. - d * d / ( peak + d - StartCompression );
	color *= newPeak / peak;
	float g = 1. - 1. / ( Desaturation * ( peak - newPeak ) + 1. );
	return mix( color, vec3( newPeak ), g );
}
vec3 CustomToneMapping( vec3 color ) { return color; }`,B0=`#ifdef USE_TRANSMISSION
	material.transmission = transmission;
	material.transmissionAlpha = 1.0;
	material.thickness = thickness;
	material.attenuationDistance = attenuationDistance;
	material.attenuationColor = attenuationColor;
	#ifdef USE_TRANSMISSIONMAP
		material.transmission *= texture2D( transmissionMap, vTransmissionMapUv ).r;
	#endif
	#ifdef USE_THICKNESSMAP
		material.thickness *= texture2D( thicknessMap, vThicknessMapUv ).g;
	#endif
	vec3 pos = vWorldPosition;
	vec3 v = normalize( cameraPosition - pos );
	vec3 n = transformNormalByInverseViewMatrix( normal, viewMatrix );
	vec4 transmitted = getIBLVolumeRefraction(
		n, v, material.roughness, material.diffuseContribution, material.specularColorBlended, material.specularF90,
		pos, modelMatrix, viewMatrix, projectionMatrix, material.dispersion, material.ior, material.thickness,
		material.attenuationColor, material.attenuationDistance );
	material.transmissionAlpha = mix( material.transmissionAlpha, transmitted.a, material.transmission );
	totalDiffuse = mix( totalDiffuse, transmitted.rgb, material.transmission );
#endif`,z0=`#ifdef USE_TRANSMISSION
	uniform float transmission;
	uniform float thickness;
	uniform float attenuationDistance;
	uniform vec3 attenuationColor;
	#ifdef USE_TRANSMISSIONMAP
		uniform sampler2D transmissionMap;
	#endif
	#ifdef USE_THICKNESSMAP
		uniform sampler2D thicknessMap;
	#endif
	uniform vec2 transmissionSamplerSize;
	uniform sampler2D transmissionSamplerMap;
	uniform mat4 modelMatrix;
	uniform mat4 projectionMatrix;
	varying vec3 vWorldPosition;
	float w0( float a ) {
		return ( 1.0 / 6.0 ) * ( a * ( a * ( - a + 3.0 ) - 3.0 ) + 1.0 );
	}
	float w1( float a ) {
		return ( 1.0 / 6.0 ) * ( a *  a * ( 3.0 * a - 6.0 ) + 4.0 );
	}
	float w2( float a ){
		return ( 1.0 / 6.0 ) * ( a * ( a * ( - 3.0 * a + 3.0 ) + 3.0 ) + 1.0 );
	}
	float w3( float a ) {
		return ( 1.0 / 6.0 ) * ( a * a * a );
	}
	float g0( float a ) {
		return w0( a ) + w1( a );
	}
	float g1( float a ) {
		return w2( a ) + w3( a );
	}
	float h0( float a ) {
		return - 1.0 + w1( a ) / ( w0( a ) + w1( a ) );
	}
	float h1( float a ) {
		return 1.0 + w3( a ) / ( w2( a ) + w3( a ) );
	}
	vec4 bicubic( sampler2D tex, vec2 uv, vec4 texelSize, float lod ) {
		uv = uv * texelSize.zw + 0.5;
		vec2 iuv = floor( uv );
		vec2 fuv = fract( uv );
		float g0x = g0( fuv.x );
		float g1x = g1( fuv.x );
		float h0x = h0( fuv.x );
		float h1x = h1( fuv.x );
		float h0y = h0( fuv.y );
		float h1y = h1( fuv.y );
		vec2 p0 = ( vec2( iuv.x + h0x, iuv.y + h0y ) - 0.5 ) * texelSize.xy;
		vec2 p1 = ( vec2( iuv.x + h1x, iuv.y + h0y ) - 0.5 ) * texelSize.xy;
		vec2 p2 = ( vec2( iuv.x + h0x, iuv.y + h1y ) - 0.5 ) * texelSize.xy;
		vec2 p3 = ( vec2( iuv.x + h1x, iuv.y + h1y ) - 0.5 ) * texelSize.xy;
		return g0( fuv.y ) * ( g0x * textureLod( tex, p0, lod ) + g1x * textureLod( tex, p1, lod ) ) +
			g1( fuv.y ) * ( g0x * textureLod( tex, p2, lod ) + g1x * textureLod( tex, p3, lod ) );
	}
	vec4 textureBicubic( sampler2D sampler, vec2 uv, float lod ) {
		vec2 fLodSize = vec2( textureSize( sampler, int( lod ) ) );
		vec2 cLodSize = vec2( textureSize( sampler, int( lod + 1.0 ) ) );
		vec2 fLodSizeInv = 1.0 / fLodSize;
		vec2 cLodSizeInv = 1.0 / cLodSize;
		vec4 fSample = bicubic( sampler, uv, vec4( fLodSizeInv, fLodSize ), floor( lod ) );
		vec4 cSample = bicubic( sampler, uv, vec4( cLodSizeInv, cLodSize ), ceil( lod ) );
		return mix( fSample, cSample, fract( lod ) );
	}
	vec3 getVolumeTransmissionRay( const in vec3 n, const in vec3 v, const in float thickness, const in float ior, const in mat4 modelMatrix ) {
		vec3 refractionVector = refract( - v, normalize( n ), 1.0 / ior );
		vec3 modelScale;
		modelScale.x = length( vec3( modelMatrix[ 0 ].xyz ) );
		modelScale.y = length( vec3( modelMatrix[ 1 ].xyz ) );
		modelScale.z = length( vec3( modelMatrix[ 2 ].xyz ) );
		return normalize( refractionVector ) * thickness * modelScale;
	}
	float applyIorToRoughness( const in float roughness, const in float ior ) {
		return roughness * clamp( ior * 2.0 - 2.0, 0.0, 1.0 );
	}
	vec4 getTransmissionSample( const in vec2 fragCoord, const in float roughness, const in float ior ) {
		float lod = log2( transmissionSamplerSize.x ) * applyIorToRoughness( roughness, ior );
		return textureBicubic( transmissionSamplerMap, fragCoord.xy, lod );
	}
	vec3 volumeAttenuation( const in float transmissionDistance, const in vec3 attenuationColor, const in float attenuationDistance ) {
		if ( isinf( attenuationDistance ) ) {
			return vec3( 1.0 );
		} else {
			vec3 attenuationCoefficient = -log( attenuationColor ) / attenuationDistance;
			vec3 transmittance = exp( - attenuationCoefficient * transmissionDistance );			return transmittance;
		}
	}
	vec4 getIBLVolumeRefraction( const in vec3 n, const in vec3 v, const in float roughness, const in vec3 diffuseColor,
		const in vec3 specularColor, const in float specularF90, const in vec3 position, const in mat4 modelMatrix,
		const in mat4 viewMatrix, const in mat4 projMatrix, const in float dispersion, const in float ior, const in float thickness,
		const in vec3 attenuationColor, const in float attenuationDistance ) {
		vec4 transmittedLight;
		vec3 transmittance;
		#ifdef USE_DISPERSION
			float halfSpread = ( ior - 1.0 ) * 0.025 * dispersion;
			vec3 iors = vec3( ior - halfSpread, ior, ior + halfSpread );
			for ( int i = 0; i < 3; i ++ ) {
				vec3 transmissionRay = getVolumeTransmissionRay( n, v, thickness, iors[ i ], modelMatrix );
				vec3 refractedRayExit = position + transmissionRay;
				vec4 ndcPos = projMatrix * viewMatrix * vec4( refractedRayExit, 1.0 );
				vec2 refractionCoords = ndcPos.xy / ndcPos.w;
				refractionCoords += 1.0;
				refractionCoords /= 2.0;
				vec4 transmissionSample = getTransmissionSample( refractionCoords, roughness, iors[ i ] );
				transmittedLight[ i ] = transmissionSample[ i ];
				transmittedLight.a += transmissionSample.a;
				transmittance[ i ] = diffuseColor[ i ] * volumeAttenuation( length( transmissionRay ), attenuationColor, attenuationDistance )[ i ];
			}
			transmittedLight.a /= 3.0;
		#else
			vec3 transmissionRay = getVolumeTransmissionRay( n, v, thickness, ior, modelMatrix );
			vec3 refractedRayExit = position + transmissionRay;
			vec4 ndcPos = projMatrix * viewMatrix * vec4( refractedRayExit, 1.0 );
			vec2 refractionCoords = ndcPos.xy / ndcPos.w;
			refractionCoords += 1.0;
			refractionCoords /= 2.0;
			transmittedLight = getTransmissionSample( refractionCoords, roughness, ior );
			transmittance = diffuseColor * volumeAttenuation( length( transmissionRay ), attenuationColor, attenuationDistance );
		#endif
		vec3 attenuatedColor = transmittance * transmittedLight.rgb;
		vec3 F = EnvironmentBRDF( n, v, specularColor, specularF90, roughness );
		float transmittanceFactor = ( transmittance.r + transmittance.g + transmittance.b ) / 3.0;
		return vec4( ( 1.0 - F ) * attenuatedColor, 1.0 - ( 1.0 - transmittedLight.a ) * transmittanceFactor );
	}
#endif`,k0=`#if defined( USE_UV ) || defined( USE_ANISOTROPY )
	varying vec2 vUv;
#endif
#ifdef USE_MAP
	varying vec2 vMapUv;
#endif
#ifdef USE_ALPHAMAP
	varying vec2 vAlphaMapUv;
#endif
#ifdef USE_LIGHTMAP
	varying vec2 vLightMapUv;
#endif
#ifdef USE_AOMAP
	varying vec2 vAoMapUv;
#endif
#ifdef USE_BUMPMAP
	varying vec2 vBumpMapUv;
#endif
#ifdef USE_NORMALMAP
	varying vec2 vNormalMapUv;
#endif
#ifdef USE_EMISSIVEMAP
	varying vec2 vEmissiveMapUv;
#endif
#ifdef USE_METALNESSMAP
	varying vec2 vMetalnessMapUv;
#endif
#ifdef USE_ROUGHNESSMAP
	varying vec2 vRoughnessMapUv;
#endif
#ifdef USE_DIFFUSE_ROUGHNESSMAP
	varying vec2 vDiffuseRoughnessMapUv;
#endif
#ifdef USE_ANISOTROPYMAP
	varying vec2 vAnisotropyMapUv;
#endif
#ifdef USE_CLEARCOATMAP
	varying vec2 vClearcoatMapUv;
#endif
#ifdef USE_CLEARCOAT_NORMALMAP
	varying vec2 vClearcoatNormalMapUv;
#endif
#ifdef USE_CLEARCOAT_ROUGHNESSMAP
	varying vec2 vClearcoatRoughnessMapUv;
#endif
#ifdef USE_IRIDESCENCEMAP
	varying vec2 vIridescenceMapUv;
#endif
#ifdef USE_IRIDESCENCE_THICKNESSMAP
	varying vec2 vIridescenceThicknessMapUv;
#endif
#ifdef USE_SHEEN_COLORMAP
	varying vec2 vSheenColorMapUv;
#endif
#ifdef USE_SHEEN_ROUGHNESSMAP
	varying vec2 vSheenRoughnessMapUv;
#endif
#ifdef USE_SPECULARMAP
	varying vec2 vSpecularMapUv;
#endif
#ifdef USE_SPECULAR_COLORMAP
	varying vec2 vSpecularColorMapUv;
#endif
#ifdef USE_SPECULAR_INTENSITYMAP
	varying vec2 vSpecularIntensityMapUv;
#endif
#ifdef USE_TRANSMISSIONMAP
	uniform mat3 transmissionMapTransform;
	varying vec2 vTransmissionMapUv;
#endif
#ifdef USE_THICKNESSMAP
	uniform mat3 thicknessMapTransform;
	varying vec2 vThicknessMapUv;
#endif`,H0=`#if defined( USE_UV ) || defined( USE_ANISOTROPY )
	varying vec2 vUv;
#endif
#ifdef USE_MAP
	uniform mat3 mapTransform;
	varying vec2 vMapUv;
#endif
#ifdef USE_ALPHAMAP
	uniform mat3 alphaMapTransform;
	varying vec2 vAlphaMapUv;
#endif
#ifdef USE_LIGHTMAP
	uniform mat3 lightMapTransform;
	varying vec2 vLightMapUv;
#endif
#ifdef USE_AOMAP
	uniform mat3 aoMapTransform;
	varying vec2 vAoMapUv;
#endif
#ifdef USE_BUMPMAP
	uniform mat3 bumpMapTransform;
	varying vec2 vBumpMapUv;
#endif
#ifdef USE_NORMALMAP
	uniform mat3 normalMapTransform;
	varying vec2 vNormalMapUv;
#endif
#ifdef USE_DISPLACEMENTMAP
	uniform mat3 displacementMapTransform;
	varying vec2 vDisplacementMapUv;
#endif
#ifdef USE_EMISSIVEMAP
	uniform mat3 emissiveMapTransform;
	varying vec2 vEmissiveMapUv;
#endif
#ifdef USE_METALNESSMAP
	uniform mat3 metalnessMapTransform;
	varying vec2 vMetalnessMapUv;
#endif
#ifdef USE_ROUGHNESSMAP
	uniform mat3 roughnessMapTransform;
	varying vec2 vRoughnessMapUv;
#endif
#ifdef USE_DIFFUSE_ROUGHNESSMAP
	uniform mat3 diffuseRoughnessMapTransform;
	varying vec2 vDiffuseRoughnessMapUv;
#endif
#ifdef USE_ANISOTROPYMAP
	uniform mat3 anisotropyMapTransform;
	varying vec2 vAnisotropyMapUv;
#endif
#ifdef USE_CLEARCOATMAP
	uniform mat3 clearcoatMapTransform;
	varying vec2 vClearcoatMapUv;
#endif
#ifdef USE_CLEARCOAT_NORMALMAP
	uniform mat3 clearcoatNormalMapTransform;
	varying vec2 vClearcoatNormalMapUv;
#endif
#ifdef USE_CLEARCOAT_ROUGHNESSMAP
	uniform mat3 clearcoatRoughnessMapTransform;
	varying vec2 vClearcoatRoughnessMapUv;
#endif
#ifdef USE_SHEEN_COLORMAP
	uniform mat3 sheenColorMapTransform;
	varying vec2 vSheenColorMapUv;
#endif
#ifdef USE_SHEEN_ROUGHNESSMAP
	uniform mat3 sheenRoughnessMapTransform;
	varying vec2 vSheenRoughnessMapUv;
#endif
#ifdef USE_IRIDESCENCEMAP
	uniform mat3 iridescenceMapTransform;
	varying vec2 vIridescenceMapUv;
#endif
#ifdef USE_IRIDESCENCE_THICKNESSMAP
	uniform mat3 iridescenceThicknessMapTransform;
	varying vec2 vIridescenceThicknessMapUv;
#endif
#ifdef USE_SPECULARMAP
	uniform mat3 specularMapTransform;
	varying vec2 vSpecularMapUv;
#endif
#ifdef USE_SPECULAR_COLORMAP
	uniform mat3 specularColorMapTransform;
	varying vec2 vSpecularColorMapUv;
#endif
#ifdef USE_SPECULAR_INTENSITYMAP
	uniform mat3 specularIntensityMapTransform;
	varying vec2 vSpecularIntensityMapUv;
#endif
#ifdef USE_TRANSMISSIONMAP
	uniform mat3 transmissionMapTransform;
	varying vec2 vTransmissionMapUv;
#endif
#ifdef USE_THICKNESSMAP
	uniform mat3 thicknessMapTransform;
	varying vec2 vThicknessMapUv;
#endif`,V0=`#if defined( USE_UV ) || defined( USE_ANISOTROPY )
	vUv = vec3( uv, 1 ).xy;
#endif
#ifdef USE_MAP
	vMapUv = ( mapTransform * vec3( MAP_UV, 1 ) ).xy;
#endif
#ifdef USE_ALPHAMAP
	vAlphaMapUv = ( alphaMapTransform * vec3( ALPHAMAP_UV, 1 ) ).xy;
#endif
#ifdef USE_LIGHTMAP
	vLightMapUv = ( lightMapTransform * vec3( LIGHTMAP_UV, 1 ) ).xy;
#endif
#ifdef USE_AOMAP
	vAoMapUv = ( aoMapTransform * vec3( AOMAP_UV, 1 ) ).xy;
#endif
#ifdef USE_BUMPMAP
	vBumpMapUv = ( bumpMapTransform * vec3( BUMPMAP_UV, 1 ) ).xy;
#endif
#ifdef USE_NORMALMAP
	vNormalMapUv = ( normalMapTransform * vec3( NORMALMAP_UV, 1 ) ).xy;
#endif
#ifdef USE_DISPLACEMENTMAP
	vDisplacementMapUv = ( displacementMapTransform * vec3( DISPLACEMENTMAP_UV, 1 ) ).xy;
#endif
#ifdef USE_EMISSIVEMAP
	vEmissiveMapUv = ( emissiveMapTransform * vec3( EMISSIVEMAP_UV, 1 ) ).xy;
#endif
#ifdef USE_METALNESSMAP
	vMetalnessMapUv = ( metalnessMapTransform * vec3( METALNESSMAP_UV, 1 ) ).xy;
#endif
#ifdef USE_ROUGHNESSMAP
	vRoughnessMapUv = ( roughnessMapTransform * vec3( ROUGHNESSMAP_UV, 1 ) ).xy;
#endif
#ifdef USE_DIFFUSE_ROUGHNESSMAP
	vDiffuseRoughnessMapUv = ( diffuseRoughnessMapTransform * vec3( DIFFUSE_ROUGHNESSMAP_UV, 1 ) ).xy;
#endif
#ifdef USE_ANISOTROPYMAP
	vAnisotropyMapUv = ( anisotropyMapTransform * vec3( ANISOTROPYMAP_UV, 1 ) ).xy;
#endif
#ifdef USE_CLEARCOATMAP
	vClearcoatMapUv = ( clearcoatMapTransform * vec3( CLEARCOATMAP_UV, 1 ) ).xy;
#endif
#ifdef USE_CLEARCOAT_NORMALMAP
	vClearcoatNormalMapUv = ( clearcoatNormalMapTransform * vec3( CLEARCOAT_NORMALMAP_UV, 1 ) ).xy;
#endif
#ifdef USE_CLEARCOAT_ROUGHNESSMAP
	vClearcoatRoughnessMapUv = ( clearcoatRoughnessMapTransform * vec3( CLEARCOAT_ROUGHNESSMAP_UV, 1 ) ).xy;
#endif
#ifdef USE_IRIDESCENCEMAP
	vIridescenceMapUv = ( iridescenceMapTransform * vec3( IRIDESCENCEMAP_UV, 1 ) ).xy;
#endif
#ifdef USE_IRIDESCENCE_THICKNESSMAP
	vIridescenceThicknessMapUv = ( iridescenceThicknessMapTransform * vec3( IRIDESCENCE_THICKNESSMAP_UV, 1 ) ).xy;
#endif
#ifdef USE_SHEEN_COLORMAP
	vSheenColorMapUv = ( sheenColorMapTransform * vec3( SHEEN_COLORMAP_UV, 1 ) ).xy;
#endif
#ifdef USE_SHEEN_ROUGHNESSMAP
	vSheenRoughnessMapUv = ( sheenRoughnessMapTransform * vec3( SHEEN_ROUGHNESSMAP_UV, 1 ) ).xy;
#endif
#ifdef USE_SPECULARMAP
	vSpecularMapUv = ( specularMapTransform * vec3( SPECULARMAP_UV, 1 ) ).xy;
#endif
#ifdef USE_SPECULAR_COLORMAP
	vSpecularColorMapUv = ( specularColorMapTransform * vec3( SPECULAR_COLORMAP_UV, 1 ) ).xy;
#endif
#ifdef USE_SPECULAR_INTENSITYMAP
	vSpecularIntensityMapUv = ( specularIntensityMapTransform * vec3( SPECULAR_INTENSITYMAP_UV, 1 ) ).xy;
#endif
#ifdef USE_TRANSMISSIONMAP
	vTransmissionMapUv = ( transmissionMapTransform * vec3( TRANSMISSIONMAP_UV, 1 ) ).xy;
#endif
#ifdef USE_THICKNESSMAP
	vThicknessMapUv = ( thicknessMapTransform * vec3( THICKNESSMAP_UV, 1 ) ).xy;
#endif`,G0=`#if defined( USE_ENVMAP ) || defined( DISTANCE ) || defined ( USE_SHADOWMAP ) || defined ( USE_TRANSMISSION ) || NUM_SPOT_LIGHT_COORDS > 0
	vec4 worldPosition = vec4( transformed, 1.0 );
	#ifdef USE_BATCHING
		worldPosition = batchingMatrix * worldPosition;
	#endif
	#ifdef USE_INSTANCING
		worldPosition = instanceMatrix * worldPosition;
	#endif
	worldPosition = modelMatrix * worldPosition;
#endif`,W0=`varying vec2 vUv;
uniform mat3 uvTransform;
void main() {
	vUv = ( uvTransform * vec3( uv, 1 ) ).xy;
	gl_Position = vec4( position.xy, 1.0, 1.0 );
}`,X0=`uniform sampler2D t2D;
uniform float backgroundIntensity;
varying vec2 vUv;
void main() {
	vec4 texColor = texture2D( t2D, vUv );
	#ifdef DECODE_VIDEO_TEXTURE
		texColor = vec4( mix( pow( texColor.rgb * 0.9478672986 + vec3( 0.0521327014 ), vec3( 2.4 ) ), texColor.rgb * 0.0773993808, vec3( lessThanEqual( texColor.rgb, vec3( 0.04045 ) ) ) ), texColor.w );
	#endif
	texColor.rgb *= backgroundIntensity;
	gl_FragColor = texColor;
	#include <tonemapping_fragment>
	#include <colorspace_fragment>
}`,q0=`varying vec3 vWorldDirection;
#include <common>
void main() {
	vWorldDirection = transformDirection( position, modelMatrix );
	#include <begin_vertex>
	#include <project_vertex>
	gl_Position.z = gl_Position.w;
}`,Y0=`uniform float backgroundBlurriness;
uniform float backgroundIntensity;
uniform mat3 backgroundRotation;
varying vec3 vWorldDirection;
#include <envmap_common_pars_fragment>
void main() {
	#ifdef ENVMAP_TYPE_PMREM
		vec4 texColor = textureLod( envMap, backgroundRotation * vWorldDirection, roughnessToMip( backgroundBlurriness ) );
	#else
		vec4 texColor = textureCube( envMap, backgroundRotation * vWorldDirection );
	#endif
	texColor.rgb *= backgroundIntensity;
	gl_FragColor = texColor;
	#include <tonemapping_fragment>
	#include <colorspace_fragment>
}`,Z0=`varying vec3 vWorldDirection;
#include <common>
void main() {
	vWorldDirection = transformDirection( position, modelMatrix );
	#include <begin_vertex>
	#include <project_vertex>
	gl_Position.z = gl_Position.w;
}`,K0=`uniform samplerCube tCube;
uniform float tFlip;
uniform float opacity;
varying vec3 vWorldDirection;
void main() {
	vec4 texColor = textureCube( tCube, vec3( tFlip * vWorldDirection.x, vWorldDirection.yz ) );
	gl_FragColor = texColor;
	gl_FragColor.a *= opacity;
	#include <tonemapping_fragment>
	#include <colorspace_fragment>
}`,J0=`#include <common>
#include <batching_pars_vertex>
#include <uv_pars_vertex>
#include <displacementmap_pars_vertex>
#include <morphtarget_pars_vertex>
#include <skinning_pars_vertex>
#include <logdepthbuf_pars_vertex>
#include <clipping_planes_pars_vertex>
varying vec2 vHighPrecisionZW;
void main() {
	#include <uv_vertex>
	#include <batching_vertex>
	#include <skinbase_vertex>
	#include <morphinstance_vertex>
	#ifdef USE_DISPLACEMENTMAP
		#include <beginnormal_vertex>
		#include <morphnormal_vertex>
		#include <skinnormal_vertex>
	#endif
	#include <begin_vertex>
	#include <morphtarget_vertex>
	#include <skinning_vertex>
	#include <displacementmap_vertex>
	#include <project_vertex>
	#include <logdepthbuf_vertex>
	#include <clipping_planes_vertex>
	vHighPrecisionZW = gl_Position.zw;
}`,$0=`#if DEPTH_PACKING == 3200
	uniform float opacity;
#endif
#include <common>
#include <packing>
#include <uv_pars_fragment>
#include <map_pars_fragment>
#include <alphamap_pars_fragment>
#include <alphatest_pars_fragment>
#include <alphahash_pars_fragment>
#include <logdepthbuf_pars_fragment>
#include <clipping_planes_pars_fragment>
varying vec2 vHighPrecisionZW;
void main() {
	vec4 diffuseColor = vec4( 1.0 );
	#include <clipping_planes_fragment>
	#if DEPTH_PACKING == 3200
		diffuseColor.a = opacity;
	#endif
	#include <map_fragment>
	#include <alphamap_fragment>
	#include <alphatest_fragment>
	#include <alphahash_fragment>
	#include <logdepthbuf_fragment>
	#ifdef USE_REVERSED_DEPTH_BUFFER
		float fragCoordZ = vHighPrecisionZW[ 0 ] / vHighPrecisionZW[ 1 ];
	#else
		float fragCoordZ = 0.5 * vHighPrecisionZW[ 0 ] / vHighPrecisionZW[ 1 ] + 0.5;
	#endif
	#if DEPTH_PACKING == 3200
		gl_FragColor = vec4( vec3( 1.0 - fragCoordZ ), opacity );
	#elif DEPTH_PACKING == 3201
		gl_FragColor = packDepthToRGBA( fragCoordZ );
	#elif DEPTH_PACKING == 3202
		gl_FragColor = vec4( packDepthToRGB( fragCoordZ ), 1.0 );
	#elif DEPTH_PACKING == 3203
		gl_FragColor = vec4( packDepthToRG( fragCoordZ ), 0.0, 1.0 );
	#endif
}`,j0=`#define DISTANCE
varying vec3 vWorldPosition;
#include <common>
#include <batching_pars_vertex>
#include <uv_pars_vertex>
#include <displacementmap_pars_vertex>
#include <morphtarget_pars_vertex>
#include <skinning_pars_vertex>
#include <clipping_planes_pars_vertex>
void main() {
	#include <uv_vertex>
	#include <batching_vertex>
	#include <skinbase_vertex>
	#include <morphinstance_vertex>
	#ifdef USE_DISPLACEMENTMAP
		#include <beginnormal_vertex>
		#include <morphnormal_vertex>
		#include <skinnormal_vertex>
	#endif
	#include <begin_vertex>
	#include <morphtarget_vertex>
	#include <skinning_vertex>
	#include <displacementmap_vertex>
	#include <project_vertex>
	#include <worldpos_vertex>
	#include <clipping_planes_vertex>
	vWorldPosition = worldPosition.xyz;
}`,Q0=`#define DISTANCE
uniform vec3 referencePosition;
uniform float nearDistance;
uniform float farDistance;
varying vec3 vWorldPosition;
#include <common>
#include <uv_pars_fragment>
#include <map_pars_fragment>
#include <alphamap_pars_fragment>
#include <alphatest_pars_fragment>
#include <alphahash_pars_fragment>
#include <clipping_planes_pars_fragment>
void main() {
	vec4 diffuseColor = vec4( 1.0 );
	#include <clipping_planes_fragment>
	#include <map_fragment>
	#include <alphamap_fragment>
	#include <alphatest_fragment>
	#include <alphahash_fragment>
	float dist = length( vWorldPosition - referencePosition );
	dist = ( dist - nearDistance ) / ( farDistance - nearDistance );
	dist = saturate( dist );
	gl_FragColor = vec4( dist, 0.0, 0.0, 1.0 );
}`,tx=`varying vec3 vWorldDirection;
#include <common>
void main() {
	vWorldDirection = transformDirection( position, modelMatrix );
	#include <begin_vertex>
	#include <project_vertex>
}`,ex=`uniform sampler2D tEquirect;
varying vec3 vWorldDirection;
#include <common>
void main() {
	vec3 direction = normalize( vWorldDirection );
	vec2 sampleUV = equirectUv( direction );
	gl_FragColor = texture2D( tEquirect, sampleUV );
	#include <tonemapping_fragment>
	#include <colorspace_fragment>
}`,ix=`uniform float scale;
attribute float lineDistance;
varying float vLineDistance;
#include <common>
#include <uv_pars_vertex>
#include <color_pars_vertex>
#include <fog_pars_vertex>
#include <morphtarget_pars_vertex>
#include <logdepthbuf_pars_vertex>
#include <clipping_planes_pars_vertex>
void main() {
	vLineDistance = scale * lineDistance;
	#include <uv_vertex>
	#include <color_vertex>
	#include <morphinstance_vertex>
	#include <morphcolor_vertex>
	#include <begin_vertex>
	#include <morphtarget_vertex>
	#include <project_vertex>
	#include <logdepthbuf_vertex>
	#include <clipping_planes_vertex>
	#include <fog_vertex>
}`,nx=`uniform vec3 diffuse;
uniform float opacity;
uniform float dashSize;
uniform float totalSize;
varying float vLineDistance;
#include <common>
#include <color_pars_fragment>
#include <uv_pars_fragment>
#include <map_pars_fragment>
#include <fog_pars_fragment>
#include <logdepthbuf_pars_fragment>
#include <clipping_planes_pars_fragment>
void main() {
	vec4 diffuseColor = vec4( diffuse, opacity );
	#include <clipping_planes_fragment>
	if ( mod( vLineDistance, totalSize ) > dashSize ) {
		discard;
	}
	vec3 outgoingLight = vec3( 0.0 );
	#include <logdepthbuf_fragment>
	#include <map_fragment>
	#include <color_fragment>
	outgoingLight = diffuseColor.rgb;
	#include <opaque_fragment>
	#include <tonemapping_fragment>
	#include <colorspace_fragment>
	#include <fog_fragment>
	#include <premultiplied_alpha_fragment>
}`,sx=`#include <common>
#include <batching_pars_vertex>
#include <uv_pars_vertex>
#include <envmap_pars_vertex>
#include <color_pars_vertex>
#include <fog_pars_vertex>
#include <morphtarget_pars_vertex>
#include <skinning_pars_vertex>
#include <logdepthbuf_pars_vertex>
#include <clipping_planes_pars_vertex>
void main() {
	#include <uv_vertex>
	#include <color_vertex>
	#include <morphinstance_vertex>
	#include <morphcolor_vertex>
	#include <batching_vertex>
	#if defined ( USE_ENVMAP ) || defined ( USE_SKINNING )
		#include <beginnormal_vertex>
		#include <morphnormal_vertex>
		#include <skinbase_vertex>
		#include <skinnormal_vertex>
		#include <defaultnormal_vertex>
	#endif
	#include <begin_vertex>
	#include <morphtarget_vertex>
	#include <skinning_vertex>
	#include <project_vertex>
	#include <logdepthbuf_vertex>
	#include <clipping_planes_vertex>
	#include <worldpos_vertex>
	#include <envmap_vertex>
	#include <fog_vertex>
}`,rx=`uniform vec3 diffuse;
uniform float opacity;
#ifndef FLAT_SHADED
	varying vec3 vNormal;
#endif
#include <common>
#include <dithering_pars_fragment>
#include <color_pars_fragment>
#include <uv_pars_fragment>
#include <map_pars_fragment>
#include <alphamap_pars_fragment>
#include <alphatest_pars_fragment>
#include <alphahash_pars_fragment>
#include <aomap_pars_fragment>
#include <lightmap_pars_fragment>
#include <envmap_common_pars_fragment>
#include <envmap_pars_fragment>
#include <fog_pars_fragment>
#include <specularmap_pars_fragment>
#include <logdepthbuf_pars_fragment>
#include <clipping_planes_pars_fragment>
void main() {
	vec4 diffuseColor = vec4( diffuse, opacity );
	#include <clipping_planes_fragment>
	#include <logdepthbuf_fragment>
	#include <map_fragment>
	#include <color_fragment>
	#include <alphamap_fragment>
	#include <alphatest_fragment>
	#include <alphahash_fragment>
	#include <specularmap_fragment>
	ReflectedLight reflectedLight = ReflectedLight( vec3( 0.0 ), vec3( 0.0 ), vec3( 0.0 ), vec3( 0.0 ) );
	#ifdef USE_LIGHTMAP
		vec4 lightMapTexel = texture2D( lightMap, vLightMapUv );
		reflectedLight.indirectDiffuse += lightMapTexel.rgb * lightMapIntensity * RECIPROCAL_PI;
	#else
		reflectedLight.indirectDiffuse += vec3( 1.0 );
	#endif
	#include <aomap_fragment>
	reflectedLight.indirectDiffuse *= diffuseColor.rgb;
	vec3 outgoingLight = reflectedLight.indirectDiffuse;
	#include <envmap_fragment>
	#include <opaque_fragment>
	#include <tonemapping_fragment>
	#include <colorspace_fragment>
	#include <fog_fragment>
	#include <premultiplied_alpha_fragment>
	#include <dithering_fragment>
}`,ox=`#define LAMBERT
varying vec3 vViewPosition;
#include <common>
#include <batching_pars_vertex>
#include <uv_pars_vertex>
#include <displacementmap_pars_vertex>
#include <envmap_pars_vertex>
#include <color_pars_vertex>
#include <fog_pars_vertex>
#include <normal_pars_vertex>
#include <morphtarget_pars_vertex>
#include <skinning_pars_vertex>
#include <shadowmap_pars_vertex>
#include <logdepthbuf_pars_vertex>
#include <clipping_planes_pars_vertex>
void main() {
	#include <uv_vertex>
	#include <color_vertex>
	#include <morphinstance_vertex>
	#include <morphcolor_vertex>
	#include <batching_vertex>
	#include <beginnormal_vertex>
	#include <morphnormal_vertex>
	#include <skinbase_vertex>
	#include <skinnormal_vertex>
	#include <defaultnormal_vertex>
	#include <normal_vertex>
	#include <begin_vertex>
	#include <morphtarget_vertex>
	#include <skinning_vertex>
	#include <displacementmap_vertex>
	#include <project_vertex>
	#include <logdepthbuf_vertex>
	#include <clipping_planes_vertex>
	vViewPosition = - mvPosition.xyz;
	#include <worldpos_vertex>
	#include <envmap_vertex>
	#include <shadowmap_vertex>
	#include <fog_vertex>
}`,ax=`#define LAMBERT
uniform vec3 diffuse;
uniform vec3 emissive;
uniform float opacity;
#include <common>
#include <dithering_pars_fragment>
#include <color_pars_fragment>
#include <uv_pars_fragment>
#include <map_pars_fragment>
#include <alphamap_pars_fragment>
#include <alphatest_pars_fragment>
#include <alphahash_pars_fragment>
#include <aomap_pars_fragment>
#include <lightmap_pars_fragment>
#include <emissivemap_pars_fragment>
#include <envmap_common_pars_fragment>
#include <envmap_pars_fragment>
#include <envmap_physical_pars_fragment>
#include <fog_pars_fragment>
#include <bsdfs>
#include <lights_pars_begin>
#include <normal_pars_fragment>
#include <lights_lambert_pars_fragment>
#include <shadowmap_pars_fragment>
#include <bumpmap_pars_fragment>
#include <normalmap_pars_fragment>
#include <specularmap_pars_fragment>
#include <logdepthbuf_pars_fragment>
#include <clipping_planes_pars_fragment>
void main() {
	vec4 diffuseColor = vec4( diffuse, opacity );
	#include <clipping_planes_fragment>
	ReflectedLight reflectedLight = ReflectedLight( vec3( 0.0 ), vec3( 0.0 ), vec3( 0.0 ), vec3( 0.0 ) );
	vec3 totalEmissiveRadiance = emissive;
	#include <logdepthbuf_fragment>
	#include <map_fragment>
	#include <color_fragment>
	#include <alphamap_fragment>
	#include <alphatest_fragment>
	#include <alphahash_fragment>
	#include <specularmap_fragment>
	#include <normal_fragment_begin>
	#include <normal_fragment_maps>
	#include <emissivemap_fragment>
	#include <lights_lambert_fragment>
	#include <lights_fragment_begin>
	#include <lights_fragment_maps>
	#include <lights_fragment_end>
	#include <aomap_fragment>
	vec3 outgoingLight = reflectedLight.directDiffuse + reflectedLight.indirectDiffuse + totalEmissiveRadiance;
	#include <envmap_fragment>
	#include <opaque_fragment>
	#include <tonemapping_fragment>
	#include <colorspace_fragment>
	#include <fog_fragment>
	#include <premultiplied_alpha_fragment>
	#include <dithering_fragment>
}`,lx=`#define MATCAP
varying vec3 vViewPosition;
#include <common>
#include <batching_pars_vertex>
#include <uv_pars_vertex>
#include <color_pars_vertex>
#include <displacementmap_pars_vertex>
#include <fog_pars_vertex>
#include <normal_pars_vertex>
#include <morphtarget_pars_vertex>
#include <skinning_pars_vertex>
#include <logdepthbuf_pars_vertex>
#include <clipping_planes_pars_vertex>
void main() {
	#include <uv_vertex>
	#include <color_vertex>
	#include <morphinstance_vertex>
	#include <morphcolor_vertex>
	#include <batching_vertex>
	#include <beginnormal_vertex>
	#include <morphnormal_vertex>
	#include <skinbase_vertex>
	#include <skinnormal_vertex>
	#include <defaultnormal_vertex>
	#include <normal_vertex>
	#include <begin_vertex>
	#include <morphtarget_vertex>
	#include <skinning_vertex>
	#include <displacementmap_vertex>
	#include <project_vertex>
	#include <logdepthbuf_vertex>
	#include <clipping_planes_vertex>
	#include <fog_vertex>
	vViewPosition = - mvPosition.xyz;
}`,cx=`#define MATCAP
uniform vec3 diffuse;
uniform float opacity;
uniform sampler2D matcap;
varying vec3 vViewPosition;
#include <common>
#include <dithering_pars_fragment>
#include <color_pars_fragment>
#include <uv_pars_fragment>
#include <map_pars_fragment>
#include <alphamap_pars_fragment>
#include <alphatest_pars_fragment>
#include <alphahash_pars_fragment>
#include <fog_pars_fragment>
#include <normal_pars_fragment>
#include <bumpmap_pars_fragment>
#include <normalmap_pars_fragment>
#include <logdepthbuf_pars_fragment>
#include <clipping_planes_pars_fragment>
void main() {
	vec4 diffuseColor = vec4( diffuse, opacity );
	#include <clipping_planes_fragment>
	#include <logdepthbuf_fragment>
	#include <map_fragment>
	#include <color_fragment>
	#include <alphamap_fragment>
	#include <alphatest_fragment>
	#include <alphahash_fragment>
	#include <normal_fragment_begin>
	#include <normal_fragment_maps>
	vec3 viewDir = normalize( vViewPosition );
	vec3 x = normalize( vec3( viewDir.z, 0.0, - viewDir.x ) );
	vec3 y = cross( viewDir, x );
	vec2 uv = vec2( dot( x, normal ), dot( y, normal ) ) * 0.495 + 0.5;
	#ifdef USE_MATCAP
		vec4 matcapColor = texture2D( matcap, uv );
	#else
		vec4 matcapColor = vec4( vec3( mix( 0.2, 0.8, uv.y ) ), 1.0 );
	#endif
	vec3 outgoingLight = diffuseColor.rgb * matcapColor.rgb;
	#include <opaque_fragment>
	#include <tonemapping_fragment>
	#include <colorspace_fragment>
	#include <fog_fragment>
	#include <premultiplied_alpha_fragment>
	#include <dithering_fragment>
}`,hx=`#define NORMAL
#if defined( FLAT_SHADED ) || defined( USE_BUMPMAP ) || defined( USE_NORMALMAP_TANGENTSPACE )
	varying vec3 vViewPosition;
#endif
#include <common>
#include <batching_pars_vertex>
#include <uv_pars_vertex>
#include <displacementmap_pars_vertex>
#include <normal_pars_vertex>
#include <morphtarget_pars_vertex>
#include <skinning_pars_vertex>
#include <logdepthbuf_pars_vertex>
#include <clipping_planes_pars_vertex>
void main() {
	#include <uv_vertex>
	#include <batching_vertex>
	#include <beginnormal_vertex>
	#include <morphinstance_vertex>
	#include <morphnormal_vertex>
	#include <skinbase_vertex>
	#include <skinnormal_vertex>
	#include <defaultnormal_vertex>
	#include <normal_vertex>
	#include <begin_vertex>
	#include <morphtarget_vertex>
	#include <skinning_vertex>
	#include <displacementmap_vertex>
	#include <project_vertex>
	#include <logdepthbuf_vertex>
	#include <clipping_planes_vertex>
#if defined( FLAT_SHADED ) || defined( USE_BUMPMAP ) || defined( USE_NORMALMAP_TANGENTSPACE )
	vViewPosition = - mvPosition.xyz;
#endif
}`,ux=`#define NORMAL
uniform float opacity;
#if defined( FLAT_SHADED ) || defined( USE_BUMPMAP ) || defined( USE_NORMALMAP_TANGENTSPACE )
	varying vec3 vViewPosition;
#endif
#include <uv_pars_fragment>
#include <normal_pars_fragment>
#include <bumpmap_pars_fragment>
#include <normalmap_pars_fragment>
#include <logdepthbuf_pars_fragment>
#include <clipping_planes_pars_fragment>
void main() {
	vec4 diffuseColor = vec4( 0.0, 0.0, 0.0, opacity );
	#include <clipping_planes_fragment>
	#include <logdepthbuf_fragment>
	#include <normal_fragment_begin>
	#include <normal_fragment_maps>
	gl_FragColor = vec4( normalize( normal ) * 0.5 + 0.5, diffuseColor.a );
	#ifdef OPAQUE
		gl_FragColor.a = 1.0;
	#endif
}`,dx=`#define PHONG
varying vec3 vViewPosition;
#include <common>
#include <batching_pars_vertex>
#include <uv_pars_vertex>
#include <displacementmap_pars_vertex>
#include <envmap_pars_vertex>
#include <color_pars_vertex>
#include <fog_pars_vertex>
#include <normal_pars_vertex>
#include <morphtarget_pars_vertex>
#include <skinning_pars_vertex>
#include <shadowmap_pars_vertex>
#include <logdepthbuf_pars_vertex>
#include <clipping_planes_pars_vertex>
void main() {
	#include <uv_vertex>
	#include <color_vertex>
	#include <morphcolor_vertex>
	#include <batching_vertex>
	#include <beginnormal_vertex>
	#include <morphinstance_vertex>
	#include <morphnormal_vertex>
	#include <skinbase_vertex>
	#include <skinnormal_vertex>
	#include <defaultnormal_vertex>
	#include <normal_vertex>
	#include <begin_vertex>
	#include <morphtarget_vertex>
	#include <skinning_vertex>
	#include <displacementmap_vertex>
	#include <project_vertex>
	#include <logdepthbuf_vertex>
	#include <clipping_planes_vertex>
	vViewPosition = - mvPosition.xyz;
	#include <worldpos_vertex>
	#include <envmap_vertex>
	#include <shadowmap_vertex>
	#include <fog_vertex>
}`,fx=`#define PHONG
uniform vec3 diffuse;
uniform vec3 emissive;
uniform vec3 specular;
uniform float shininess;
uniform float opacity;
#include <common>
#include <dithering_pars_fragment>
#include <color_pars_fragment>
#include <uv_pars_fragment>
#include <map_pars_fragment>
#include <alphamap_pars_fragment>
#include <alphatest_pars_fragment>
#include <alphahash_pars_fragment>
#include <aomap_pars_fragment>
#include <lightmap_pars_fragment>
#include <emissivemap_pars_fragment>
#include <envmap_common_pars_fragment>
#include <envmap_pars_fragment>
#include <envmap_physical_pars_fragment>
#include <fog_pars_fragment>
#include <bsdfs>
#include <lights_pars_begin>
#include <normal_pars_fragment>
#include <lights_phong_pars_fragment>
#include <shadowmap_pars_fragment>
#include <bumpmap_pars_fragment>
#include <normalmap_pars_fragment>
#include <specularmap_pars_fragment>
#include <logdepthbuf_pars_fragment>
#include <clipping_planes_pars_fragment>
void main() {
	vec4 diffuseColor = vec4( diffuse, opacity );
	#include <clipping_planes_fragment>
	ReflectedLight reflectedLight = ReflectedLight( vec3( 0.0 ), vec3( 0.0 ), vec3( 0.0 ), vec3( 0.0 ) );
	vec3 totalEmissiveRadiance = emissive;
	#include <logdepthbuf_fragment>
	#include <map_fragment>
	#include <color_fragment>
	#include <alphamap_fragment>
	#include <alphatest_fragment>
	#include <alphahash_fragment>
	#include <specularmap_fragment>
	#include <normal_fragment_begin>
	#include <normal_fragment_maps>
	#include <emissivemap_fragment>
	#include <lights_phong_fragment>
	#include <lights_fragment_begin>
	#include <lights_fragment_maps>
	#include <lights_fragment_end>
	#include <aomap_fragment>
	vec3 outgoingLight = reflectedLight.directDiffuse + reflectedLight.indirectDiffuse + reflectedLight.directSpecular + reflectedLight.indirectSpecular + totalEmissiveRadiance;
	#include <envmap_fragment>
	#include <opaque_fragment>
	#include <tonemapping_fragment>
	#include <colorspace_fragment>
	#include <fog_fragment>
	#include <premultiplied_alpha_fragment>
	#include <dithering_fragment>
}`,px=`#define STANDARD
varying vec3 vViewPosition;
#ifdef USE_TRANSMISSION
	varying vec3 vWorldPosition;
#endif
#include <common>
#include <batching_pars_vertex>
#include <uv_pars_vertex>
#include <displacementmap_pars_vertex>
#include <color_pars_vertex>
#include <fog_pars_vertex>
#include <normal_pars_vertex>
#include <morphtarget_pars_vertex>
#include <skinning_pars_vertex>
#include <shadowmap_pars_vertex>
#include <logdepthbuf_pars_vertex>
#include <clipping_planes_pars_vertex>
void main() {
	#include <uv_vertex>
	#include <color_vertex>
	#include <morphinstance_vertex>
	#include <morphcolor_vertex>
	#include <batching_vertex>
	#include <beginnormal_vertex>
	#include <morphnormal_vertex>
	#include <skinbase_vertex>
	#include <skinnormal_vertex>
	#include <defaultnormal_vertex>
	#include <normal_vertex>
	#include <begin_vertex>
	#include <morphtarget_vertex>
	#include <skinning_vertex>
	#include <displacementmap_vertex>
	#include <project_vertex>
	#include <logdepthbuf_vertex>
	#include <clipping_planes_vertex>
	vViewPosition = - mvPosition.xyz;
	#include <worldpos_vertex>
	#include <shadowmap_vertex>
	#include <fog_vertex>
#ifdef USE_TRANSMISSION
	vWorldPosition = worldPosition.xyz;
#endif
}`,mx=`#define STANDARD
#ifdef PHYSICAL
	#define IOR
	#define USE_SPECULAR
#endif
uniform vec3 diffuse;
uniform vec3 emissive;
uniform float roughness;
uniform float metalness;
uniform float opacity;
#ifdef IOR
	uniform float ior;
#endif
#ifdef USE_SPECULAR
	uniform float specularIntensity;
	uniform vec3 specularColor;
	#ifdef USE_SPECULAR_COLORMAP
		uniform sampler2D specularColorMap;
	#endif
	#ifdef USE_SPECULAR_INTENSITYMAP
		uniform sampler2D specularIntensityMap;
	#endif
#endif
#ifdef USE_CLEARCOAT
	uniform float clearcoat;
	uniform float clearcoatRoughness;
#endif
#ifdef USE_DIFFUSE_ROUGHNESS
	uniform float diffuseRoughness;
	#ifdef USE_DIFFUSE_ROUGHNESSMAP
		uniform sampler2D diffuseRoughnessMap;
	#endif
#endif
#ifdef USE_DISPERSION
	uniform float dispersion;
#endif
#ifdef USE_RETROREFLECTION
	uniform float retroreflectivity;
#endif
#ifdef USE_IRIDESCENCE
	uniform float iridescence;
	uniform float iridescenceIOR;
	uniform float iridescenceThicknessMinimum;
	uniform float iridescenceThicknessMaximum;
#endif
#ifdef USE_SHEEN
	uniform vec3 sheenColor;
	uniform float sheenRoughness;
	#ifdef USE_SHEEN_COLORMAP
		uniform sampler2D sheenColorMap;
	#endif
	#ifdef USE_SHEEN_ROUGHNESSMAP
		uniform sampler2D sheenRoughnessMap;
	#endif
#endif
#ifdef USE_ANISOTROPY
	uniform vec2 anisotropyVector;
	#ifdef USE_ANISOTROPYMAP
		uniform sampler2D anisotropyMap;
	#endif
#endif
varying vec3 vViewPosition;
#include <common>
#include <dithering_pars_fragment>
#include <color_pars_fragment>
#include <uv_pars_fragment>
#include <map_pars_fragment>
#include <alphamap_pars_fragment>
#include <alphatest_pars_fragment>
#include <alphahash_pars_fragment>
#include <aomap_pars_fragment>
#include <lightmap_pars_fragment>
#include <emissivemap_pars_fragment>
#include <iridescence_fragment>
#include <envmap_common_pars_fragment>
#include <envmap_physical_pars_fragment>
#include <fog_pars_fragment>
#include <lights_pars_begin>
#include <normal_pars_fragment>
#include <lights_physical_pars_fragment>
#include <transmission_pars_fragment>
#include <shadowmap_pars_fragment>
#include <bumpmap_pars_fragment>
#include <normalmap_pars_fragment>
#include <clearcoat_pars_fragment>
#include <iridescence_pars_fragment>
#include <roughnessmap_pars_fragment>
#include <metalnessmap_pars_fragment>
#include <logdepthbuf_pars_fragment>
#include <clipping_planes_pars_fragment>
void main() {
	vec4 diffuseColor = vec4( diffuse, opacity );
	#include <clipping_planes_fragment>
	ReflectedLight reflectedLight = ReflectedLight( vec3( 0.0 ), vec3( 0.0 ), vec3( 0.0 ), vec3( 0.0 ) );
	vec3 totalEmissiveRadiance = emissive;
	#include <logdepthbuf_fragment>
	#include <map_fragment>
	#include <color_fragment>
	#include <alphamap_fragment>
	#include <alphatest_fragment>
	#include <alphahash_fragment>
	#include <roughnessmap_fragment>
	#include <metalnessmap_fragment>
	#include <normal_fragment_begin>
	#include <normal_fragment_maps>
	#include <clearcoat_normal_fragment_begin>
	#include <clearcoat_normal_fragment_maps>
	#include <emissivemap_fragment>
	#include <lights_physical_fragment>
	#include <lights_fragment_begin>
	#include <lights_fragment_maps>
	#include <lights_fragment_end>
	#include <aomap_fragment>
	vec3 totalDiffuse = reflectedLight.directDiffuse + reflectedLight.indirectDiffuse;
	vec3 totalSpecular = reflectedLight.directSpecular + reflectedLight.indirectSpecular;
	#include <transmission_fragment>
	vec3 outgoingLight = totalDiffuse + totalSpecular + totalEmissiveRadiance;
	#ifdef USE_SHEEN

		outgoingLight = outgoingLight + sheenSpecularDirect + sheenSpecularIndirect;

	#endif
	#ifdef USE_CLEARCOAT
		float dotNVcc = saturate( dot( geometryClearcoatNormal, geometryViewDir ) );
		vec3 Fcc = F_Schlick( material.clearcoatF0, material.clearcoatF90, dotNVcc );
		outgoingLight = outgoingLight * ( 1.0 - material.clearcoat * Fcc ) + ( clearcoatSpecularDirect + clearcoatSpecularIndirect ) * material.clearcoat;
	#endif
	#include <opaque_fragment>
	#include <tonemapping_fragment>
	#include <colorspace_fragment>
	#include <fog_fragment>
	#include <premultiplied_alpha_fragment>
	#include <dithering_fragment>
}`,gx=`#define TOON
varying vec3 vViewPosition;
#include <common>
#include <batching_pars_vertex>
#include <uv_pars_vertex>
#include <displacementmap_pars_vertex>
#include <color_pars_vertex>
#include <fog_pars_vertex>
#include <normal_pars_vertex>
#include <morphtarget_pars_vertex>
#include <skinning_pars_vertex>
#include <shadowmap_pars_vertex>
#include <logdepthbuf_pars_vertex>
#include <clipping_planes_pars_vertex>
void main() {
	#include <uv_vertex>
	#include <color_vertex>
	#include <morphinstance_vertex>
	#include <morphcolor_vertex>
	#include <batching_vertex>
	#include <beginnormal_vertex>
	#include <morphnormal_vertex>
	#include <skinbase_vertex>
	#include <skinnormal_vertex>
	#include <defaultnormal_vertex>
	#include <normal_vertex>
	#include <begin_vertex>
	#include <morphtarget_vertex>
	#include <skinning_vertex>
	#include <displacementmap_vertex>
	#include <project_vertex>
	#include <logdepthbuf_vertex>
	#include <clipping_planes_vertex>
	vViewPosition = - mvPosition.xyz;
	#include <worldpos_vertex>
	#include <shadowmap_vertex>
	#include <fog_vertex>
}`,xx=`#define TOON
uniform vec3 diffuse;
uniform vec3 emissive;
uniform float opacity;
#include <common>
#include <dithering_pars_fragment>
#include <color_pars_fragment>
#include <uv_pars_fragment>
#include <map_pars_fragment>
#include <alphamap_pars_fragment>
#include <alphatest_pars_fragment>
#include <alphahash_pars_fragment>
#include <aomap_pars_fragment>
#include <lightmap_pars_fragment>
#include <emissivemap_pars_fragment>
#include <gradientmap_pars_fragment>
#include <fog_pars_fragment>
#include <bsdfs>
#include <lights_pars_begin>
#include <normal_pars_fragment>
#include <lights_toon_pars_fragment>
#include <shadowmap_pars_fragment>
#include <bumpmap_pars_fragment>
#include <normalmap_pars_fragment>
#include <logdepthbuf_pars_fragment>
#include <clipping_planes_pars_fragment>
void main() {
	vec4 diffuseColor = vec4( diffuse, opacity );
	#include <clipping_planes_fragment>
	ReflectedLight reflectedLight = ReflectedLight( vec3( 0.0 ), vec3( 0.0 ), vec3( 0.0 ), vec3( 0.0 ) );
	vec3 totalEmissiveRadiance = emissive;
	#include <logdepthbuf_fragment>
	#include <map_fragment>
	#include <color_fragment>
	#include <alphamap_fragment>
	#include <alphatest_fragment>
	#include <alphahash_fragment>
	#include <normal_fragment_begin>
	#include <normal_fragment_maps>
	#include <emissivemap_fragment>
	#include <lights_toon_fragment>
	#include <lights_fragment_begin>
	#include <lights_fragment_maps>
	#include <lights_fragment_end>
	#include <aomap_fragment>
	vec3 outgoingLight = reflectedLight.directDiffuse + reflectedLight.indirectDiffuse + totalEmissiveRadiance;
	#include <opaque_fragment>
	#include <tonemapping_fragment>
	#include <colorspace_fragment>
	#include <fog_fragment>
	#include <premultiplied_alpha_fragment>
	#include <dithering_fragment>
}`,_x=`uniform float size;
uniform float scale;
#include <common>
#include <color_pars_vertex>
#include <fog_pars_vertex>
#include <morphtarget_pars_vertex>
#include <logdepthbuf_pars_vertex>
#include <clipping_planes_pars_vertex>
#ifdef USE_POINTS_UV
	varying vec2 vUv;
	uniform mat3 uvTransform;
#endif
void main() {
	#ifdef USE_POINTS_UV
		vUv = ( uvTransform * vec3( uv, 1 ) ).xy;
	#endif
	#include <color_vertex>
	#include <morphinstance_vertex>
	#include <morphcolor_vertex>
	#include <begin_vertex>
	#include <morphtarget_vertex>
	#include <project_vertex>
	gl_PointSize = size;
	#ifdef USE_SIZEATTENUATION
		bool isPerspective = isPerspectiveMatrix( projectionMatrix );
		if ( isPerspective ) gl_PointSize *= ( scale / - mvPosition.z );
	#endif
	#include <logdepthbuf_vertex>
	#include <clipping_planes_vertex>
	#include <worldpos_vertex>
	#include <fog_vertex>
}`,vx=`uniform vec3 diffuse;
uniform float opacity;
#include <common>
#include <color_pars_fragment>
#include <map_particle_pars_fragment>
#include <alphatest_pars_fragment>
#include <alphahash_pars_fragment>
#include <fog_pars_fragment>
#include <logdepthbuf_pars_fragment>
#include <clipping_planes_pars_fragment>
void main() {
	vec4 diffuseColor = vec4( diffuse, opacity );
	#include <clipping_planes_fragment>
	vec3 outgoingLight = vec3( 0.0 );
	#include <logdepthbuf_fragment>
	#include <map_particle_fragment>
	#include <color_fragment>
	#include <alphatest_fragment>
	#include <alphahash_fragment>
	outgoingLight = diffuseColor.rgb;
	#include <opaque_fragment>
	#include <tonemapping_fragment>
	#include <colorspace_fragment>
	#include <fog_fragment>
	#include <premultiplied_alpha_fragment>
}`,yx=`#include <common>
#include <batching_pars_vertex>
#include <fog_pars_vertex>
#include <morphtarget_pars_vertex>
#include <skinning_pars_vertex>
#include <logdepthbuf_pars_vertex>
#include <shadowmap_pars_vertex>
void main() {
	#include <batching_vertex>
	#include <beginnormal_vertex>
	#include <morphinstance_vertex>
	#include <morphnormal_vertex>
	#include <skinbase_vertex>
	#include <skinnormal_vertex>
	#include <defaultnormal_vertex>
	#include <begin_vertex>
	#include <morphtarget_vertex>
	#include <skinning_vertex>
	#include <project_vertex>
	#include <logdepthbuf_vertex>
	#include <worldpos_vertex>
	#include <shadowmap_vertex>
	#include <fog_vertex>
}`,Mx=`uniform vec3 color;
uniform float opacity;
#include <common>
#include <fog_pars_fragment>
#include <bsdfs>
#include <lights_pars_begin>
#include <logdepthbuf_pars_fragment>
#include <shadowmap_pars_fragment>
#include <shadowmask_pars_fragment>
void main() {
	#include <logdepthbuf_fragment>
	gl_FragColor = vec4( color, opacity * ( 1.0 - getShadowMask() ) );
	#include <tonemapping_fragment>
	#include <colorspace_fragment>
	#include <fog_fragment>
	#include <premultiplied_alpha_fragment>
}`,Sx=`uniform float rotation;
uniform vec2 center;
#include <common>
#include <uv_pars_vertex>
#include <fog_pars_vertex>
#include <logdepthbuf_pars_vertex>
#include <clipping_planes_pars_vertex>
void main() {
	#include <uv_vertex>
	vec4 mvPosition = modelViewMatrix[ 3 ];
	vec2 scale = vec2( length( modelMatrix[ 0 ].xyz ), length( modelMatrix[ 1 ].xyz ) );
	#ifndef USE_SIZEATTENUATION
		bool isPerspective = isPerspectiveMatrix( projectionMatrix );
		if ( isPerspective ) scale *= - mvPosition.z;
	#endif
	vec2 alignedPosition = ( position.xy - ( center - vec2( 0.5 ) ) ) * scale;
	vec2 rotatedPosition;
	rotatedPosition.x = cos( rotation ) * alignedPosition.x - sin( rotation ) * alignedPosition.y;
	rotatedPosition.y = sin( rotation ) * alignedPosition.x + cos( rotation ) * alignedPosition.y;
	mvPosition.xy += rotatedPosition;
	gl_Position = projectionMatrix * mvPosition;
	#include <logdepthbuf_vertex>
	#include <clipping_planes_vertex>
	#include <fog_vertex>
}`,bx=`uniform vec3 diffuse;
uniform float opacity;
#include <common>
#include <uv_pars_fragment>
#include <map_pars_fragment>
#include <alphamap_pars_fragment>
#include <alphatest_pars_fragment>
#include <alphahash_pars_fragment>
#include <fog_pars_fragment>
#include <logdepthbuf_pars_fragment>
#include <clipping_planes_pars_fragment>
void main() {
	vec4 diffuseColor = vec4( diffuse, opacity );
	#include <clipping_planes_fragment>
	vec3 outgoingLight = vec3( 0.0 );
	#include <logdepthbuf_fragment>
	#include <map_fragment>
	#include <alphamap_fragment>
	#include <alphatest_fragment>
	#include <alphahash_fragment>
	outgoingLight = diffuseColor.rgb;
	#include <opaque_fragment>
	#include <tonemapping_fragment>
	#include <colorspace_fragment>
	#include <fog_fragment>
}`,Ht={alphahash_fragment:Xm,alphahash_pars_fragment:qm,alphamap_fragment:Ym,alphamap_pars_fragment:Zm,alphatest_fragment:Km,alphatest_pars_fragment:Jm,aomap_fragment:$m,aomap_pars_fragment:jm,batching_pars_vertex:Qm,batching_vertex:tg,begin_vertex:eg,beginnormal_vertex:ig,bsdfs:ng,iridescence_fragment:sg,bumpmap_pars_fragment:rg,clipping_planes_fragment:og,clipping_planes_pars_fragment:ag,clipping_planes_pars_vertex:lg,clipping_planes_vertex:cg,color_fragment:hg,color_pars_fragment:ug,color_pars_vertex:dg,color_vertex:fg,common:pg,defaultnormal_vertex:mg,displacementmap_pars_vertex:gg,displacementmap_vertex:xg,emissivemap_fragment:_g,emissivemap_pars_fragment:vg,colorspace_fragment:yg,colorspace_pars_fragment:Mg,envmap_fragment:Sg,envmap_common_pars_fragment:bg,envmap_pars_fragment:wg,envmap_pars_vertex:Eg,envmap_physical_pars_fragment:Fg,envmap_vertex:Tg,fog_vertex:Ag,fog_pars_vertex:Rg,fog_fragment:Cg,fog_pars_fragment:Pg,gradientmap_pars_fragment:Ig,lightmap_pars_fragment:Dg,lights_lambert_fragment:Lg,lights_lambert_pars_fragment:Ng,lights_pars_begin:Ug,lights_toon_fragment:Og,lights_toon_pars_fragment:Bg,lights_phong_fragment:zg,lights_phong_pars_fragment:kg,lights_physical_fragment:Hg,lights_physical_pars_fragment:Vg,lights_fragment_begin:Gg,lights_fragment_maps:Wg,lights_fragment_end:Xg,lightprobes_pars_fragment:qg,logdepthbuf_fragment:Yg,logdepthbuf_pars_fragment:Zg,logdepthbuf_pars_vertex:Kg,logdepthbuf_vertex:Jg,map_fragment:$g,map_pars_fragment:jg,map_particle_fragment:Qg,map_particle_pars_fragment:t0,metalnessmap_fragment:e0,metalnessmap_pars_fragment:i0,morphinstance_vertex:n0,morphcolor_vertex:s0,morphnormal_vertex:r0,morphtarget_pars_vertex:o0,morphtarget_vertex:a0,normal_fragment_begin:l0,normal_fragment_maps:c0,normal_pars_fragment:h0,normal_pars_vertex:u0,normal_vertex:d0,normalmap_pars_fragment:f0,clearcoat_normal_fragment_begin:p0,clearcoat_normal_fragment_maps:m0,clearcoat_pars_fragment:g0,iridescence_pars_fragment:x0,opaque_fragment:_0,packing:v0,premultiplied_alpha_fragment:y0,project_vertex:M0,dithering_fragment:S0,dithering_pars_fragment:b0,roughnessmap_fragment:w0,roughnessmap_pars_fragment:E0,shadowmap_pars_fragment:T0,shadowmap_pars_vertex:A0,shadowmap_vertex:R0,shadowmask_pars_fragment:C0,skinbase_vertex:P0,skinning_pars_vertex:I0,skinning_vertex:D0,skinnormal_vertex:L0,specularmap_fragment:N0,specularmap_pars_fragment:U0,tonemapping_fragment:F0,tonemapping_pars_fragment:O0,transmission_fragment:B0,transmission_pars_fragment:z0,uv_pars_fragment:k0,uv_pars_vertex:H0,uv_vertex:V0,worldpos_vertex:G0,background_vert:W0,background_frag:X0,backgroundCube_vert:q0,backgroundCube_frag:Y0,cube_vert:Z0,cube_frag:K0,depth_vert:J0,depth_frag:$0,distance_vert:j0,distance_frag:Q0,equirect_vert:tx,equirect_frag:ex,linedashed_vert:ix,linedashed_frag:nx,meshbasic_vert:sx,meshbasic_frag:rx,meshlambert_vert:ox,meshlambert_frag:ax,meshmatcap_vert:lx,meshmatcap_frag:cx,meshnormal_vert:hx,meshnormal_frag:ux,meshphong_vert:dx,meshphong_frag:fx,meshphysical_vert:px,meshphysical_frag:mx,meshtoon_vert:gx,meshtoon_frag:xx,points_vert:_x,points_frag:vx,shadow_vert:yx,shadow_frag:Mx,sprite_vert:Sx,sprite_frag:bx},vt={common:{diffuse:{value:new St(16777215)},opacity:{value:1},map:{value:null},mapTransform:{value:new Ft},alphaMap:{value:null},alphaMapTransform:{value:new Ft},alphaTest:{value:0}},specularmap:{specularMap:{value:null},specularMapTransform:{value:new Ft}},envmap:{envMap:{value:null},envMapRotation:{value:new Ft},reflectivity:{value:1},ior:{value:1.5},refractionRatio:{value:.98},dfgLUT:{value:null}},aomap:{aoMap:{value:null},aoMapIntensity:{value:1},aoMapTransform:{value:new Ft}},lightmap:{lightMap:{value:null},lightMapIntensity:{value:1},lightMapTransform:{value:new Ft}},bumpmap:{bumpMap:{value:null},bumpMapTransform:{value:new Ft},bumpScale:{value:1}},normalmap:{normalMap:{value:null},normalMapTransform:{value:new Ft},normalScale:{value:new et(1,1)}},displacementmap:{displacementMap:{value:null},displacementMapTransform:{value:new Ft},displacementScale:{value:1},displacementBias:{value:0}},emissivemap:{emissiveMap:{value:null},emissiveMapTransform:{value:new Ft}},metalnessmap:{metalnessMap:{value:null},metalnessMapTransform:{value:new Ft}},roughnessmap:{roughnessMap:{value:null},roughnessMapTransform:{value:new Ft}},gradientmap:{gradientMap:{value:null}},fog:{fogDensity:{value:25e-5},fogNear:{value:1},fogFar:{value:2e3},fogColor:{value:new St(16777215)}},lights:{ambientLightColor:{value:[]},lightProbe:{value:[]},sunLights:{value:[],properties:{direction:{},color:{}}},sunLightShadows:{value:[],properties:{shadowIntensity:1,shadowBias:{},shadowNormalBias:{},shadowRadius:{},shadowMapSize:{}}},sunShadowMatrix:{value:[]},sunShadowCascade:{value:[]},directionalLights:{value:[],properties:{direction:{},color:{}}},directionalLightShadows:{value:[],properties:{shadowIntensity:1,shadowBias:{},shadowNormalBias:{},shadowRadius:{},shadowMapSize:{}}},directionalShadowMatrix:{value:[]},spotLights:{value:[],properties:{color:{},position:{},direction:{},distance:{},coneCos:{},penumbraCos:{},decay:{}}},spotLightShadows:{value:[],properties:{shadowIntensity:1,shadowBias:{},shadowNormalBias:{},shadowRadius:{},shadowMapSize:{}}},spotLightMap:{value:[]},spotLightMatrix:{value:[]},pointLights:{value:[],properties:{color:{},position:{},decay:{},distance:{}}},pointLightShadows:{value:[],properties:{shadowIntensity:1,shadowBias:{},shadowNormalBias:{},shadowRadius:{},shadowMapSize:{},shadowCameraNear:{},shadowCameraFar:{}}},pointShadowMatrix:{value:[]},hemisphereLights:{value:[],properties:{direction:{},skyColor:{},groundColor:{}}},rectAreaLights:{value:[],properties:{color:{},position:{},width:{},height:{}}},ltc_1:{value:null},ltc_2:{value:null},probesSH:{value:[]},probesMin:{value:[]},probesMax:{value:[]},probesResolution:{value:[]}},points:{diffuse:{value:new St(16777215)},opacity:{value:1},size:{value:1},scale:{value:1},map:{value:null},alphaMap:{value:null},alphaMapTransform:{value:new Ft},alphaTest:{value:0},uvTransform:{value:new Ft}},sprite:{diffuse:{value:new St(16777215)},opacity:{value:1},center:{value:new et(.5,.5)},rotation:{value:0},map:{value:null},mapTransform:{value:new Ft},alphaMap:{value:null},alphaMapTransform:{value:new Ft},alphaTest:{value:0}}},Ki={basic:{uniforms:ei([vt.common,vt.specularmap,vt.envmap,vt.aomap,vt.lightmap,vt.fog]),vertexShader:Ht.meshbasic_vert,fragmentShader:Ht.meshbasic_frag},lambert:{uniforms:ei([vt.common,vt.specularmap,vt.envmap,vt.aomap,vt.lightmap,vt.emissivemap,vt.bumpmap,vt.normalmap,vt.displacementmap,vt.fog,vt.lights,{emissive:{value:new St(0)},envMapIntensity:{value:1}}]),vertexShader:Ht.meshlambert_vert,fragmentShader:Ht.meshlambert_frag},phong:{uniforms:ei([vt.common,vt.specularmap,vt.envmap,vt.aomap,vt.lightmap,vt.emissivemap,vt.bumpmap,vt.normalmap,vt.displacementmap,vt.fog,vt.lights,{emissive:{value:new St(0)},specular:{value:new St(1118481)},shininess:{value:30},envMapIntensity:{value:1}}]),vertexShader:Ht.meshphong_vert,fragmentShader:Ht.meshphong_frag},standard:{uniforms:ei([vt.common,vt.envmap,vt.aomap,vt.lightmap,vt.emissivemap,vt.bumpmap,vt.normalmap,vt.displacementmap,vt.roughnessmap,vt.metalnessmap,vt.fog,vt.lights,{emissive:{value:new St(0)},roughness:{value:1},metalness:{value:0},envMapIntensity:{value:1}}]),vertexShader:Ht.meshphysical_vert,fragmentShader:Ht.meshphysical_frag},toon:{uniforms:ei([vt.common,vt.aomap,vt.lightmap,vt.emissivemap,vt.bumpmap,vt.normalmap,vt.displacementmap,vt.gradientmap,vt.fog,vt.lights,{emissive:{value:new St(0)}}]),vertexShader:Ht.meshtoon_vert,fragmentShader:Ht.meshtoon_frag},matcap:{uniforms:ei([vt.common,vt.bumpmap,vt.normalmap,vt.displacementmap,vt.fog,{matcap:{value:null}}]),vertexShader:Ht.meshmatcap_vert,fragmentShader:Ht.meshmatcap_frag},points:{uniforms:ei([vt.points,vt.fog]),vertexShader:Ht.points_vert,fragmentShader:Ht.points_frag},dashed:{uniforms:ei([vt.common,vt.fog,{scale:{value:1},dashSize:{value:1},totalSize:{value:2}}]),vertexShader:Ht.linedashed_vert,fragmentShader:Ht.linedashed_frag},depth:{uniforms:ei([vt.common,vt.displacementmap]),vertexShader:Ht.depth_vert,fragmentShader:Ht.depth_frag},normal:{uniforms:ei([vt.common,vt.bumpmap,vt.normalmap,vt.displacementmap,{opacity:{value:1}}]),vertexShader:Ht.meshnormal_vert,fragmentShader:Ht.meshnormal_frag},sprite:{uniforms:ei([vt.sprite,vt.fog]),vertexShader:Ht.sprite_vert,fragmentShader:Ht.sprite_frag},background:{uniforms:{uvTransform:{value:new Ft},t2D:{value:null},backgroundIntensity:{value:1}},vertexShader:Ht.background_vert,fragmentShader:Ht.background_frag},backgroundCube:{uniforms:{envMap:{value:null},backgroundBlurriness:{value:0},backgroundIntensity:{value:1},backgroundRotation:{value:new Ft}},vertexShader:Ht.backgroundCube_vert,fragmentShader:Ht.backgroundCube_frag},cube:{uniforms:{tCube:{value:null},tFlip:{value:-1},opacity:{value:1}},vertexShader:Ht.cube_vert,fragmentShader:Ht.cube_frag},equirect:{uniforms:{tEquirect:{value:null}},vertexShader:Ht.equirect_vert,fragmentShader:Ht.equirect_frag},distance:{uniforms:ei([vt.common,vt.displacementmap,{referencePosition:{value:new R},nearDistance:{value:1},farDistance:{value:1e3}}]),vertexShader:Ht.distance_vert,fragmentShader:Ht.distance_frag},shadow:{uniforms:ei([vt.lights,vt.fog,{color:{value:new St(0)},opacity:{value:1}}]),vertexShader:Ht.shadow_vert,fragmentShader:Ht.shadow_frag}};Ki.physical={uniforms:ei([Ki.standard.uniforms,{clearcoat:{value:0},clearcoatMap:{value:null},clearcoatMapTransform:{value:new Ft},clearcoatNormalMap:{value:null},clearcoatNormalMapTransform:{value:new Ft},clearcoatNormalScale:{value:new et(1,1)},clearcoatRoughness:{value:0},clearcoatRoughnessMap:{value:null},clearcoatRoughnessMapTransform:{value:new Ft},diffuseRoughness:{value:0},diffuseRoughnessMap:{value:null},diffuseRoughnessMapTransform:{value:new Ft},dispersion:{value:0},retroreflectivity:{value:0},iridescence:{value:0},iridescenceMap:{value:null},iridescenceMapTransform:{value:new Ft},iridescenceIOR:{value:1.3},iridescenceThicknessMinimum:{value:100},iridescenceThicknessMaximum:{value:400},iridescenceThicknessMap:{value:null},iridescenceThicknessMapTransform:{value:new Ft},sheen:{value:0},sheenColor:{value:new St(0)},sheenColorMap:{value:null},sheenColorMapTransform:{value:new Ft},sheenRoughness:{value:1},sheenRoughnessMap:{value:null},sheenRoughnessMapTransform:{value:new Ft},transmission:{value:0},transmissionMap:{value:null},transmissionMapTransform:{value:new Ft},transmissionSamplerSize:{value:new et},transmissionSamplerMap:{value:null},thickness:{value:0},thicknessMap:{value:null},thicknessMapTransform:{value:new Ft},attenuationDistance:{value:0},attenuationColor:{value:new St(0)},specularColor:{value:new St(1,1,1)},specularColorMap:{value:null},specularColorMapTransform:{value:new Ft},specularIntensity:{value:1},specularIntensityMap:{value:null},specularIntensityMapTransform:{value:new Ft},anisotropyVector:{value:new et},anisotropyMap:{value:null},anisotropyMapTransform:{value:new Ft}}]),vertexShader:Ht.meshphysical_vert,fragmentShader:Ht.meshphysical_frag};var Ll={r:0,b:0,g:0},wx=new Gt,gf=new Ft;gf.set(-1,0,0,0,1,0,0,0,1);function Ex(n,t,e,i,s,r){let o=new St(0),a=s===!0?0:1,l,c,h=null,u=0,d=null;function f(_){let y=_.isScene===!0?_.background:null;if(y&&y.isTexture){let S=_.backgroundBlurriness>0;y=t.get(y,S)}return y}function p(_){let y=!1,S=f(_);S===null?m(o,a):S&&S.isColor&&(m(S,1),y=!0);let T=n.xr.getEnvironmentBlendMode();T==="additive"?e.buffers.color.setClear(0,0,0,1,r):T==="alpha-blend"&&e.buffers.color.setClear(0,0,0,0,r),(n.autoClear||y)&&(e.buffers.depth.setTest(!0),e.buffers.depth.setMask(!0),e.buffers.color.setMask(!0),n.clear(n.autoClearColor,n.autoClearDepth,n.autoClearStencil))}function x(_,y){let S=f(y);S&&S.isCubeTexture?(c===void 0&&(c=new Lt(new be(1,1,1),new ae({name:"BackgroundCubeMaterial",uniforms:hs(Ki.backgroundCube.uniforms),vertexShader:Ki.backgroundCube.vertexShader,fragmentShader:Ki.backgroundCube.fragmentShader,side:qe,depthTest:!1,depthWrite:!1,fog:!1,allowOverride:!1})),c.geometry.deleteAttribute("normal"),c.geometry.deleteAttribute("uv"),c.onBeforeRender=function(T,w,v){this.matrixWorld.copyPosition(v.matrixWorld)},Object.defineProperty(c.material,"envMap",{get:function(){return this.uniforms.envMap.value}}),i.update(c)),c.material.uniforms.envMap.value=S,c.material.uniforms.backgroundBlurriness.value=y.backgroundBlurriness,c.material.uniforms.backgroundIntensity.value=y.backgroundIntensity,c.material.uniforms.backgroundRotation.value.setFromMatrix4(wx.makeRotationFromEuler(y.backgroundRotation)).transpose(),S.isRenderTargetTexture===!1&&c.material.uniforms.backgroundRotation.value.premultiply(gf),c.material.toneMapped=qt.getTransfer(S.colorSpace)!==Qt,(h!==S||u!==S.version||d!==n.toneMapping)&&(c.material.needsUpdate=!0,h=S,u=S.version,d=n.toneMapping),c.layers.enableAll(),_.unshift(c,c.geometry,c.material,0,0,null)):S&&S.isTexture&&(l===void 0&&(l=new Lt(new _i(2,2),new ae({name:"BackgroundMaterial",uniforms:hs(Ki.background.uniforms),vertexShader:Ki.background.vertexShader,fragmentShader:Ki.background.fragmentShader,side:Fn,depthTest:!1,depthWrite:!1,fog:!1,allowOverride:!1})),l.geometry.deleteAttribute("normal"),Object.defineProperty(l.material,"map",{get:function(){return this.uniforms.t2D.value}}),i.update(l)),l.material.uniforms.t2D.value=S,l.material.uniforms.backgroundIntensity.value=y.backgroundIntensity,l.material.toneMapped=qt.getTransfer(S.colorSpace)!==Qt,S.matrixAutoUpdate===!0&&S.updateMatrix(),l.material.uniforms.uvTransform.value.copy(S.matrix),(h!==S||u!==S.version||d!==n.toneMapping)&&(l.material.needsUpdate=!0,h=S,u=S.version,d=n.toneMapping),l.layers.enableAll(),_.unshift(l,l.geometry,l.material,0,0,null))}function m(_,y){_.getRGB(Ll,wh(n)),e.buffers.color.setClear(Ll.r,Ll.g,Ll.b,y,r)}function g(){c!==void 0&&(c.geometry.dispose(),c.material.dispose(),c=void 0),l!==void 0&&(l.geometry.dispose(),l.material.dispose(),l=void 0)}return{getClearColor:function(){return o},setClearColor:function(_,y=1){o.set(_),a=y,m(o,a)},getClearAlpha:function(){return a},setClearAlpha:function(_){a=_,m(o,a)},render:p,addToRenderList:x,dispose:g}}function Tx(n,t){let e=n.getParameter(n.MAX_VERTEX_ATTRIBS),i={},s=d(null),r=s,o=!1;function a(A,C,U,L,B){let G=!1,z=u(A,L,U,C);r!==z&&(r=z,c(r.object)),G=f(A,L,U,B),G&&p(A,L,U,B),B!==null&&t.update(B,n.ELEMENT_ARRAY_BUFFER),(G||o)&&(o=!1,S(A,C,U,L),B!==null&&n.bindBuffer(n.ELEMENT_ARRAY_BUFFER,t.get(B).buffer))}function l(){return n.createVertexArray()}function c(A){return n.bindVertexArray(A)}function h(A){return n.deleteVertexArray(A)}function u(A,C,U,L){let B=L.wireframe===!0,G=i[C.id];G===void 0&&(G={},i[C.id]=G);let z=A.isInstancedMesh===!0?A.id:0,tt=G[z];tt===void 0&&(tt={},G[z]=tt);let W=tt[U.id];W===void 0&&(W={},tt[U.id]=W);let J=W[B];return J===void 0&&(J=d(l()),W[B]=J),J}function d(A){let C=[],U=[],L=[];for(let B=0;B<e;B++)C[B]=0,U[B]=0,L[B]=0;return{geometry:null,program:null,wireframe:!1,newAttributes:C,enabledAttributes:U,attributeDivisors:L,object:A,attributes:{},index:null}}function f(A,C,U,L){let B=r.attributes,G=C.attributes,z=0,tt=U.getAttributes();for(let W in tt)if(tt[W].location>=0){let j=B[W],xt=G[W];if(xt===void 0&&(W==="instanceMatrix"&&A.instanceMatrix&&(xt=A.instanceMatrix),W==="instanceColor"&&A.instanceColor&&(xt=A.instanceColor)),j===void 0||j.attribute!==xt||xt&&j.data!==xt.data)return!0;z++}return r.attributesNum!==z||r.index!==L}function p(A,C,U,L){let B={},G=C.attributes,z=0,tt=U.getAttributes();for(let W in tt)if(tt[W].location>=0){let j=G[W];j===void 0&&(W==="instanceMatrix"&&A.instanceMatrix&&(j=A.instanceMatrix),W==="instanceColor"&&A.instanceColor&&(j=A.instanceColor));let xt={};xt.attribute=j,j&&j.data&&(xt.data=j.data),B[W]=xt,z++}r.attributes=B,r.attributesNum=z,r.index=L}function x(){let A=r.newAttributes;for(let C=0,U=A.length;C<U;C++)A[C]=0}function m(A){g(A,0)}function g(A,C){let U=r.newAttributes,L=r.enabledAttributes,B=r.attributeDivisors;U[A]=1,L[A]===0&&(n.enableVertexAttribArray(A),L[A]=1),B[A]!==C&&(n.vertexAttribDivisor(A,C),B[A]=C)}function _(){let A=r.newAttributes,C=r.enabledAttributes;for(let U=0,L=C.length;U<L;U++)C[U]!==A[U]&&(n.disableVertexAttribArray(U),C[U]=0)}function y(A,C,U,L,B,G,z){z===!0?n.vertexAttribIPointer(A,C,U,B,G):n.vertexAttribPointer(A,C,U,L,B,G)}function S(A,C,U,L){x();let B=L.attributes,G=U.getAttributes(),z=C.defaultAttributeValues;for(let tt in G){let W=G[tt];if(W.location>=0){let J=B[tt];if(J===void 0&&(tt==="instanceMatrix"&&A.instanceMatrix&&(J=A.instanceMatrix),tt==="instanceColor"&&A.instanceColor&&(J=A.instanceColor)),J!==void 0){let j=J.normalized,xt=J.itemSize,Mt=t.get(J);if(Mt===void 0)continue;let q=Mt.buffer,ot=Mt.type,gt=Mt.bytesPerElement,wt=ot===n.INT||ot===n.UNSIGNED_INT||J.gpuType===$a;if(J.isInterleavedBufferAttribute){let rt=J.data,Dt=rt.isInterleavedBuffer===!0?rt.stride:rt.itemSize,ie=J.offset;if(rt.isInstancedInterleavedBuffer||rt.isInstancedBufferAttribute){for(let bt=0;bt<W.locationSize;bt++)g(W.location+bt,rt.meshPerAttribute);A.isInstancedMesh!==!0&&L._maxInstanceCount===void 0&&(L._maxInstanceCount=rt.meshPerAttribute*rt.count)}else for(let bt=0;bt<W.locationSize;bt++)m(W.location+bt);n.bindBuffer(n.ARRAY_BUFFER,q);for(let bt=0;bt<W.locationSize;bt++)y(W.location+bt,xt/W.locationSize,ot,j,Dt*gt,(ie+xt/W.locationSize*bt)*gt,wt)}else{if(J.isInstancedBufferAttribute){for(let rt=0;rt<W.locationSize;rt++)g(W.location+rt,J.meshPerAttribute);A.isInstancedMesh!==!0&&L._maxInstanceCount===void 0&&(L._maxInstanceCount=J.meshPerAttribute*J.count)}else for(let rt=0;rt<W.locationSize;rt++)m(W.location+rt);n.bindBuffer(n.ARRAY_BUFFER,q);for(let rt=0;rt<W.locationSize;rt++)y(W.location+rt,xt/W.locationSize,ot,j,xt*gt,xt/W.locationSize*rt*gt,wt)}}else if(z!==void 0){let j=z[tt];if(j!==void 0)switch(j.length){case 2:n.vertexAttrib2fv(W.location,j);break;case 3:n.vertexAttrib3fv(W.location,j);break;case 4:n.vertexAttrib4fv(W.location,j);break;default:n.vertexAttrib1fv(W.location,j)}}}}_()}function T(){D();for(let A in i){let C=i[A];for(let U in C){let L=C[U];for(let B in L){let G=L[B];for(let z in G)h(G[z].object),delete G[z];delete L[B]}}delete i[A]}}function w(A){if(i[A.id]===void 0)return;let C=i[A.id];for(let U in C){let L=C[U];for(let B in L){let G=L[B];for(let z in G)h(G[z].object),delete G[z];delete L[B]}}delete i[A.id]}function v(A){for(let C in i){let U=i[C];for(let L in U){let B=U[L];if(B[A.id]===void 0)continue;let G=B[A.id];for(let z in G)h(G[z].object),delete G[z];delete B[A.id]}}}function M(A){for(let C in i){let U=i[C],L=A.isInstancedMesh===!0?A.id:0,B=U[L];if(B!==void 0){for(let G in B){let z=B[G];for(let tt in z)h(z[tt].object),delete z[tt];delete B[G]}delete U[L],Object.keys(U).length===0&&delete i[C]}}}function D(){P(),o=!0,r!==s&&(r=s,c(r.object))}function P(){s.geometry=null,s.program=null,s.wireframe=!1}return{setup:a,reset:D,resetDefaultState:P,dispose:T,releaseStatesOfGeometry:w,releaseStatesOfObject:M,releaseStatesOfProgram:v,initAttributes:x,enableAttribute:m,disableUnusedAttributes:_}}function Ax(n,t,e){let i;function s(l){i=l}function r(l,c){n.drawArrays(i,l,c),e.update(c,i,1)}function o(l,c,h){h!==0&&(n.drawArraysInstanced(i,l,c,h),e.update(c,i,h))}function a(l,c,h){if(h===0)return;t.get("WEBGL_multi_draw").multiDrawArraysWEBGL(i,l,0,c,0,h);let d=0;for(let f=0;f<h;f++)d+=c[f];e.update(d,i,1)}this.setMode=s,this.render=r,this.renderInstances=o,this.renderMultiDraw=a}function Rx(n,t,e,i){let s;function r(){if(s!==void 0)return s;if(t.has("EXT_texture_filter_anisotropic")===!0){let v=t.get("EXT_texture_filter_anisotropic");s=n.getParameter(v.MAX_TEXTURE_MAX_ANISOTROPY_EXT)}else s=0;return s}function o(v){return!(v!==ai&&i.convert(v)!==n.getParameter(n.IMPLEMENTATION_COLOR_READ_FORMAT))}function a(v){let M=v===de&&(t.has("EXT_color_buffer_half_float")||t.has("EXT_color_buffer_float"));return!(v!==ti&&v!==ui&&!M&&i.convert(v)!==n.getParameter(n.IMPLEMENTATION_COLOR_READ_TYPE))}function l(v){if(v==="highp"){if(n.getShaderPrecisionFormat(n.VERTEX_SHADER,n.HIGH_FLOAT).precision>0&&n.getShaderPrecisionFormat(n.FRAGMENT_SHADER,n.HIGH_FLOAT).precision>0)return"highp";v="mediump"}return v==="mediump"&&n.getShaderPrecisionFormat(n.VERTEX_SHADER,n.MEDIUM_FLOAT).precision>0&&n.getShaderPrecisionFormat(n.FRAGMENT_SHADER,n.MEDIUM_FLOAT).precision>0?"mediump":"lowp"}let c=e.precision!==void 0?e.precision:"highp",h=l(c);h!==c&&(It("WebGLRenderer:",c,"not supported, using",h,"instead."),c=h);let u=e.reversedDepthBuffer===!0&&t.has("EXT_clip_control"),d=e.logarithmicDepthBuffer===!0&&u===!1;e.reversedDepthBuffer===!0&&u===!1&&It("WebGLRenderer: Unable to use reversed depth buffer due to missing EXT_clip_control extension. Fallback to default depth buffer."),e.logarithmicDepthBuffer===!0&&u===!0&&It("WebGLRenderer: Logarithmic depth buffer is ignored when reversed depth buffer is enabled.");let f=n.getParameter(n.MAX_TEXTURE_IMAGE_UNITS),p=n.getParameter(n.MAX_VERTEX_TEXTURE_IMAGE_UNITS),x=n.getParameter(n.MAX_TEXTURE_SIZE),m=n.getParameter(n.MAX_CUBE_MAP_TEXTURE_SIZE),g=n.getParameter(n.MAX_VERTEX_ATTRIBS),_=n.getParameter(n.MAX_VERTEX_UNIFORM_VECTORS),y=n.getParameter(n.MAX_VARYING_VECTORS),S=n.getParameter(n.MAX_FRAGMENT_UNIFORM_VECTORS),T=n.getParameter(n.MAX_SAMPLES),w=n.getParameter(n.SAMPLES);return{isWebGL2:!0,getMaxAnisotropy:r,getMaxPrecision:l,textureFormatReadable:o,textureTypeReadable:a,precision:c,logarithmicDepthBuffer:d,reversedDepthBuffer:u,maxTextures:f,maxVertexTextures:p,maxTextureSize:x,maxCubemapSize:m,maxAttributes:g,maxVertexUniforms:_,maxVaryings:y,maxFragmentUniforms:S,maxSamples:T,samples:w}}function Cx(n){let t=this,e=null,i=0,s=!1,r=!1,o=new si,a=new Ft,l={value:null,needsUpdate:!1};this.uniform=l,this.numPlanes=0,this.numIntersection=0,this.init=function(u,d){let f=u.length!==0||d||i!==0||s;return s=d,i=u.length,f},this.beginShadows=function(){r=!0,h(null)},this.endShadows=function(){r=!1},this.setGlobalState=function(u,d){e=h(u,d,0)},this.setState=function(u,d,f){let p=u.clippingPlanes,x=u.clipIntersection,m=u.clipShadows,g=n.get(u);if(!s||p===null||p.length===0||r&&!m)r?h(null):c();else{let _=r?0:i,y=_*4,S=g.clippingState||null;l.value=S,S=h(p,d,y,f);for(let T=0;T!==y;++T)S[T]=e[T];g.clippingState=S,this.numIntersection=x?this.numPlanes:0,this.numPlanes+=_}};function c(){l.value!==e&&(l.value=e,l.needsUpdate=i>0),t.numPlanes=i,t.numIntersection=0}function h(u,d,f,p){let x=u!==null?u.length:0,m=null;if(x!==0){if(m=l.value,p!==!0||m===null){let g=f+x*4,_=d.matrixWorldInverse;a.getNormalMatrix(_),(m===null||m.length<g)&&(m=new Float32Array(g));for(let y=0,S=f;y!==x;++y,S+=4)o.copy(u[y]).applyMatrix4(_,a),o.normal.toArray(m,S),m[S+3]=o.constant}l.value=m,l.needsUpdate=!0}return t.numPlanes=x,t.numIntersection=0,m}}var mo=class extends he{constructor(t=1,e={}){super(t,t,e),this.isWebGLCubeRenderTarget=!0;let i={width:t,height:t,depth:1},s=[i,i,i,i,i,i];this.texture=new Ur(s),this._setTextureOptions(e),this.texture.isRenderTargetTexture=!0}fromEquirectangularTexture(t,e){this.texture.type=e.type,this.texture.colorSpace=e.colorSpace,this.texture.generateMipmaps=e.generateMipmaps,this.texture.minFilter=e.minFilter,this.texture.magFilter=e.magFilter;let i={uniforms:{tEquirect:{value:null}},vertexShader:`

				varying vec3 vWorldDirection;

				vec3 transformDirection( in vec3 dir, in mat4 matrix ) {

					return normalize( ( matrix * vec4( dir, 0.0 ) ).xyz );

				}

				void main() {

					vWorldDirection = transformDirection( position, modelMatrix );

					#include <begin_vertex>
					#include <project_vertex>

				}
			`,fragmentShader:`

				uniform sampler2D tEquirect;

				varying vec3 vWorldDirection;

				#include <common>

				void main() {

					vec3 direction = normalize( vWorldDirection );

					vec2 sampleUV = equirectUv( direction );

					gl_FragColor = texture2D( tEquirect, sampleUV );

				}
			`},s=new be(5,5,5),r=new ae({name:"CubemapFromEquirect",uniforms:hs(i.uniforms),vertexShader:i.vertexShader,fragmentShader:i.fragmentShader,side:qe,blending:yi});r.uniforms.tEquirect.value=e;let o=new Lt(s,r),a=e.minFilter;return e.minFilter===Bi&&(e.minFilter=oe),new Zs(1,10,this).update(t,o),e.minFilter=a,o.geometry.dispose(),o.material.dispose(),this}clear(t,e=!0,i=!0,s=!0){let r=t.getRenderTarget();for(let o=0;o<6;o++)t.setRenderTarget(this,o),t.clear(e,i,s);t.setRenderTarget(r)}},Yd=256,Px=3,Ix=20,xf=256,_f=16,Dx=3,Lx=new R,Nx=new St,rr=class n{constructor(t){this._renderer=t,this._cubeSize=0,this._sourceTarget=null,this._cubeCamera=new Zs(1,10,null),this._boxMesh=new Lt(new be(5,5,5),null),this._cubemapMaterial=null,this._equirectMaterial=null,this._blurMaterial=null,this._ggxMaterial=null,this._integrationMaterial=null}fromScene(t,e=0,i=.1,s=100,r={}){let{size:o=256,position:a=Lx}=r,l=this._renderer;this._setSize(o);let c=this._allocateTarget(),h=this._getSourceTarget(!0);e>0&&(h.texture.mipmapsAutoUpdate=!1);let u=l.autoClear,d=l.autoClearColor,f=l.autoClearDepth,p=l.autoClearStencil,x=t.background;l.autoClear=!0,l.autoClearColor=!0,l.autoClearDepth=!0,l.autoClearStencil=!0,x===null&&(t.background=l.getClearColor(Nx));let m=new Zs(i,s,h);return m.position.copy(a),m.update(l,t),l.autoClear=u,l.autoClearColor=d,l.autoClearDepth=f,l.autoClearStencil=p,t.background=x,e>0&&(h.texture.mipmapsAutoUpdate=!0,this._blur(c,e)),this._applyPMREM(c),c}fromEquirectangular(t,e=null){return this._fromTexture(t,e)}fromCubemap(t,e=null){return this._fromTexture(t,e)}compileCubemapShader(){this._cubemapMaterial===null&&(this._cubemapMaterial=Jd(),this._compileMaterial(this._cubemapMaterial))}compileEquirectangularShader(){this._equirectMaterial===null&&(this._equirectMaterial=Kd(),this._compileMaterial(this._equirectMaterial))}dispose(){this._sourceTarget!==null&&this._sourceTarget.dispose(),this._cubemapMaterial!==null&&this._cubemapMaterial.dispose(),this._equirectMaterial!==null&&this._equirectMaterial.dispose(),this._blurMaterial!==null&&this._blurMaterial.dispose(),this._ggxMaterial!==null&&this._ggxMaterial.dispose(),this._integrationMaterial!==null&&this._integrationMaterial.dispose(),this._boxMesh.geometry.dispose()}static lodToRoughness(t,e){return e>0?1-Math.sqrt(1-t/e):0}_setSize(t){this._cubeSize=Math.max(Yd,Mh(t))}_fromTexture(t,e){t.mapping===ls||t.mapping===Qs?this._setSize(t.image.length===0?Yd:t.image[0].width||t.image[0].image.width):this._setSize(t.image.width/4);let i=e||this._allocateTarget();return this._textureToCubemap(t),this._applyPMREM(i),i}_allocateTarget(){let t=this._cubeSize,e=Zd(t,!1,!1),i=Math.log2(t)-Px;for(let s=0;s<=i;s++)e.texture.mipmaps.push({width:t>>s,height:t>>s});return e.texture.name="PMREM",e.texture.isPMREMTexture=!0,e}_getSourceTarget(t=!1){let e=this._cubeSize,i=this._sourceTarget;return(i===null||i.width!==e||t&&i.depthBuffer===!1)&&(i!==null&&i.dispose(),this._sourceTarget=Zd(e,!0,t)),this._sourceTarget}_compileMaterial(t){this._boxMesh.material=t,this._renderer.compile(this._boxMesh,this._cubeCamera.children[0])}_renderCube(t,e,i){let s=this._boxMesh,r=this._cubeCamera;s.material=i,r.renderTarget=t,r.activeMipmapLevel=e;let o=t.width>>e;t.viewport.set(0,0,o,o),r.update(this._renderer,s),t.viewport.set(0,0,t.width,t.height)}_textureToCubemap(t){let e;t.mapping===ls||t.mapping===Qs?(this._cubemapMaterial===null&&(this._cubemapMaterial=Jd()),e=this._cubemapMaterial,e.uniforms.flipEnvMap.value=t.isRenderTargetTexture===!1?-1:1):(this._equirectMaterial===null&&(this._equirectMaterial=Kd()),e=this._equirectMaterial),e.uniforms.envMap.value=t,this._renderCube(this._getSourceTarget(),0,e)}_applyPMREM(t){this._ggxMaterial===null&&(this._ggxMaterial=Ux(),this._integrationMaterial=Fx());let e=this._cubeSize,i=t.texture.mipmaps.length-1,s=this._ggxMaterial.uniforms;s.envMap.value=this._sourceTarget.texture;let r=this._integrationMaterial.uniforms;r.envMap.value=this._sourceTarget.texture,r.sourceLod.value=Math.log2(e/_f);for(let o=0;o<=i;o++){let a=n.lodToRoughness(o,i);if(o>i-Dx)r.roughness.value=a,this._renderCube(t,o,this._integrationMaterial);else{let l=a>0?Math.log2(e)+.5*Math.log2(6/(xf*Math.pow(a,4)))+.5:0;s.roughness.value=a,s.lodBias.value=l,this._renderCube(t,o,this._ggxMaterial)}}}_blur(t,e){this._blurMaterial===null&&(this._blurMaterial=Ox());let i=this._blurMaterial,s=i.uniforms,r=this._sourceTarget;s.sigma.value=Math.min(e,Math.PI)/Math.SQRT2,s.envMap.value=r.texture,this._renderCube(t,0,i),s.envMap.value=t.texture,this._renderCube(r,0,i)}};function Zd(n,t,e){return new mo(n,{minFilter:Bi,generateMipmaps:t,type:de,colorSpace:Cn,depthBuffer:e})}function go(n,t,e){return new ae({name:n,uniforms:t,vertexShader:Ht.cube_vert,fragmentShader:e,side:qe,blending:yi,depthTest:!1,depthWrite:!1})}function Ux(){return go("PMREMGGXConvolution",{envMap:{value:null},roughness:{value:0},lodBias:{value:0}},`

		#define GGX_SAMPLES ${xf}u

		varying vec3 vWorldDirection;

		uniform samplerCube envMap;
		uniform float roughness;
		uniform float lodBias;

		#include <common>

		// Van der Corput radical inverse
		float radicalInverse_VdC( uint bits ) {

			bits = ( bits << 16u ) | ( bits >> 16u );
			bits = ( ( bits & 0x55555555u ) << 1u ) | ( ( bits & 0xAAAAAAAAu ) >> 1u );
			bits = ( ( bits & 0x33333333u ) << 2u ) | ( ( bits & 0xCCCCCCCCu ) >> 2u );
			bits = ( ( bits & 0x0F0F0F0Fu ) << 4u ) | ( ( bits & 0xF0F0F0F0u ) >> 4u );
			bits = ( ( bits & 0x00FF00FFu ) << 8u ) | ( ( bits & 0xFF00FF00u ) >> 8u );
			return float( bits ) * 2.3283064365386963e-10; // / 0x100000000

		}

		// Hammersley sequence
		vec2 hammersley( uint i, uint N ) {

			return vec2( float( i ) / float( N ), radicalInverse_VdC( i ) );

		}

		void main() {

			vec3 N = normalize( vWorldDirection );

			// For very low roughness, just sample the environment directly
			if ( roughness < 0.001 ) {

				gl_FragColor = vec4( textureLod( envMap, N, 0.0 ).rgb, 1.0 );
				return;

			}

			float alpha = roughness * roughness;
			float alpha2 = alpha * alpha;

			// Tangent space basis for VNDF sampling
			vec3 up = abs( N.z ) < 0.999 ? vec3( 0.0, 0.0, 1.0 ) : vec3( 1.0, 0.0, 0.0 );
			vec3 tangent = normalize( cross( up, N ) );
			vec3 bitangent = cross( N, tangent );

			vec3 prefilteredColor = vec3( 0.0 );
			float totalWeight = 0.0;

			for ( uint i = 0u; i < GGX_SAMPLES; i ++ ) {

				vec2 Xi = hammersley( i, GGX_SAMPLES );

				// With V = N, sample the reflected direction directly.
				float invQ = 1.0 / ( 1.0 - Xi.x + alpha2 * Xi.x );
				float NdotL = ( 1.0 - Xi.x - alpha2 * Xi.x ) * invQ;

				if ( NdotL > 0.0 ) {

					float phi = 2.0 * PI * Xi.y;
					float sinTheta = 2.0 * alpha * sqrt( Xi.x * ( 1.0 - Xi.x ) ) * invQ;
					vec3 L = N * NdotL + ( tangent * cos( phi ) + bitangent * sin( phi ) ) * sinTheta;

					// Match the source mip to the sample's solid angle. The terms
					// independent of the sample are precomputed in lodBias.
					float d = alpha2 * invQ;
					float lod = max( log2( d ) + lodBias, 0.0 );

					// Weight by NdotL for the split-sum approximation
					prefilteredColor += textureLod( envMap, L, lod ).rgb * NdotL;
					totalWeight += NdotL;

				}

			}

			gl_FragColor = vec4( prefilteredColor / totalWeight, 1.0 );

		}
	`)}function Fx(){return go("PMREMGGXIntegration",{envMap:{value:null},roughness:{value:0},sourceLod:{value:0},sourceSize:{value:_f}},`

		varying vec3 vWorldDirection;

		uniform samplerCube envMap;
		uniform float roughness;
		uniform float sourceLod;
		uniform int sourceSize;

		void main() {

			vec3 N = normalize( vWorldDirection );

			float alpha = roughness * roughness;
			float alpha2 = alpha * alpha;

			float texelSize = 2.0 / float( sourceSize );

			vec3 prefilteredColor = vec3( 0.0 );
			float totalWeight = 0.0;

			// Pair opposite texels: only the one in N's hemisphere contributes.
			for ( int face = 0; face < 3; face ++ ) {

				for ( int y = 0; y < sourceSize; y ++ ) {

					for ( int x = 0; x < sourceSize; x ++ ) {

						vec2 uv = ( vec2( x, y ) + 0.5 ) * texelSize - 1.0;
						vec3 texelDirection = face == 0 ? vec3( 1.0, uv ) : ( face == 1 ? vec3( uv.x, 1.0, uv.y ) : vec3( uv, 1.0 ) );

						float invDistance = inversesqrt( 1.0 + dot( uv, uv ) );
						float NdotL = dot( N, texelDirection );
						texelDirection *= NdotL < 0.0 ? - 1.0 : 1.0;
						NdotL = abs( NdotL ) * invDistance;

						// With V = N, NdotH squared is ( 1 + NdotL ) / 2. Common factors
						// in the GGX distribution and texel solid angle cancel when normalized.
						float d = 1.0 + alpha2 + ( alpha2 - 1.0 ) * NdotL;
						float weight = NdotL * invDistance * invDistance * invDistance / ( d * d );

						prefilteredColor += textureLod( envMap, texelDirection, sourceLod ).rgb * weight;
						totalWeight += weight;

					}

				}

			}

			gl_FragColor = vec4( prefilteredColor / totalWeight, 1.0 );

		}
	`)}function Ox(){return go("PMREMSphericalGaussianBlur",{envMap:{value:null},sigma:{value:0}},`

		#define SAMPLES ${Ix}
		#define GOLDEN_ANGLE 2.39996322973

		varying vec3 vWorldDirection;

		uniform samplerCube envMap;
		uniform float sigma;

		#include <common>

		void main() {

			vec3 outputDirection = normalize( vWorldDirection );

			vec3 up = abs( outputDirection.z ) < 0.999 ? vec3( 0.0, 0.0, 1.0 ) : vec3( 1.0, 0.0, 0.0 );
			vec3 tangent = normalize( cross( up, outputDirection ) );
			vec3 bitangent = cross( outputDirection, tangent );

			// Truncate the kernel at three standard deviations or at the antipode.
			float thetaMax = min( 3.0 * sigma, PI );
			float truncation = 1.0 - exp( - 0.5 * thetaMax * thetaMax / ( sigma * sigma ) );

			vec3 accumColor = vec3( 0.0 );
			float accumWeight = 0.0;

			for ( int i = 0; i < SAMPLES; i ++ ) {

				// Stratified inverse-CDF sampling of the Gaussian, placed on a golden-angle spiral.
				float stratum = ( float( i ) + 0.5 ) / float( SAMPLES );
				float theta = sigma * sqrt( - 2.0 * log( 1.0 - stratum * truncation ) );
				float phi = float( i ) * GOLDEN_ANGLE;

				vec3 offset = cos( phi ) * tangent + sin( phi ) * bitangent;
				vec3 sampleDirection = cos( theta ) * outputDirection + sin( theta ) * offset;

				// Correct the planar sample density to solid angle.
				float weight = sin( theta ) / theta;

				accumColor += weight * textureLod( envMap, sampleDirection, 0.0 ).rgb;
				accumWeight += weight;

			}

			gl_FragColor = vec4( accumColor / accumWeight, 1.0 );

		}
	`)}function Kd(){return go("PMREMEquirectangularToCubemap",{envMap:{value:null}},`

		varying vec3 vWorldDirection;

		uniform sampler2D envMap;

		#include <common>

		void main() {

			// Average four subpixel samples to preserve small, bright features.
			vec3 direction = normalize( vWorldDirection );
			vec3 dx = dFdx( direction ) * 0.25;
			vec3 dy = dFdy( direction ) * 0.25;

			vec3 color = textureLod( envMap, equirectUv( normalize( direction - dx - dy ) ), 0.0 ).rgb;
			color += textureLod( envMap, equirectUv( normalize( direction + dx - dy ) ), 0.0 ).rgb;
			color += textureLod( envMap, equirectUv( normalize( direction - dx + dy ) ), 0.0 ).rgb;
			color += textureLod( envMap, equirectUv( normalize( direction + dx + dy ) ), 0.0 ).rgb;
			gl_FragColor = vec4( color * 0.25, 1.0 );

		}
	`)}function Jd(){return go("PMREMCubemapToCubemap",{envMap:{value:null},flipEnvMap:{value:-1}},`

		varying vec3 vWorldDirection;

		uniform samplerCube envMap;
		uniform float flipEnvMap;

		void main() {

			gl_FragColor = vec4( textureCube( envMap, vec3( flipEnvMap * vWorldDirection.x, vWorldDirection.yz ) ).rgb, 1.0 );

		}
	`)}function Bx(n){let t=new WeakMap,e=new WeakMap,i=null;function s(d,f=!1){return d==null?null:f?o(d):r(d)}function r(d){if(d&&d.isTexture){let f=d.mapping;if(f===Za||f===Ka)if(t.has(d)){let p=t.get(d).texture;return a(p,d.mapping)}else{let p=d.image;if(p&&p.height>0){let x=new mo(p.height);return x.fromEquirectangularTexture(n,d),t.set(d,x),d.addEventListener("dispose",c),a(x.texture,d.mapping)}else return null}}return d}function o(d){if(d&&d.isTexture&&d.isPMREMTexture!==!0){let f=d.mapping,p=f===Za||f===Ka,x=f===ls||f===Qs;if(p||x){let m=e.get(d),g=m!==void 0?m.texture.pmremVersion:0;if(d.isRenderTargetTexture&&d.pmremVersion!==g)return i===null&&(i=new rr(n)),m=p?i.fromEquirectangular(d,m):i.fromCubemap(d,m),m.texture.pmremVersion=d.pmremVersion,e.set(d,m),m.texture;if(m!==void 0)return m.texture;{let _=d.image;return p&&_&&_.height>0||x&&_&&l(_)?(i===null&&(i=new rr(n)),m=p?i.fromEquirectangular(d):i.fromCubemap(d),m.texture.pmremVersion=d.pmremVersion,e.set(d,m),d.addEventListener("dispose",h),m.texture):null}}}return d}function a(d,f){return f===Za?d.mapping=ls:f===Ka&&(d.mapping=Qs),d}function l(d){let f=0,p=6;for(let x=0;x<p;x++)d[x]!==void 0&&f++;return f===p}function c(d){let f=d.target;f.removeEventListener("dispose",c);let p=t.get(f);p!==void 0&&(t.delete(f),p.dispose())}function h(d){let f=d.target;f.removeEventListener("dispose",h);let p=e.get(f);p!==void 0&&(e.delete(f),p.dispose())}function u(){t=new WeakMap,e=new WeakMap,i!==null&&(i.dispose(),i=null)}return{get:s,dispose:u}}function zx(n){let t={};function e(i){if(t[i]!==void 0)return t[i];let s=n.getExtension(i);return t[i]=s,s}return{has:function(i){return e(i)!==null},init:function(){e("EXT_color_buffer_float"),e("WEBGL_clip_cull_distance"),e("OES_texture_float_linear"),e("EXT_color_buffer_half_float"),e("WEBGL_multisampled_render_to_texture"),e("WEBGL_render_shared_exponent")},get:function(i){let s=e(i);return s===null&&Kn("WebGLRenderer: "+i+" extension not supported."),s}}}function kx(n,t,e,i){let s={},r=new WeakMap,o=new FinalizationRegistry(p=>delete s[p]);function a(p){l(p.target)}function l(p){p.index!==null&&t.remove(p.index);for(let m in p.attributes)t.remove(p.attributes[m]);p.removeEventListener("dispose",a),delete s[p.id],o.unregister(p);let x=r.get(p);x&&(t.remove(x),r.delete(p)),i.releaseStatesOfGeometry(p),p.isInstancedBufferGeometry===!0&&delete p._maxInstanceCount,e.memory.geometries--}function c(p,x){return s[x.id]!==void 0||(x.addEventListener("dispose",a),s[x.id]=new WeakRef(x),o.register(x,x.id,x),e.memory.geometries++),x}function h(p){let x=p.attributes;for(let m in x)t.update(x[m],n.ARRAY_BUFFER)}function u(p){let x=p.index,m=p.attributes.position,g=0;if(m===void 0)return;let _=x!==null?x.array.length:m.array.length/3-1,y=m.count>=65535?Lr:Dr,S=new y(Math.ceil(_/3)*6,1),T=S.array;if(x!==null){let v=x.array;g=x.version;for(let M=0,D=0;M<_;M+=3){let P=v[M+0],A=v[M+1],C=v[M+2];T[D++]=P,T[D++]=A,T[D++]=A,T[D++]=C,T[D++]=C,T[D++]=P}}else{g=m.version;for(let v=0,M=0;v<_;v+=3){let D=v+0,P=v+1,A=v+2;T[M++]=D,T[M++]=P,T[M++]=P,T[M++]=A,T[M++]=A,T[M++]=D}}S.version=g;let w=r.get(p);w&&t.remove(w),r.set(p,S)}function d(p){let x=r.get(p);if(x){let m=p.index;m!==null&&x.version<m.version&&u(p)}else u(p);return r.get(p)}function f(){for(let p in s){let x=s[p].deref();x!==void 0&&l(x)}}return{get:c,update:h,getWireframeAttribute:d,dispose:f}}function Hx(n,t,e){let i;function s(u){i=u}let r,o;function a(u){r=u.type,o=u.bytesPerElement}function l(u,d){n.drawElements(i,d,r,u*o),e.update(d,i,1)}function c(u,d,f){f!==0&&(n.drawElementsInstanced(i,d,r,u*o,f),e.update(d,i,f))}function h(u,d,f){if(f===0)return;t.get("WEBGL_multi_draw").multiDrawElementsWEBGL(i,d,0,r,u,0,f);let x=0;for(let m=0;m<f;m++)x+=d[m];e.update(x,i,1)}this.setMode=s,this.setIndex=a,this.render=l,this.renderInstances=c,this.renderMultiDraw=h}function Vx(n){let t={geometries:0,textures:0},e={frame:0,calls:0,triangles:0,points:0,lines:0};function i(r,o,a){switch(e.calls++,o){case n.TRIANGLES:e.triangles+=a*(r/3);break;case n.LINES:e.lines+=a*(r/2);break;case n.LINE_STRIP:e.lines+=a*(r-1);break;case n.LINE_LOOP:e.lines+=a*r;break;case n.POINTS:e.points+=a*r;break;default:Ut("WebGLInfo: Unknown draw mode:",o);break}}function s(){e.calls=0,e.triangles=0,e.points=0,e.lines=0}return{memory:t,render:e,programs:null,autoReset:!0,reset:s,update:i}}function Gx(n,t,e){let i=new WeakMap,s=new ne;function r(o,a,l){let c=o.morphTargetInfluences,h=a.morphAttributes.position||a.morphAttributes.normal||a.morphAttributes.color,u=h!==void 0?h.length:0,d=i.get(a);if(d===void 0||d.count!==u){let D=function(){v.dispose(),i.delete(a),a.removeEventListener("dispose",D)};d!==void 0&&d.texture.dispose();let f=a.morphAttributes.position!==void 0,p=a.morphAttributes.normal!==void 0,x=a.morphAttributes.color!==void 0,m=a.morphAttributes.position||[],g=a.morphAttributes.normal||[],_=a.morphAttributes.color||[],y=0;f===!0&&(y=1),p===!0&&(y=2),x===!0&&(y=3);let S=a.attributes.position.count*y,T=1;S>t.maxTextureSize&&(T=Math.ceil(S/t.maxTextureSize),S=t.maxTextureSize);let w=new Float32Array(S*T*4*u),v=new Ir(w,S,T,u);v.type=ui,v.needsUpdate=!0;let M=y*4;for(let P=0;P<u;P++){let A=m[P],C=g[P],U=_[P],L=S*T*4*P;for(let B=0;B<A.count;B++){let G=B*M;f===!0&&(s.fromBufferAttribute(A,B),w[L+G+0]=s.x,w[L+G+1]=s.y,w[L+G+2]=s.z,w[L+G+3]=0),p===!0&&(s.fromBufferAttribute(C,B),w[L+G+4]=s.x,w[L+G+5]=s.y,w[L+G+6]=s.z,w[L+G+7]=0),x===!0&&(s.fromBufferAttribute(U,B),w[L+G+8]=s.x,w[L+G+9]=s.y,w[L+G+10]=s.z,w[L+G+11]=U.itemSize===4?s.w:1)}}d={count:u,texture:v,size:new et(S,T)},i.set(a,d),a.addEventListener("dispose",D)}if(o.isInstancedMesh===!0&&o.morphTexture!==null)l.getUniforms().setValue(n,"morphTexture",o.morphTexture,e);else{let f=0;for(let x=0;x<c.length;x++)f+=c[x];let p=a.morphTargetsRelative?1:1-f;l.getUniforms().setValue(n,"morphTargetBaseInfluence",p),l.getUniforms().setValue(n,"morphTargetInfluences",c)}l.getUniforms().setValue(n,"morphTargetsTexture",d.texture,e),l.getUniforms().setValue(n,"morphTargetsTextureSize",d.size)}return{update:r}}function Wx(n,t,e,i,s){let r=new WeakMap;function o(c){let h=s.render.frame,u=c.geometry,d=t.get(c,u);if(r.get(d)!==h&&(t.update(d),r.set(d,h)),c.isInstancedMesh&&(c.hasEventListener("dispose",l)===!1&&c.addEventListener("dispose",l),r.get(c)!==h&&(e.update(c.instanceMatrix,n.ARRAY_BUFFER),c.instanceColor!==null&&e.update(c.instanceColor,n.ARRAY_BUFFER),r.set(c,h))),c.isSkinnedMesh){let f=c.skeleton;r.get(f)!==h&&(f.update(),r.set(f,h))}return d}function a(){r=new WeakMap}function l(c){let h=c.target;h.removeEventListener("dispose",l),i.releaseStatesOfObject(h),e.remove(h.instanceMatrix),h.instanceColor!==null&&e.remove(h.instanceColor)}return{update:o,dispose:a}}var Xx={[jr]:"LINEAR_TONE_MAPPING",[Qr]:"REINHARD_TONE_MAPPING",[to]:"CINEON_TONE_MAPPING",[as]:"ACES_FILMIC_TONE_MAPPING",[io]:"AGX_TONE_MAPPING",[no]:"NEUTRAL_TONE_MAPPING",[eo]:"CUSTOM_TONE_MAPPING"};function qx(n,t,e,i,s,r){let o=new he(t,e,{type:n,depthBuffer:s,stencilBuffer:r,samples:i?4:0,storeMultisampledDepthBuffer:!1,storeMultisampledStencilBuffer:!1,resolveDepthBuffer:!1,resolveStencilBuffer:!1}),a=null,l=null,c=null,h=new Yt;h.setAttribute("position",new jt([-1,3,0,-1,-1,0,3,-1,0],3)),h.setAttribute("uv",new jt([0,2,0,0,2,0],2));let u=new In({uniforms:{tDiffuse:{value:null}},vertexShader:`
			precision highp float;

			uniform mat4 modelViewMatrix;
			uniform mat4 projectionMatrix;

			attribute vec3 position;
			attribute vec2 uv;

			varying vec2 vUv;

			void main() {
				vUv = uv;
				gl_Position = projectionMatrix * modelViewMatrix * vec4( position, 1.0 );
			}`,fragmentShader:`
			precision highp float;

			uniform sampler2D tDiffuse;

			varying vec2 vUv;

			#include <tonemapping_pars_fragment>
			#include <colorspace_pars_fragment>

			void main() {
				gl_FragColor = texture2D( tDiffuse, vUv );

				#ifdef LINEAR_TONE_MAPPING
					gl_FragColor.rgb = LinearToneMapping( gl_FragColor.rgb );
				#elif defined( REINHARD_TONE_MAPPING )
					gl_FragColor.rgb = ReinhardToneMapping( gl_FragColor.rgb );
				#elif defined( CINEON_TONE_MAPPING )
					gl_FragColor.rgb = CineonToneMapping( gl_FragColor.rgb );
				#elif defined( ACES_FILMIC_TONE_MAPPING )
					gl_FragColor.rgb = ACESFilmicToneMapping( gl_FragColor.rgb );
				#elif defined( AGX_TONE_MAPPING )
					gl_FragColor.rgb = AgXToneMapping( gl_FragColor.rgb );
				#elif defined( NEUTRAL_TONE_MAPPING )
					gl_FragColor.rgb = NeutralToneMapping( gl_FragColor.rgb );
				#elif defined( CUSTOM_TONE_MAPPING )
					gl_FragColor.rgb = CustomToneMapping( gl_FragColor.rgb );
				#endif

				#ifdef SRGB_TRANSFER
					gl_FragColor = sRGBTransferOETF( gl_FragColor );
				#endif
			}`,depthTest:!1,depthWrite:!1}),d=new Lt(h,u),f=new rs(-1,1,1,-1,0,1),p=null,x=null,m=!1,g,_=null,y=[],S=!1;this.setSize=function(T,w){o.setSize(T,w),a!==null&&a.setSize(T,w),l!==null&&l.setSize(T,w);for(let v=0;v<y.length;v++){let M=y[v];M.setSize&&M.setSize(T,w)}},this.setEffects=function(T){y=T,S=y.length>0&&y[0].isRenderPass===!0;let w=o.width,v=o.height;y.length>0&&a===null&&(a=new he(w,v,{type:de,depthBuffer:!1,stencilBuffer:!1}),l=new he(w,v,{type:de,depthBuffer:!1,stencilBuffer:!1}));let M=s===!0&&y.some(D=>D.needsDepthTexture===!0);M!==(o.depthTexture!==null)&&(M===!0&&c===null&&(c=new Fi(w,v),r===!0&&(c.format=mn,c.type=cs)),o.dispose(),o.depthTexture=M?c:null,o.resolveDepthBuffer=M,o.storeMultisampledDepthBuffer=M);for(let D=0;D<y.length;D++){let P=y[D];P.setSize&&P.setSize(w,v),M&&P.needsDepthTexture===!0&&P.setDepthTexture&&P.setDepthTexture(c)}},this.begin=function(T,w){if(m||T.toneMapping===qi&&y.length===0)return!1;if(_=w,w!==null){let v=w.width,M=w.height;(o.width!==v||o.height!==M)&&this.setSize(v,M)}return S===!1&&T.setRenderTarget(o),g=T.toneMapping,T.toneMapping=qi,!0},this.hasRenderPass=function(){return S},this.end=function(T,w){T.toneMapping=g,m=!0;let v=o,M=a;for(let D=0;D<y.length;D++){let P=y[D];P.enabled!==!1&&(P.render(T,M,v,w),P.needsSwap!==!1&&(v=M,M=M===a?l:a))}if(p!==T.outputColorSpace||x!==T.toneMapping){p=T.outputColorSpace,x=T.toneMapping,u.defines={},qt.getTransfer(p)===Qt&&(u.defines.SRGB_TRANSFER="");let D=Xx[x];D&&(u.defines[D]=""),u.needsUpdate=!0}u.uniforms.tDiffuse.value=v.texture,T.setRenderTarget(_),T.render(d,f),_=null,m=!1},this.isCompositing=function(){return m},this.dispose=function(){o.dispose(),a!==null&&a.dispose(),l!==null&&l.dispose(),h.dispose(),u.dispose()}}var vf=new We,yf=new Fi(1,1),Mf=new Fr(1),Sf=new Ir,bf=new Bs,wf=new Ur,$d=[],jd=[],Qd=new Float32Array(16),tf=new Float32Array(9),ef=new Float32Array(4);function or(n,t,e){let i=n[0];if(i<=0||i>0)return n;let s=t*e,r=$d[s];if(r===void 0&&(r=new Float32Array(s),$d[s]=r),t!==0){i.toArray(r,0);for(let o=1,a=0;o!==t;++o)a+=e,n[o].toArray(r,a)}return r}function Le(n,t){if(n.length!==t.length)return!1;for(let e=0,i=n.length;e<i;e++)if(n[e]!==t[e])return!1;return!0}function Ne(n,t){for(let e=0,i=t.length;e<i;e++)n[e]=t[e]}function Fl(n,t){let e=jd[t];e===void 0&&(e=new Int32Array(t),jd[t]=e);for(let i=0;i!==t;++i)e[i]=n.allocateTextureUnit();return e}function Yx(n,t){let e=this.cache;e[0]!==t&&(n.uniform1f(this.addr,t),e[0]=t)}function Zx(n,t){let e=this.cache;if(t.x!==void 0)(e[0]!==t.x||e[1]!==t.y)&&(n.uniform2f(this.addr,t.x,t.y),e[0]=t.x,e[1]=t.y);else{if(Le(e,t))return;n.uniform2fv(this.addr,t),Ne(e,t)}}function Kx(n,t){let e=this.cache;if(t.x!==void 0)(e[0]!==t.x||e[1]!==t.y||e[2]!==t.z)&&(n.uniform3f(this.addr,t.x,t.y,t.z),e[0]=t.x,e[1]=t.y,e[2]=t.z);else if(t.r!==void 0)(e[0]!==t.r||e[1]!==t.g||e[2]!==t.b)&&(n.uniform3f(this.addr,t.r,t.g,t.b),e[0]=t.r,e[1]=t.g,e[2]=t.b);else{if(Le(e,t))return;n.uniform3fv(this.addr,t),Ne(e,t)}}function Jx(n,t){let e=this.cache;if(t.x!==void 0)(e[0]!==t.x||e[1]!==t.y||e[2]!==t.z||e[3]!==t.w)&&(n.uniform4f(this.addr,t.x,t.y,t.z,t.w),e[0]=t.x,e[1]=t.y,e[2]=t.z,e[3]=t.w);else{if(Le(e,t))return;n.uniform4fv(this.addr,t),Ne(e,t)}}function $x(n,t){let e=this.cache,i=t.elements;if(i===void 0){if(Le(e,t))return;n.uniformMatrix2fv(this.addr,!1,t),Ne(e,t)}else{if(Le(e,i))return;ef.set(i),n.uniformMatrix2fv(this.addr,!1,ef),Ne(e,i)}}function jx(n,t){let e=this.cache,i=t.elements;if(i===void 0){if(Le(e,t))return;n.uniformMatrix3fv(this.addr,!1,t),Ne(e,t)}else{if(Le(e,i))return;tf.set(i),n.uniformMatrix3fv(this.addr,!1,tf),Ne(e,i)}}function Qx(n,t){let e=this.cache,i=t.elements;if(i===void 0){if(Le(e,t))return;n.uniformMatrix4fv(this.addr,!1,t),Ne(e,t)}else{if(Le(e,i))return;Qd.set(i),n.uniformMatrix4fv(this.addr,!1,Qd),Ne(e,i)}}function t_(n,t){let e=this.cache;e[0]!==t&&(n.uniform1i(this.addr,t),e[0]=t)}function e_(n,t){let e=this.cache;if(t.x!==void 0)(e[0]!==t.x||e[1]!==t.y)&&(n.uniform2i(this.addr,t.x,t.y),e[0]=t.x,e[1]=t.y);else{if(Le(e,t))return;n.uniform2iv(this.addr,t),Ne(e,t)}}function i_(n,t){let e=this.cache;if(t.x!==void 0)(e[0]!==t.x||e[1]!==t.y||e[2]!==t.z)&&(n.uniform3i(this.addr,t.x,t.y,t.z),e[0]=t.x,e[1]=t.y,e[2]=t.z);else{if(Le(e,t))return;n.uniform3iv(this.addr,t),Ne(e,t)}}function n_(n,t){let e=this.cache;if(t.x!==void 0)(e[0]!==t.x||e[1]!==t.y||e[2]!==t.z||e[3]!==t.w)&&(n.uniform4i(this.addr,t.x,t.y,t.z,t.w),e[0]=t.x,e[1]=t.y,e[2]=t.z,e[3]=t.w);else{if(Le(e,t))return;n.uniform4iv(this.addr,t),Ne(e,t)}}function s_(n,t){let e=this.cache;e[0]!==t&&(n.uniform1ui(this.addr,t),e[0]=t)}function r_(n,t){let e=this.cache;if(t.x!==void 0)(e[0]!==t.x||e[1]!==t.y)&&(n.uniform2ui(this.addr,t.x,t.y),e[0]=t.x,e[1]=t.y);else{if(Le(e,t))return;n.uniform2uiv(this.addr,t),Ne(e,t)}}function o_(n,t){let e=this.cache;if(t.x!==void 0)(e[0]!==t.x||e[1]!==t.y||e[2]!==t.z)&&(n.uniform3ui(this.addr,t.x,t.y,t.z),e[0]=t.x,e[1]=t.y,e[2]=t.z);else{if(Le(e,t))return;n.uniform3uiv(this.addr,t),Ne(e,t)}}function a_(n,t){let e=this.cache;if(t.x!==void 0)(e[0]!==t.x||e[1]!==t.y||e[2]!==t.z||e[3]!==t.w)&&(n.uniform4ui(this.addr,t.x,t.y,t.z,t.w),e[0]=t.x,e[1]=t.y,e[2]=t.z,e[3]=t.w);else{if(Le(e,t))return;n.uniform4uiv(this.addr,t),Ne(e,t)}}function Ol(n){return n.version===0&&(n.compareFunction=_h,n.needsUpdate=!0),n}function l_(n,t,e){let i=this.cache,s=e.allocateTextureUnit();i[0]!==s&&(n.uniform1i(this.addr,s),i[0]=s);let r;this.type===n.SAMPLER_2D_SHADOW?r=Ol(yf):r=vf,e.setTexture2D(t||r,s)}function c_(n,t,e){let i=this.cache,s=e.allocateTextureUnit();i[0]!==s&&(n.uniform1i(this.addr,s),i[0]=s),e.setTexture3D(t||bf,s)}function h_(n,t,e){let i=this.cache,s=e.allocateTextureUnit();i[0]!==s&&(n.uniform1i(this.addr,s),i[0]=s);let r;this.type===n.SAMPLER_CUBE_SHADOW?r=Ol(Mf):r=wf,e.setTextureCube(t||r,s)}function u_(n,t,e){let i=this.cache,s=e.allocateTextureUnit();i[0]!==s&&(n.uniform1i(this.addr,s),i[0]=s),e.setTexture2DArray(t||Sf,s)}function d_(n){switch(n){case 5126:return Yx;case 35664:return Zx;case 35665:return Kx;case 35666:return Jx;case 35674:return $x;case 35675:return jx;case 35676:return Qx;case 5124:case 35670:return t_;case 35667:case 35671:return e_;case 35668:case 35672:return i_;case 35669:case 35673:return n_;case 5125:return s_;case 36294:return r_;case 36295:return o_;case 36296:return a_;case 35678:case 36198:case 36298:case 36306:case 35682:return l_;case 35679:case 36299:case 36307:return c_;case 35680:case 36300:case 36308:case 36293:return h_;case 36289:case 36303:case 36311:case 36292:return u_}}function f_(n,t){n.uniform1fv(this.addr,t)}function p_(n,t){let e=or(t,this.size,2);n.uniform2fv(this.addr,e)}function m_(n,t){let e=or(t,this.size,3);n.uniform3fv(this.addr,e)}function g_(n,t){let e=or(t,this.size,4);n.uniform4fv(this.addr,e)}function x_(n,t){let e=or(t,this.size,4);n.uniformMatrix2fv(this.addr,!1,e)}function __(n,t){let e=or(t,this.size,9);n.uniformMatrix3fv(this.addr,!1,e)}function v_(n,t){let e=or(t,this.size,16);n.uniformMatrix4fv(this.addr,!1,e)}function y_(n,t){n.uniform1iv(this.addr,t)}function M_(n,t){n.uniform2iv(this.addr,t)}function S_(n,t){n.uniform3iv(this.addr,t)}function b_(n,t){n.uniform4iv(this.addr,t)}function w_(n,t){n.uniform1uiv(this.addr,t)}function E_(n,t){n.uniform2uiv(this.addr,t)}function T_(n,t){n.uniform3uiv(this.addr,t)}function A_(n,t){n.uniform4uiv(this.addr,t)}function R_(n,t,e){let i=this.cache,s=t.length,r=Fl(e,s);Le(i,r)||(n.uniform1iv(this.addr,r),Ne(i,r));let o;this.type===n.SAMPLER_2D_SHADOW?o=Ol(yf):o=vf;for(let a=0;a!==s;++a)e.setTexture2D(t[a]||o,r[a])}function C_(n,t,e){let i=this.cache,s=t.length,r=Fl(e,s);Le(i,r)||(n.uniform1iv(this.addr,r),Ne(i,r));for(let o=0;o!==s;++o)e.setTexture3D(t[o]||bf,r[o])}function P_(n,t,e){let i=this.cache,s=t.length,r=Fl(e,s);Le(i,r)||(n.uniform1iv(this.addr,r),Ne(i,r));let o;this.type===n.SAMPLER_CUBE_SHADOW?o=Ol(Mf):o=wf;for(let a=0;a!==s;++a)e.setTextureCube(t[a]||o,r[a])}function I_(n,t,e){let i=this.cache,s=t.length,r=Fl(e,s);Le(i,r)||(n.uniform1iv(this.addr,r),Ne(i,r));for(let o=0;o!==s;++o)e.setTexture2DArray(t[o]||Sf,r[o])}function D_(n){switch(n){case 5126:return f_;case 35664:return p_;case 35665:return m_;case 35666:return g_;case 35674:return x_;case 35675:return __;case 35676:return v_;case 5124:case 35670:return y_;case 35667:case 35671:return M_;case 35668:case 35672:return S_;case 35669:case 35673:return b_;case 5125:return w_;case 36294:return E_;case 36295:return T_;case 36296:return A_;case 35678:case 36198:case 36298:case 36306:case 35682:return R_;case 35679:case 36299:case 36307:return C_;case 35680:case 36300:case 36308:case 36293:return P_;case 36289:case 36303:case 36311:case 36292:return I_}}var Ph=class{constructor(t,e,i){this.id=t,this.addr=i,this.cache=[],this.type=e.type,this.setValue=d_(e.type)}},Ih=class{constructor(t,e,i){this.id=t,this.addr=i,this.cache=[],this.type=e.type,this.size=e.size,this.setValue=D_(e.type)}},Dh=class{constructor(t){this.id=t,this.seq=[],this.map={}}setValue(t,e,i){let s=this.seq;for(let r=0,o=s.length;r!==o;++r){let a=s[r];a.setValue(t,e[a.id],i)}}},Rh=/(\w+)(\])?(\[|\.)?/g;function nf(n,t){n.seq.push(t),n.map[t.id]=t}function L_(n,t,e){let i=n.name,s=i.length;for(Rh.lastIndex=0;;){let r=Rh.exec(i),o=Rh.lastIndex,a=r[1],l=r[2]==="]",c=r[3];if(l&&(a=a|0),c===void 0||c==="["&&o+2===s){nf(e,c===void 0?new Ph(a,n,t):new Ih(a,n,t));break}else{let u=e.map[a];u===void 0&&(u=new Dh(a),nf(e,u)),e=u}}}var sr=class{constructor(t,e){this.seq=[],this.map={};let i=t.getProgramParameter(e,t.ACTIVE_UNIFORMS);for(let o=0;o<i;++o){let a=t.getActiveUniform(e,o),l=t.getUniformLocation(e,a.name);L_(a,l,this)}let s=[],r=[];for(let o of this.seq)o.type===t.SAMPLER_2D_SHADOW||o.type===t.SAMPLER_CUBE_SHADOW||o.type===t.SAMPLER_2D_ARRAY_SHADOW?s.push(o):r.push(o);s.length>0&&(this.seq=s.concat(r))}setValue(t,e,i,s){let r=this.map[e];r!==void 0&&r.setValue(t,i,s)}setOptional(t,e,i){let s=e[i];s!==void 0&&this.setValue(t,i,s)}static upload(t,e,i,s){for(let r=0,o=e.length;r!==o;++r){let a=e[r],l=i[a.id];l.needsUpdate!==!1&&a.setValue(t,l.value,s)}}static seqWithValue(t,e){let i=[];for(let s=0,r=t.length;s!==r;++s){let o=t[s];o.id in e&&i.push(o)}return i}};function sf(n,t,e){let i=n.createShader(t);return n.shaderSource(i,e),n.compileShader(i),i}var N_=37297,U_=0;function F_(n,t){let e=n.split(`
`),i=[],s=Math.max(t-6,0),r=Math.min(t+6,e.length);for(let o=s;o<r;o++){let a=o+1;i.push(`${a===t?">":" "} ${a}: ${e[o]}`)}return i.join(`
`)}var rf=new Ft;function O_(n){qt._getMatrix(rf,qt.workingColorSpace,n);let t=`mat3( ${rf.elements.map(e=>e.toFixed(4))} )`;switch(qt.getTransfer(n)){case Cr:return[t,"LinearTransferOETF"];case Qt:return[t,"sRGBTransferOETF"];default:return It("WebGLProgram: Unsupported color space: ",n),[t,"LinearTransferOETF"]}}function of(n,t,e){let i=n.getShaderParameter(t,n.COMPILE_STATUS),r=(n.getShaderInfoLog(t)||"").trim();if(i&&r==="")return"";let o=/ERROR: 0:(\d+)/.exec(r);if(o){let a=parseInt(o[1]);return e.toUpperCase()+`

`+r+`

`+F_(n.getShaderSource(t),a)}else return r}function B_(n,t){let e=O_(t);return[`vec4 ${n}( vec4 value ) {`,`	return ${e[1]}( vec4( ${e[0]} * value.rgb, value.a ) );`,"}"].join(`
`)}var z_={[jr]:"Linear",[Qr]:"Reinhard",[to]:"Cineon",[as]:"ACESFilmic",[io]:"AgX",[no]:"Neutral",[eo]:"Custom"};function k_(n,t){let e=z_[t];return e===void 0?(It("WebGLProgram: Unsupported toneMapping:",t),"vec3 "+n+"( vec3 color ) { return LinearToneMapping( color ); }"):"vec3 "+n+"( vec3 color ) { return "+e+"ToneMapping( color ); }"}var Nl=new R;function H_(){qt.getLuminanceCoefficients(Nl);let n=Nl.x.toFixed(4),t=Nl.y.toFixed(4),e=Nl.z.toFixed(4);return["float luminance( const in vec3 rgb ) {",`	const vec3 weights = vec3( ${n}, ${t}, ${e} );`,"	return dot( weights, rgb );","}"].join(`
`)}function V_(n){return[n.extensionClipCullDistance?"#extension GL_ANGLE_clip_cull_distance : require":"",n.extensionMultiDraw?"#extension GL_ANGLE_multi_draw : require":""].filter(po).join(`
`)}function G_(n){let t=[];for(let e in n){let i=n[e];i!==!1&&t.push("#define "+e+" "+i)}return t.join(`
`)}function W_(n,t){let e={},i=n.getProgramParameter(t,n.ACTIVE_ATTRIBUTES);for(let s=0;s<i;s++){let r=n.getActiveAttrib(t,s),o=r.name,a=1;r.type===n.FLOAT_MAT2&&(a=2),r.type===n.FLOAT_MAT3&&(a=3),r.type===n.FLOAT_MAT4&&(a=4),e[o]={type:r.type,location:n.getAttribLocation(t,o),locationSize:a}}return e}function po(n){return n!==""}function af(n,t){let e=t.numSpotLightShadows+t.numSpotLightMaps-t.numSpotLightShadowsWithMaps;return n.replace(/NUM_SUN_LIGHTS/g,t.numSunLights).replace(/NUM_DIR_LIGHTS/g,t.numDirLights).replace(/NUM_SPOT_LIGHTS/g,t.numSpotLights).replace(/NUM_SPOT_LIGHT_MAPS/g,t.numSpotLightMaps).replace(/NUM_SPOT_LIGHT_COORDS/g,e).replace(/NUM_RECT_AREA_LIGHTS/g,t.numRectAreaLights).replace(/NUM_POINT_LIGHTS/g,t.numPointLights).replace(/NUM_HEMI_LIGHTS/g,t.numHemiLights).replace(/NUM_SUN_LIGHT_SHADOWS/g,t.numSunLightShadows).replace(/NUM_DIR_LIGHT_SHADOWS/g,t.numDirLightShadows).replace(/NUM_SPOT_LIGHT_SHADOWS_WITH_MAPS/g,t.numSpotLightShadowsWithMaps).replace(/NUM_SPOT_LIGHT_SHADOWS/g,t.numSpotLightShadows).replace(/NUM_POINT_LIGHT_SHADOWS/g,t.numPointLightShadows).replace(/NUM_LIGHT_PROBE_GRIDS/g,t.numLightProbeGrids)}function lf(n,t){return n.replace(/NUM_CLIPPING_PLANES/g,t.numClippingPlanes).replace(/UNION_CLIPPING_PLANES/g,t.numClippingPlanes-t.numClipIntersection)}var X_=/^[ \t]*#include +<([\w\d./]+)>/gm;function Lh(n){return n.replace(X_,q_)}function q_(n,t){let e=Ht[t];if(e===void 0)throw new Error("THREE.WebGLProgram: Can not resolve #include <"+t+">");return Lh(e)}var Y_=/#pragma unroll_loop_start\s+for\s*\(\s*int\s+i\s*=\s*(\d+)\s*;\s*i\s*<\s*(\d+)\s*;\s*i\s*\+\+\s*\)\s*{([\s\S]+?)}\s+#pragma unroll_loop_end/g;function cf(n){return n.replace(Y_,Z_)}function Z_(n,t,e,i){let s="";for(let r=parseInt(t);r<parseInt(e);r++)s+=i.replace(/\[\s*i\s*\]/g,"[ "+r+" ]").replace(/UNROLLED_LOOP_INDEX/g,r);return s}function hf(n){let t=`precision ${n.precision} float;
	precision ${n.precision} int;
	precision ${n.precision} sampler2D;
	precision ${n.precision} samplerCube;
	precision ${n.precision} sampler3D;
	precision ${n.precision} sampler2DArray;
	precision ${n.precision} sampler2DShadow;
	precision ${n.precision} samplerCubeShadow;
	precision ${n.precision} sampler2DArrayShadow;
	precision ${n.precision} isampler2D;
	precision ${n.precision} isampler3D;
	precision ${n.precision} isamplerCube;
	precision ${n.precision} isampler2DArray;
	precision ${n.precision} usampler2D;
	precision ${n.precision} usampler3D;
	precision ${n.precision} usamplerCube;
	precision ${n.precision} usampler2DArray;
	`;return n.precision==="highp"?t+=`
#define HIGH_PRECISION`:n.precision==="mediump"?t+=`
#define MEDIUM_PRECISION`:n.precision==="lowp"&&(t+=`
#define LOW_PRECISION`),t}var K_={[os]:"SHADOWMAP_TYPE_PCF",[Js]:"SHADOWMAP_TYPE_VSM"};function J_(n){return K_[n.shadowMapType]||"SHADOWMAP_TYPE_BASIC"}var $_={[Qs]:"ENVMAP_MODE_REFRACTION"};function j_(n){return n.envMap===!1?"ENVMAP_MODE_REFLECTION":$_[n.envMapMode]||"ENVMAP_MODE_REFLECTION"}var Q_={[Ya]:"ENVMAP_BLENDING_MULTIPLY",[Td]:"ENVMAP_BLENDING_MIX",[Ad]:"ENVMAP_BLENDING_ADD"};function t1(n){return n.envMap===!1?"ENVMAP_BLENDING_NONE":Q_[n.combine]||"ENVMAP_BLENDING_NONE"}function e1(n,t,e,i){let s=n.getContext(),r=e.defines,o=e.vertexShader,a=e.fragmentShader,l=J_(e),c=e.envMapPMREM===!0?"ENVMAP_TYPE_PMREM":"ENVMAP_TYPE_CUBE",h=j_(e),u=t1(e),d=V_(e),f=G_(r),p=s.createProgram(),x,m,g=e.glslVersion?"#version "+e.glslVersion+`
`:"";e.isRawShaderMaterial?(x=["#define SHADER_TYPE "+e.shaderType,"#define SHADER_NAME "+e.shaderName,f].filter(po).join(`
`),x.length>0&&(x+=`
`),m=["#define SHADER_TYPE "+e.shaderType,"#define SHADER_NAME "+e.shaderName,f].filter(po).join(`
`),m.length>0&&(m+=`
`)):(x=[hf(e),"#define SHADER_TYPE "+e.shaderType,"#define SHADER_NAME "+e.shaderName,f,e.extensionClipCullDistance?"#define USE_CLIP_DISTANCE":"",e.batching?"#define USE_BATCHING":"",e.batchingColor?"#define USE_BATCHING_COLOR":"",e.instancing?"#define USE_INSTANCING":"",e.instancingColor?"#define USE_INSTANCING_COLOR":"",e.instancingMorph?"#define USE_INSTANCING_MORPH":"",e.useFog&&e.fog?"#define USE_FOG":"",e.useFog&&e.fogExp2?"#define FOG_EXP2":"",e.map?"#define USE_MAP":"",e.envMap?"#define USE_ENVMAP":"",e.envMap?"#define "+h:"",e.lightMap?"#define USE_LIGHTMAP":"",e.aoMap?"#define USE_AOMAP":"",e.bumpMap?"#define USE_BUMPMAP":"",e.normalMap?"#define USE_NORMALMAP":"",e.normalMapObjectSpace?"#define USE_NORMALMAP_OBJECTSPACE":"",e.normalMapTangentSpace?"#define USE_NORMALMAP_TANGENTSPACE":"",e.displacementMap?"#define USE_DISPLACEMENTMAP":"",e.emissiveMap?"#define USE_EMISSIVEMAP":"",e.anisotropy?"#define USE_ANISOTROPY":"",e.anisotropyMap?"#define USE_ANISOTROPYMAP":"",e.clearcoatMap?"#define USE_CLEARCOATMAP":"",e.clearcoatRoughnessMap?"#define USE_CLEARCOAT_ROUGHNESSMAP":"",e.clearcoatNormalMap?"#define USE_CLEARCOAT_NORMALMAP":"",e.diffuseRoughnessMap?"#define USE_DIFFUSE_ROUGHNESSMAP":"",e.iridescenceMap?"#define USE_IRIDESCENCEMAP":"",e.iridescenceThicknessMap?"#define USE_IRIDESCENCE_THICKNESSMAP":"",e.specularMap?"#define USE_SPECULARMAP":"",e.specularColorMap?"#define USE_SPECULAR_COLORMAP":"",e.specularIntensityMap?"#define USE_SPECULAR_INTENSITYMAP":"",e.roughnessMap?"#define USE_ROUGHNESSMAP":"",e.metalnessMap?"#define USE_METALNESSMAP":"",e.alphaMap?"#define USE_ALPHAMAP":"",e.alphaHash?"#define USE_ALPHAHASH":"",e.transmission?"#define USE_TRANSMISSION":"",e.transmissionMap?"#define USE_TRANSMISSIONMAP":"",e.thicknessMap?"#define USE_THICKNESSMAP":"",e.sheenColorMap?"#define USE_SHEEN_COLORMAP":"",e.sheenRoughnessMap?"#define USE_SHEEN_ROUGHNESSMAP":"",e.mapUv?"#define MAP_UV "+e.mapUv:"",e.alphaMapUv?"#define ALPHAMAP_UV "+e.alphaMapUv:"",e.lightMapUv?"#define LIGHTMAP_UV "+e.lightMapUv:"",e.aoMapUv?"#define AOMAP_UV "+e.aoMapUv:"",e.emissiveMapUv?"#define EMISSIVEMAP_UV "+e.emissiveMapUv:"",e.bumpMapUv?"#define BUMPMAP_UV "+e.bumpMapUv:"",e.normalMapUv?"#define NORMALMAP_UV "+e.normalMapUv:"",e.displacementMapUv?"#define DISPLACEMENTMAP_UV "+e.displacementMapUv:"",e.metalnessMapUv?"#define METALNESSMAP_UV "+e.metalnessMapUv:"",e.roughnessMapUv?"#define ROUGHNESSMAP_UV "+e.roughnessMapUv:"",e.anisotropyMapUv?"#define ANISOTROPYMAP_UV "+e.anisotropyMapUv:"",e.clearcoatMapUv?"#define CLEARCOATMAP_UV "+e.clearcoatMapUv:"",e.clearcoatNormalMapUv?"#define CLEARCOAT_NORMALMAP_UV "+e.clearcoatNormalMapUv:"",e.clearcoatRoughnessMapUv?"#define CLEARCOAT_ROUGHNESSMAP_UV "+e.clearcoatRoughnessMapUv:"",e.diffuseRoughnessMapUv?"#define DIFFUSE_ROUGHNESSMAP_UV "+e.diffuseRoughnessMapUv:"",e.iridescenceMapUv?"#define IRIDESCENCEMAP_UV "+e.iridescenceMapUv:"",e.iridescenceThicknessMapUv?"#define IRIDESCENCE_THICKNESSMAP_UV "+e.iridescenceThicknessMapUv:"",e.sheenColorMapUv?"#define SHEEN_COLORMAP_UV "+e.sheenColorMapUv:"",e.sheenRoughnessMapUv?"#define SHEEN_ROUGHNESSMAP_UV "+e.sheenRoughnessMapUv:"",e.specularMapUv?"#define SPECULARMAP_UV "+e.specularMapUv:"",e.specularColorMapUv?"#define SPECULAR_COLORMAP_UV "+e.specularColorMapUv:"",e.specularIntensityMapUv?"#define SPECULAR_INTENSITYMAP_UV "+e.specularIntensityMapUv:"",e.transmissionMapUv?"#define TRANSMISSIONMAP_UV "+e.transmissionMapUv:"",e.thicknessMapUv?"#define THICKNESSMAP_UV "+e.thicknessMapUv:"",e.vertexTangents&&e.flatShading===!1?"#define USE_TANGENT":"",e.vertexNormals?"#define HAS_NORMAL":"",e.vertexColors?"#define USE_COLOR":"",e.vertexAlphas?"#define USE_COLOR_ALPHA":"",e.vertexUv1s?"#define USE_UV1":"",e.vertexUv2s?"#define USE_UV2":"",e.vertexUv3s?"#define USE_UV3":"",e.pointsUvs?"#define USE_POINTS_UV":"",e.flatShading?"#define FLAT_SHADED":"",e.skinning?"#define USE_SKINNING":"",e.morphTargets?"#define USE_MORPHTARGETS":"",e.morphNormals&&e.flatShading===!1?"#define USE_MORPHNORMALS":"",e.morphColors?"#define USE_MORPHCOLORS":"",e.morphTargetsCount>0?"#define MORPHTARGETS_TEXTURE_STRIDE "+e.morphTextureStride:"",e.morphTargetsCount>0?"#define MORPHTARGETS_COUNT "+e.morphTargetsCount:"",e.doubleSided?"#define DOUBLE_SIDED":"",e.flipSided?"#define FLIP_SIDED":"",e.shadowMapEnabled?"#define USE_SHADOWMAP":"",e.shadowMapEnabled?"#define "+l:"",e.sizeAttenuation?"#define USE_SIZEATTENUATION":"",e.numLightProbes>0?"#define USE_LIGHT_PROBES":"",e.logarithmicDepthBuffer?"#define USE_LOGARITHMIC_DEPTH_BUFFER":"",e.reversedDepthBuffer?"#define USE_REVERSED_DEPTH_BUFFER":"","uniform mat4 modelMatrix;","uniform mat4 modelViewMatrix;","uniform mat4 projectionMatrix;","uniform mat4 viewMatrix;","uniform mat3 normalMatrix;","uniform vec3 cameraPosition;","uniform bool isOrthographic;","#ifdef USE_INSTANCING","	attribute mat4 instanceMatrix;","#endif","#ifdef USE_INSTANCING_COLOR","	attribute vec3 instanceColor;","#endif","#ifdef USE_INSTANCING_MORPH","	uniform sampler2D morphTexture;","#endif","attribute vec3 position;","attribute vec3 normal;","attribute vec2 uv;","#ifdef USE_UV1","	attribute vec2 uv1;","#endif","#ifdef USE_UV2","	attribute vec2 uv2;","#endif","#ifdef USE_UV3","	attribute vec2 uv3;","#endif","#ifdef USE_TANGENT","	attribute vec4 tangent;","#endif","#if defined( USE_COLOR_ALPHA )","	attribute vec4 color;","#elif defined( USE_COLOR )","	attribute vec3 color;","#endif","#ifdef USE_SKINNING","	attribute vec4 skinIndex;","	attribute vec4 skinWeight;","#endif",`
`].filter(po).join(`
`),m=[hf(e),"#define SHADER_TYPE "+e.shaderType,"#define SHADER_NAME "+e.shaderName,f,e.useFog&&e.fog?"#define USE_FOG":"",e.useFog&&e.fogExp2?"#define FOG_EXP2":"",e.alphaToCoverage?"#define ALPHA_TO_COVERAGE":"",e.map?"#define USE_MAP":"",e.matcap?"#define USE_MATCAP":"",e.envMap?"#define USE_ENVMAP":"",e.envMap?"#define "+c:"",e.envMap?"#define "+h:"",e.envMap?"#define "+u:"",e.envMapPMREM?"#define ENVMAP_MAX_LOD "+e.envMapMaxLod+".0":"",e.envMapPMREM?"#define ENVMAP_SIZE "+e.envMapSize+".0":"",e.lightMap?"#define USE_LIGHTMAP":"",e.aoMap?"#define USE_AOMAP":"",e.bumpMap?"#define USE_BUMPMAP":"",e.normalMap?"#define USE_NORMALMAP":"",e.normalMapObjectSpace?"#define USE_NORMALMAP_OBJECTSPACE":"",e.normalMapTangentSpace?"#define USE_NORMALMAP_TANGENTSPACE":"",e.packedNormalMap?"#define USE_PACKED_NORMALMAP":"",e.emissiveMap?"#define USE_EMISSIVEMAP":"",e.anisotropy?"#define USE_ANISOTROPY":"",e.anisotropyMap?"#define USE_ANISOTROPYMAP":"",e.clearcoat?"#define USE_CLEARCOAT":"",e.clearcoatMap?"#define USE_CLEARCOATMAP":"",e.clearcoatRoughnessMap?"#define USE_CLEARCOAT_ROUGHNESSMAP":"",e.clearcoatNormalMap?"#define USE_CLEARCOAT_NORMALMAP":"",e.diffuseRoughness?"#define USE_DIFFUSE_ROUGHNESS":"",e.diffuseRoughnessMap?"#define USE_DIFFUSE_ROUGHNESSMAP":"",e.dispersion?"#define USE_DISPERSION":"",e.retroreflection?"#define USE_RETROREFLECTION":"",e.iridescence?"#define USE_IRIDESCENCE":"",e.iridescenceMap?"#define USE_IRIDESCENCEMAP":"",e.iridescenceThicknessMap?"#define USE_IRIDESCENCE_THICKNESSMAP":"",e.specularMap?"#define USE_SPECULARMAP":"",e.specularColorMap?"#define USE_SPECULAR_COLORMAP":"",e.specularIntensityMap?"#define USE_SPECULAR_INTENSITYMAP":"",e.roughnessMap?"#define USE_ROUGHNESSMAP":"",e.metalnessMap?"#define USE_METALNESSMAP":"",e.alphaMap?"#define USE_ALPHAMAP":"",e.alphaTest?"#define USE_ALPHATEST":"",e.alphaHash?"#define USE_ALPHAHASH":"",e.sheen?"#define USE_SHEEN":"",e.sheenColorMap?"#define USE_SHEEN_COLORMAP":"",e.sheenRoughnessMap?"#define USE_SHEEN_ROUGHNESSMAP":"",e.transmission?"#define USE_TRANSMISSION":"",e.transmissionMap?"#define USE_TRANSMISSIONMAP":"",e.thicknessMap?"#define USE_THICKNESSMAP":"",e.vertexTangents&&e.flatShading===!1?"#define USE_TANGENT":"",e.vertexColors||e.instancingColor?"#define USE_COLOR":"",e.vertexAlphas||e.batchingColor?"#define USE_COLOR_ALPHA":"",e.vertexUv1s?"#define USE_UV1":"",e.vertexUv2s?"#define USE_UV2":"",e.vertexUv3s?"#define USE_UV3":"",e.pointsUvs?"#define USE_POINTS_UV":"",e.gradientMap?"#define USE_GRADIENTMAP":"",e.flatShading?"#define FLAT_SHADED":"",e.doubleSided?"#define DOUBLE_SIDED":"",e.flipSided?"#define FLIP_SIDED":"",e.shadowMapEnabled?"#define USE_SHADOWMAP":"",e.shadowMapEnabled?"#define "+l:"",e.premultipliedAlpha?"#define PREMULTIPLIED_ALPHA":"",e.numLightProbes>0?"#define USE_LIGHT_PROBES":"",e.decodeVideoTexture?"#define DECODE_VIDEO_TEXTURE":"",e.decodeVideoTextureEmissive?"#define DECODE_VIDEO_TEXTURE_EMISSIVE":"",e.logarithmicDepthBuffer?"#define USE_LOGARITHMIC_DEPTH_BUFFER":"",e.reversedDepthBuffer?"#define USE_REVERSED_DEPTH_BUFFER":"","uniform mat4 viewMatrix;","uniform vec3 cameraPosition;","uniform bool isOrthographic;",e.toneMapping!==qi?"#define TONE_MAPPING":"",e.toneMapping!==qi?Ht.tonemapping_pars_fragment:"",e.toneMapping!==qi?k_("toneMapping",e.toneMapping):"",e.dithering?"#define DITHERING":"",e.opaque?"#define OPAQUE":"",Ht.colorspace_pars_fragment,B_("linearToOutputTexel",e.outputColorSpace),H_(),e.useDepthPacking?"#define DEPTH_PACKING "+e.depthPacking:"",`
`].filter(po).join(`
`)),o=Lh(o),o=af(o,e),o=lf(o,e),a=Lh(a),a=af(a,e),a=lf(a,e),o=cf(o),a=cf(a),e.isRawShaderMaterial!==!0&&(g=`#version 300 es
`,x=[d,"#define attribute in","#define varying out","#define texture2D texture"].join(`
`)+`
`+x,m=["#define varying in",e.glslVersion===gn?"":"layout(location = 0) out highp vec4 pc_fragColor;",e.glslVersion===gn?"":"#define gl_FragColor pc_fragColor","#define gl_FragDepthEXT gl_FragDepth","#define texture2D texture","#define textureCube texture","#define texture2DProj textureProj","#define texture2DLodEXT textureLod","#define texture2DProjLodEXT textureProjLod","#define textureCubeLodEXT textureLod","#define texture2DGradEXT textureGrad","#define texture2DProjGradEXT textureProjGrad","#define textureCubeGradEXT textureGrad"].join(`
`)+`
`+m);let _=g+x+o,y=g+m+a,S=sf(s,s.VERTEX_SHADER,_),T=sf(s,s.FRAGMENT_SHADER,y);s.attachShader(p,S),s.attachShader(p,T),e.index0AttributeName!==void 0?s.bindAttribLocation(p,0,e.index0AttributeName):e.hasPositionAttribute===!0&&s.bindAttribLocation(p,0,"position"),s.linkProgram(p);function w(P){if(n.debug.checkShaderErrors){let A=s.getProgramInfoLog(p)||"",C=s.getShaderInfoLog(S)||"",U=s.getShaderInfoLog(T)||"",L=A.trim(),B=C.trim(),G=U.trim(),z=!0,tt=!0;if(s.getProgramParameter(p,s.LINK_STATUS)===!1)if(z=!1,typeof n.debug.onShaderError=="function")n.debug.onShaderError(s,p,S,T);else{let W=of(s,S,"vertex"),J=of(s,T,"fragment");Ut("WebGLProgram: Shader Error "+s.getError()+" - VALIDATE_STATUS "+s.getProgramParameter(p,s.VALIDATE_STATUS)+`

Material Name: `+P.name+`
Material Type: `+P.type+`

Program Info Log: `+L+`
`+W+`
`+J)}else L!==""?It("WebGLProgram: Program Info Log:",L):(B===""||G==="")&&(tt=!1);tt&&(P.diagnostics={runnable:z,programLog:L,vertexShader:{log:B,prefix:x},fragmentShader:{log:G,prefix:m}})}s.deleteShader(S),s.deleteShader(T),v=new sr(s,p),M=W_(s,p)}let v;this.getUniforms=function(){return v===void 0&&w(this),v};let M;this.getAttributes=function(){return M===void 0&&w(this),M};let D=e.rendererExtensionParallelShaderCompile===!1;return this.isReady=function(){return D===!1&&(D=s.getProgramParameter(p,N_)),D},this.destroy=function(){i.releaseStatesOfProgram(this),s.deleteProgram(p),this.program=void 0},this.type=e.shaderType,this.name=e.shaderName,this.id=U_++,this.cacheKey=t,this.usedTimes=1,this.program=p,this.vertexShader=S,this.fragmentShader=T,this}var i1=0,Nh=class{constructor(){this.shaderCache=new Map,this.materialCache=new Map}update(t,e,i){let s=this._getShaderCacheForMaterial(t);return s.has(e)===!1&&(s.add(e),e.usedTimes++),s.has(i)===!1&&(s.add(i),i.usedTimes++),this}remove(t){let e=this.materialCache.get(t);for(let i of e)i.usedTimes--,i.usedTimes===0&&this.shaderCache.delete(i.code);return this.materialCache.delete(t),this}getVertexShaderStage(t){return this._getShaderStage(t.vertexShader)}getFragmentShaderStage(t){return this._getShaderStage(t.fragmentShader)}dispose(){this.shaderCache.clear(),this.materialCache.clear()}_getShaderCacheForMaterial(t){let e=this.materialCache,i=e.get(t);return i===void 0&&(i=new Set,e.set(t,i)),i}_getShaderStage(t){let e=this.shaderCache,i=e.get(t);return i===void 0&&(i=new Uh(t),e.set(t,i)),i}},Uh=class{constructor(t){this.id=i1++,this.code=t,this.usedTimes=0}};function n1(n){return n===Bn||n===co||n===ho}function s1(n,t,e,i,s,r){let o=new zs,a=new Nh,l=new Set,c=[],h=new Map,u=i.logarithmicDepthBuffer,d={MeshDepthMaterial:"depth",MeshDistanceMaterial:"distance",MeshNormalMaterial:"normal",MeshBasicMaterial:"basic",MeshLambertMaterial:"lambert",MeshPhongMaterial:"phong",MeshToonMaterial:"toon",MeshStandardMaterial:"physical",MeshPhysicalMaterial:"physical",MeshMatcapMaterial:"matcap",LineBasicMaterial:"basic",LineDashedMaterial:"dashed",PointsMaterial:"points",ShadowMaterial:"shadow",SpriteMaterial:"sprite"};function f(v){return l.add(v),v===0?"uv":`uv${v}`}function p(v,M,D,P,A,C){let U=P.fog,L=A.geometry,B=v.isMeshStandardMaterial||v.isMeshLambertMaterial||v.isMeshPhongMaterial?P.environment:null,G=v.isMeshStandardMaterial||v.isMeshLambertMaterial&&!v.envMap||v.isMeshPhongMaterial&&!v.envMap,z=t.get(v.envMap||B,G),tt=d[v.type],W=i.precision;v.precision!==null&&(W=i.getMaxPrecision(v.precision),W!==v.precision&&It("WebGLProgram.getParameters:",v.precision,"not supported, using",W,"instead."));let J=L.morphAttributes.position||L.morphAttributes.normal||L.morphAttributes.color,j=J!==void 0?J.length:0,xt=0;L.morphAttributes.position!==void 0&&(xt=1),L.morphAttributes.normal!==void 0&&(xt=2),L.morphAttributes.color!==void 0&&(xt=3);let Mt,q,ot,gt;if(tt){let _t=Ki[tt];Mt=_t.vertexShader,q=_t.fragmentShader}else{Mt=v.vertexShader,q=v.fragmentShader;let _t=a.getVertexShaderStage(v),se=a.getFragmentShaderStage(v);a.update(v,_t,se),ot=_t.id,gt=se.id}let wt=n.getRenderTarget(),rt=n.state.buffers.depth.getReversed(),Dt=A.isInstancedMesh===!0,ie=A.isBatchedMesh===!0,bt=!!v.map,Bt=!!v.matcap,Jt=!!z,Wt=Jt&&z.isPMREMTexture===!0,fe=!!v.aoMap,Se=!!v.lightMap,Ze=!!v.bumpMap&&v.wireframe===!1,pe=!!v.normalMap,fi=!!v.displacementMap,Ee=!!v.emissiveMap,He=!!v.metalnessMap,Mi=!!v.roughnessMap,Si=v.anisotropy>0,O=v.clearcoat>0,bi=v.diffuseRoughness>0,xe=v.dispersion>0,_e=v.retroreflectivity>0,mt=v.iridescence>0,Ie=v.sheen>0,Nt=v.transmission>0,Ot=Si&&!!v.anisotropyMap,I=O&&!!v.clearcoatMap,b=O&&!!v.clearcoatNormalMap,H=O&&!!v.clearcoatRoughnessMap,it=bi&&!!v.diffuseRoughnessMap,Q=mt&&!!v.iridescenceMap,at=mt&&!!v.iridescenceThicknessMap,dt=Ie&&!!v.sheenColorMap,Z=Ie&&!!v.sheenRoughnessMap,K=!!v.specularMap,ct=!!v.specularColorMap,N=!!v.specularIntensityMap,$=Nt&&!!v.transmissionMap,Y=Nt&&!!v.thicknessMap,ht=!!v.gradientMap,st=!!v.alphaMap,lt=v.alphaTest>0,Ct=!!v.alphaHash,Xt=!!v.extensions,nt=qi;v.toneMapped&&(wt===null||wt.isXRRenderTarget===!0)&&(nt=n.toneMapping);let Pt=Wt?z.isCompressedCubeTexture?z.image[0].mipmaps:z.mipmaps:null,Vt={shaderID:tt,shaderType:v.type,shaderName:v.name,vertexShader:Mt,fragmentShader:q,defines:v.defines,customVertexShaderID:ot,customFragmentShaderID:gt,isRawShaderMaterial:v.isRawShaderMaterial===!0,glslVersion:v.glslVersion,precision:W,batching:ie,batchingColor:ie&&A._colorsTexture!==null,instancing:Dt,instancingColor:Dt&&A.instanceColor!==null,instancingMorph:Dt&&A.morphTexture!==null,outputColorSpace:wt===null?n.outputColorSpace:wt.isXRRenderTarget===!0?wt.texture.colorSpace:qt.workingColorSpace,alphaToCoverage:!!v.alphaToCoverage,map:bt,matcap:Bt,envMap:Jt,envMapMode:Jt&&z.mapping,envMapPMREM:Wt,envMapMaxLod:Wt?Pt.length-1:null,envMapSize:Wt?Pt[0].width:null,aoMap:fe,lightMap:Se,bumpMap:Ze,normalMap:pe,displacementMap:fi,emissiveMap:Ee,normalMapObjectSpace:pe&&v.normalMapType===Pd,normalMapTangentSpace:pe&&v.normalMapType===uo,packedNormalMap:pe&&v.normalMapType===uo&&n1(v.normalMap.format),metalnessMap:He,roughnessMap:Mi,anisotropy:Si,anisotropyMap:Ot,clearcoat:O,clearcoatMap:I,clearcoatNormalMap:b,clearcoatRoughnessMap:H,diffuseRoughness:bi,diffuseRoughnessMap:it,dispersion:xe,retroreflection:_e,iridescence:mt,iridescenceMap:Q,iridescenceThicknessMap:at,sheen:Ie,sheenColorMap:dt,sheenRoughnessMap:Z,specularMap:K,specularColorMap:ct,specularIntensityMap:N,transmission:Nt,transmissionMap:$,thicknessMap:Y,gradientMap:ht,opaque:v.transparent===!1&&v.blending===$s&&v.alphaToCoverage===!1,alphaMap:st,alphaTest:lt,alphaHash:Ct,combine:v.combine,mapUv:bt&&f(v.map.channel),aoMapUv:fe&&f(v.aoMap.channel),lightMapUv:Se&&f(v.lightMap.channel),bumpMapUv:Ze&&f(v.bumpMap.channel),normalMapUv:pe&&f(v.normalMap.channel),displacementMapUv:fi&&f(v.displacementMap.channel),emissiveMapUv:Ee&&f(v.emissiveMap.channel),metalnessMapUv:He&&f(v.metalnessMap.channel),roughnessMapUv:Mi&&f(v.roughnessMap.channel),anisotropyMapUv:Ot&&f(v.anisotropyMap.channel),clearcoatMapUv:I&&f(v.clearcoatMap.channel),clearcoatNormalMapUv:b&&f(v.clearcoatNormalMap.channel),clearcoatRoughnessMapUv:H&&f(v.clearcoatRoughnessMap.channel),diffuseRoughnessMapUv:it&&f(v.diffuseRoughnessMap.channel),iridescenceMapUv:Q&&f(v.iridescenceMap.channel),iridescenceThicknessMapUv:at&&f(v.iridescenceThicknessMap.channel),sheenColorMapUv:dt&&f(v.sheenColorMap.channel),sheenRoughnessMapUv:Z&&f(v.sheenRoughnessMap.channel),specularMapUv:K&&f(v.specularMap.channel),specularColorMapUv:ct&&f(v.specularColorMap.channel),specularIntensityMapUv:N&&f(v.specularIntensityMap.channel),transmissionMapUv:$&&f(v.transmissionMap.channel),thicknessMapUv:Y&&f(v.thicknessMap.channel),alphaMapUv:st&&f(v.alphaMap.channel),vertexTangents:!!L.attributes.tangent&&(pe||Si),vertexNormals:!!L.attributes.normal,vertexColors:v.vertexColors,vertexAlphas:v.vertexColors===!0&&!!L.attributes.color&&L.attributes.color.itemSize===4,pointsUvs:A.isPoints===!0&&!!L.attributes.uv&&(bt||st),fog:!!U,useFog:v.fog===!0,fogExp2:!!U&&U.isFogExp2,flatShading:v.wireframe===!1&&(v.flatShading===!0||L.attributes.normal===void 0&&pe===!1&&(v.isMeshLambertMaterial||v.isMeshPhongMaterial||v.isMeshStandardMaterial||v.isMeshPhysicalMaterial)),sizeAttenuation:v.sizeAttenuation===!0,logarithmicDepthBuffer:u,reversedDepthBuffer:rt,skinning:A.isSkinnedMesh===!0,hasPositionAttribute:L.attributes.position!==void 0,morphTargets:L.morphAttributes.position!==void 0,morphNormals:L.morphAttributes.normal!==void 0,morphColors:L.morphAttributes.color!==void 0,morphTargetsCount:j,morphTextureStride:xt,numSunLights:M.sun.length,numDirLights:M.directional.length,numPointLights:M.point.length,numSpotLights:M.spot.length,numSpotLightMaps:M.spotLightMap.length,numRectAreaLights:M.rectArea.length,numHemiLights:M.hemi.length,numSunLightShadows:M.sunShadowMap.length,numDirLightShadows:M.directionalShadowMap.length,numPointLightShadows:M.pointShadowMap.length,numSpotLightShadows:M.spotShadowMap.length,numSpotLightShadowsWithMaps:M.numSpotLightShadowsWithMaps,numLightProbes:M.numLightProbes,numLightProbeGrids:C.filter(_t=>_t.texture!==null).length,numClippingPlanes:r.numPlanes,numClipIntersection:r.numIntersection,dithering:v.dithering,shadowMapEnabled:n.shadowMap.enabled&&D.length>0,shadowMapType:n.shadowMap.type,toneMapping:nt,decodeVideoTexture:bt&&v.map.isVideoTexture===!0&&qt.getTransfer(v.map.colorSpace)===Qt,decodeVideoTextureEmissive:Ee&&v.emissiveMap.isVideoTexture===!0&&qt.getTransfer(v.emissiveMap.colorSpace)===Qt,premultipliedAlpha:v.premultipliedAlpha,doubleSided:v.side===De,flipSided:v.side===qe,useDepthPacking:v.depthPacking>=0,depthPacking:v.depthPacking||0,index0AttributeName:v.index0AttributeName,extensionClipCullDistance:Xt&&v.extensions.clipCullDistance===!0&&e.has("WEBGL_clip_cull_distance"),extensionMultiDraw:(Xt&&v.extensions.multiDraw===!0||ie)&&e.has("WEBGL_multi_draw"),rendererExtensionParallelShaderCompile:e.has("KHR_parallel_shader_compile"),customProgramCacheKey:v.customProgramCacheKey()};return Vt.vertexUv1s=l.has(1),Vt.vertexUv2s=l.has(2),Vt.vertexUv3s=l.has(3),l.clear(),Vt}function x(v){let M=[];if(v.shaderID?M.push(v.shaderID):(M.push(v.customVertexShaderID),M.push(v.customFragmentShaderID)),v.defines!==void 0)for(let D in v.defines)M.push(D),M.push(v.defines[D]);return v.isRawShaderMaterial===!1&&(m(M,v),g(M,v),M.push(n.outputColorSpace)),M.push(v.customProgramCacheKey),M.join()}function m(v,M){v.push(M.precision),v.push(M.outputColorSpace),v.push(M.envMapMode),v.push(M.envMapPMREM),v.push(M.envMapMaxLod),v.push(M.envMapSize),v.push(M.mapUv),v.push(M.alphaMapUv),v.push(M.lightMapUv),v.push(M.aoMapUv),v.push(M.bumpMapUv),v.push(M.normalMapUv),v.push(M.displacementMapUv),v.push(M.emissiveMapUv),v.push(M.metalnessMapUv),v.push(M.roughnessMapUv),v.push(M.anisotropyMapUv),v.push(M.clearcoatMapUv),v.push(M.clearcoatNormalMapUv),v.push(M.clearcoatRoughnessMapUv),v.push(M.diffuseRoughnessMapUv),v.push(M.iridescenceMapUv),v.push(M.iridescenceThicknessMapUv),v.push(M.sheenColorMapUv),v.push(M.sheenRoughnessMapUv),v.push(M.specularMapUv),v.push(M.specularColorMapUv),v.push(M.specularIntensityMapUv),v.push(M.transmissionMapUv),v.push(M.thicknessMapUv),v.push(M.combine),v.push(M.fogExp2),v.push(M.sizeAttenuation),v.push(M.morphTargetsCount),v.push(M.numSunLights),v.push(M.numDirLights),v.push(M.numPointLights),v.push(M.numSpotLights),v.push(M.numSpotLightMaps),v.push(M.numHemiLights),v.push(M.numRectAreaLights),v.push(M.numSunLightShadows),v.push(M.numDirLightShadows),v.push(M.numPointLightShadows),v.push(M.numSpotLightShadows),v.push(M.numSpotLightShadowsWithMaps),v.push(M.numLightProbes),v.push(M.numLightProbeGrids),v.push(M.shadowMapType),v.push(M.toneMapping),v.push(M.numClippingPlanes),v.push(M.numClipIntersection),v.push(M.depthPacking)}function g(v,M){o.disableAll(),M.instancing&&o.enable(0),M.instancingColor&&o.enable(1),M.instancingMorph&&o.enable(2),M.matcap&&o.enable(3),M.envMap&&o.enable(4),M.normalMapObjectSpace&&o.enable(5),M.normalMapTangentSpace&&o.enable(6),M.clearcoat&&o.enable(7),M.iridescence&&o.enable(8),M.alphaTest&&o.enable(9),M.vertexColors&&o.enable(10),M.vertexAlphas&&o.enable(11),M.vertexUv1s&&o.enable(12),M.vertexUv2s&&o.enable(13),M.vertexUv3s&&o.enable(14),M.vertexTangents&&o.enable(15),M.anisotropy&&o.enable(16),M.alphaHash&&o.enable(17),M.batching&&o.enable(18),M.dispersion&&o.enable(19),M.retroreflection&&o.enable(24),M.diffuseRoughness&&o.enable(25),M.batchingColor&&o.enable(20),M.gradientMap&&o.enable(21),M.packedNormalMap&&o.enable(22),M.vertexNormals&&o.enable(23),v.push(o.mask),o.disableAll(),M.fog&&o.enable(0),M.useFog&&o.enable(1),M.flatShading&&o.enable(2),M.logarithmicDepthBuffer&&o.enable(3),M.reversedDepthBuffer&&o.enable(4),M.skinning&&o.enable(5),M.morphTargets&&o.enable(6),M.morphNormals&&o.enable(7),M.morphColors&&o.enable(8),M.premultipliedAlpha&&o.enable(9),M.shadowMapEnabled&&o.enable(10),M.doubleSided&&o.enable(11),M.flipSided&&o.enable(12),M.useDepthPacking&&o.enable(13),M.dithering&&o.enable(14),M.transmission&&o.enable(15),M.sheen&&o.enable(16),M.opaque&&o.enable(17),M.pointsUvs&&o.enable(18),M.decodeVideoTexture&&o.enable(19),M.decodeVideoTextureEmissive&&o.enable(20),M.alphaToCoverage&&o.enable(21),M.numLightProbeGrids>0&&o.enable(22),M.hasPositionAttribute&&o.enable(23),v.push(o.mask)}function _(v){let M=d[v.type],D;if(M){let P=Ki[M];D=ki.clone(P.uniforms)}else D=v.uniforms;return D}function y(v,M){let D=h.get(M);return D!==void 0?++D.usedTimes:(D=new e1(n,M,v,s),c.push(D),h.set(M,D)),D}function S(v){if(--v.usedTimes===0){let M=c.indexOf(v);c[M]=c[c.length-1],c.pop(),h.delete(v.cacheKey),v.destroy()}}function T(v){a.remove(v)}function w(){a.dispose()}return{getParameters:p,getProgramCacheKey:x,getUniforms:_,acquireProgram:y,releaseProgram:S,releaseShaderCache:T,programs:c,dispose:w}}function r1(){let n=new WeakMap;function t(o){return n.has(o)}function e(o){let a=n.get(o);return a===void 0&&(a={},n.set(o,a)),a}function i(o){n.delete(o)}function s(o,a,l){n.get(o)[a]=l}function r(){n=new WeakMap}return{has:t,get:e,remove:i,update:s,dispose:r}}function o1(n,t){return n.groupOrder!==t.groupOrder?n.groupOrder-t.groupOrder:n.renderOrder!==t.renderOrder?n.renderOrder-t.renderOrder:n.material.id!==t.material.id?n.material.id-t.material.id:n.materialVariant!==t.materialVariant?n.materialVariant-t.materialVariant:n.z!==t.z?n.z-t.z:n.id-t.id}function uf(n,t){return n.groupOrder!==t.groupOrder?n.groupOrder-t.groupOrder:n.renderOrder!==t.renderOrder?n.renderOrder-t.renderOrder:n.z!==t.z?t.z-n.z:n.id-t.id}function df(){let n=[],t=0,e=[],i=[],s=[];function r(){t=0,e.length=0,i.length=0,s.length=0}function o(d){let f=0;return d.isInstancedMesh&&(f+=2),d.isSkinnedMesh&&(f+=1),f}function a(d,f,p,x,m,g){let _=n[t];return _===void 0?(_={id:d.id,object:d,geometry:f,material:p,materialVariant:o(d),groupOrder:x,renderOrder:d.renderOrder,z:m,group:g},n[t]=_):(_.id=d.id,_.object=d,_.geometry=f,_.material=p,_.materialVariant=o(d),_.groupOrder=x,_.renderOrder=d.renderOrder,_.z=m,_.group=g),t++,_}function l(d,f,p,x,m,g,_){_.reversedDepth===!0&&(m=-m);let y=a(d,f,p,x,m,g);p.transmission>0?i.push(y):p.transparent===!0?s.push(y):e.push(y)}function c(d,f,p,x,m,g){let _=a(d,f,p,x,m,g);p.transmission>0?i.unshift(_):p.transparent===!0?s.unshift(_):e.unshift(_)}function h(d,f){e.length>1&&e.sort(d||o1),i.length>1&&i.sort(f||uf),s.length>1&&s.sort(f||uf)}function u(){for(let d=t,f=n.length;d<f;d++){let p=n[d];if(p.id===null)break;p.id=null,p.object=null,p.geometry=null,p.material=null,p.group=null}}return{opaque:e,transmissive:i,transparent:s,init:r,push:l,unshift:c,finish:u,sort:h}}function a1(){let n=new WeakMap;function t(i,s){let r=n.get(i),o;return r===void 0?(o=new df,n.set(i,[o])):s>=r.length?(o=new df,r.push(o)):o=r[s],o}function e(){n=new WeakMap}return{get:t,dispose:e}}function l1(){let n={};return{get:function(t){if(n[t.id]!==void 0)return n[t.id];let e;switch(t.type){case"SunLight":case"DirectionalLight":e={direction:new R,color:new St};break;case"SpotLight":e={position:new R,direction:new R,color:new St,distance:0,coneCos:0,penumbraCos:0,decay:0};break;case"PointLight":e={position:new R,color:new St,distance:0,decay:0};break;case"HemisphereLight":e={direction:new R,skyColor:new St,groundColor:new St};break;case"RectAreaLight":e={color:new St,position:new R,halfWidth:new R,halfHeight:new R};break}return n[t.id]=e,e}}}function c1(){let n={};return{get:function(t){if(n[t.id]!==void 0)return n[t.id];let e;switch(t.type){case"SunLight":case"DirectionalLight":e={shadowIntensity:1,shadowBias:0,shadowNormalBias:0,shadowRadius:1,shadowMapSize:new et};break;case"SpotLight":e={shadowIntensity:1,shadowBias:0,shadowNormalBias:0,shadowRadius:1,shadowMapSize:new et};break;case"PointLight":e={shadowIntensity:1,shadowBias:0,shadowNormalBias:0,shadowRadius:1,shadowMapSize:new et,shadowCameraNear:1,shadowCameraFar:1e3};break}return n[t.id]=e,e}}}var h1=0;function u1(n,t){return(t.castShadow?2:0)-(n.castShadow?2:0)+(t.map?1:0)-(n.map?1:0)}function d1(n){let t=new l1,e=c1(),i={version:0,hash:{sunLength:-1,directionalLength:-1,pointLength:-1,spotLength:-1,rectAreaLength:-1,hemiLength:-1,numSunShadows:-1,numDirectionalShadows:-1,numPointShadows:-1,numSpotShadows:-1,numSpotMaps:-1,numLightProbes:-1},ambient:[0,0,0],probe:[],sun:[],sunShadow:[],sunShadowMap:[],sunShadowMatrix:[],sunShadowCascade:[],directional:[],directionalShadow:[],directionalShadowMap:[],directionalShadowMatrix:[],spot:[],spotLightMap:[],spotShadow:[],spotShadowMap:[],spotLightMatrix:[],rectArea:[],rectAreaLTC1:null,rectAreaLTC2:null,point:[],pointShadow:[],pointShadowMap:[],pointShadowMatrix:[],hemi:[],numSpotLightShadowsWithMaps:0,numLightProbes:0};for(let c=0;c<9;c++)i.probe.push(new R);let s=new R,r=new Gt,o=new Gt;function a(c){let h=0,u=0,d=0;for(let C=0;C<9;C++)i.probe[C].set(0,0,0);let f=0,p=0,x=0,m=0,g=0,_=0,y=0,S=0,T=0,w=0,v=0,M=0,D=0,P=0;c.sort(u1);for(let C=0,U=c.length;C<U;C++){let L=c[C],B=L.color,G=L.intensity,z=L.distance,tt=null;if(L.shadow&&L.shadow.map&&(L.shadow.map.texture.format===Bn?tt=L.shadow.map.texture:tt=L.shadow.map.depthTexture||L.shadow.map.texture),L.isAmbientLight)h+=B.r*G,u+=B.g*G,d+=B.b*G;else if(L.isLightProbe){for(let W=0;W<9;W++)i.probe[W].addScaledVector(L.sh.coefficients[W],G);P++}else if(L.isSunLight){let W=t.get(L);if(W.color.copy(L.color).multiplyScalar(L.intensity),L.castShadow){let J=L.shadow,j=e.get(L);j.shadowIntensity=J.intensity,j.shadowBias=J.bias,j.shadowNormalBias=J.normalBias,j.shadowRadius=J.radius,j.shadowMapSize.copy(J.mapSize).multiply(J.getFrameExtents()),i.sunShadow[p]=j,i.sunShadowMap[p]=tt;let xt=J.getViewportCount();for(let Mt=0;Mt<xt;Mt++)i.sunShadowMatrix[x+Mt]=J.getMatrix(Mt),i.sunShadowCascade[x+Mt]=J._cascadeData[Mt];x+=xt,p++}i.sun[f]=W,f++}else if(L.isDirectionalLight){let W=t.get(L);if(W.color.copy(L.color).multiplyScalar(L.intensity),L.castShadow){let J=L.shadow,j=e.get(L);j.shadowIntensity=J.intensity,j.shadowBias=J.bias,j.shadowNormalBias=J.normalBias,j.shadowRadius=J.radius,j.shadowMapSize=J.mapSize,i.directionalShadow[m]=j,i.directionalShadowMap[m]=tt,i.directionalShadowMatrix[m]=L.shadow.matrix,T++}i.directional[m]=W,m++}else if(L.isSpotLight){let W=t.get(L);W.position.setFromMatrixPosition(L.matrixWorld),W.color.copy(B).multiplyScalar(G),W.distance=z,W.coneCos=Math.cos(L.angle),W.penumbraCos=Math.cos(L.angle*(1-L.penumbra)),W.decay=L.decay,i.spot[_]=W;let J=L.shadow;if(L.map&&(i.spotLightMap[M]=L.map,M++,J.updateMatrices(L),L.castShadow&&D++),i.spotLightMatrix[_]=J.matrix,L.castShadow){let j=e.get(L);j.shadowIntensity=J.intensity,j.shadowBias=J.bias,j.shadowNormalBias=J.normalBias,j.shadowRadius=J.radius,j.shadowMapSize=J.mapSize,i.spotShadow[_]=j,i.spotShadowMap[_]=tt,v++}_++}else if(L.isRectAreaLight){let W=t.get(L);W.color.copy(B).multiplyScalar(G),W.halfWidth.set(L.width*.5,0,0),W.halfHeight.set(0,L.height*.5,0),i.rectArea[y]=W,y++}else if(L.isPointLight){let W=t.get(L);if(W.color.copy(L.color).multiplyScalar(L.intensity),W.distance=L.distance,W.decay=L.decay,L.castShadow){let J=L.shadow,j=e.get(L);j.shadowIntensity=J.intensity,j.shadowBias=J.bias,j.shadowNormalBias=J.normalBias,j.shadowRadius=J.radius,j.shadowMapSize=J.mapSize,j.shadowCameraNear=J.camera.near,j.shadowCameraFar=J.camera.far,i.pointShadow[g]=j,i.pointShadowMap[g]=tt,i.pointShadowMatrix[g]=L.shadow.matrix,w++}i.point[g]=W,g++}else if(L.isHemisphereLight){let W=t.get(L);W.skyColor.copy(L.color).multiplyScalar(G),W.groundColor.copy(L.groundColor).multiplyScalar(G),i.hemi[S]=W,S++}}y>0&&(n.has("OES_texture_float_linear")===!0?(i.rectAreaLTC1=vt.LTC_FLOAT_1,i.rectAreaLTC2=vt.LTC_FLOAT_2):(i.rectAreaLTC1=vt.LTC_HALF_1,i.rectAreaLTC2=vt.LTC_HALF_2)),i.ambient[0]=h,i.ambient[1]=u,i.ambient[2]=d;let A=i.hash;(A.sunLength!==f||A.directionalLength!==m||A.pointLength!==g||A.spotLength!==_||A.rectAreaLength!==y||A.hemiLength!==S||A.numSunShadows!==p||A.numDirectionalShadows!==T||A.numPointShadows!==w||A.numSpotShadows!==v||A.numSpotMaps!==M||A.numLightProbes!==P)&&(i.sun.length=f,i.directional.length=m,i.spot.length=_,i.rectArea.length=y,i.point.length=g,i.hemi.length=S,i.sunShadow.length=p,i.sunShadowMap.length=p,i.sunShadowMatrix.length=x,i.sunShadowCascade.length=x,i.directionalShadow.length=T,i.directionalShadowMap.length=T,i.directionalShadowMatrix.length=T,i.pointShadow.length=w,i.pointShadowMap.length=w,i.pointShadowMatrix.length=w,i.spotShadow.length=v,i.spotShadowMap.length=v,i.spotLightMatrix.length=v+M-D,i.spotLightMap.length=M,i.numSpotLightShadowsWithMaps=D,i.numLightProbes=P,A.sunLength=f,A.directionalLength=m,A.pointLength=g,A.spotLength=_,A.rectAreaLength=y,A.hemiLength=S,A.numSunShadows=p,A.numDirectionalShadows=T,A.numPointShadows=w,A.numSpotShadows=v,A.numSpotMaps=M,A.numLightProbes=P,i.version=h1++)}function l(c,h){let u=0,d=0,f=0,p=0,x=0,m=0,g=h.matrixWorldInverse;for(let _=0,y=c.length;_<y;_++){let S=c[_];if(S.isSunLight){let T=i.sun[u];T.direction.setFromMatrixPosition(S.matrixWorld),T.direction.transformDirection(g),u++}else if(S.isDirectionalLight){let T=i.directional[d];T.direction.setFromMatrixPosition(S.matrixWorld),s.setFromMatrixPosition(S.target.matrixWorld),T.direction.sub(s),T.direction.transformDirection(g),d++}else if(S.isSpotLight){let T=i.spot[p];T.position.setFromMatrixPosition(S.matrixWorld),T.position.applyMatrix4(g),T.direction.setFromMatrixPosition(S.matrixWorld),s.setFromMatrixPosition(S.target.matrixWorld),T.direction.sub(s),T.direction.transformDirection(g),p++}else if(S.isRectAreaLight){let T=i.rectArea[x];T.position.setFromMatrixPosition(S.matrixWorld),T.position.applyMatrix4(g),o.identity(),r.copy(S.matrixWorld),r.premultiply(g),o.extractRotation(r),T.halfWidth.set(S.width*.5,0,0),T.halfHeight.set(0,S.height*.5,0),T.halfWidth.applyMatrix4(o),T.halfHeight.applyMatrix4(o),x++}else if(S.isPointLight){let T=i.point[f];T.position.setFromMatrixPosition(S.matrixWorld),T.position.applyMatrix4(g),f++}else if(S.isHemisphereLight){let T=i.hemi[m];T.direction.setFromMatrixPosition(S.matrixWorld),T.direction.transformDirection(g),m++}}}return{setup:a,setupView:l,state:i}}function ff(n){let t=new d1(n),e=[],i=[],s=[];function r(u){h.camera=u,e.length=0,i.length=0,s.length=0}function o(u){u.isLightProbeGrid?s.push(u):e.push(u)}function a(u){i.push(u)}function l(){t.setup(e),s.sort((u,d)=>u.priority-d.priority||u.id-d.id)}function c(u){t.setupView(e,u)}let h={lightsArray:e,shadowsArray:i,lightProbeGridArray:s,camera:null,lights:t,transmissionRenderTarget:{},textureUnits:0};return{init:r,state:h,setupLights:l,setupLightsView:c,pushLight:o,pushShadow:a}}function f1(n){let t=new WeakMap;function e(s,r=0){let o=t.get(s),a;return o===void 0?(a=new ff(n),t.set(s,[a])):r>=o.length?(a=new ff(n),o.push(a)):a=o[r],a}function i(){t=new WeakMap}return{get:e,dispose:i}}var p1=`void main() {
	gl_Position = vec4( position, 1.0 );
}`,m1=`uniform sampler2D shadow_pass;
uniform vec2 resolution;
uniform float radius;
void main() {
	const float samples = float( VSM_SAMPLES );
	float mean = 0.0;
	float squared_mean = 0.0;
	float uvStride = samples <= 1.0 ? 0.0 : 2.0 / ( samples - 1.0 );
	float uvStart = samples <= 1.0 ? 0.0 : - 1.0;
	for ( float i = 0.0; i < samples; i ++ ) {
		float uvOffset = uvStart + i * uvStride;
		#ifdef HORIZONTAL_PASS
			vec2 distribution = texture2D( shadow_pass, ( gl_FragCoord.xy + vec2( uvOffset, 0.0 ) * radius ) / resolution ).rg;
			mean += distribution.x;
			squared_mean += distribution.y * distribution.y + distribution.x * distribution.x;
		#else
			float depth = texture2D( shadow_pass, ( gl_FragCoord.xy + vec2( 0.0, uvOffset ) * radius ) / resolution ).r;
			mean += depth;
			squared_mean += depth * depth;
		#endif
	}
	mean = mean / samples;
	squared_mean = squared_mean / samples;
	float std_dev = sqrt( max( 0.0, squared_mean - mean * mean ) );
	gl_FragColor = vec4( mean, std_dev, 0.0, 1.0 );
}`,g1=[new R(1,0,0),new R(-1,0,0),new R(0,1,0),new R(0,-1,0),new R(0,0,1),new R(0,0,-1)],x1=[new R(0,-1,0),new R(0,-1,0),new R(0,0,1),new R(0,0,-1),new R(0,-1,0),new R(0,-1,0)],pf=new Gt,fo=new R,Ch=new R;function _1(n,t,e){let i=new Pn,s=new et,r=new et,o=new ne,a=new Da,l=new La,c={},h=e.maxTextureSize,u={[Fn]:qe,[qe]:Fn,[De]:De},d=new ae({defines:{VSM_SAMPLES:8},uniforms:{shadow_pass:{value:null},resolution:{value:new et},radius:{value:4}},vertexShader:p1,fragmentShader:m1}),f=d.clone();f.defines.HORIZONTAL_PASS=1;let p=new Yt;p.setAttribute("position",new re(new Float32Array([-1,-1,.5,3,-1,.5,-1,3,.5]),3));let x=new Lt(p,d),m=this;this.enabled=!1,this.autoUpdate=!0,this.needsUpdate=!1,this.type=os;let g=this.type;this.render=function(w,v,M){if(m.enabled===!1||m.autoUpdate===!1&&m.needsUpdate===!1||w.length===0)return;this.type===hd&&(It("WebGLShadowMap: PCFSoftShadowMap has been removed. Using PCFShadowMap instead."),this.type=os);let D=n.getRenderTarget(),P=n.getActiveCubeFace(),A=n.getActiveMipmapLevel(),C=n.state;C.setBlending(yi),C.buffers.depth.getReversed()===!0?C.buffers.color.setClear(0,0,0,0):C.buffers.color.setClear(1,1,1,1),C.buffers.depth.setTest(!0),C.setScissorTest(!1);let U=g!==this.type;U&&v.traverse(function(L){L.material&&(Array.isArray(L.material)?L.material.forEach(B=>B.needsUpdate=!0):L.material.needsUpdate=!0)});for(let L=0,B=w.length;L<B;L++){let G=w[L],z=G.shadow;if(z===void 0){It("WebGLShadowMap:",G,"has no shadow.");continue}if(z.autoUpdate===!1&&z.needsUpdate===!1)continue;s.copy(z.mapSize);let tt=z.getFrameExtents();s.multiply(tt),r.copy(z.mapSize),(s.x>h||s.y>h)&&(s.x>h&&(r.x=Math.floor(h/tt.x),s.x=r.x*tt.x,z.mapSize.x=r.x),s.y>h&&(r.y=Math.floor(h/tt.y),s.y=r.y*tt.y,z.mapSize.y=r.y));let W=n.state.buffers.depth.getReversed();if(z.camera._reversedDepth=W,z.map===null||U===!0){if(z.map!==null&&(z.map.depthTexture!==null&&(z.map.depthTexture.dispose(),z.map.depthTexture=null),z.map.dispose()),this.type===Js){if(G.isPointLight){It("WebGLShadowMap: VSM shadow maps are not supported for PointLights. Use PCF or BasicShadowMap instead.");continue}z.map=new he(s.x,s.y,{format:Bn,type:de,minFilter:oe,magFilter:oe,generateMipmaps:!1}),z.map.texture.name=G.name+".shadowMap",z.map.depthTexture=new Fi(s.x,s.y,ui),z.map.depthTexture.name=G.name+".shadowMapDepth",z.map.depthTexture.format=Di,z.map.depthTexture.compareFunction=null,z.map.depthTexture.minFilter=Ce,z.map.depthTexture.magFilter=Ce}else G.isPointLight?(z.map=new mo(s.x),z.map.depthTexture=new Fr(s.x,oi)):(z.map=new he(s.x,s.y),z.map.depthTexture=new Fi(s.x,s.y,oi)),z.map.depthTexture.name=G.name+".shadowMap",z.map.depthTexture.format=Di,this.type===os?(z.map.depthTexture.compareFunction=W?xh:er,z.map.depthTexture.minFilter=oe,z.map.depthTexture.magFilter=oe):(z.map.depthTexture.compareFunction=null,z.map.depthTexture.minFilter=Ce,z.map.depthTexture.magFilter=Ce);z.camera.updateProjectionMatrix()}z.map.isWebGLCubeRenderTarget!==!0&&(z.map.width!==s.x||z.map.height!==s.y)&&z.map.setSize(s.x,s.y);let J=z.map.isWebGLCubeRenderTarget?6:z.getViewportCount();G.isPointLight!==!0&&z.updateMatrices(G,M);let j=(z.camera.layers.mask&4294967294)!==0?z.camera.layers:M.layers;for(let xt=0;xt<J;xt++){let Mt=z.getCamera(xt);if(G.isPointLight){let q=z.camera,ot=z.matrix,gt=G.distance||q.far;gt!==q.far&&(q.far=gt,q.updateProjectionMatrix()),fo.setFromMatrixPosition(G.matrixWorld),q.position.copy(fo),Ch.copy(q.position),Ch.add(g1[xt]),q.up.copy(x1[xt]),q.lookAt(Ch),q.updateMatrixWorld(),ot.makeTranslation(-fo.x,-fo.y,-fo.z),pf.multiplyMatrices(q.projectionMatrix,q.matrixWorldInverse),z._frustum.setFromProjectionMatrix(pf,q.coordinateSystem,q.reversedDepth)}if(z.map.isWebGLCubeRenderTarget)n.setRenderTarget(z.map,xt),n.clear();else{xt===0&&(n.setRenderTarget(z.map),n.clear());let q=z.getViewport(xt);o.set(r.x*q.x,r.y*q.y,r.x*q.z,r.y*q.w),C.viewport(o)}i=z.getFrustum(xt),S(v,M,Mt,G,j,this.type)}z.isPointLightShadow!==!0&&this.type===Js&&_(z,M),z.needsUpdate=!1}g=this.type,m.needsUpdate=!1,n.setRenderTarget(D,P,A)};function _(w,v){let M=t.update(x);d.defines.VSM_SAMPLES!==w.blurSamples&&(d.defines.VSM_SAMPLES=w.blurSamples,f.defines.VSM_SAMPLES=w.blurSamples,d.needsUpdate=!0,f.needsUpdate=!0),w.mapPass===null?w.mapPass=new he(s.x,s.y,{format:Bn,type:de}):(w.mapPass.width!==w.map.width||w.mapPass.height!==w.map.height)&&w.mapPass.setSize(w.map.width,w.map.height),d.uniforms.shadow_pass.value=w.map.depthTexture,d.uniforms.resolution.value.set(w.map.width,w.map.height),d.uniforms.radius.value=w.radius,n.setRenderTarget(w.mapPass),n.clear(),n.renderBufferDirect(v,null,M,d,x,null),f.uniforms.shadow_pass.value=w.mapPass.texture,f.uniforms.resolution.value.set(w.map.width,w.map.height),f.uniforms.radius.value=w.radius,n.setRenderTarget(w.map),n.clear(),n.renderBufferDirect(v,null,M,f,x,null)}function y(w,v,M,D){let P=null,A=M.isPointLight===!0?w.customDistanceMaterial:w.customDepthMaterial;if(A!==void 0)P=A;else if(P=M.isPointLight===!0?l:a,n.localClippingEnabled&&v.clipShadows===!0&&Array.isArray(v.clippingPlanes)&&v.clippingPlanes.length!==0||v.displacementMap&&v.displacementScale!==0||v.alphaMap&&v.alphaTest>0||v.map&&v.alphaTest>0||v.alphaToCoverage===!0){let C=P.uuid,U=v.uuid,L=c[C];L===void 0&&(L={},c[C]=L);let B=L[U];B===void 0&&(B=P.clone(),L[U]=B,v.addEventListener("dispose",T)),P=B}if(P.visible=v.visible,P.wireframe=v.wireframe,D===Js?P.side=v.shadowSide!==null?v.shadowSide:v.side:P.side=v.shadowSide!==null?v.shadowSide:u[v.side],P.alphaMap=v.alphaMap,P.alphaTest=v.alphaToCoverage===!0?.5:v.alphaTest,P.map=v.map,P.clipShadows=v.clipShadows,P.clippingPlanes=v.clippingPlanes,P.clipIntersection=v.clipIntersection,P.displacementMap=v.displacementMap,P.displacementScale=v.displacementScale,P.displacementBias=v.displacementBias,P.wireframeLinewidth=v.wireframeLinewidth,P.linewidth=v.linewidth,M.isPointLight===!0&&P.isMeshDistanceMaterial===!0){let C=n.properties.get(P);C.light=M}return P}function S(w,v,M,D,P,A){if(w.visible===!1)return;if(w.layers.test(P)&&(w.isMesh||w.isLine||w.isPoints)&&(w.castShadow||w.receiveShadow&&A===Js)&&(!w.frustumCulled||w.intersectsFrustum(i))){w.modelViewMatrix.multiplyMatrices(M.matrixWorldInverse,w.matrixWorld);let L=t.update(w),B=w.material;if(Array.isArray(B)){let G=L.groups;for(let z=0,tt=G.length;z<tt;z++){let W=G[z],J=B[W.materialIndex];if(J&&J.visible){let j=y(w,J,D,A);w.onBeforeShadow(n,w,v,M,L,j,W),n.renderBufferDirect(M,null,L,j,w,W),w.onAfterShadow(n,w,v,M,L,j,W)}}}else if(B.visible){let G=y(w,B,D,A);w.onBeforeShadow(n,w,v,M,L,G,null),n.renderBufferDirect(M,null,L,G,w,null),w.onAfterShadow(n,w,v,M,L,G,null)}}let U=w.children;for(let L=0,B=U.length;L<B;L++)S(U[L],v,M,D,P,A)}function T(w){w.target.removeEventListener("dispose",T);for(let M in c){let D=c[M],P=w.target.uuid;P in D&&(D[P].dispose(),delete D[P])}}}function v1(n,t){function e(){let N=!1,$=new ne,Y=null,ht=new ne(0,0,0,0);return{setMask:function(st){Y!==st&&!N&&(n.colorMask(st,st,st,st),Y=st)},setLocked:function(st){N=st},setClear:function(st,lt,Ct,Xt,nt){nt===!0&&(st*=Xt,lt*=Xt,Ct*=Xt),$.set(st,lt,Ct,Xt),ht.equals($)===!1&&(n.clearColor(st,lt,Ct,Xt),ht.copy($))},reset:function(){N=!1,Y=null,ht.set(-1,0,0,0)}}}function i(){let N=!1,$=!1,Y=null,ht=null,st=null;return{setReversed:function(lt){if($!==lt){let Ct=t.get("EXT_clip_control");lt?Ct.clipControlEXT(Ct.LOWER_LEFT_EXT,Ct.ZERO_TO_ONE_EXT):Ct.clipControlEXT(Ct.LOWER_LEFT_EXT,Ct.NEGATIVE_ONE_TO_ONE_EXT),$=lt;let Xt=st;st=null,this.setClear(Xt)}},getReversed:function(){return $},setTest:function(lt){lt?rt(n.DEPTH_TEST):Dt(n.DEPTH_TEST)},setMask:function(lt){Y!==lt&&!N&&(n.depthMask(lt),Y=lt)},setFunc:function(lt){if($&&(lt=Hd[lt]),ht!==lt){switch(lt){case da:n.depthFunc(n.NEVER);break;case fa:n.depthFunc(n.ALWAYS);break;case pa:n.depthFunc(n.LESS);break;case Ns:n.depthFunc(n.LEQUAL);break;case ma:n.depthFunc(n.EQUAL);break;case ga:n.depthFunc(n.GEQUAL);break;case xa:n.depthFunc(n.GREATER);break;case _a:n.depthFunc(n.NOTEQUAL);break;default:n.depthFunc(n.LEQUAL)}ht=lt}},setLocked:function(lt){N=lt},setClear:function(lt){st!==lt&&(st=lt,$&&(lt=1-lt),n.clearDepth(lt))},reset:function(){N=!1,Y=null,ht=null,st=null,$=!1}}}function s(){let N=!1,$=null,Y=null,ht=null,st=null,lt=null,Ct=null,Xt=null,nt=null;return{setTest:function(Pt){N||(Pt?rt(n.STENCIL_TEST):Dt(n.STENCIL_TEST))},setMask:function(Pt){$!==Pt&&!N&&(n.stencilMask(Pt),$=Pt)},setFunc:function(Pt,Vt,_t){(Y!==Pt||ht!==Vt||st!==_t)&&(n.stencilFunc(Pt,Vt,_t),Y=Pt,ht=Vt,st=_t)},setOp:function(Pt,Vt,_t){(lt!==Pt||Ct!==Vt||Xt!==_t)&&(n.stencilOp(Pt,Vt,_t),lt=Pt,Ct=Vt,Xt=_t)},setLocked:function(Pt){N=Pt},setClear:function(Pt){nt!==Pt&&(n.clearStencil(Pt),nt=Pt)},reset:function(){N=!1,$=null,Y=null,ht=null,st=null,lt=null,Ct=null,Xt=null,nt=null}}}let r=new e,o=new i,a=new s,l={},c=new WeakMap,h={},u={},d={},f=new WeakMap,p=[],x=null,m=!1,g=null,_=null,y=null,S=null,T=null,w=null,v=null,M=new St(0,0,0),D=0,P=!1,A=null,C=null,U=null,L=null,B=null,G=n.getParameter(n.MAX_COMBINED_TEXTURE_IMAGE_UNITS),z=!1,tt=0,W=n.getParameter(n.VERSION);W.indexOf("WebGL")!==-1?(tt=parseFloat(/^WebGL (\d)/.exec(W)[1]),z=tt>=1):W.indexOf("OpenGL ES")!==-1&&(tt=parseFloat(/^OpenGL ES (\d)/.exec(W)[1]),z=tt>=2);let J=null,j={},xt=n.getParameter(n.SCISSOR_BOX),Mt=n.getParameter(n.VIEWPORT),q=new ne().fromArray(xt),ot=new ne().fromArray(Mt);function gt(N,$,Y,ht){let st=new Uint8Array(4),lt=n.createTexture();n.bindTexture(N,lt),n.texParameteri(N,n.TEXTURE_MIN_FILTER,n.NEAREST),n.texParameteri(N,n.TEXTURE_MAG_FILTER,n.NEAREST);for(let Ct=0;Ct<Y;Ct++)N===n.TEXTURE_3D||N===n.TEXTURE_2D_ARRAY?n.texImage3D($,0,n.RGBA,1,1,ht,0,n.RGBA,n.UNSIGNED_BYTE,st):n.texImage2D($+Ct,0,n.RGBA,1,1,0,n.RGBA,n.UNSIGNED_BYTE,st);return lt}let wt={};wt[n.TEXTURE_2D]=gt(n.TEXTURE_2D,n.TEXTURE_2D,1),wt[n.TEXTURE_CUBE_MAP]=gt(n.TEXTURE_CUBE_MAP,n.TEXTURE_CUBE_MAP_POSITIVE_X,6),wt[n.TEXTURE_2D_ARRAY]=gt(n.TEXTURE_2D_ARRAY,n.TEXTURE_2D_ARRAY,1,1),wt[n.TEXTURE_3D]=gt(n.TEXTURE_3D,n.TEXTURE_3D,1,1),r.setClear(0,0,0,1),o.setClear(1),a.setClear(0),rt(n.DEPTH_TEST),o.setFunc(Ns),Ze(!1),pe(rh),rt(n.CULL_FACE),fe(yi);function rt(N){h[N]!==!0&&(n.enable(N),h[N]=!0)}function Dt(N){h[N]!==!1&&(n.disable(N),h[N]=!1)}function ie(N,$){return d[N]!==$?(n.bindFramebuffer(N,$),d[N]=$,N===n.DRAW_FRAMEBUFFER&&(d[n.FRAMEBUFFER]=$),N===n.FRAMEBUFFER&&(d[n.DRAW_FRAMEBUFFER]=$),!0):!1}function bt(N,$){let Y=p,ht=!1;if(N){Y=f.get($),Y===void 0&&(Y=[],f.set($,Y));let st=N.textures;if(Y.length!==st.length||Y[0]!==n.COLOR_ATTACHMENT0){for(let lt=0,Ct=st.length;lt<Ct;lt++)Y[lt]=n.COLOR_ATTACHMENT0+lt;Y.length=st.length,ht=!0}}else Y[0]!==n.BACK&&(Y[0]=n.BACK,ht=!0);ht&&n.drawBuffers(Y)}function Bt(N){return x!==N?(n.useProgram(N),x=N,!0):!1}let Jt={[pn]:n.FUNC_ADD,[ud]:n.FUNC_SUBTRACT,[dd]:n.FUNC_REVERSE_SUBTRACT};Jt[fd]=n.MIN,Jt[pd]=n.MAX;let Wt={[qa]:n.ZERO,[js]:n.ONE,[md]:n.SRC_COLOR,[lh]:n.SRC_ALPHA,[Md]:n.SRC_ALPHA_SATURATE,[vd]:n.DST_COLOR,[xd]:n.DST_ALPHA,[gd]:n.ONE_MINUS_SRC_COLOR,[ch]:n.ONE_MINUS_SRC_ALPHA,[yd]:n.ONE_MINUS_DST_COLOR,[_d]:n.ONE_MINUS_DST_ALPHA,[Sd]:n.CONSTANT_COLOR,[bd]:n.ONE_MINUS_CONSTANT_COLOR,[wd]:n.CONSTANT_ALPHA,[Ed]:n.ONE_MINUS_CONSTANT_ALPHA};function fe(N,$,Y,ht,st,lt,Ct,Xt,nt,Pt){if(N===yi){m===!0&&(Dt(n.BLEND),m=!1);return}if(m===!1&&(rt(n.BLEND),m=!0),N!==Xa){if(N!==g||Pt!==P){if((_!==pn||T!==pn)&&(n.blendEquation(n.FUNC_ADD),_=pn,T=pn),Pt)switch(N){case $s:n.blendFuncSeparate(n.ONE,n.ONE_MINUS_SRC_ALPHA,n.ONE,n.ONE_MINUS_SRC_ALPHA);break;case $r:n.blendFunc(n.ONE,n.ONE);break;case oh:n.blendFuncSeparate(n.ZERO,n.ONE_MINUS_SRC_COLOR,n.ZERO,n.ONE);break;case ah:n.blendFuncSeparate(n.DST_COLOR,n.ONE_MINUS_SRC_ALPHA,n.ZERO,n.ONE);break;default:Ut("WebGLState: Invalid blending: ",N);break}else switch(N){case $s:n.blendFuncSeparate(n.SRC_ALPHA,n.ONE_MINUS_SRC_ALPHA,n.ONE,n.ONE_MINUS_SRC_ALPHA);break;case $r:n.blendFuncSeparate(n.SRC_ALPHA,n.ONE,n.ONE,n.ONE);break;case oh:Ut("WebGLState: SubtractiveBlending requires material.premultipliedAlpha = true");break;case ah:Ut("WebGLState: MultiplyBlending requires material.premultipliedAlpha = true");break;default:Ut("WebGLState: Invalid blending: ",N);break}y=null,S=null,w=null,v=null,M.set(0,0,0),D=0,g=N,P=Pt}return}st=st||$,lt=lt||Y,Ct=Ct||ht,($!==_||st!==T)&&(n.blendEquationSeparate(Jt[$],Jt[st]),_=$,T=st),(Y!==y||ht!==S||lt!==w||Ct!==v)&&(n.blendFuncSeparate(Wt[Y],Wt[ht],Wt[lt],Wt[Ct]),y=Y,S=ht,w=lt,v=Ct),(Xt.equals(M)===!1||nt!==D)&&(n.blendColor(Xt.r,Xt.g,Xt.b,nt),M.copy(Xt),D=nt),g=N,P=!1}function Se(N,$){N.side===De?Dt(n.CULL_FACE):rt(n.CULL_FACE);let Y=N.side===qe;$&&(Y=!Y),Ze(Y),N.blending===$s&&N.transparent===!1?fe(yi):fe(N.blending,N.blendEquation,N.blendSrc,N.blendDst,N.blendEquationAlpha,N.blendSrcAlpha,N.blendDstAlpha,N.blendColor,N.blendAlpha,N.premultipliedAlpha),o.setFunc(N.depthFunc),o.setTest(N.depthTest),o.setMask(N.depthWrite),r.setMask(N.colorWrite);let ht=N.stencilWrite;a.setTest(ht),ht&&(a.setMask(N.stencilWriteMask),a.setFunc(N.stencilFunc,N.stencilRef,N.stencilFuncMask),a.setOp(N.stencilFail,N.stencilZFail,N.stencilZPass)),Ee(N.polygonOffset,N.polygonOffsetFactor,N.polygonOffsetUnits),N.alphaToCoverage===!0?rt(n.SAMPLE_ALPHA_TO_COVERAGE):Dt(n.SAMPLE_ALPHA_TO_COVERAGE)}function Ze(N){A!==N&&(N?n.frontFace(n.CW):n.frontFace(n.CCW),A=N)}function pe(N){N!==ld?(rt(n.CULL_FACE),N!==C&&(N===rh?n.cullFace(n.BACK):N===cd?n.cullFace(n.FRONT):n.cullFace(n.FRONT_AND_BACK))):Dt(n.CULL_FACE),C=N}function fi(N){N!==U&&(z&&n.lineWidth(N),U=N)}function Ee(N,$,Y){N?(rt(n.POLYGON_OFFSET_FILL),(L!==$||B!==Y)&&(L=$,B=Y,o.getReversed()&&($=-$,Y=-Y),n.polygonOffset($,Y))):Dt(n.POLYGON_OFFSET_FILL)}function He(N){N?rt(n.SCISSOR_TEST):Dt(n.SCISSOR_TEST)}function Mi(N){N===void 0&&(N=n.TEXTURE0+G-1),J!==N&&(n.activeTexture(N),J=N)}function Si(N,$,Y){Y===void 0&&(J===null?Y=n.TEXTURE0+G-1:Y=J);let ht=j[Y];ht===void 0&&(ht={type:void 0,texture:void 0},j[Y]=ht),(ht.type!==N||ht.texture!==$)&&(J!==Y&&(n.activeTexture(Y),J=Y),n.bindTexture(N,$||wt[N]),ht.type=N,ht.texture=$)}function O(){let N=j[J];N!==void 0&&N.type!==void 0&&(n.bindTexture(N.type,null),N.type=void 0,N.texture=void 0)}function bi(){try{n.compressedTexImage2D(...arguments)}catch(N){Ut("WebGLState:",N)}}function xe(){try{n.compressedTexImage3D(...arguments)}catch(N){Ut("WebGLState:",N)}}function _e(){try{n.texSubImage2D(...arguments)}catch(N){Ut("WebGLState:",N)}}function mt(){try{n.texSubImage3D(...arguments)}catch(N){Ut("WebGLState:",N)}}function Ie(){try{n.compressedTexSubImage2D(...arguments)}catch(N){Ut("WebGLState:",N)}}function Nt(){try{n.compressedTexSubImage3D(...arguments)}catch(N){Ut("WebGLState:",N)}}function Ot(){try{n.texStorage2D(...arguments)}catch(N){Ut("WebGLState:",N)}}function I(){try{n.texStorage3D(...arguments)}catch(N){Ut("WebGLState:",N)}}function b(){try{n.texImage2D(...arguments)}catch(N){Ut("WebGLState:",N)}}function H(){try{n.texImage3D(...arguments)}catch(N){Ut("WebGLState:",N)}}function it(N){return u[N]!==void 0?u[N]:n.getParameter(N)}function Q(N,$){u[N]!==$&&(n.pixelStorei(N,$),u[N]=$)}function at(N){q.equals(N)===!1&&(n.scissor(N.x,N.y,N.z,N.w),q.copy(N))}function dt(N){ot.equals(N)===!1&&(n.viewport(N.x,N.y,N.z,N.w),ot.copy(N))}function Z(N,$){let Y=c.get($);Y===void 0&&(Y=new WeakMap,c.set($,Y));let ht=Y.get(N);ht===void 0&&(ht=n.getUniformBlockIndex($,N.name),n.uniformBlockBinding($,ht,ht),Y.set(N,ht))}function K(N,$,Y){let st=c.get($).get(N);l[st]!==Y&&(n.bindBufferBase(n.UNIFORM_BUFFER,st,Y),l[st]=Y)}function ct(){n.disable(n.BLEND),n.disable(n.CULL_FACE),n.disable(n.DEPTH_TEST),n.disable(n.POLYGON_OFFSET_FILL),n.disable(n.SCISSOR_TEST),n.disable(n.STENCIL_TEST),n.disable(n.SAMPLE_ALPHA_TO_COVERAGE),n.blendEquation(n.FUNC_ADD),n.blendFunc(n.ONE,n.ZERO),n.blendFuncSeparate(n.ONE,n.ZERO,n.ONE,n.ZERO),n.blendColor(0,0,0,0),n.colorMask(!0,!0,!0,!0),n.clearColor(0,0,0,0),n.depthMask(!0),n.depthFunc(n.LESS),o.setReversed(!1),n.clearDepth(1),n.stencilMask(4294967295),n.stencilFunc(n.ALWAYS,0,4294967295),n.stencilOp(n.KEEP,n.KEEP,n.KEEP),n.clearStencil(0),n.cullFace(n.BACK),n.frontFace(n.CCW),n.polygonOffset(0,0),n.activeTexture(n.TEXTURE0),n.bindFramebuffer(n.FRAMEBUFFER,null),n.bindFramebuffer(n.DRAW_FRAMEBUFFER,null),n.bindFramebuffer(n.READ_FRAMEBUFFER,null),n.useProgram(null),n.lineWidth(1),n.scissor(0,0,n.canvas.width,n.canvas.height),n.viewport(0,0,n.canvas.width,n.canvas.height),n.pixelStorei(n.PACK_ALIGNMENT,4),n.pixelStorei(n.UNPACK_ALIGNMENT,4),n.pixelStorei(n.UNPACK_FLIP_Y_WEBGL,!1),n.pixelStorei(n.UNPACK_PREMULTIPLY_ALPHA_WEBGL,!1),n.pixelStorei(n.UNPACK_COLORSPACE_CONVERSION_WEBGL,n.BROWSER_DEFAULT_WEBGL),n.pixelStorei(n.PACK_ROW_LENGTH,0),n.pixelStorei(n.PACK_SKIP_PIXELS,0),n.pixelStorei(n.PACK_SKIP_ROWS,0),n.pixelStorei(n.UNPACK_ROW_LENGTH,0),n.pixelStorei(n.UNPACK_IMAGE_HEIGHT,0),n.pixelStorei(n.UNPACK_SKIP_PIXELS,0),n.pixelStorei(n.UNPACK_SKIP_ROWS,0),n.pixelStorei(n.UNPACK_SKIP_IMAGES,0),h={},u={},J=null,j={},d={},l={},f=new WeakMap,p=[],x=null,m=!1,g=null,_=null,y=null,S=null,T=null,w=null,v=null,M=new St(0,0,0),D=0,P=!1,A=null,C=null,U=null,L=null,B=null,q.set(0,0,n.canvas.width,n.canvas.height),ot.set(0,0,n.canvas.width,n.canvas.height),r.reset(),o.reset(),a.reset()}return{buffers:{color:r,depth:o,stencil:a},enable:rt,disable:Dt,bindFramebuffer:ie,drawBuffers:bt,useProgram:Bt,setBlending:fe,setMaterial:Se,setFlipSided:Ze,setCullFace:pe,setLineWidth:fi,setPolygonOffset:Ee,setScissorTest:He,activeTexture:Mi,bindTexture:Si,unbindTexture:O,compressedTexImage2D:bi,compressedTexImage3D:xe,texImage2D:b,texImage3D:H,pixelStorei:Q,getParameter:it,updateUBOMapping:Z,uniformBlockBinding:K,texStorage2D:Ot,texStorage3D:I,texSubImage2D:_e,texSubImage3D:mt,compressedTexSubImage2D:Ie,compressedTexSubImage3D:Nt,scissor:at,viewport:dt,reset:ct}}function y1(n,t,e,i,s,r,o){let a=t.has("WEBGL_multisampled_render_to_texture")?t.get("WEBGL_multisampled_render_to_texture"):null,l=typeof navigator>"u"?!1:/OculusBrowser/g.test(navigator.userAgent),c=new et,h=new WeakMap,u=new Set,d,f=new WeakMap,p=new Set,x=new FinalizationRegistry(I=>p.delete(I)),m=!1;try{m=typeof OffscreenCanvas<"u"&&new OffscreenCanvas(1,1).getContext("2d")!==null}catch{}function g(I,b){return m?new OffscreenCanvas(I,b):Pr("canvas")}function _(I,b,H){let it=1,Q=Ot(I);if((Q.width>H||Q.height>H)&&(it=H/Math.max(Q.width,Q.height)),it<1)if(typeof HTMLImageElement<"u"&&I instanceof HTMLImageElement||typeof HTMLCanvasElement<"u"&&I instanceof HTMLCanvasElement||typeof ImageBitmap<"u"&&I instanceof ImageBitmap||typeof VideoFrame<"u"&&I instanceof VideoFrame){let at=Math.floor(it*Q.width),dt=Math.floor(it*Q.height);d===void 0&&(d=g(at,dt));let Z=b?g(at,dt):d;return Z.width=at,Z.height=dt,Z.getContext("2d").drawImage(I,0,0,at,dt),It("WebGLRenderer: Texture has been resized from ("+Q.width+"x"+Q.height+") to ("+at+"x"+dt+")."),Z}else return"data"in I&&It("WebGLRenderer: Image in DataTexture is too big ("+Q.width+"x"+Q.height+")."),I;return I}function y(I){return I.generateMipmaps}function S(I){n.generateMipmap(I)}function T(I){return I.isWebGLCubeRenderTarget?n.TEXTURE_CUBE_MAP:I.isWebGL3DRenderTarget?n.TEXTURE_3D:I.isWebGLArrayRenderTarget||I.isCompressedArrayTexture?n.TEXTURE_2D_ARRAY:n.TEXTURE_2D}function w(I,b,H,it,Q,at=!1){if(I!==null){if(n[I]!==void 0)return n[I];It("WebGLRenderer: Attempt to use non-existing WebGL internal format '"+I+"'")}let dt;it&&(dt=t.get("EXT_texture_norm16"),dt||It("WebGLRenderer: Unable to use normalized textures without EXT_texture_norm16 extension"));let Z=b;if(b===n.RED&&(H===n.FLOAT&&(Z=n.R32F),H===n.HALF_FLOAT&&(Z=n.R16F),H===n.UNSIGNED_BYTE&&(Z=n.R8),H===n.UNSIGNED_SHORT&&dt&&(Z=dt.R16_EXT),H===n.SHORT&&dt&&(Z=dt.R16_SNORM_EXT)),b===n.RED_INTEGER&&(H===n.UNSIGNED_BYTE&&(Z=n.R8UI),H===n.UNSIGNED_SHORT&&(Z=n.R16UI),H===n.UNSIGNED_INT&&(Z=n.R32UI),H===n.BYTE&&(Z=n.R8I),H===n.SHORT&&(Z=n.R16I),H===n.INT&&(Z=n.R32I)),b===n.RG&&(H===n.FLOAT&&(Z=n.RG32F),H===n.HALF_FLOAT&&(Z=n.RG16F),H===n.UNSIGNED_BYTE&&(Z=n.RG8),H===n.UNSIGNED_SHORT&&dt&&(Z=dt.RG16_EXT),H===n.SHORT&&dt&&(Z=dt.RG16_SNORM_EXT)),b===n.RG_INTEGER&&(H===n.UNSIGNED_BYTE&&(Z=n.RG8UI),H===n.UNSIGNED_SHORT&&(Z=n.RG16UI),H===n.UNSIGNED_INT&&(Z=n.RG32UI),H===n.BYTE&&(Z=n.RG8I),H===n.SHORT&&(Z=n.RG16I),H===n.INT&&(Z=n.RG32I)),b===n.RGB_INTEGER&&(H===n.UNSIGNED_BYTE&&(Z=n.RGB8UI),H===n.UNSIGNED_SHORT&&(Z=n.RGB16UI),H===n.UNSIGNED_INT&&(Z=n.RGB32UI),H===n.BYTE&&(Z=n.RGB8I),H===n.SHORT&&(Z=n.RGB16I),H===n.INT&&(Z=n.RGB32I)),b===n.RGBA_INTEGER&&(H===n.UNSIGNED_BYTE&&(Z=n.RGBA8UI),H===n.UNSIGNED_SHORT&&(Z=n.RGBA16UI),H===n.UNSIGNED_INT&&(Z=n.RGBA32UI),H===n.BYTE&&(Z=n.RGBA8I),H===n.SHORT&&(Z=n.RGBA16I),H===n.INT&&(Z=n.RGBA32I)),b===n.RGB&&(H===n.UNSIGNED_SHORT&&dt&&(Z=dt.RGB16_EXT),H===n.SHORT&&dt&&(Z=dt.RGB16_SNORM_EXT),H===n.UNSIGNED_INT_5_9_9_9_REV&&(Z=n.RGB9_E5),H===n.UNSIGNED_INT_10F_11F_11F_REV&&(Z=n.R11F_G11F_B10F)),b===n.RGBA){let K=at?Cr:qt.getTransfer(Q);H===n.FLOAT&&(Z=n.RGBA32F),H===n.HALF_FLOAT&&(Z=n.RGBA16F),H===n.UNSIGNED_BYTE&&(Z=K===Qt?n.SRGB8_ALPHA8:n.RGBA8),H===n.UNSIGNED_SHORT&&dt&&(Z=dt.RGBA16_EXT),H===n.SHORT&&dt&&(Z=dt.RGBA16_SNORM_EXT),H===n.UNSIGNED_SHORT_4_4_4_4&&(Z=n.RGBA4),H===n.UNSIGNED_SHORT_5_5_5_1&&(Z=n.RGB5_A1)}return(Z===n.R16F||Z===n.R32F||Z===n.RG16F||Z===n.RG32F||Z===n.RGBA16F||Z===n.RGBA32F)&&t.get("EXT_color_buffer_float"),Z}function v(I,b){let H;return I?b===null||b===oi||b===cs?H=n.DEPTH24_STENCIL8:b===ui?H=n.DEPTH32F_STENCIL8:b===tr&&(H=n.DEPTH24_STENCIL8,It("DepthTexture: 16 bit depth attachment is not supported with stencil. Using 24-bit attachment.")):b===null||b===oi||b===cs?H=n.DEPTH_COMPONENT24:b===ui?H=n.DEPTH_COMPONENT32F:b===tr&&(H=n.DEPTH_COMPONENT16),H}function M(I,b){return y(I)===!0||I.isFramebufferTexture&&I.minFilter!==Ce&&I.minFilter!==oe?Math.log2(Math.max(b.width,b.height))+1:I.mipmaps.length>0?I.mipmaps.length:I.isCompressedTexture&&Array.isArray(I.image)?b.mipmaps.length:1}function D(I){let b=new WeakRef(I);i.get(I).__ref=b,p.add(b),x.register(I,b,b)}function P(I){let b=i.get(I).__ref;b!==void 0&&(p.delete(b),x.unregister(b))}function A(I){U(I.target)}function C(I){L(I.target)}function U(I){I.removeEventListener("dispose",A),P(I),G(I),I.isVideoTexture&&h.delete(I),I.isHTMLTexture&&u.delete(I)}function L(I){I.removeEventListener("dispose",C),P(I),tt(I)}function B(){for(let I of p){let b=I.deref();b!==void 0&&(b.isRenderTarget===!0?L(b):U(b))}p.clear(),u.clear()}function G(I){let b=i.get(I);if(b.__webglInit===void 0)return;let H=I.source,it=f.get(H);if(it){let Q=it[b.__cacheKey];Q.usedTimes--,Q.usedTimes===0&&z(I),Object.keys(it).length===0&&f.delete(H)}i.remove(I)}function z(I){let b=i.get(I);n.deleteTexture(b.__webglTexture);let H=I.source,it=f.get(H);delete it[b.__cacheKey],o.memory.textures--}function tt(I){let b=i.get(I);b.__depthDisposeCallback&&b.__depthDisposeCallback();let H=I.depthTexture;if(H&&H.renderTarget===I&&U(H),I.isWebGLCubeRenderTarget)for(let Q=0;Q<6;Q++){if(Array.isArray(b.__webglFramebuffer[Q]))for(let at=0;at<b.__webglFramebuffer[Q].length;at++)n.deleteFramebuffer(b.__webglFramebuffer[Q][at]);else n.deleteFramebuffer(b.__webglFramebuffer[Q]);b.__webglDepthbuffer&&n.deleteRenderbuffer(b.__webglDepthbuffer[Q])}else{if(Array.isArray(b.__webglFramebuffer))for(let Q=0;Q<b.__webglFramebuffer.length;Q++)n.deleteFramebuffer(b.__webglFramebuffer[Q]);else n.deleteFramebuffer(b.__webglFramebuffer);if(b.__webglDepthbuffer&&n.deleteRenderbuffer(b.__webglDepthbuffer),b.__webglMultisampledFramebuffer&&n.deleteFramebuffer(b.__webglMultisampledFramebuffer),b.__webglColorRenderbuffer)for(let Q=0;Q<b.__webglColorRenderbuffer.length;Q++)b.__webglColorRenderbuffer[Q]&&n.deleteRenderbuffer(b.__webglColorRenderbuffer[Q]);b.__webglDepthRenderbuffer&&n.deleteRenderbuffer(b.__webglDepthRenderbuffer)}let it=I.textures;for(let Q=0,at=it.length;Q<at;Q++){let dt=i.get(it[Q]);dt.__webglTexture&&(n.deleteTexture(dt.__webglTexture),o.memory.textures--),i.remove(it[Q])}i.remove(I)}let W=0;function J(){W=0}function j(){return W}function xt(I){W=I}function Mt(){let I=W;return I>=s.maxTextures&&It("WebGLTextures: Trying to use "+(I+1)+" texture units while this GPU supports only "+s.maxTextures),W+=1,I}function q(I){let b=[];return b.push(I.wrapS),b.push(I.wrapT),b.push(I.wrapR||0),b.push(I.magFilter),b.push(I.minFilter),b.push(I.anisotropy),b.push(I.internalFormat),b.push(I.format),b.push(I.type),b.push(I.generateMipmaps),b.push(I.mipmapsAutoUpdate),b.push(I.premultiplyAlpha),b.push(I.flipY),b.push(I.unpackAlignment),b.push(I.colorSpace),b.join()}function ot(I,b){let H=i.get(I);if(I.isVideoTexture&&Ie(I),I.isRenderTargetTexture===!1&&I.isExternalTexture!==!0&&I.version>0&&H.__version!==I.version){let it=I.image;if(it===null)It("WebGLRenderer: Texture marked for update but no image data found.");else if(it.complete===!1)It("WebGLRenderer: Texture marked for update but image is incomplete");else{Se(H,I,b);return}}else I.isExternalTexture&&(H.__webglTexture=I.sourceTexture?I.sourceTexture:null);e.bindTexture(n.TEXTURE_2D,H.__webglTexture,n.TEXTURE0+b)}function gt(I,b){let H=i.get(I);if(I.isRenderTargetTexture===!1&&I.version>0&&H.__version!==I.version){Se(H,I,b);return}else I.isExternalTexture&&(H.__webglTexture=I.sourceTexture?I.sourceTexture:null);e.bindTexture(n.TEXTURE_2D_ARRAY,H.__webglTexture,n.TEXTURE0+b)}function wt(I,b){let H=i.get(I);if(I.isRenderTargetTexture===!1&&I.version>0&&H.__version!==I.version){Se(H,I,b);return}e.bindTexture(n.TEXTURE_3D,H.__webglTexture,n.TEXTURE0+b)}function rt(I,b){let H=i.get(I);if(I.version>0&&H.__version!==I.version){Ze(H,I,b);return}e.bindTexture(n.TEXTURE_CUBE_MAP,H.__webglTexture,n.TEXTURE0+b)}let Dt={[cn]:n.REPEAT,[Gi]:n.CLAMP_TO_EDGE,[va]:n.MIRRORED_REPEAT},ie={[Ce]:n.NEAREST,[Rd]:n.NEAREST_MIPMAP_NEAREST,[so]:n.NEAREST_MIPMAP_LINEAR,[oe]:n.LINEAR,[Ja]:n.LINEAR_MIPMAP_NEAREST,[Bi]:n.LINEAR_MIPMAP_LINEAR},bt={[Dd]:n.NEVER,[_h]:n.ALWAYS,[Ld]:n.LESS,[er]:n.LEQUAL,[Nd]:n.EQUAL,[xh]:n.GEQUAL,[Ud]:n.GREATER,[Fd]:n.NOTEQUAL};function Bt(I,b){if(b.type===ui&&t.has("OES_texture_float_linear")===!1&&(b.magFilter===oe||b.magFilter===Ja||b.magFilter===so||b.magFilter===Bi||b.minFilter===oe||b.minFilter===Ja||b.minFilter===so||b.minFilter===Bi)&&It("WebGLRenderer: Unable to use linear filtering with floating point textures. OES_texture_float_linear not supported on this device."),n.texParameteri(I,n.TEXTURE_WRAP_S,Dt[b.wrapS]),n.texParameteri(I,n.TEXTURE_WRAP_T,Dt[b.wrapT]),(I===n.TEXTURE_3D||I===n.TEXTURE_2D_ARRAY)&&n.texParameteri(I,n.TEXTURE_WRAP_R,Dt[b.wrapR]),n.texParameteri(I,n.TEXTURE_MAG_FILTER,ie[b.magFilter]),n.texParameteri(I,n.TEXTURE_MIN_FILTER,ie[b.minFilter]),b.compareFunction&&(n.texParameteri(I,n.TEXTURE_COMPARE_MODE,n.COMPARE_REF_TO_TEXTURE),n.texParameteri(I,n.TEXTURE_COMPARE_FUNC,bt[b.compareFunction])),t.has("EXT_texture_filter_anisotropic")===!0){if(b.magFilter===Ce||b.minFilter!==so&&b.minFilter!==Bi||b.type===ui&&t.has("OES_texture_float_linear")===!1)return;if(b.anisotropy>1||i.get(b).__currentAnisotropy){let H=t.get("EXT_texture_filter_anisotropic");n.texParameterf(I,H.TEXTURE_MAX_ANISOTROPY_EXT,Math.min(b.anisotropy,s.getMaxAnisotropy())),i.get(b).__currentAnisotropy=b.anisotropy}}}function Jt(I,b){let H=!1;I.__webglInit===void 0&&(I.__webglInit=!0,b.addEventListener("dispose",A),D(b));let it=b.source,Q=f.get(it);Q===void 0&&(Q={},f.set(it,Q));let at=q(b);if(at!==I.__cacheKey){Q[at]===void 0&&(Q[at]={texture:n.createTexture(),usedTimes:0},o.memory.textures++,H=!0),Q[at].usedTimes++;let dt=Q[I.__cacheKey];dt!==void 0&&(Q[I.__cacheKey].usedTimes--,dt.usedTimes===0&&z(b)),I.__cacheKey=at,I.__webglTexture=Q[at].texture}return H}function Wt(I,b,H){return Math.floor(Math.floor(I/H)/b)}function fe(I,b,H,it){let at=I.updateRanges;if(at.length===0)e.texSubImage2D(n.TEXTURE_2D,0,0,0,b.width,b.height,H,it,b.data);else{at.sort((N,$)=>N.start-$.start);let dt=0;for(let N=1;N<at.length;N++){let $=at[dt],Y=at[N],ht=$.start+$.count,st=Wt(Y.start,b.width,4),lt=Wt($.start,b.width,4);Y.start<=ht+1&&st===lt&&Wt(Y.start+Y.count-1,b.width,4)===st?$.count=Math.max($.count,Y.start+Y.count-$.start):(++dt,at[dt]=Y)}at.length=dt+1;let Z=e.getParameter(n.UNPACK_ROW_LENGTH),K=e.getParameter(n.UNPACK_SKIP_PIXELS),ct=e.getParameter(n.UNPACK_SKIP_ROWS);e.pixelStorei(n.UNPACK_ROW_LENGTH,b.width);for(let N=0,$=at.length;N<$;N++){let Y=at[N],ht=Math.floor(Y.start/4),st=Math.ceil(Y.count/4),lt=ht%b.width,Ct=Math.floor(ht/b.width),Xt=st,nt=1;e.pixelStorei(n.UNPACK_SKIP_PIXELS,lt),e.pixelStorei(n.UNPACK_SKIP_ROWS,Ct),e.texSubImage2D(n.TEXTURE_2D,0,lt,Ct,Xt,nt,H,it,b.data)}I.clearUpdateRanges(),e.pixelStorei(n.UNPACK_ROW_LENGTH,Z),e.pixelStorei(n.UNPACK_SKIP_PIXELS,K),e.pixelStorei(n.UNPACK_SKIP_ROWS,ct)}}function Se(I,b,H){let it=n.TEXTURE_2D;(b.isDataArrayTexture||b.isCompressedArrayTexture)&&(it=n.TEXTURE_2D_ARRAY),b.isData3DTexture&&(it=n.TEXTURE_3D);let Q=Jt(I,b),at=b.source;e.bindTexture(it,I.__webglTexture,n.TEXTURE0+H);let dt=i.get(at);if(at.version!==dt.__version||Q===!0){if(e.activeTexture(n.TEXTURE0+H),(typeof ImageBitmap<"u"&&b.image instanceof ImageBitmap)===!1){let nt=qt.getPrimaries(qt.workingColorSpace),Pt=b.colorSpace===zi?null:qt.getPrimaries(b.colorSpace),Vt=b.colorSpace===zi||nt===Pt?n.NONE:n.BROWSER_DEFAULT_WEBGL;e.pixelStorei(n.UNPACK_FLIP_Y_WEBGL,b.flipY),e.pixelStorei(n.UNPACK_PREMULTIPLY_ALPHA_WEBGL,b.premultiplyAlpha),e.pixelStorei(n.UNPACK_COLORSPACE_CONVERSION_WEBGL,Vt)}e.pixelStorei(n.UNPACK_ALIGNMENT,b.unpackAlignment);let K=_(b.image,!1,s.maxTextureSize);K=Nt(b,K);let ct=r.convert(b.format,b.colorSpace),N=r.convert(b.type),$=w(b.internalFormat,ct,N,b.normalized,b.colorSpace,b.isVideoTexture);Bt(it,b);let Y,ht=b.mipmaps,st=b.isVideoTexture!==!0,lt=dt.__version===void 0||Q===!0,Ct=at.dataReady,Xt=M(b,K);if(b.isDepthTexture)$=v(b.format===mn,b.type),lt&&(st?e.texStorage2D(n.TEXTURE_2D,1,$,K.width,K.height):e.texImage2D(n.TEXTURE_2D,0,$,K.width,K.height,0,ct,N,null));else if(b.isDataTexture)if(ht.length>0){st&&lt&&e.texStorage2D(n.TEXTURE_2D,Xt,$,ht[0].width,ht[0].height);for(let nt=0,Pt=ht.length;nt<Pt;nt++)Y=ht[nt],st?Ct&&e.texSubImage2D(n.TEXTURE_2D,nt,0,0,Y.width,Y.height,ct,N,Y.data):e.texImage2D(n.TEXTURE_2D,nt,$,Y.width,Y.height,0,ct,N,Y.data);b.generateMipmaps=!1}else st?(lt&&e.texStorage2D(n.TEXTURE_2D,Xt,$,K.width,K.height),Ct&&fe(b,K,ct,N)):e.texImage2D(n.TEXTURE_2D,0,$,K.width,K.height,0,ct,N,K.data);else if(b.isCompressedTexture)if(b.isCompressedArrayTexture){st&&lt&&e.texStorage3D(n.TEXTURE_2D_ARRAY,Xt,$,ht[0].width,ht[0].height,K.depth);for(let nt=0,Pt=ht.length;nt<Pt;nt++)if(Y=ht[nt],b.format!==ai)if(ct!==null)if(st){if(Ct)if(b.layerUpdates.size>0){let Vt=Ah(Y.width,Y.height,b.format,b.type);for(let _t of b.layerUpdates){let se=Y.data.subarray(_t*Vt/Y.data.BYTES_PER_ELEMENT,(_t+1)*Vt/Y.data.BYTES_PER_ELEMENT);e.compressedTexSubImage3D(n.TEXTURE_2D_ARRAY,nt,0,0,_t,Y.width,Y.height,1,ct,se)}}else e.compressedTexSubImage3D(n.TEXTURE_2D_ARRAY,nt,0,0,0,Y.width,Y.height,K.depth,ct,Y.data)}else e.compressedTexImage3D(n.TEXTURE_2D_ARRAY,nt,$,Y.width,Y.height,K.depth,0,Y.data,0,0);else It("WebGLRenderer: Attempt to load unsupported compressed texture format in .uploadTexture()");else st?Ct&&e.texSubImage3D(n.TEXTURE_2D_ARRAY,nt,0,0,0,Y.width,Y.height,K.depth,ct,N,Y.data):e.texImage3D(n.TEXTURE_2D_ARRAY,nt,$,Y.width,Y.height,K.depth,0,ct,N,Y.data);b.layerUpdates.size>0&&b.clearLayerUpdates()}else{st&&lt&&e.texStorage2D(n.TEXTURE_2D,Xt,$,ht[0].width,ht[0].height);for(let nt=0,Pt=ht.length;nt<Pt;nt++)Y=ht[nt],b.format!==ai?ct!==null?st?Ct&&e.compressedTexSubImage2D(n.TEXTURE_2D,nt,0,0,Y.width,Y.height,ct,Y.data):e.compressedTexImage2D(n.TEXTURE_2D,nt,$,Y.width,Y.height,0,Y.data):It("WebGLRenderer: Attempt to load unsupported compressed texture format in .uploadTexture()"):st?Ct&&e.texSubImage2D(n.TEXTURE_2D,nt,0,0,Y.width,Y.height,ct,N,Y.data):e.texImage2D(n.TEXTURE_2D,nt,$,Y.width,Y.height,0,ct,N,Y.data)}else if(b.isDataArrayTexture)if(st){if(lt&&e.texStorage3D(n.TEXTURE_2D_ARRAY,Xt,$,K.width,K.height,K.depth),Ct)if(b.layerUpdates.size>0){let nt=Ah(K.width,K.height,b.format,b.type);for(let Pt of b.layerUpdates){let Vt=K.data.subarray(Pt*nt/K.data.BYTES_PER_ELEMENT,(Pt+1)*nt/K.data.BYTES_PER_ELEMENT);e.texSubImage3D(n.TEXTURE_2D_ARRAY,0,0,0,Pt,K.width,K.height,1,ct,N,Vt)}b.clearLayerUpdates()}else e.texSubImage3D(n.TEXTURE_2D_ARRAY,0,0,0,0,K.width,K.height,K.depth,ct,N,K.data)}else e.texImage3D(n.TEXTURE_2D_ARRAY,0,$,K.width,K.height,K.depth,0,ct,N,K.data);else if(b.isData3DTexture)st?(lt&&e.texStorage3D(n.TEXTURE_3D,Xt,$,K.width,K.height,K.depth),Ct&&e.texSubImage3D(n.TEXTURE_3D,0,0,0,0,K.width,K.height,K.depth,ct,N,K.data)):e.texImage3D(n.TEXTURE_3D,0,$,K.width,K.height,K.depth,0,ct,N,K.data);else if(b.isFramebufferTexture){if(lt)if(st)e.texStorage2D(n.TEXTURE_2D,Xt,$,K.width,K.height);else{let nt=K.width,Pt=K.height;for(let Vt=0;Vt<Xt;Vt++)e.texImage2D(n.TEXTURE_2D,Vt,$,nt,Pt,0,ct,N,null),nt>>=1,Pt>>=1}}else if(b.isHTMLTexture){if("texElementImage2D"in n){let nt=n.canvas;if(nt.hasAttribute("layoutsubtree")||nt.setAttribute("layoutsubtree","true"),K.parentNode!==nt){nt.appendChild(K),u.add(b),nt.onpaint=Pt=>{let Vt=Pt.changedElements;for(let _t of u)Vt.includes(_t.image)&&(_t.needsUpdate=!0)},nt.requestPaint();return}if(n.texElementImage2D.length===3)n.texElementImage2D(n.TEXTURE_2D,n.RGBA8,K);else{let Vt=n.RGBA,_t=n.RGBA,se=n.UNSIGNED_BYTE;n.texElementImage2D(n.TEXTURE_2D,0,Vt,_t,se,K)}n.texParameteri(n.TEXTURE_2D,n.TEXTURE_MIN_FILTER,n.LINEAR),n.texParameteri(n.TEXTURE_2D,n.TEXTURE_WRAP_S,n.CLAMP_TO_EDGE),n.texParameteri(n.TEXTURE_2D,n.TEXTURE_WRAP_T,n.CLAMP_TO_EDGE)}}else if(ht.length>0){if(st&&lt){let nt=Ot(ht[0]);e.texStorage2D(n.TEXTURE_2D,Xt,$,nt.width,nt.height)}for(let nt=0,Pt=ht.length;nt<Pt;nt++)Y=ht[nt],st?Ct&&e.texSubImage2D(n.TEXTURE_2D,nt,0,0,ct,N,Y):e.texImage2D(n.TEXTURE_2D,nt,$,ct,N,Y);b.generateMipmaps=!1}else if(st){if(lt){let nt=Ot(K);e.texStorage2D(n.TEXTURE_2D,Xt,$,nt.width,nt.height)}Ct&&e.texSubImage2D(n.TEXTURE_2D,0,0,0,ct,N,K)}else e.texImage2D(n.TEXTURE_2D,0,$,ct,N,K);y(b)&&b.mipmapsAutoUpdate===!0&&S(it),dt.__version=at.version,b.onUpdate&&b.onUpdate(b)}I.__version=b.version}function Ze(I,b,H){if(b.image.length!==6)return;let it=Jt(I,b),Q=b.source;e.bindTexture(n.TEXTURE_CUBE_MAP,I.__webglTexture,n.TEXTURE0+H);let at=i.get(Q);if(Q.version!==at.__version||it===!0){e.activeTexture(n.TEXTURE0+H);let dt=qt.getPrimaries(qt.workingColorSpace),Z=b.colorSpace===zi?null:qt.getPrimaries(b.colorSpace),K=b.colorSpace===zi||dt===Z?n.NONE:n.BROWSER_DEFAULT_WEBGL;e.pixelStorei(n.UNPACK_FLIP_Y_WEBGL,b.flipY),e.pixelStorei(n.UNPACK_PREMULTIPLY_ALPHA_WEBGL,b.premultiplyAlpha),e.pixelStorei(n.UNPACK_ALIGNMENT,b.unpackAlignment),e.pixelStorei(n.UNPACK_COLORSPACE_CONVERSION_WEBGL,K);let ct=b.isCompressedTexture||b.image[0].isCompressedTexture,N=b.image[0]&&b.image[0].isDataTexture,$=[];for(let _t=0;_t<6;_t++)!ct&&!N?$[_t]=_(b.image[_t],!0,s.maxCubemapSize):$[_t]=N?b.image[_t].image:b.image[_t],$[_t]=Nt(b,$[_t]);let Y=$[0],ht=r.convert(b.format,b.colorSpace),st=r.convert(b.type),lt=w(b.internalFormat,ht,st,b.normalized,b.colorSpace),Ct=b.isVideoTexture!==!0,Xt=at.__version===void 0||it===!0,nt=Q.dataReady,Pt=M(b,Y);Bt(n.TEXTURE_CUBE_MAP,b);let Vt;if(b.isDepthTexture){if(lt=v(b.format===mn,b.type),Xt)if(Ct)e.texStorage2D(n.TEXTURE_CUBE_MAP,1,lt,Y.width,Y.height);else for(let _t=0;_t<6;_t++)e.texImage2D(n.TEXTURE_CUBE_MAP_POSITIVE_X+_t,0,lt,Y.width,Y.height,0,ht,st,null)}else if(ct){Ct&&Xt&&e.texStorage2D(n.TEXTURE_CUBE_MAP,Pt,lt,Y.width,Y.height);for(let _t=0;_t<6;_t++){Vt=$[_t].mipmaps;for(let se=0;se<Vt.length;se++){let Be=Vt[se];b.format!==ai?ht!==null?Ct?nt&&e.compressedTexSubImage2D(n.TEXTURE_CUBE_MAP_POSITIVE_X+_t,se,0,0,Be.width,Be.height,ht,Be.data):e.compressedTexImage2D(n.TEXTURE_CUBE_MAP_POSITIVE_X+_t,se,lt,Be.width,Be.height,0,Be.data):It("WebGLRenderer: Attempt to load unsupported compressed texture format in .setTextureCube()"):Ct?nt&&e.texSubImage2D(n.TEXTURE_CUBE_MAP_POSITIVE_X+_t,se,0,0,Be.width,Be.height,ht,st,Be.data):e.texImage2D(n.TEXTURE_CUBE_MAP_POSITIVE_X+_t,se,lt,Be.width,Be.height,0,ht,st,Be.data)}}}else{if(Vt=b.mipmaps,Ct&&Xt){Vt.length>0&&Pt++;let _t=Ot($[0]);e.texStorage2D(n.TEXTURE_CUBE_MAP,Pt,lt,_t.width,_t.height)}for(let _t=0;_t<6;_t++)if(N){Ct?nt&&e.texSubImage2D(n.TEXTURE_CUBE_MAP_POSITIVE_X+_t,0,0,0,$[_t].width,$[_t].height,ht,st,$[_t].data):e.texImage2D(n.TEXTURE_CUBE_MAP_POSITIVE_X+_t,0,lt,$[_t].width,$[_t].height,0,ht,st,$[_t].data);for(let se=0;se<Vt.length;se++){let Mn=Vt[se].image[_t].image;Ct?nt&&e.texSubImage2D(n.TEXTURE_CUBE_MAP_POSITIVE_X+_t,se+1,0,0,Mn.width,Mn.height,ht,st,Mn.data):e.texImage2D(n.TEXTURE_CUBE_MAP_POSITIVE_X+_t,se+1,lt,Mn.width,Mn.height,0,ht,st,Mn.data)}}else{Ct?nt&&e.texSubImage2D(n.TEXTURE_CUBE_MAP_POSITIVE_X+_t,0,0,0,ht,st,$[_t]):e.texImage2D(n.TEXTURE_CUBE_MAP_POSITIVE_X+_t,0,lt,ht,st,$[_t]);for(let se=0;se<Vt.length;se++){let Be=Vt[se];Ct?nt&&e.texSubImage2D(n.TEXTURE_CUBE_MAP_POSITIVE_X+_t,se+1,0,0,ht,st,Be.image[_t]):e.texImage2D(n.TEXTURE_CUBE_MAP_POSITIVE_X+_t,se+1,lt,ht,st,Be.image[_t])}}}y(b)&&b.mipmapsAutoUpdate===!0&&S(n.TEXTURE_CUBE_MAP),at.__version=Q.version,b.onUpdate&&b.onUpdate(b)}I.__version=b.version}function pe(I,b,H,it,Q,at){let dt=r.convert(H.format,H.colorSpace),Z=r.convert(H.type),K=w(H.internalFormat,dt,Z,H.normalized,H.colorSpace),ct=i.get(b),N=i.get(H);if(N.__renderTarget=b,!ct.__hasExternalTextures){let $=Math.max(1,b.width>>at),Y=Math.max(1,b.height>>at);Q===n.TEXTURE_3D||Q===n.TEXTURE_2D_ARRAY?e.texImage3D(Q,at,K,$,Y,b.depth,0,dt,Z,null):e.texImage2D(Q,at,K,$,Y,0,dt,Z,null)}e.bindFramebuffer(n.FRAMEBUFFER,I),mt(b)?a.framebufferTexture2DMultisampleEXT(n.FRAMEBUFFER,it,Q,N.__webglTexture,0,_e(b)):(Q===n.TEXTURE_2D||Q>=n.TEXTURE_CUBE_MAP_POSITIVE_X&&Q<=n.TEXTURE_CUBE_MAP_NEGATIVE_Z)&&n.framebufferTexture2D(n.FRAMEBUFFER,it,Q,N.__webglTexture,at),e.bindFramebuffer(n.FRAMEBUFFER,null)}function fi(I,b,H){if(n.bindRenderbuffer(n.RENDERBUFFER,I),b.depthBuffer){let it=b.depthTexture,Q=it&&it.isDepthTexture?it.type:null,at=v(b.stencilBuffer,Q),dt=b.stencilBuffer?n.DEPTH_STENCIL_ATTACHMENT:n.DEPTH_ATTACHMENT;mt(b)?a.renderbufferStorageMultisampleEXT(n.RENDERBUFFER,_e(b),at,b.width,b.height):H?n.renderbufferStorageMultisample(n.RENDERBUFFER,_e(b),at,b.width,b.height):n.renderbufferStorage(n.RENDERBUFFER,at,b.width,b.height),n.framebufferRenderbuffer(n.FRAMEBUFFER,dt,n.RENDERBUFFER,I)}else{let it=b.textures;for(let Q=0;Q<it.length;Q++){let at=it[Q],dt=r.convert(at.format,at.colorSpace),Z=r.convert(at.type),K=w(at.internalFormat,dt,Z,at.normalized,at.colorSpace);mt(b)?a.renderbufferStorageMultisampleEXT(n.RENDERBUFFER,_e(b),K,b.width,b.height):H?n.renderbufferStorageMultisample(n.RENDERBUFFER,_e(b),K,b.width,b.height):n.renderbufferStorage(n.RENDERBUFFER,K,b.width,b.height)}}n.bindRenderbuffer(n.RENDERBUFFER,null)}function Ee(I,b,H){let it=b.isWebGLCubeRenderTarget===!0;if(e.bindFramebuffer(n.FRAMEBUFFER,I),!(b.depthTexture&&b.depthTexture.isDepthTexture))throw new Error("THREE.WebGLTextures: renderTarget.depthTexture must be an instance of THREE.DepthTexture.");let Q=i.get(b.depthTexture);Q.__renderTarget=b;let at=it?b.depthTexture.image[0]:b.depthTexture.image;(!Q.__webglTexture||at.width!==b.width||at.height!==b.height)&&(at.width=b.width,at.height=b.height,b.depthTexture.needsUpdate=!0),it?rt(b.depthTexture,0):ot(b.depthTexture,0);let dt=Q.__webglTexture,Z=_e(b),K=it?n.TEXTURE_CUBE_MAP_POSITIVE_X+H:n.TEXTURE_2D,ct=b.depthTexture.format===mn?n.DEPTH_STENCIL_ATTACHMENT:n.DEPTH_ATTACHMENT;if(b.depthTexture.format===Di)mt(b)?a.framebufferTexture2DMultisampleEXT(n.FRAMEBUFFER,ct,K,dt,0,Z):n.framebufferTexture2D(n.FRAMEBUFFER,ct,K,dt,0);else if(b.depthTexture.format===mn)mt(b)?a.framebufferTexture2DMultisampleEXT(n.FRAMEBUFFER,ct,K,dt,0,Z):n.framebufferTexture2D(n.FRAMEBUFFER,ct,K,dt,0);else throw new Error("THREE.WebGLTextures: Unknown depthTexture format.")}function He(I){let b=i.get(I),H=I.isWebGLCubeRenderTarget===!0;if(b.__boundDepthTexture!==I.depthTexture){let it=I.depthTexture;if(b.__depthDisposeCallback&&b.__depthDisposeCallback(),it){let Q=()=>{delete b.__boundDepthTexture,delete b.__depthDisposeCallback,it.removeEventListener("dispose",Q)};it.addEventListener("dispose",Q),b.__depthDisposeCallback=Q}b.__boundDepthTexture=it}if(I.depthTexture&&!b.__autoAllocateDepthBuffer)if(H)for(let it=0;it<6;it++)Ee(b.__webglFramebuffer[it],I,it);else I.texture.mipmaps.length>0?Ee(b.__webglFramebuffer[0],I,0):Ee(b.__webglFramebuffer,I,0);else if(H){b.__webglDepthbuffer=[];for(let it=0;it<6;it++)if(e.bindFramebuffer(n.FRAMEBUFFER,b.__webglFramebuffer[it]),b.__webglDepthbuffer[it]===void 0)b.__webglDepthbuffer[it]=n.createRenderbuffer(),fi(b.__webglDepthbuffer[it],I,!1);else{let Q=I.stencilBuffer?n.DEPTH_STENCIL_ATTACHMENT:n.DEPTH_ATTACHMENT,at=b.__webglDepthbuffer[it];n.bindRenderbuffer(n.RENDERBUFFER,at),n.framebufferRenderbuffer(n.FRAMEBUFFER,Q,n.RENDERBUFFER,at)}}else if(I.texture.mipmaps.length>0?e.bindFramebuffer(n.FRAMEBUFFER,b.__webglFramebuffer[0]):e.bindFramebuffer(n.FRAMEBUFFER,b.__webglFramebuffer),b.__webglDepthbuffer===void 0)b.__webglDepthbuffer=n.createRenderbuffer(),fi(b.__webglDepthbuffer,I,!1);else{let Q=I.stencilBuffer?n.DEPTH_STENCIL_ATTACHMENT:n.DEPTH_ATTACHMENT,at=b.__webglDepthbuffer;n.bindRenderbuffer(n.RENDERBUFFER,at),n.framebufferRenderbuffer(n.FRAMEBUFFER,Q,n.RENDERBUFFER,at)}e.bindFramebuffer(n.FRAMEBUFFER,null)}function Mi(I,b,H){let it=i.get(I);b!==void 0&&pe(it.__webglFramebuffer,I,I.texture,n.COLOR_ATTACHMENT0,n.TEXTURE_2D,0),H!==void 0&&He(I)}function Si(I){let b=I.texture,H=i.get(I),it=i.get(b);I.addEventListener("dispose",C),D(I);let Q=I.textures,at=I.isWebGLCubeRenderTarget===!0,dt=Q.length>1;if(dt||(it.__webglTexture===void 0&&(it.__webglTexture=n.createTexture()),it.__version=b.version,o.memory.textures++),at){H.__webglFramebuffer=[];for(let Z=0;Z<6;Z++)if(b.mipmaps.length>0){H.__webglFramebuffer[Z]=[];for(let K=0;K<b.mipmaps.length;K++)H.__webglFramebuffer[Z][K]=n.createFramebuffer()}else H.__webglFramebuffer[Z]=n.createFramebuffer()}else{if(b.mipmaps.length>0){H.__webglFramebuffer=[];for(let Z=0;Z<b.mipmaps.length;Z++)H.__webglFramebuffer[Z]=n.createFramebuffer()}else H.__webglFramebuffer=n.createFramebuffer();if(dt)for(let Z=0,K=Q.length;Z<K;Z++){let ct=i.get(Q[Z]);ct.__webglTexture===void 0&&(ct.__webglTexture=n.createTexture(),o.memory.textures++)}if(I.samples>0&&mt(I)===!1){H.__webglMultisampledFramebuffer=n.createFramebuffer(),H.__webglColorRenderbuffer=[],e.bindFramebuffer(n.FRAMEBUFFER,H.__webglMultisampledFramebuffer);for(let Z=0;Z<Q.length;Z++){let K=Q[Z];H.__webglColorRenderbuffer[Z]=n.createRenderbuffer(),n.bindRenderbuffer(n.RENDERBUFFER,H.__webglColorRenderbuffer[Z]);let ct=r.convert(K.format,K.colorSpace),N=r.convert(K.type),$=w(K.internalFormat,ct,N,K.normalized,K.colorSpace,I.isXRRenderTarget===!0),Y=_e(I);n.renderbufferStorageMultisample(n.RENDERBUFFER,Y,$,I.width,I.height),n.framebufferRenderbuffer(n.FRAMEBUFFER,n.COLOR_ATTACHMENT0+Z,n.RENDERBUFFER,H.__webglColorRenderbuffer[Z])}n.bindRenderbuffer(n.RENDERBUFFER,null),I.depthBuffer&&(H.__webglDepthRenderbuffer=n.createRenderbuffer(),fi(H.__webglDepthRenderbuffer,I,!0)),e.bindFramebuffer(n.FRAMEBUFFER,null)}}if(at){e.bindTexture(n.TEXTURE_CUBE_MAP,it.__webglTexture),Bt(n.TEXTURE_CUBE_MAP,b);for(let Z=0;Z<6;Z++)if(b.mipmaps.length>0)for(let K=0;K<b.mipmaps.length;K++)pe(H.__webglFramebuffer[Z][K],I,b,n.COLOR_ATTACHMENT0,n.TEXTURE_CUBE_MAP_POSITIVE_X+Z,K);else pe(H.__webglFramebuffer[Z],I,b,n.COLOR_ATTACHMENT0,n.TEXTURE_CUBE_MAP_POSITIVE_X+Z,0);y(b)?S(n.TEXTURE_CUBE_MAP):b.mipmaps.length>0&&n.texParameteri(n.TEXTURE_CUBE_MAP,n.TEXTURE_MAX_LEVEL,b.mipmaps.length-1),e.unbindTexture()}else if(dt){for(let Z=0,K=Q.length;Z<K;Z++){let ct=Q[Z],N=i.get(ct),$=n.TEXTURE_2D;(I.isWebGL3DRenderTarget||I.isWebGLArrayRenderTarget)&&($=I.isWebGL3DRenderTarget?n.TEXTURE_3D:n.TEXTURE_2D_ARRAY),e.bindTexture($,N.__webglTexture),Bt($,ct),pe(H.__webglFramebuffer,I,ct,n.COLOR_ATTACHMENT0+Z,$,0),y(ct)&&S($)}e.unbindTexture()}else{let Z=n.TEXTURE_2D;if((I.isWebGL3DRenderTarget||I.isWebGLArrayRenderTarget)&&(Z=I.isWebGL3DRenderTarget?n.TEXTURE_3D:n.TEXTURE_2D_ARRAY),e.bindTexture(Z,it.__webglTexture),Bt(Z,b),b.mipmaps.length>0)for(let K=0;K<b.mipmaps.length;K++)pe(H.__webglFramebuffer[K],I,b,n.COLOR_ATTACHMENT0,Z,K);else pe(H.__webglFramebuffer,I,b,n.COLOR_ATTACHMENT0,Z,0);y(b)?S(Z):b.mipmaps.length>0&&n.texParameteri(Z,n.TEXTURE_MAX_LEVEL,b.mipmaps.length-1),e.unbindTexture()}I.depthBuffer&&He(I)}function O(I){let b=I.textures;for(let H=0,it=b.length;H<it;H++){let Q=b[H];if(y(Q)&&Q.mipmapsAutoUpdate===!0){let at=T(I),dt=i.get(Q).__webglTexture;e.bindTexture(at,dt),S(at),e.unbindTexture()}}}let bi=[];function xe(I){if(I.samples>0){if(mt(I)===!1){let b=I.textures,H=I.width,it=I.height,Q=n.COLOR_BUFFER_BIT,at=I.stencilBuffer?n.DEPTH_STENCIL_ATTACHMENT:n.DEPTH_ATTACHMENT,dt=i.get(I),Z=b.length>1;if(Z)for(let ct=0;ct<b.length;ct++)e.bindFramebuffer(n.FRAMEBUFFER,dt.__webglMultisampledFramebuffer),n.framebufferRenderbuffer(n.FRAMEBUFFER,n.COLOR_ATTACHMENT0+ct,n.RENDERBUFFER,null),e.bindFramebuffer(n.FRAMEBUFFER,dt.__webglFramebuffer),n.framebufferTexture2D(n.DRAW_FRAMEBUFFER,n.COLOR_ATTACHMENT0+ct,n.TEXTURE_2D,null,0);e.bindFramebuffer(n.READ_FRAMEBUFFER,dt.__webglMultisampledFramebuffer),I.texture.mipmaps.length>0?e.bindFramebuffer(n.DRAW_FRAMEBUFFER,dt.__webglFramebuffer[0]):e.bindFramebuffer(n.DRAW_FRAMEBUFFER,dt.__webglFramebuffer);for(let ct=0;ct<b.length;ct++){if(I.resolveDepthBuffer&&(I.depthBuffer&&(Q|=n.DEPTH_BUFFER_BIT),I.stencilBuffer&&I.resolveStencilBuffer&&(Q|=n.STENCIL_BUFFER_BIT)),Z){n.framebufferRenderbuffer(n.READ_FRAMEBUFFER,n.COLOR_ATTACHMENT0,n.RENDERBUFFER,dt.__webglColorRenderbuffer[ct]);let N=i.get(b[ct]).__webglTexture;n.framebufferTexture2D(n.DRAW_FRAMEBUFFER,n.COLOR_ATTACHMENT0,n.TEXTURE_2D,N,0)}n.blitFramebuffer(0,0,H,it,0,0,H,it,Q,n.NEAREST),l===!0&&(bi.length=0,bi.push(n.COLOR_ATTACHMENT0+ct),I.depthBuffer&&I.storeMultisampledDepthBuffer===!1&&bi.push(at),n.invalidateFramebuffer(n.READ_FRAMEBUFFER,bi))}if(e.bindFramebuffer(n.READ_FRAMEBUFFER,null),e.bindFramebuffer(n.DRAW_FRAMEBUFFER,null),Z)for(let ct=0;ct<b.length;ct++){e.bindFramebuffer(n.FRAMEBUFFER,dt.__webglMultisampledFramebuffer),n.framebufferRenderbuffer(n.FRAMEBUFFER,n.COLOR_ATTACHMENT0+ct,n.RENDERBUFFER,dt.__webglColorRenderbuffer[ct]);let N=i.get(b[ct]).__webglTexture;e.bindFramebuffer(n.FRAMEBUFFER,dt.__webglFramebuffer),n.framebufferTexture2D(n.DRAW_FRAMEBUFFER,n.COLOR_ATTACHMENT0+ct,n.TEXTURE_2D,N,0)}e.bindFramebuffer(n.DRAW_FRAMEBUFFER,dt.__webglMultisampledFramebuffer)}else if(I.depthBuffer&&I.storeMultisampledDepthBuffer===!1&&l){let b=I.stencilBuffer?n.DEPTH_STENCIL_ATTACHMENT:n.DEPTH_ATTACHMENT;n.invalidateFramebuffer(n.DRAW_FRAMEBUFFER,[b])}}}function _e(I){return Math.min(s.maxSamples,I.samples)}function mt(I){let b=i.get(I);return I.samples>0&&t.has("WEBGL_multisampled_render_to_texture")===!0&&b.__useRenderToTexture!==!1}function Ie(I){let b=o.render.frame;h.get(I)!==b&&(h.set(I,b),I.update())}function Nt(I,b){let H=I.colorSpace,it=I.format,Q=I.type;return I.isCompressedTexture===!0||I.isVideoTexture===!0||H!==Cn&&H!==zi&&(qt.getTransfer(H)===Qt?(it!==ai||Q!==ti)&&It("WebGLTextures: sRGB encoded textures have to use RGBAFormat and UnsignedByteType."):Ut("WebGLTextures: Unsupported texture color space:",H)),b}function Ot(I){return typeof HTMLImageElement<"u"&&I instanceof HTMLImageElement?(c.width=I.naturalWidth||I.width,c.height=I.naturalHeight||I.height):typeof VideoFrame<"u"&&I instanceof VideoFrame?(c.width=I.displayWidth,c.height=I.displayHeight):(c.width=I.width,c.height=I.height),c}this.allocateTextureUnit=Mt,this.resetTextureUnits=J,this.getTextureUnits=j,this.setTextureUnits=xt,this.setTexture2D=ot,this.setTexture2DArray=gt,this.setTexture3D=wt,this.setTextureCube=rt,this.rebindTextures=Mi,this.setupRenderTarget=Si,this.updateRenderTargetMipmap=O,this.updateMultisampleRenderTarget=xe,this.setupDepthRenderbuffer=He,this.setupFrameBufferTexture=pe,this.useMultisampledRTT=mt,this.dispose=B}function M1(n,t){function e(i,s=zi){let r,o=qt.getTransfer(s);if(i===ti)return n.UNSIGNED_BYTE;if(i===ja)return n.UNSIGNED_SHORT_4_4_4_4;if(i===Qa)return n.UNSIGNED_SHORT_5_5_5_1;if(i===fh)return n.UNSIGNED_INT_5_9_9_9_REV;if(i===ph)return n.UNSIGNED_INT_10F_11F_11F_REV;if(i===uh)return n.BYTE;if(i===dh)return n.SHORT;if(i===tr)return n.UNSIGNED_SHORT;if(i===$a)return n.INT;if(i===oi)return n.UNSIGNED_INT;if(i===ui)return n.FLOAT;if(i===de)return n.HALF_FLOAT;if(i===mh)return n.ALPHA;if(i===gh)return n.RGB;if(i===ai)return n.RGBA;if(i===Di)return n.DEPTH_COMPONENT;if(i===mn)return n.DEPTH_STENCIL;if(i===On)return n.RED;if(i===tl)return n.RED_INTEGER;if(i===Bn)return n.RG;if(i===el)return n.RG_INTEGER;if(i===il)return n.RGBA_INTEGER;if(i===ro||i===oo||i===ao||i===lo)if(o===Qt)if(r=t.get("WEBGL_compressed_texture_s3tc_srgb"),r!==null){if(i===ro)return r.COMPRESSED_SRGB_S3TC_DXT1_EXT;if(i===oo)return r.COMPRESSED_SRGB_ALPHA_S3TC_DXT1_EXT;if(i===ao)return r.COMPRESSED_SRGB_ALPHA_S3TC_DXT3_EXT;if(i===lo)return r.COMPRESSED_SRGB_ALPHA_S3TC_DXT5_EXT}else return null;else if(r=t.get("WEBGL_compressed_texture_s3tc"),r!==null){if(i===ro)return r.COMPRESSED_RGB_S3TC_DXT1_EXT;if(i===oo)return r.COMPRESSED_RGBA_S3TC_DXT1_EXT;if(i===ao)return r.COMPRESSED_RGBA_S3TC_DXT3_EXT;if(i===lo)return r.COMPRESSED_RGBA_S3TC_DXT5_EXT}else return null;if(i===nl||i===sl||i===rl||i===ol)if(r=t.get("WEBGL_compressed_texture_pvrtc"),r!==null){if(i===nl)return r.COMPRESSED_RGB_PVRTC_4BPPV1_IMG;if(i===sl)return r.COMPRESSED_RGB_PVRTC_2BPPV1_IMG;if(i===rl)return r.COMPRESSED_RGBA_PVRTC_4BPPV1_IMG;if(i===ol)return r.COMPRESSED_RGBA_PVRTC_2BPPV1_IMG}else return null;if(i===al||i===ll||i===cl||i===hl||i===ul||i===co||i===dl)if(r=t.get("WEBGL_compressed_texture_etc"),r!==null){if(i===al||i===ll)return o===Qt?r.COMPRESSED_SRGB8_ETC2:r.COMPRESSED_RGB8_ETC2;if(i===cl)return o===Qt?r.COMPRESSED_SRGB8_ALPHA8_ETC2_EAC:r.COMPRESSED_RGBA8_ETC2_EAC;if(i===hl)return r.COMPRESSED_R11_EAC;if(i===ul)return r.COMPRESSED_SIGNED_R11_EAC;if(i===co)return r.COMPRESSED_RG11_EAC;if(i===dl)return r.COMPRESSED_SIGNED_RG11_EAC}else return null;if(i===fl||i===pl||i===ml||i===gl||i===xl||i===_l||i===vl||i===yl||i===Ml||i===Sl||i===bl||i===wl||i===El||i===Tl)if(r=t.get("WEBGL_compressed_texture_astc"),r!==null){if(i===fl)return o===Qt?r.COMPRESSED_SRGB8_ALPHA8_ASTC_4x4_KHR:r.COMPRESSED_RGBA_ASTC_4x4_KHR;if(i===pl)return o===Qt?r.COMPRESSED_SRGB8_ALPHA8_ASTC_5x4_KHR:r.COMPRESSED_RGBA_ASTC_5x4_KHR;if(i===ml)return o===Qt?r.COMPRESSED_SRGB8_ALPHA8_ASTC_5x5_KHR:r.COMPRESSED_RGBA_ASTC_5x5_KHR;if(i===gl)return o===Qt?r.COMPRESSED_SRGB8_ALPHA8_ASTC_6x5_KHR:r.COMPRESSED_RGBA_ASTC_6x5_KHR;if(i===xl)return o===Qt?r.COMPRESSED_SRGB8_ALPHA8_ASTC_6x6_KHR:r.COMPRESSED_RGBA_ASTC_6x6_KHR;if(i===_l)return o===Qt?r.COMPRESSED_SRGB8_ALPHA8_ASTC_8x5_KHR:r.COMPRESSED_RGBA_ASTC_8x5_KHR;if(i===vl)return o===Qt?r.COMPRESSED_SRGB8_ALPHA8_ASTC_8x6_KHR:r.COMPRESSED_RGBA_ASTC_8x6_KHR;if(i===yl)return o===Qt?r.COMPRESSED_SRGB8_ALPHA8_ASTC_8x8_KHR:r.COMPRESSED_RGBA_ASTC_8x8_KHR;if(i===Ml)return o===Qt?r.COMPRESSED_SRGB8_ALPHA8_ASTC_10x5_KHR:r.COMPRESSED_RGBA_ASTC_10x5_KHR;if(i===Sl)return o===Qt?r.COMPRESSED_SRGB8_ALPHA8_ASTC_10x6_KHR:r.COMPRESSED_RGBA_ASTC_10x6_KHR;if(i===bl)return o===Qt?r.COMPRESSED_SRGB8_ALPHA8_ASTC_10x8_KHR:r.COMPRESSED_RGBA_ASTC_10x8_KHR;if(i===wl)return o===Qt?r.COMPRESSED_SRGB8_ALPHA8_ASTC_10x10_KHR:r.COMPRESSED_RGBA_ASTC_10x10_KHR;if(i===El)return o===Qt?r.COMPRESSED_SRGB8_ALPHA8_ASTC_12x10_KHR:r.COMPRESSED_RGBA_ASTC_12x10_KHR;if(i===Tl)return o===Qt?r.COMPRESSED_SRGB8_ALPHA8_ASTC_12x12_KHR:r.COMPRESSED_RGBA_ASTC_12x12_KHR}else return null;if(i===Al||i===Rl||i===Cl)if(r=t.get("EXT_texture_compression_bptc"),r!==null){if(i===Al)return o===Qt?r.COMPRESSED_SRGB_ALPHA_BPTC_UNORM_EXT:r.COMPRESSED_RGBA_BPTC_UNORM_EXT;if(i===Rl)return r.COMPRESSED_RGB_BPTC_SIGNED_FLOAT_EXT;if(i===Cl)return r.COMPRESSED_RGB_BPTC_UNSIGNED_FLOAT_EXT}else return null;if(i===Pl||i===Il||i===ho||i===Dl)if(r=t.get("EXT_texture_compression_rgtc"),r!==null){if(i===Pl)return r.COMPRESSED_RED_RGTC1_EXT;if(i===Il)return r.COMPRESSED_SIGNED_RED_RGTC1_EXT;if(i===ho)return r.COMPRESSED_RED_GREEN_RGTC2_EXT;if(i===Dl)return r.COMPRESSED_SIGNED_RED_GREEN_RGTC2_EXT}else return null;return i===cs?n.UNSIGNED_INT_24_8:n[i]!==void 0?n[i]:null}return{convert:e}}var S1=`
void main() {

	gl_Position = vec4( position, 1.0 );

}`,b1=`
uniform sampler2DArray depthColor;
uniform float depthWidth;
uniform float depthHeight;

void main() {

	vec2 coord = vec2( gl_FragCoord.x / depthWidth, gl_FragCoord.y / depthHeight );

	if ( coord.x >= 1.0 ) {

		gl_FragDepth = texture( depthColor, vec3( coord.x - 1.0, coord.y, 1 ) ).r;

	} else {

		gl_FragDepth = texture( depthColor, vec3( coord.x, coord.y, 0 ) ).r;

	}

}`,Fh=class{constructor(){this.texture=null,this.mesh=null,this.depthNear=0,this.depthFar=0}init(t,e){if(this.texture===null){let i=new Or(t.texture);(t.depthNear!==e.depthNear||t.depthFar!==e.depthFar)&&(this.depthNear=t.depthNear,this.depthFar=t.depthFar),this.texture=i}}getMesh(t){if(this.texture!==null&&this.mesh===null){let e=t.cameras[0].viewport,i=new ae({vertexShader:S1,fragmentShader:b1,uniforms:{depthColor:{value:this.texture},depthWidth:{value:e.z},depthHeight:{value:e.w}}});this.mesh=new Lt(new _i(20,20),i)}return this.mesh}reset(){this.texture=null,this.mesh=null}getDepthTexture(){return this.texture}},Oh=class extends Li{constructor(t,e){super();let i=this,s=null,r=1,o=null,a="local-floor",l=1,c=null,h=null,u=null,d=null,f=null,p=null,x=typeof XRWebGLBinding<"u",m=new Fh,g={},_=e.getContextAttributes(),y=null,S=null,T=[],w=[],v=new et,M=null,D=null,P=new Ge;P.viewport=new ne;let A=new Ge;A.viewport=new ne;let C=[P,A],U=new Wa,L=null,B=null;this.cameraAutoUpdate=!0,this.enabled=!1,this.isPresenting=!1,this.getController=function(q){let ot=T[q];return ot===void 0&&(ot=new ks,T[q]=ot),ot.getTargetRaySpace()},this.getControllerGrip=function(q){let ot=T[q];return ot===void 0&&(ot=new ks,T[q]=ot),ot.getGripSpace()},this.getHand=function(q){let ot=T[q];return ot===void 0&&(ot=new ks,T[q]=ot),ot.getHandSpace()};function G(q){let ot=w.indexOf(q.inputSource);if(ot===-1)return;let gt=T[ot];gt!==void 0&&(gt.update(q.inputSource,q.frame,c||o),gt.dispatchEvent({type:q.type,data:q.inputSource}))}function z(){s.removeEventListener("select",G),s.removeEventListener("selectstart",G),s.removeEventListener("selectend",G),s.removeEventListener("squeeze",G),s.removeEventListener("squeezestart",G),s.removeEventListener("squeezeend",G),s.removeEventListener("end",z),s.removeEventListener("inputsourceschange",tt);for(let q=0;q<T.length;q++){let ot=w[q];ot!==null&&(w[q]=null,T[q].disconnect(ot))}L=null,B=null,m.reset();for(let q in g)delete g[q];if(t.setRenderTarget(y),f=null,d=null,u=null,s=null,S=null,Mt.stop(),i.isPresenting=!1,t.setPixelRatio(M),t.setSize(v.width,v.height,!1),D!==null){let q=D.camera;q.fov=D.fov,q.zoom=D.zoom,q.updateProjectionMatrix(),D=null}i.dispatchEvent({type:"sessionend"})}this.setFramebufferScaleFactor=function(q){r=q,i.isPresenting===!0&&It("WebXRManager: Cannot change framebuffer scale while presenting.")},this.setReferenceSpaceType=function(q){a=q,i.isPresenting===!0&&It("WebXRManager: Cannot change reference space type while presenting.")},this.getReferenceSpace=function(){return c||o},this.setReferenceSpace=function(q){c=q},this.getBaseLayer=function(){return d!==null?d:f},this.getBinding=function(){return u===null&&x&&(u=new XRWebGLBinding(s,e)),u},this.getFrame=function(){return p},this.getSession=function(){return s},this.setSession=async function(q){if(s=q,s!==null){if(y=t.getRenderTarget(),s.addEventListener("select",G),s.addEventListener("selectstart",G),s.addEventListener("selectend",G),s.addEventListener("squeeze",G),s.addEventListener("squeezestart",G),s.addEventListener("squeezeend",G),s.addEventListener("end",z),s.addEventListener("inputsourceschange",tt),_.xrCompatible!==!0&&await e.makeXRCompatible(),M=t.getPixelRatio(),t.getSize(v),x&&"createProjectionLayer"in XRWebGLBinding.prototype){let gt=null,wt=null,rt=null;_.depth&&(rt=_.stencil?e.DEPTH24_STENCIL8:e.DEPTH_COMPONENT24,gt=_.stencil?mn:Di,wt=_.stencil?cs:oi);let Dt={colorFormat:e.RGBA8,depthFormat:rt,scaleFactor:r};u=this.getBinding(),d=u.createProjectionLayer(Dt),s.updateRenderState({layers:[d]}),t.setPixelRatio(1),t.setSize(d.textureWidth,d.textureHeight,!1),S=new he(d.textureWidth,d.textureHeight,{format:ai,type:ti,depthTexture:new Fi(d.textureWidth,d.textureHeight,wt,void 0,void 0,void 0,void 0,void 0,void 0,gt),stencilBuffer:_.stencil,colorSpace:t.outputColorSpace,samples:_.antialias?4:0,resolveDepthBuffer:d.ignoreDepthValues===!1,resolveStencilBuffer:d.ignoreDepthValues===!1,storeMultisampledDepthBuffer:d.ignoreDepthValues===!1,storeMultisampledStencilBuffer:d.ignoreDepthValues===!1})}else{let gt={antialias:_.antialias,alpha:!0,depth:_.depth,stencil:_.stencil,framebufferScaleFactor:r};f=new XRWebGLLayer(s,e,gt),s.updateRenderState({baseLayer:f}),t.setPixelRatio(1),t.setSize(f.framebufferWidth,f.framebufferHeight,!1),S=new he(f.framebufferWidth,f.framebufferHeight,{format:ai,type:ti,colorSpace:t.outputColorSpace,stencilBuffer:_.stencil,resolveDepthBuffer:f.ignoreDepthValues===!1,resolveStencilBuffer:f.ignoreDepthValues===!1,storeMultisampledDepthBuffer:f.ignoreDepthValues===!1,storeMultisampledStencilBuffer:f.ignoreDepthValues===!1})}S.isXRRenderTarget=!0,this.setFoveation(l),c=null,o=await s.requestReferenceSpace(a),Mt.setContext(s),Mt.start(),i.isPresenting=!0,i.dispatchEvent({type:"sessionstart"})}},this.getEnvironmentBlendMode=function(){if(s!==null)return s.environmentBlendMode},this.getDepthTexture=function(){return m.getDepthTexture()};function tt(q){for(let ot=0;ot<q.removed.length;ot++){let gt=q.removed[ot],wt=w.indexOf(gt);wt>=0&&(w[wt]=null,T[wt].disconnect(gt))}for(let ot=0;ot<q.added.length;ot++){let gt=q.added[ot],wt=w.indexOf(gt);if(wt===-1){for(let Dt=0;Dt<T.length;Dt++)if(Dt>=w.length){w.push(gt),wt=Dt;break}else if(w[Dt]===null){w[Dt]=gt,wt=Dt;break}if(wt===-1)break}let rt=T[wt];rt&&rt.connect(gt)}}function W(q,ot){ot===null?q.matrixWorld.copy(q.matrix):q.matrixWorld.multiplyMatrices(ot.matrixWorld,q.matrix),q.matrixWorldInverse.copy(q.matrixWorld).invert()}this.updateCamera=function(q){if(s===null)return;let ot=q.near,gt=q.far;m.texture!==null&&(m.depthNear>0&&(ot=m.depthNear),m.depthFar>0&&(gt=m.depthFar)),U.near=A.near=P.near=ot,U.far=A.far=P.far=gt,(L!==U.near||B!==U.far)&&(s.updateRenderState({depthNear:U.near,depthFar:U.far}),L=U.near,B=U.far),U.layers.mask=q.layers.mask|6,P.layers.mask=U.layers.mask&-5,A.layers.mask=U.layers.mask&-3;let wt=q.parent,rt=U.cameras;W(U,wt);for(let Dt=0;Dt<rt.length;Dt++)W(rt[Dt],wt);U.projectionMatrix.copy(P.projectionMatrix),U.projectionMatrixInverse.copy(P.projectionMatrixInverse),D===null&&q.isPerspectiveCamera&&(D={camera:q,fov:q.fov,zoom:q.zoom}),J(q,U,wt)};function J(q,ot,gt){gt===null?q.matrix.copy(ot.matrixWorld):(q.matrix.copy(gt.matrixWorld),q.matrix.invert(),q.matrix.multiply(ot.matrixWorld)),q.matrix.decompose(q.position,q.quaternion,q.scale),q.updateMatrixWorld(!0),q.projectionMatrix.copy(ot.projectionMatrix),q.projectionMatrixInverse.copy(ot.projectionMatrixInverse),q.isPerspectiveCamera&&(q.fov=Jn*2*Math.atan(1/q.projectionMatrix.elements[5]),q.zoom=1)}this.getCamera=function(){return U},this.getFoveation=function(){if(!(d===null&&f===null))return l},this.setFoveation=function(q){l=q,d!==null&&(d.fixedFoveation=q),f!==null&&f.fixedFoveation!==void 0&&(f.fixedFoveation=q)},this.hasDepthSensing=function(){return m.texture!==null},this.getDepthSensingMesh=function(){return m.getMesh(U)},this.getCameraTexture=function(q){return g[q]};let j=null;function xt(q,ot){if(h=ot.getViewerPose(c||o),p=ot,h!==null){let gt=h.views;f!==null&&(t.setRenderTargetFramebuffer(S,f.framebuffer),t.setRenderTarget(S));let wt=!1;gt.length!==U.cameras.length&&(U.cameras.length=0,wt=!0);for(let bt=0;bt<gt.length;bt++){let Bt=gt[bt],Jt=null;if(f!==null)Jt=f.getViewport(Bt);else{let fe=u.getViewSubImage(d,Bt);Jt=fe.viewport,bt===0&&(t.setRenderTargetTextures(S,fe.colorTexture,fe.depthStencilTexture),t.setRenderTarget(S))}let Wt=C[bt];Wt===void 0&&(Wt=new Ge,Wt.layers.enable(bt),Wt.viewport=new ne,C[bt]=Wt),Wt.matrix.fromArray(Bt.transform.matrix),Wt.matrix.decompose(Wt.position,Wt.quaternion,Wt.scale),Wt.projectionMatrix.fromArray(Bt.projectionMatrix),Wt.projectionMatrixInverse.copy(Wt.projectionMatrix).invert(),Wt.viewport.set(Jt.x,Jt.y,Jt.width,Jt.height),bt===0&&(U.matrix.copy(Wt.matrix),U.matrix.decompose(U.position,U.quaternion,U.scale)),wt===!0&&U.cameras.push(Wt)}let rt=s.enabledFeatures;if(rt&&rt.includes("depth-sensing")&&s.depthUsage=="gpu-optimized"&&x){u=i.getBinding();let bt=u.getDepthInformation(gt[0]);bt&&bt.isValid&&bt.texture&&m.init(bt,s.renderState)}if(rt&&rt.includes("camera-access")&&x){t.state.unbindTexture(),u=i.getBinding();for(let bt=0;bt<gt.length;bt++){let Bt=gt[bt].camera;if(Bt){let Jt=g[Bt];Jt||(Jt=new Or,g[Bt]=Jt);let Wt=u.getCameraImage(Bt);Jt.sourceTexture=Wt}}}}for(let gt=0;gt<T.length;gt++){let wt=w[gt],rt=T[gt];wt!==null&&rt!==void 0&&rt.update(wt,ot,c||o)}j&&j(q,ot),ot.detectedPlanes&&i.dispatchEvent({type:"planesdetected",data:ot}),p=null}let Mt=new mf;Mt.setAnimationLoop(xt),this.setAnimationLoop=function(q){j=q},this.dispose=function(){}}},w1=new Gt,Ef=new Ft;Ef.set(-1,0,0,0,1,0,0,0,1);function E1(n,t){function e(m,g){m.matrixAutoUpdate===!0&&m.updateMatrix(),g.value.copy(m.matrix)}function i(m,g){g.color.getRGB(m.fogColor.value,wh(n)),g.isFog?(m.fogNear.value=g.near,m.fogFar.value=g.far):g.isFogExp2&&(m.fogDensity.value=g.density)}function s(m,g,_,y,S){g.isNodeMaterial?g.uniformsNeedUpdate=!1:g.isMeshBasicMaterial?r(m,g):g.isMeshLambertMaterial?(r(m,g),g.envMap&&(m.envMapIntensity.value=g.envMapIntensity)):g.isMeshToonMaterial?(r(m,g),u(m,g)):g.isMeshPhongMaterial?(r(m,g),h(m,g),g.envMap&&(m.envMapIntensity.value=g.envMapIntensity)):g.isMeshStandardMaterial?(r(m,g),d(m,g),g.isMeshPhysicalMaterial&&f(m,g,S)):g.isMeshMatcapMaterial?(r(m,g),p(m,g)):g.isMeshDepthMaterial?r(m,g):g.isMeshDistanceMaterial?(r(m,g),x(m,g)):g.isMeshNormalMaterial?r(m,g):g.isLineBasicMaterial?(o(m,g),g.isLineDashedMaterial&&a(m,g)):g.isPointsMaterial?l(m,g,_,y):g.isSpriteMaterial?c(m,g):g.isShadowMaterial?(m.color.value.copy(g.color),m.opacity.value=g.opacity):g.isShaderMaterial&&(g.uniformsNeedUpdate=!1)}function r(m,g){m.opacity.value=g.opacity,g.color&&m.diffuse.value.copy(g.color),g.emissive&&m.emissive.value.copy(g.emissive).multiplyScalar(g.emissiveIntensity),g.map&&(m.map.value=g.map,e(g.map,m.mapTransform)),g.alphaMap&&(m.alphaMap.value=g.alphaMap,e(g.alphaMap,m.alphaMapTransform)),g.bumpMap&&(m.bumpMap.value=g.bumpMap,e(g.bumpMap,m.bumpMapTransform),m.bumpScale.value=g.bumpScale,g.side===qe&&(m.bumpScale.value*=-1)),g.normalMap&&(m.normalMap.value=g.normalMap,e(g.normalMap,m.normalMapTransform),m.normalScale.value.copy(g.normalScale),g.side===qe&&m.normalScale.value.negate()),g.displacementMap&&(m.displacementMap.value=g.displacementMap,e(g.displacementMap,m.displacementMapTransform),m.displacementScale.value=g.displacementScale,m.displacementBias.value=g.displacementBias),g.emissiveMap&&(m.emissiveMap.value=g.emissiveMap,e(g.emissiveMap,m.emissiveMapTransform)),g.specularMap&&(m.specularMap.value=g.specularMap,e(g.specularMap,m.specularMapTransform)),g.alphaTest>0&&(m.alphaTest.value=g.alphaTest);let _=t.get(g),y=_.envMap,S=_.envMapRotation;y&&(m.envMap.value=y,m.envMapRotation.value.setFromMatrix4(w1.makeRotationFromEuler(S)).transpose(),y.isCubeTexture&&y.isRenderTargetTexture===!1&&m.envMapRotation.value.premultiply(Ef),m.reflectivity.value=g.reflectivity,m.ior.value=g.ior,m.refractionRatio.value=g.refractionRatio),g.lightMap&&(m.lightMap.value=g.lightMap,m.lightMapIntensity.value=g.lightMapIntensity,e(g.lightMap,m.lightMapTransform)),g.aoMap&&(m.aoMap.value=g.aoMap,m.aoMapIntensity.value=g.aoMapIntensity,e(g.aoMap,m.aoMapTransform))}function o(m,g){m.diffuse.value.copy(g.color),m.opacity.value=g.opacity,g.map&&(m.map.value=g.map,e(g.map,m.mapTransform))}function a(m,g){m.dashSize.value=g.dashSize,m.totalSize.value=g.dashSize+g.gapSize,m.scale.value=g.scale}function l(m,g,_,y){m.diffuse.value.copy(g.color),m.opacity.value=g.opacity,m.size.value=g.size*_,m.scale.value=y*.5,g.map&&(m.map.value=g.map,e(g.map,m.uvTransform)),g.alphaMap&&(m.alphaMap.value=g.alphaMap,e(g.alphaMap,m.alphaMapTransform)),g.alphaTest>0&&(m.alphaTest.value=g.alphaTest)}function c(m,g){m.diffuse.value.copy(g.color),m.opacity.value=g.opacity,m.rotation.value=g.rotation,g.map&&(m.map.value=g.map,e(g.map,m.mapTransform)),g.alphaMap&&(m.alphaMap.value=g.alphaMap,e(g.alphaMap,m.alphaMapTransform)),g.alphaTest>0&&(m.alphaTest.value=g.alphaTest)}function h(m,g){m.specular.value.copy(g.specular),m.shininess.value=Math.max(g.shininess,1e-4)}function u(m,g){g.gradientMap&&(m.gradientMap.value=g.gradientMap)}function d(m,g){m.metalness.value=g.metalness,g.metalnessMap&&(m.metalnessMap.value=g.metalnessMap,e(g.metalnessMap,m.metalnessMapTransform)),m.roughness.value=g.roughness,g.roughnessMap&&(m.roughnessMap.value=g.roughnessMap,e(g.roughnessMap,m.roughnessMapTransform)),g.envMap&&(m.envMapIntensity.value=g.envMapIntensity)}function f(m,g,_){m.ior.value=g.ior,g.diffuseRoughness>0&&(m.diffuseRoughness.value=g.diffuseRoughness,g.diffuseRoughnessMap&&(m.diffuseRoughnessMap.value=g.diffuseRoughnessMap,e(g.diffuseRoughnessMap,m.diffuseRoughnessMapTransform))),g.sheen>0&&(m.sheenColor.value.copy(g.sheenColor).multiplyScalar(g.sheen),m.sheenRoughness.value=g.sheenRoughness,g.sheenColorMap&&(m.sheenColorMap.value=g.sheenColorMap,e(g.sheenColorMap,m.sheenColorMapTransform)),g.sheenRoughnessMap&&(m.sheenRoughnessMap.value=g.sheenRoughnessMap,e(g.sheenRoughnessMap,m.sheenRoughnessMapTransform))),g.clearcoat>0&&(m.clearcoat.value=g.clearcoat,m.clearcoatRoughness.value=g.clearcoatRoughness,g.clearcoatMap&&(m.clearcoatMap.value=g.clearcoatMap,e(g.clearcoatMap,m.clearcoatMapTransform)),g.clearcoatRoughnessMap&&(m.clearcoatRoughnessMap.value=g.clearcoatRoughnessMap,e(g.clearcoatRoughnessMap,m.clearcoatRoughnessMapTransform)),g.clearcoatNormalMap&&(m.clearcoatNormalMap.value=g.clearcoatNormalMap,e(g.clearcoatNormalMap,m.clearcoatNormalMapTransform),m.clearcoatNormalScale.value.copy(g.clearcoatNormalScale),g.side===qe&&m.clearcoatNormalScale.value.negate())),g.dispersion>0&&(m.dispersion.value=g.dispersion),g.retroreflectivity>0&&(m.retroreflectivity.value=g.retroreflectivity),g.iridescence>0&&(m.iridescence.value=g.iridescence,m.iridescenceIOR.value=g.iridescenceIOR,m.iridescenceThicknessMinimum.value=g.iridescenceThicknessMinimum,m.iridescenceThicknessMaximum.value=g.iridescenceThicknessMaximum,g.iridescenceMap&&(m.iridescenceMap.value=g.iridescenceMap,e(g.iridescenceMap,m.iridescenceMapTransform)),g.iridescenceThicknessMap&&(m.iridescenceThicknessMap.value=g.iridescenceThicknessMap,e(g.iridescenceThicknessMap,m.iridescenceThicknessMapTransform))),g.transmission>0&&(m.transmission.value=g.transmission,m.transmissionSamplerMap.value=_.texture,m.transmissionSamplerSize.value.set(_.width,_.height),g.transmissionMap&&(m.transmissionMap.value=g.transmissionMap,e(g.transmissionMap,m.transmissionMapTransform)),m.thickness.value=g.thickness,g.thicknessMap&&(m.thicknessMap.value=g.thicknessMap,e(g.thicknessMap,m.thicknessMapTransform)),m.attenuationDistance.value=g.attenuationDistance,m.attenuationColor.value.copy(g.attenuationColor)),g.anisotropy>0&&(m.anisotropyVector.value.set(g.anisotropy*Math.cos(g.anisotropyRotation),g.anisotropy*Math.sin(g.anisotropyRotation)),g.anisotropyMap&&(m.anisotropyMap.value=g.anisotropyMap,e(g.anisotropyMap,m.anisotropyMapTransform))),m.specularIntensity.value=g.specularIntensity,m.specularColor.value.copy(g.specularColor),g.specularColorMap&&(m.specularColorMap.value=g.specularColorMap,e(g.specularColorMap,m.specularColorMapTransform)),g.specularIntensityMap&&(m.specularIntensityMap.value=g.specularIntensityMap,e(g.specularIntensityMap,m.specularIntensityMapTransform))}function p(m,g){g.matcap&&(m.matcap.value=g.matcap)}function x(m,g){let _=t.get(g).light;m.referencePosition.value.setFromMatrixPosition(_.matrixWorld),m.nearDistance.value=_.shadow.camera.near,m.farDistance.value=_.shadow.camera.far}return{refreshFogUniforms:i,refreshMaterialUniforms:s}}function T1(n,t,e,i){let s={},r={};function o(_,y){let S=y.program;i.uniformBlockBinding(_,S,s[_.id])}function a(_,y){let S=s[_.id];S===void 0&&(p(_),S=l(_),s[_.id]=S,_.addEventListener("dispose",m));let T=y.program;i.updateUBOMapping(_,T);let w=t.render.frame;r[_.id]!==w&&(c(_),r[_.id]=w)}function l(_){let y=n.createBuffer(),S=_.__size,T=_.usage;return n.bindBuffer(n.UNIFORM_BUFFER,y),n.bufferData(n.UNIFORM_BUFFER,S,T),n.bindBuffer(n.UNIFORM_BUFFER,null),y}function c(_){let y=s[_.id],S=_.uniforms,T=_.__cache;n.bindBuffer(n.UNIFORM_BUFFER,y);for(let w=0,v=S.length;w<v;w++){let M=S[w];if(Array.isArray(M))for(let D=0,P=M.length;D<P;D++)h(M[D],w,D,T);else h(M,w,0,T)}n.bindBuffer(n.UNIFORM_BUFFER,null)}function h(_,y,S,T){if(d(_,y,S,T)===!0){let w=_.__offset,v=_.value;if(Array.isArray(v)){let M=0;for(let D=0;D<v.length;D++){let P=v[D],A=x(P,!0);u(P,_.__data,M),M+=A.storage/Float32Array.BYTES_PER_ELEMENT}}else u(v,_.__data,0);n.bufferSubData(n.UNIFORM_BUFFER,w,_.__data)}}function u(_,y,S){typeof _=="number"||typeof _=="boolean"?y[S]=_:_.isMatrix3?(y[S]=_.elements[0],y[S+1]=_.elements[1],y[S+2]=_.elements[2],y[S+3]=0,y[S+4]=_.elements[3],y[S+5]=_.elements[4],y[S+6]=_.elements[5],y[S+7]=0,y[S+8]=_.elements[6],y[S+9]=_.elements[7],y[S+10]=_.elements[8],y[S+11]=0):ArrayBuffer.isView(_)?y.set(_.subarray(0,y.length-S),S):_.toArray(y,S)}function d(_,y,S,T){let w=_.value,v=y+"_"+S;if(Array.isArray(w)){T[v]===void 0&&(T[v]=[]);let M=T[v],D=!1;for(let P=0;P<w.length;P++)f(w[P],M,P)===!0&&(D=!0);return D}return f(w,T,v)}function f(_,y,S){if(y[S]===void 0)return typeof _=="number"||typeof _=="boolean"?y[S]=_:ArrayBuffer.isView(_)?y[S]=_.slice():y[S]=_.clone(),!0;{let T=y[S];if(typeof _=="number"||typeof _=="boolean"){if(T!==_)return y[S]=_,!0}else{if(ArrayBuffer.isView(_))return!0;if(T.equals(_)===!1)return T.copy(_),!0}}return!1}function p(_){let y=_.uniforms,S=0,T=16;for(let v=0,M=y.length;v<M;v++){let D=Array.isArray(y[v])?y[v]:[y[v]];for(let P=0,A=D.length;P<A;P++){let C=D[P],U=Array.isArray(C.value)?C.value:[C.value],L=Array.isArray(y[v])||Array.isArray(C.value),B=S;for(let z=0,tt=U.length;z<tt;z++){let W=U[z],J=x(W,L),j=S%T,xt=j%J.boundary,Mt=j+xt;S+=xt,Mt!==0&&T-Mt<J.storage&&(S+=T-Mt),z===0&&(B=S),S+=J.storage}let G=C.type==="int"?Int32Array:C.type==="uint"?Uint32Array:Float32Array;C.__data=new G((S-B)/Float32Array.BYTES_PER_ELEMENT),C.__offset=B}}let w=S%T;return w>0&&(S+=T-w),_.__size=S,_.__cache={},this}function x(_,y){let S={boundary:0,storage:0};return typeof _=="number"||typeof _=="boolean"?(S.boundary=4,S.storage=4):_.isVector2?(S.boundary=8,S.storage=8):_.isVector3||_.isColor?(S.boundary=16,S.storage=12):_.isVector4?(S.boundary=16,S.storage=16):_.isMatrix3?(S.boundary=16,S.storage=48):_.isMatrix4?(S.boundary=16,S.storage=64):_.isTexture?It("WebGLRenderer: Texture samplers can not be part of an uniforms group."):ArrayBuffer.isView(_)?(S.boundary=16,S.storage=_.byteLength):It("WebGLRenderer: Unsupported uniform value type.",_),y===!0&&(S.boundary=16,S.storage=Math.ceil(S.storage/16)*16),S}function m(_){let y=_.target;y.removeEventListener("dispose",m),n.deleteBuffer(s[y.id]),delete s[y.id],delete r[y.id]}function g(){for(let _ in s)n.deleteBuffer(s[_]);s={},r={}}return{bind:o,update:a,dispose:g}}var A1=new Uint16Array([12469,15057,12620,14925,13266,14620,13807,14376,14323,13990,14545,13625,14713,13328,14840,12882,14931,12528,14996,12233,15039,11829,15066,11525,15080,11295,15085,10976,15082,10705,15073,10495,13880,14564,13898,14542,13977,14430,14158,14124,14393,13732,14556,13410,14702,12996,14814,12596,14891,12291,14937,11834,14957,11489,14958,11194,14943,10803,14921,10506,14893,10278,14858,9960,14484,14039,14487,14025,14499,13941,14524,13740,14574,13468,14654,13106,14743,12678,14818,12344,14867,11893,14889,11509,14893,11180,14881,10751,14852,10428,14812,10128,14765,9754,14712,9466,14764,13480,14764,13475,14766,13440,14766,13347,14769,13070,14786,12713,14816,12387,14844,11957,14860,11549,14868,11215,14855,10751,14825,10403,14782,10044,14729,9651,14666,9352,14599,9029,14967,12835,14966,12831,14963,12804,14954,12723,14936,12564,14917,12347,14900,11958,14886,11569,14878,11247,14859,10765,14828,10401,14784,10011,14727,9600,14660,9289,14586,8893,14508,8533,15111,12234,15110,12234,15104,12216,15092,12156,15067,12010,15028,11776,14981,11500,14942,11205,14902,10752,14861,10393,14812,9991,14752,9570,14682,9252,14603,8808,14519,8445,14431,8145,15209,11449,15208,11451,15202,11451,15190,11438,15163,11384,15117,11274,15055,10979,14994,10648,14932,10343,14871,9936,14803,9532,14729,9218,14645,8742,14556,8381,14461,8020,14365,7603,15273,10603,15272,10607,15267,10619,15256,10631,15231,10614,15182,10535,15118,10389,15042,10167,14963,9787,14883,9447,14800,9115,14710,8665,14615,8318,14514,7911,14411,7507,14279,7198,15314,9675,15313,9683,15309,9712,15298,9759,15277,9797,15229,9773,15166,9668,15084,9487,14995,9274,14898,8910,14800,8539,14697,8234,14590,7790,14479,7409,14367,7067,14178,6621,15337,8619,15337,8631,15333,8677,15325,8769,15305,8871,15264,8940,15202,8909,15119,8775,15022,8565,14916,8328,14804,8009,14688,7614,14569,7287,14448,6888,14321,6483,14088,6171,15350,7402,15350,7419,15347,7480,15340,7613,15322,7804,15287,7973,15229,8057,15148,8012,15046,7846,14933,7611,14810,7357,14682,7069,14552,6656,14421,6316,14251,5948,14007,5528,15356,5942,15356,5977,15353,6119,15348,6294,15332,6551,15302,6824,15249,7044,15171,7122,15070,7050,14949,6861,14818,6611,14679,6349,14538,6067,14398,5651,14189,5311,13935,4958,15359,4123,15359,4153,15356,4296,15353,4646,15338,5160,15311,5508,15263,5829,15188,6042,15088,6094,14966,6001,14826,5796,14678,5543,14527,5287,14377,4985,14133,4586,13869,4257,15360,1563,15360,1642,15358,2076,15354,2636,15341,3350,15317,4019,15273,4429,15203,4732,15105,4911,14981,4932,14836,4818,14679,4621,14517,4386,14359,4156,14083,3795,13808,3437,15360,122,15360,137,15358,285,15355,636,15344,1274,15322,2177,15281,2765,15215,3223,15120,3451,14995,3569,14846,3567,14681,3466,14511,3305,14344,3121,14037,2800,13753,2467,15360,0,15360,1,15359,21,15355,89,15346,253,15325,479,15287,796,15225,1148,15133,1492,15008,1749,14856,1882,14685,1886,14506,1783,14324,1608,13996,1398,13702,1183]),Zi=null;function R1(){return Zi===null&&(Zi=new Ui(A1,16,16,Bn,de),Zi.name="DFG_LUT",Zi.minFilter=oe,Zi.magFilter=oe,Zi.wrapS=Gi,Zi.wrapT=Gi,Zi.generateMipmaps=!1,Zi.needsUpdate=!0),Zi}var Ul=class{constructor(t={}){let{canvas:e=Bd(),context:i=null,depth:s=!0,stencil:r=!1,alpha:o=!1,antialias:a=!1,premultipliedAlpha:l=!0,preserveDrawingBuffer:c=!1,powerPreference:h="default",failIfMajorPerformanceCaveat:u=!1,reversedDepthBuffer:d=!1,outputBufferType:f=ti}=t;this.isWebGLRenderer=!0;let p;if(i!==null){if(typeof WebGLRenderingContext<"u"&&i instanceof WebGLRenderingContext)throw new Error("THREE.WebGLRenderer: WebGL 1 is not supported since r163.");p=i.getContextAttributes().alpha}else p=o;let x=f,m=new Set([il,el,tl]),g=new Set([ti,oi,tr,cs,ja,Qa]),_=new Uint32Array(4),y=new Int32Array(4),S={textures:[],min:[],max:[],resolution:[]},T=null,w=null,v=[],M=[],D=null;this.domElement=e,this.debug={checkShaderErrors:!0,diagnostics:{keywords:!1},onShaderError:null},this.autoClear=!0,this.autoClearColor=!0,this.autoClearDepth=!0,this.autoClearStencil=!0,this.sortObjects=!0,this.clippingPlanes=[],this.localClippingEnabled=!1,this.toneMapping=qi,this.toneMappingExposure=1,this.transmissionResolutionScale=1;let P=this,A=!1,C=null,U=null,L=null,B=null;this._outputColorSpace=Re;let G=0,z=0,tt=null,W=-1,J=null,j=new ne,xt=new ne,Mt=null,q=new St(0),ot=0,gt=e.width,wt=e.height,rt=1,Dt=null,ie=null,bt=new ne(0,0,gt,wt),Bt=new ne(0,0,gt,wt),Jt=!1,Wt=new Pn,fe=new ba,Se=!1,Ze=!1,pe=new Gt,fi=new R,Ee=new ne,He={background:null,fog:null,environment:null,overrideMaterial:null,isScene:!0},Mi=!1;function Si(){return tt===null?rt:1}let O=i;function bi(E,F){return e.getContext(E,F)}let xe,_e,mt,Ie,Nt,Ot,I,b,H,it,Q,at,dt,Z,K,ct,N,$,Y,ht,st,lt,Ct;try{let E={alpha:!0,depth:s,stencil:r,antialias:a,premultipliedAlpha:l,preserveDrawingBuffer:c,powerPreference:h,failIfMajorPerformanceCaveat:u};if("setAttribute"in e&&e.setAttribute("data-engine",`three.js r${"187"}`),e.addEventListener("webglcontextlost",Pt,!1),e.addEventListener("webglcontextrestored",Vt,!1),e.addEventListener("webglcontextcreationerror",_t,!1),O===null){let F="webgl2";if(O=bi(F,E),O===null)throw bi(F)?new Error("THREE.WebGLRenderer: Error creating WebGL context with your selected attributes."):new Error("THREE.WebGLRenderer: Error creating WebGL context.")}Xt()}catch(E){throw e.removeEventListener("webglcontextlost",Pt,!1),e.removeEventListener("webglcontextrestored",Vt,!1),e.removeEventListener("webglcontextcreationerror",_t,!1),Ut("WebGLRenderer: "+E.message),E}function Xt(){xe=new zx(O),xe.init(),st=new M1(O,xe),_e=new Rx(O,xe,t,st),mt=new v1(O,xe),_e.reversedDepthBuffer&&d&&mt.buffers.depth.setReversed(!0),U=O.createFramebuffer(),L=O.createFramebuffer(),B=O.createFramebuffer(),Ie=new Vx(O),Nt=new r1,Ot=new y1(O,xe,mt,Nt,_e,st,Ie),I=new Bx(P),b=new Wm(O),lt=new Tx(O,b),H=new kx(O,b,Ie,lt),it=new Wx(O,H,b,lt,Ie),$=new Gx(O,_e,Ot),K=new Cx(Nt),Q=new s1(P,I,xe,_e,lt,K),at=new E1(P,Nt),dt=new a1,Z=new f1(xe),N=new Ex(P,I,mt,it,p,l),ct=new _1(P,it,_e),Ct=new T1(O,Ie,_e,mt),Y=new Ax(O,xe,Ie),ht=new Hx(O,xe,Ie),Ie.programs=Q.programs,P.capabilities=_e,P.extensions=xe,P.properties=Nt,P.renderLists=dt,P.shadowMap=ct,P.state=mt,P.info=Ie}x!==ti&&(D=new qx(x,e.width,e.height,a,s,r));let nt=new Oh(P,O);this.xr=nt,this.getContext=function(){return O},this.getContextAttributes=function(){return O.getContextAttributes()},this.forceContextLoss=function(){let E=xe.get("WEBGL_lose_context");E&&E.loseContext()},this.forceContextRestore=function(){let E=xe.get("WEBGL_lose_context");E&&E.restoreContext()},this.getPixelRatio=function(){return rt},this.setPixelRatio=function(E){E!==void 0&&(rt=E,this.setSize(gt,wt,!1))},this.getSize=function(E){return E.set(gt,wt)},this.setSize=function(E,F,X=!0){if(nt.isPresenting){It("WebGLRenderer: Can't change size while VR device is presenting.");return}gt=E,wt=F,e.width=Math.floor(E*rt),e.height=Math.floor(F*rt),X===!0&&(e.style.width=E+"px",e.style.height=F+"px"),D!==null&&D.setSize(e.width,e.height),this.setViewport(0,0,E,F)},this.getDrawingBufferSize=function(E){return E.set(gt*rt,wt*rt).floor()},this.setDrawingBufferSize=function(E,F,X){gt=E,wt=F,rt=X,e.width=Math.floor(E*X),e.height=Math.floor(F*X),this.setViewport(0,0,E,F)},this.setEffects=function(E){if(x===ti){Ut("WebGLRenderer: setEffects() requires outputBufferType set to HalfFloatType or FloatType.");return}if(E){for(let F=0;F<E.length;F++)if(E[F].isOutputPass===!0){It("WebGLRenderer: OutputPass is not needed in setEffects(). Tone mapping and color space conversion are applied automatically.");break}}D.setEffects(E||[])},this.getCurrentViewport=function(E){return E.copy(j)},this.getViewport=function(E){return E.copy(bt)},this.setViewport=function(E,F,X,k){E.isVector4?bt.set(E.x,E.y,E.z,E.w):bt.set(E,F,X,k),mt.viewport(j.copy(bt).multiplyScalar(Si()).round())},this.getScissor=function(E){return E.copy(Bt)},this.setScissor=function(E,F,X,k){E.isVector4?Bt.set(E.x,E.y,E.z,E.w):Bt.set(E,F,X,k),mt.scissor(xt.copy(Bt).multiplyScalar(Si()).round())},this.getScissorTest=function(){return Jt},this.setScissorTest=function(E){mt.setScissorTest(Jt=E)},this.setOpaqueSort=function(E){Dt=E},this.setTransparentSort=function(E){ie=E},this.getClearColor=function(E){return E.copy(N.getClearColor())},this.setClearColor=function(){N.setClearColor(...arguments)},this.getClearAlpha=function(){return N.getClearAlpha()},this.setClearAlpha=function(){N.setClearAlpha(...arguments)},this.clear=function(E=!0,F=!0,X=!0){let k=0;if(E){let V=!1;if(tt!==null){let ft=tt.texture.format;V=m.has(ft)}if(V){let ft=tt.texture.type,Et=g.has(ft),ut=N.getClearColor(),yt=N.getClearAlpha(),Rt=ut.r,zt=ut.g,Zt=ut.b;Et?(_[0]=Rt,_[1]=zt,_[2]=Zt,_[3]=yt,O.clearBufferuiv(O.COLOR,0,_)):(y[0]=Rt,y[1]=zt,y[2]=Zt,y[3]=yt,O.clearBufferiv(O.COLOR,0,y))}else k|=O.COLOR_BUFFER_BIT}F&&(k|=O.DEPTH_BUFFER_BIT,this.state.buffers.depth.setMask(!0)),X&&(k|=O.STENCIL_BUFFER_BIT,this.state.buffers.stencil.setMask(4294967295)),k!==0&&O.clear(k)},this.clearColor=function(){this.clear(!0,!1,!1)},this.clearDepth=function(){this.clear(!1,!0,!1)},this.clearStencil=function(){this.clear(!1,!1,!0)},this.setNodesHandler=function(E){E.setRenderer(this),C=E},this.dispose=function(){e.removeEventListener("webglcontextlost",Pt,!1),e.removeEventListener("webglcontextrestored",Vt,!1),e.removeEventListener("webglcontextcreationerror",_t,!1),N.dispose(),dt.dispose(),Z.dispose(),Ot.dispose(),Nt.dispose(),I.dispose(),it.dispose(),H.dispose(),lt.dispose(),Ct.dispose(),Q.dispose(),nt.dispose(),nt.removeEventListener("sessionstart",pu),nt.removeEventListener("sessionend",mu),Vn.stop()};function Pt(E){E.preventDefault(),vh("WebGLRenderer: Context Lost."),A=!0}function Vt(){vh("WebGLRenderer: Context Restored."),A=!1;let E=Ie.autoReset,F=ct.enabled,X=ct.autoUpdate,k=ct.needsUpdate,V=ct.type;Xt(),Ie.autoReset=E,ct.enabled=F,ct.autoUpdate=X,ct.needsUpdate=k,ct.type=V}function _t(E){Ut("WebGLRenderer: A WebGL context could not be created. Reason: ",E.statusMessage)}function se(E){let F=E.target;F.removeEventListener("dispose",se),Be(F)}function Be(E){Mn(E),Nt.remove(E)}function Mn(E){let F=Nt.get(E).programs;F!==void 0&&(F.forEach(function(X){Q.releaseProgram(X)}),E.isShaderMaterial&&Q.releaseShaderCache(E))}this.renderBufferDirect=function(E,F,X,k,V,ft){F===null&&(F=He);let Et=V.isMesh&&V.matrixWorld.determinantAffine()<0,ut=Np(E,F,X,k,V);mt.setMaterial(k,Et);let yt=X.index,Rt=1;if(k.wireframe===!0){if(yt=H.getWireframeAttribute(X),yt===void 0)return;Rt=2}let zt=X.drawRange,Zt=X.attributes.position,Tt=zt.start*Rt,Kt=(zt.start+zt.count)*Rt;ft!==null&&(Tt=Math.max(Tt,ft.start*Rt),Kt=Math.min(Kt,(ft.start+ft.count)*Rt)),yt!==null?(Tt=Math.max(Tt,0),Kt=Math.min(Kt,yt.count)):Zt!=null&&(Tt=Math.max(Tt,0),Kt=Math.min(Kt,Zt.count));let Te=Kt-Tt;if(Te<0||Te===1/0)return;lt.setup(V,k,ut,X,yt);let ue,ce=Y;if(yt!==null&&(ue=b.get(yt),ce=ht,ce.setIndex(ue)),V.isMesh)k.wireframe===!0?(mt.setLineWidth(k.wireframeLinewidth*Si()),ce.setMode(O.LINES)):ce.setMode(O.TRIANGLES);else if(V.isLine){let Ke=k.linewidth;Ke===void 0&&(Ke=1),mt.setLineWidth(Ke*Si()),V.isLineSegments?ce.setMode(O.LINES):V.isLineLoop?ce.setMode(O.LINE_LOOP):ce.setMode(O.LINE_STRIP)}else V.isPoints?ce.setMode(O.POINTS):V.isSprite&&ce.setMode(O.TRIANGLES);if(V.isBatchedMesh)if(xe.get("WEBGL_multi_draw"))ce.renderMultiDraw(V._multiDrawStarts,V._multiDrawCounts,V._multiDrawCount);else{let Ke=V._multiDrawStarts,At=V._multiDrawCounts,ni=V._multiDrawCount,te=yt?b.get(yt).bytesPerElement:1,Gn=Nt.get(k).currentProgram.getUniforms();for(let Je=0;Je<ni;Je++)Gn.setValue(O,"_gl_DrawID",Je),ce.render(Ke[Je]/te,At[Je])}else if(V.isInstancedMesh)ce.renderInstances(Tt,Te,V.count);else if(X.isInstancedBufferGeometry){let Ke=X._maxInstanceCount!==void 0?X._maxInstanceCount:1/0,At=Math.min(X.instanceCount,Ke);ce.renderInstances(Tt,Te,At)}else ce.render(Tt,Te)};function fu(E,F,X,k){C!==null&&E.isNodeMaterial&&C.setObject(k,E),Se===!0&&K.setState(E,X,!1),E.transparent===!0&&E.side===De&&E.forceSinglePass===!1?(E.side=qe,E.needsUpdate=!0,xr(E,F,k),E.side=Fn,E.needsUpdate=!0,xr(E,F,k),E.side=De):xr(E,F,k)}this.compile=function(E,F,X=null){X===null&&(X=E),C!==null&&C.renderStart(E,F,X),w=Z.get(X),w.init(F),M.push(w),X.traverseVisible(function(V){V.isLight&&V.layers.test(F.layers)&&(w.pushLight(V),V.castShadow&&w.pushShadow(V))}),E!==X&&E.traverseVisible(function(V){V.isLight&&V.layers.test(F.layers)&&(w.pushLight(V),V.castShadow&&w.pushShadow(V))}),w.setupLights(),C!==null&&C.updateLights(w.state.lightsArray),Ze=this.localClippingEnabled,Se=K.init(this.clippingPlanes,Ze),Se===!0&&K.setGlobalState(this.clippingPlanes,F),C!==null&&ct.render(w.state.shadowsArray,X,F);let k=new Set;return E.traverse(function(V){if(!(V.isMesh||V.isPoints||V.isLine||V.isSprite))return;let ft=V.material;if(ft)if(Array.isArray(ft))for(let Et=0;Et<ft.length;Et++){let ut=ft[Et];fu(ut,X,F,V),k.add(ut)}else fu(ft,X,F,V),k.add(ft)}),w=M.pop(),C!==null&&C.renderEnd(),k},this.compileAsync=function(E,F,X=null){let k=this.compile(E,F,X);return new Promise(V=>{function ft(){if(k.forEach(function(Et){let yt=Nt.get(Et).currentProgram;(yt===void 0||yt.isReady())&&k.delete(Et)}),k.size===0){V(E);return}setTimeout(ft,10)}xe.get("KHR_parallel_shader_compile")!==null?ft():setTimeout(ft,10)})};let yc=null;function Dp(E){yc&&yc(E)}function pu(){Vn.stop()}function mu(){Vn.start()}let Vn=new mf;Vn.setAnimationLoop(Dp),typeof self<"u"&&Vn.setContext(self),this.setAnimationLoop=function(E){yc=E,nt.setAnimationLoop(E),E===null?Vn.stop():Vn.start()},nt.addEventListener("sessionstart",pu),nt.addEventListener("sessionend",mu),this.render=function(E,F){if(F!==void 0&&F.isCamera!==!0){Ut("WebGLRenderer.render: camera is not an instance of THREE.Camera.");return}if(A===!0)return;C!==null&&C.renderStart(E,F);let X=nt.enabled===!0&&nt.isPresenting===!0,k=D!==null&&(tt===null||X)&&D.begin(P,tt);if(E.matrixWorldAutoUpdate===!0&&E.updateMatrixWorld(),F.parent===null&&F.matrixWorldAutoUpdate===!0&&F.updateMatrixWorld(),nt.enabled===!0&&nt.isPresenting===!0&&(D===null||D.isCompositing()===!1)&&(nt.cameraAutoUpdate===!0&&nt.updateCamera(F),F=nt.getCamera()),E.isScene===!0&&E.onBeforeRender(P,E,F,tt),w=Z.get(E,M.length),w.init(F),w.state.textureUnits=Ot.getTextureUnits(),M.push(w),pe.multiplyMatrices(F.projectionMatrix,F.matrixWorldInverse),F.isArrayCamera?fe.setFromArrayCamera(F):Wt.setFromProjectionMatrix(pe,Ei,F.reversedDepth),Ze=this.localClippingEnabled,Se=K.init(this.clippingPlanes,Ze),T=dt.get(E,v.length),T.init(),v.push(T),this.info.render.frame++,nt.enabled===!0&&nt.isPresenting===!0){let Et=P.xr.getDepthSensingMesh();Et!==null&&Mc(Et,F,-1/0,P.sortObjects)}Mc(E,F,0,P.sortObjects),T.finish(),C!==null&&C.updateLights(w.state.lightsArray),P.sortObjects===!0&&T.sort(Dt,ie),Mi=nt.enabled===!1||nt.isPresenting===!1||nt.hasDepthSensing()===!1,Mi&&N.addToRenderList(T,E),this.info.autoReset===!0&&this.info.reset(),Se===!0&&K.beginShadows();let V=w.state.shadowsArray;if(ct.render(V,E,F),Se===!0&&K.endShadows(),(k&&D.hasRenderPass())===!1){let Et=T.opaque,ut=T.transmissive;if(w.setupLights(),F.isArrayCamera){let yt=F.cameras;if(ut.length>0)for(let Rt=0,zt=yt.length;Rt<zt;Rt++){let Zt=yt[Rt];xu(Et,ut,E,Zt)}Mi&&N.render(E);for(let Rt=0,zt=yt.length;Rt<zt;Rt++){let Zt=yt[Rt];gu(T,E,Zt,Zt.viewport)}}else ut.length>0&&xu(Et,ut,E,F),Mi&&N.render(E),gu(T,E,F)}tt!==null&&z===0&&(Ot.updateMultisampleRenderTarget(tt),Ot.updateRenderTargetMipmap(tt)),k&&D.end(P),E.isScene===!0&&E.onAfterRender(P,E,F),lt.resetDefaultState(),W=-1,J=null,M.pop(),M.length>0?(w=M[M.length-1],Ot.setTextureUnits(w.state.textureUnits),Se===!0&&K.setGlobalState(P.clippingPlanes,w.state.camera)):w=null,v.pop(),v.length>0?T=v[v.length-1]:T=null,C!==null&&C.renderEnd()};function Mc(E,F,X,k){if(E.visible===!1)return;if(E.layers.test(F.layers)){if(E.isGroup)X=E.renderOrder;else if(E.isLOD)E.autoUpdate===!0&&E.update(F);else if(E.isLight)w.pushLight(E),E.castShadow&&w.pushShadow(E);else if(E.isSprite){let Et=F.isArrayCamera?fe:Wt;if(!E.frustumCulled||E.intersectsFrustum(Et)){k&&Ee.setFromMatrixPosition(E.matrixWorld).applyMatrix4(pe);let ut=it.update(E),yt=E.material;yt.visible&&T.push(E,ut,yt,X,Ee.z,null,F)}}else if(E.isMesh||E.isLine||E.isPoints){let Et=F.isArrayCamera?fe:Wt;if(!E.frustumCulled||E.intersectsFrustum(Et)){let ut=it.update(E),yt=E.material;if(k&&(E.boundingSphere!==void 0?(E.boundingSphere===null&&E.computeBoundingSphere(),Ee.copy(E.boundingSphere.center)):(ut.boundingSphere===null&&ut.computeBoundingSphere(),Ee.copy(ut.boundingSphere.center)),Ee.applyMatrix4(E.matrixWorld).applyMatrix4(pe)),Array.isArray(yt)){let Rt=ut.groups;for(let zt=0,Zt=Rt.length;zt<Zt;zt++){let Tt=Rt[zt],Kt=yt[Tt.materialIndex];Kt&&Kt.visible&&T.push(E,ut,Kt,X,Ee.z,Tt,F)}}else yt.visible&&T.push(E,ut,yt,X,Ee.z,null,F)}}}let ft=E.children;for(let Et=0,ut=ft.length;Et<ut;Et++)Mc(ft[Et],F,X,k)}function gu(E,F,X,k){let{opaque:V,transmissive:ft,transparent:Et}=E;w.setupLightsView(X),Se===!0&&K.setGlobalState(P.clippingPlanes,X),k&&mt.viewport(j.copy(k)),V.length>0&&Do(V,F,X),ft.length>0&&Do(ft,F,X),Et.length>0&&Do(Et,F,X),mt.buffers.depth.setTest(!0),mt.buffers.depth.setMask(!0),mt.buffers.color.setMask(!0),mt.setPolygonOffset(!1)}function xu(E,F,X,k){if((X.isScene===!0?X.overrideMaterial:null)!==null)return;if(w.state.transmissionRenderTarget[k.id]===void 0){let Tt=xe.has("EXT_color_buffer_half_float")||xe.has("EXT_color_buffer_float");w.state.transmissionRenderTarget[k.id]=new he(1,1,{generateMipmaps:!0,type:Tt?de:ti,minFilter:Bi,samples:Math.max(4,_e.samples),stencilBuffer:r,resolveDepthBuffer:!1,resolveStencilBuffer:!1,storeMultisampledDepthBuffer:!1,storeMultisampledStencilBuffer:!1,colorSpace:qt.workingColorSpace})}let ft=w.state.transmissionRenderTarget[k.id],Et=k.viewport||j;ft.setSize(Et.z*P.transmissionResolutionScale,Et.w*P.transmissionResolutionScale);let ut=P.getRenderTarget(),yt=P.getActiveCubeFace(),Rt=P.getActiveMipmapLevel();P.setRenderTarget(ft),P.getClearColor(q),ot=P.getClearAlpha(),ot<1&&P.setClearColor(16777215,.5),P.clear(),Mi&&N.render(X);let zt=P.toneMapping;P.toneMapping=qi;let Zt=k.viewport;if(k.viewport!==void 0&&(k.viewport=void 0),w.setupLightsView(k),Se===!0&&K.setGlobalState(P.clippingPlanes,k),Do(E,X,k),Ot.updateMultisampleRenderTarget(ft),Ot.updateRenderTargetMipmap(ft),xe.has("WEBGL_multisampled_render_to_texture")===!1){let Tt=!1;for(let Kt=0,Te=F.length;Kt<Te;Kt++){let ue=F[Kt],{object:ce,geometry:Ke,material:At,group:ni}=ue;if(At.side===De&&ce.layers.test(k.layers)){let te=At.side;At.side=qe,At.needsUpdate=!0,_u(ce,X,k,Ke,At,ni),At.side=te,At.needsUpdate=!0,Tt=!0}}Tt===!0&&(Ot.updateMultisampleRenderTarget(ft),Ot.updateRenderTargetMipmap(ft))}P.setRenderTarget(ut,yt,Rt),P.setClearColor(q,ot),Zt!==void 0&&(k.viewport=Zt),P.toneMapping=zt}function Do(E,F,X){let k=F.isScene===!0?F.overrideMaterial:null;for(let V=0,ft=E.length;V<ft;V++){let Et=E[V],{object:ut,geometry:yt,group:Rt}=Et,zt=Et.material;zt.allowOverride===!0&&k!==null&&(zt=k),ut.layers.test(X.layers)&&_u(ut,F,X,yt,zt,Rt)}}function _u(E,F,X,k,V,ft){C!==null&&V.isNodeMaterial&&C.setObject(E,V),E.onBeforeRender(P,F,X,k,V,ft),E.modelViewMatrix.multiplyMatrices(X.matrixWorldInverse,E.matrixWorld),E.normalMatrix.getNormalMatrix(E.modelViewMatrix),V.onBeforeRender(P,F,X,k,E,ft),V.transparent===!0&&V.side===De&&V.forceSinglePass===!1?(V.side=qe,V.needsUpdate=!0,P.renderBufferDirect(X,F,k,V,E,ft),V.side=Fn,V.needsUpdate=!0,P.renderBufferDirect(X,F,k,V,E,ft),V.side=De):P.renderBufferDirect(X,F,k,V,E,ft),E.onAfterRender(P,F,X,k,V,ft)}function vu(E){let F=0;for(let X of E)X.texture!==null&&F++;return F}function xr(E,F,X){F.isScene!==!0&&(F=He);let k=Nt.get(E),V=w.state.lights,ft=w.state.shadowsArray,Et=V.state.version,ut=Q.getParameters(E,V.state,ft,F,X,w.state.lightProbeGridArray),yt=Q.getProgramCacheKey(ut),Rt=k.programs;k.environment=E.isMeshStandardMaterial||E.isMeshLambertMaterial||E.isMeshPhongMaterial?F.environment:null,k.fog=F.fog;let zt=E.isMeshStandardMaterial||E.isMeshLambertMaterial&&!E.envMap||E.isMeshPhongMaterial&&!E.envMap;k.envMap=I.get(E.envMap||k.environment,zt),k.envMapRotation=k.environment!==null&&E.envMap===null?F.environmentRotation:E.envMapRotation,Rt===void 0&&(E.addEventListener("dispose",se),Rt=new Map,k.programs=Rt,k.programVariants=new Map);let Zt=Rt.get(yt);if(Zt!==void 0){if(k.currentProgram===Zt&&k.lightsStateVersion===Et)return Mu(E,ut),Zt}else ut.uniforms=Q.getUniforms(E),C!==null&&E.isNodeMaterial&&C.build(E,X,ut),E.onBeforeCompile(ut,P),Zt=Q.acquireProgram(ut,yt),Rt.set(yt,Zt),k.uniforms=ut.uniforms;let Tt=k.uniforms;return(!E.isShaderMaterial&&!E.isRawShaderMaterial||E.clipping===!0)&&(Tt.clippingPlanes=K.uniform),Mu(E,ut),k.needsLights=Fp(E),k.lightsStateVersion=Et,k.needsLights&&(Tt.ambientLightColor.value=V.state.ambient,Tt.lightProbe.value=V.state.probe,Tt.sunLights.value=V.state.sun,Tt.sunLightShadows.value=V.state.sunShadow,Tt.directionalLights.value=V.state.directional,Tt.directionalLightShadows.value=V.state.directionalShadow,Tt.spotLights.value=V.state.spot,Tt.spotLightShadows.value=V.state.spotShadow,Tt.rectAreaLights.value=V.state.rectArea,Tt.ltc_1.value=V.state.rectAreaLTC1,Tt.ltc_2.value=V.state.rectAreaLTC2,Tt.pointLights.value=V.state.point,Tt.pointLightShadows.value=V.state.pointShadow,Tt.hemisphereLights.value=V.state.hemi,Tt.sunShadowMatrix.value=V.state.sunShadowMatrix,Tt.sunShadowCascade.value=V.state.sunShadowCascade,Tt.directionalShadowMatrix.value=V.state.directionalShadowMatrix,Tt.spotLightMatrix.value=V.state.spotLightMatrix,Tt.spotLightMap.value=V.state.spotLightMap,Tt.pointShadowMatrix.value=V.state.pointShadowMatrix),k.numLightProbeGrids=vu(w.state.lightProbeGridArray),k.currentProgram=Zt,k.uniformsList=null,Zt}function Lp(E,F,X,k,V){let ft=Nt.get(E),Et=ft.currentProgram.id+","+k+","+V,ut=ft.programVariants.get(Et);return ut===void 0?(ut=xr(E,F,X),ft.programVariants.set(Et,ut)):(ft.currentProgram=ut,ft.uniformsList=null,ft.outputColorSpace=k,ft.toneMapping=V),ut}function yu(E){if(E.uniformsList===null){let F=E.currentProgram.getUniforms();E.uniformsList=sr.seqWithValue(F.seq,E.uniforms)}return E.uniformsList}function Mu(E,F){let X=Nt.get(E);X.outputColorSpace=F.outputColorSpace,X.batching=F.batching,X.batchingColor=F.batchingColor,X.instancing=F.instancing,X.instancingColor=F.instancingColor,X.instancingMorph=F.instancingMorph,X.skinning=F.skinning,X.morphTargets=F.morphTargets,X.morphNormals=F.morphNormals,X.morphColors=F.morphColors,X.morphTargetsCount=F.morphTargetsCount,X.numClippingPlanes=F.numClippingPlanes,X.numIntersection=F.numClipIntersection,X.vertexAlphas=F.vertexAlphas,X.vertexTangents=F.vertexTangents,X.toneMapping=F.toneMapping}function Np(E,F,X,k,V){F.isScene!==!0&&(F=He),Ot.resetTextureUnits();let ft=F.fog,Et=k.isMeshStandardMaterial||k.isMeshLambertMaterial||k.isMeshPhongMaterial?F.environment:null,ut=tt===null?P.outputColorSpace:tt.isXRRenderTarget===!0?tt.texture.colorSpace:qt.workingColorSpace,yt=k.isMeshStandardMaterial||k.isMeshLambertMaterial&&!k.envMap||k.isMeshPhongMaterial&&!k.envMap,Rt=I.get(k.envMap||Et,yt),zt=k.vertexColors===!0&&!!X.attributes.color&&X.attributes.color.itemSize===4,Zt=!!X.attributes.tangent&&(!!k.normalMap||k.anisotropy>0),Tt=!!X.morphAttributes.position,Kt=!!X.morphAttributes.normal,Te=!!X.morphAttributes.color,ue=qi;k.toneMapped&&(tt===null||tt.isXRRenderTarget===!0)&&(ue=P.toneMapping);let ce=X.morphAttributes.position||X.morphAttributes.normal||X.morphAttributes.color,Ke=ce!==void 0?ce.length:0,At=Nt.get(k),ni=w.state.lights;if(Se===!0&&(Ze===!0||E!==J)){let ye=E===J&&k.id===W;K.setState(k,E,ye)}let te=!1,Gn=!1;k.version===At.__version?At.needsLights&&At.lightsStateVersion!==ni.state.version||V.isBatchedMesh&&At.batching===!1||!V.isBatchedMesh&&At.batching===!0||V.isBatchedMesh&&At.batchingColor===!0&&V._colorsTexture===null||V.isBatchedMesh&&At.batchingColor===!1&&V._colorsTexture!==null||V.isInstancedMesh&&At.instancing===!1||!V.isInstancedMesh&&At.instancing===!0||V.isSkinnedMesh&&At.skinning===!1||!V.isSkinnedMesh&&At.skinning===!0||V.isInstancedMesh&&At.instancingColor===!0&&V.instanceColor===null||V.isInstancedMesh&&At.instancingColor===!1&&V.instanceColor!==null||V.isInstancedMesh&&At.instancingMorph===!0&&V.morphTexture===null||V.isInstancedMesh&&At.instancingMorph===!1&&V.morphTexture!==null||At.envMap!==Rt||k.fog===!0&&At.fog!==ft||At.numClippingPlanes!==void 0&&(At.numClippingPlanes!==K.numPlanes||At.numIntersection!==K.numIntersection)||At.vertexAlphas!==zt||At.vertexTangents!==Zt||At.morphTargets!==Tt||At.morphNormals!==Kt||At.morphColors!==Te||At.morphTargetsCount!==Ke||At.numLightProbeGrids!==vu(w.state.lightProbeGridArray)?te=!0:(At.outputColorSpace!==ut||At.toneMapping!==ue)&&(Gn=!0):(te=!0,At.__version=k.version);let Je=At.currentProgram;(te===!0||Gn===!0)&&(Je=Gn===!0?Lp(k,F,V,ut,ue):xr(k,F,V),C&&k.isNodeMaterial&&C.onUpdateProgram(k,Je,At));let Lo=!1,Sn=!1,Wn=!1,$t=Je.getUniforms(),Ve=At.uniforms;if(mt.useProgram(Je.program)&&(Lo=!0,Sn=!0,Wn=!0),k.id!==W&&(W=k.id,Sn=!0),Lo||J!==E){mt.buffers.depth.getReversed()&&E.reversedDepth!==!0&&(E._reversedDepth=!0,E.updateProjectionMatrix()),$t.setValue(O,"projectionMatrix",E.projectionMatrix),$t.setValue(O,"viewMatrix",E.matrixWorldInverse);let ze=$t.map.cameraPosition;ze!==void 0&&ze.setValue(O,fi.setFromMatrixPosition(E.matrixWorld)),_e.logarithmicDepthBuffer&&$t.setValue(O,"logDepthBufFC",2/(Math.log(E.far+1)/Math.LN2)),(k.isMeshPhongMaterial||k.isMeshToonMaterial||k.isMeshLambertMaterial||k.isMeshBasicMaterial||k.isMeshStandardMaterial||k.isShaderMaterial)&&$t.setValue(O,"isOrthographic",E.isOrthographicCamera===!0),J!==E&&(J=E,Sn=!0,Wn=!0)}if(At.needsLights&&(ni.state.sunShadowMap.length>0&&$t.setValue(O,"sunShadowMap",ni.state.sunShadowMap,Ot),ni.state.directionalShadowMap.length>0&&$t.setValue(O,"directionalShadowMap",ni.state.directionalShadowMap,Ot),ni.state.spotShadowMap.length>0&&$t.setValue(O,"spotShadowMap",ni.state.spotShadowMap,Ot),ni.state.pointShadowMap.length>0&&$t.setValue(O,"pointShadowMap",ni.state.pointShadowMap,Ot)),V.isSkinnedMesh){$t.setOptional(O,V,"bindMatrix"),$t.setOptional(O,V,"bindMatrixInverse");let ye=V.skeleton;ye&&(ye.boneTexture===null&&ye.computeBoneTexture(),$t.setValue(O,"boneTexture",ye.boneTexture,Ot))}V.isBatchedMesh&&($t.setOptional(O,V,"batchingTexture"),$t.setValue(O,"batchingTexture",V._matricesTexture,Ot),$t.setOptional(O,V,"batchingIdTexture"),$t.setValue(O,"batchingIdTexture",V._indirectTexture,Ot),$t.setOptional(O,V,"batchingColorTexture"),V._colorsTexture!==null&&$t.setValue(O,"batchingColorTexture",V._colorsTexture,Ot));let Ri=X.morphAttributes;if((Ri.position!==void 0||Ri.normal!==void 0||Ri.color!==void 0)&&$.update(V,X,Je),(Sn||At.receiveShadow!==V.receiveShadow)&&(At.receiveShadow=V.receiveShadow,$t.setValue(O,"receiveShadow",V.receiveShadow)),(k.isMeshStandardMaterial||k.isMeshLambertMaterial||k.isMeshPhongMaterial)&&k.envMap===null&&F.environment!==null&&(Ve.envMapIntensity.value=F.environmentIntensity),Ve.dfgLUT!==void 0&&(Ve.dfgLUT.value=R1()),Sn){if($t.setValue(O,"toneMappingExposure",P.toneMappingExposure),At.needsLights&&Up(Ve,Wn),ft&&k.fog===!0&&at.refreshFogUniforms(Ve,ft),at.refreshMaterialUniforms(Ve,k,rt,wt,w.state.transmissionRenderTarget[E.id]),At.needsLights&&At.numLightProbeGrids>0){let ye=S,ze=0;for(let Qi of w.state.lightProbeGridArray)Qi.texture!==null&&(ye.textures[ze]=Qi.texture,ye.min[ze]=Qi.boundingBox.min,ye.max[ze]=Qi.boundingBox.max,ye.resolution[ze]=Qi.resolution,ze++);for(let Qi in ye)ye[Qi].length=ze;Ve.probesSH.value=ye.textures,Ve.probesMin.value=ye.min,Ve.probesMax.value=ye.max,Ve.probesResolution.value=ye.resolution}sr.upload(O,yu(At),Ve,Ot)}if(k.isShaderMaterial&&k.uniformsNeedUpdate===!0&&(sr.upload(O,yu(At),Ve,Ot),k.uniformsNeedUpdate=!1),k.isSpriteMaterial&&$t.setValue(O,"center",V.center),$t.setValue(O,"modelViewMatrix",V.modelViewMatrix),$t.setValue(O,"normalMatrix",V.normalMatrix),$t.setValue(O,"modelMatrix",V.matrixWorld),k.uniformsGroups!==void 0){let ye=k.uniformsGroups;for(let ze=0,Qi=ye.length;ze<Qi;ze++){let bu=ye[ze];Ct.update(bu,Je),Ct.bind(bu,Je)}}return Je}function Up(E,F){E.ambientLightColor.needsUpdate=F,E.lightProbe.needsUpdate=F,E.sunLights.needsUpdate=F,E.sunLightShadows.needsUpdate=F,E.directionalLights.needsUpdate=F,E.directionalLightShadows.needsUpdate=F,E.pointLights.needsUpdate=F,E.pointLightShadows.needsUpdate=F,E.spotLights.needsUpdate=F,E.spotLightShadows.needsUpdate=F,E.rectAreaLights.needsUpdate=F,E.hemisphereLights.needsUpdate=F}function Fp(E){return E.isMeshLambertMaterial||E.isMeshToonMaterial||E.isMeshPhongMaterial||E.isMeshStandardMaterial||E.isShadowMaterial||E.isShaderMaterial&&E.lights===!0}this.getActiveCubeFace=function(){return G},this.getActiveMipmapLevel=function(){return z},this.getRenderTarget=function(){return tt},this.setRenderTargetTextures=function(E,F,X){let k=Nt.get(E);k.__autoAllocateDepthBuffer=E.resolveDepthBuffer===!1,k.__autoAllocateDepthBuffer===!1&&(k.__useRenderToTexture=!1),Nt.get(E.texture).__webglTexture=F,Nt.get(E.depthTexture).__webglTexture=k.__autoAllocateDepthBuffer?void 0:X,k.__hasExternalTextures=!0},this.setRenderTargetFramebuffer=function(E,F){let X=Nt.get(E);X.__webglFramebuffer=F,X.__useDefaultFramebuffer=F===void 0},this.setRenderTarget=function(E,F=0,X=0){tt=E,G=F,z=X;let k=null,V=!1,ft=!1;if(E){let ut=Nt.get(E);if(ut.__useDefaultFramebuffer!==void 0){mt.bindFramebuffer(O.FRAMEBUFFER,ut.__webglFramebuffer),j.copy(E.viewport),xt.copy(E.scissor),Mt=E.scissorTest,mt.viewport(j),mt.scissor(xt),mt.setScissorTest(Mt),W=-1;return}else if(ut.__webglFramebuffer===void 0)Ot.setupRenderTarget(E);else if(ut.__hasExternalTextures)Ot.rebindTextures(E,Nt.get(E.texture).__webglTexture,Nt.get(E.depthTexture).__webglTexture);else if(E.depthBuffer){let zt=E.depthTexture;if(ut.__boundDepthTexture!==zt){if(zt!==null&&Nt.has(zt)&&(E.width!==zt.image.width||E.height!==zt.image.height))throw new Error("THREE.WebGLRenderer: Attached DepthTexture is initialized to the incorrect size.");Ot.setupDepthRenderbuffer(E)}}let yt=E.texture;(yt.isData3DTexture||yt.isDataArrayTexture||yt.isCompressedArrayTexture)&&(ft=!0);let Rt=Nt.get(E).__webglFramebuffer;E.isWebGLCubeRenderTarget?(Array.isArray(Rt[F])?k=Rt[F][X]:k=Rt[F],V=!0):E.samples>0&&Ot.useMultisampledRTT(E)===!1?k=Nt.get(E).__webglMultisampledFramebuffer:Array.isArray(Rt)?k=Rt[X]:k=Rt,j.copy(E.viewport),xt.copy(E.scissor),Mt=E.scissorTest}else j.copy(bt).multiplyScalar(rt).floor(),xt.copy(Bt).multiplyScalar(rt).floor(),Mt=Jt;if(X!==0&&(k=U),mt.bindFramebuffer(O.FRAMEBUFFER,k)&&mt.drawBuffers(E,k),mt.viewport(j),mt.scissor(xt),mt.setScissorTest(Mt),V){let ut=Nt.get(E.texture);O.framebufferTexture2D(O.FRAMEBUFFER,O.COLOR_ATTACHMENT0,O.TEXTURE_CUBE_MAP_POSITIVE_X+F,ut.__webglTexture,X)}else if(ft){let ut=F;for(let yt=0;yt<E.textures.length;yt++){let Rt=Nt.get(E.textures[yt]);O.framebufferTextureLayer(O.FRAMEBUFFER,O.COLOR_ATTACHMENT0+yt,Rt.__webglTexture,X,ut)}}else if(E!==null&&X!==0){let ut=Nt.get(E.texture);O.framebufferTexture2D(O.FRAMEBUFFER,O.COLOR_ATTACHMENT0,O.TEXTURE_2D,ut.__webglTexture,X)}W=-1};function Su(E){let F=Nt.get(E);return(F.__readFormat!==E.format||F.__readType!==E.type)&&(F.__readFormat=E.format,F.__readType=E.type,F.__formatReadable=_e.textureFormatReadable(E.format),F.__typeReadable=_e.textureTypeReadable(E.type)),F}this.readRenderTargetPixels=function(E,F,X,k,V,ft,Et,ut=0){if(!(E&&E.isWebGLRenderTarget)){Ut("WebGLRenderer.readRenderTargetPixels: renderTarget is not THREE.WebGLRenderTarget.");return}let yt=Nt.get(E).__webglFramebuffer;if(E.isWebGLCubeRenderTarget&&Et!==void 0&&(yt=yt[Et],Array.isArray(yt)&&(yt=yt[0])),yt){mt.bindFramebuffer(O.FRAMEBUFFER,yt);try{let Rt=E.textures[ut],zt=Rt.format,Zt=Rt.type;E.textures.length>1&&O.readBuffer(O.COLOR_ATTACHMENT0+ut);let Tt=Su(Rt);if(Tt.__formatReadable===!1){Ut("WebGLRenderer.readRenderTargetPixels: renderTarget is not in RGBA or implementation defined format.");return}if(Tt.__typeReadable===!1){Ut("WebGLRenderer.readRenderTargetPixels: renderTarget is not in UnsignedByteType or implementation defined type.");return}F>=0&&F<=E.width-k&&X>=0&&X<=E.height-V&&O.readPixels(F,X,k,V,st.convert(zt),st.convert(Zt),ft)}finally{let Rt=tt!==null?Nt.get(tt).__webglFramebuffer:null;mt.bindFramebuffer(O.FRAMEBUFFER,Rt)}}},this.readRenderTargetPixelsAsync=async function(E,F,X,k,V,ft,Et,ut=0){if(!(E&&E.isWebGLRenderTarget))throw new Error("THREE.WebGLRenderer.readRenderTargetPixels: renderTarget is not THREE.WebGLRenderTarget.");let yt=Nt.get(E).__webglFramebuffer;if(E.isWebGLCubeRenderTarget&&Et!==void 0&&(yt=yt[Et],Array.isArray(yt)&&(yt=yt[0])),yt)if(F>=0&&F<=E.width-k&&X>=0&&X<=E.height-V){mt.bindFramebuffer(O.FRAMEBUFFER,yt);let Rt=E.textures[ut],zt=Rt.format,Zt=Rt.type;E.textures.length>1&&O.readBuffer(O.COLOR_ATTACHMENT0+ut);let Tt=Su(Rt);if(Tt.__formatReadable===!1)throw new Error("THREE.WebGLRenderer.readRenderTargetPixelsAsync: renderTarget is not in RGBA or implementation defined format.");if(Tt.__typeReadable===!1)throw new Error("THREE.WebGLRenderer.readRenderTargetPixelsAsync: renderTarget is not in UnsignedByteType or implementation defined type.");let Kt=O.createBuffer();O.bindBuffer(O.PIXEL_PACK_BUFFER,Kt),O.bufferData(O.PIXEL_PACK_BUFFER,ft.byteLength,O.STREAM_READ),O.readPixels(F,X,k,V,st.convert(zt),st.convert(Zt),0),O.bindBuffer(O.PIXEL_PACK_BUFFER,null);let Te=tt!==null?Nt.get(tt).__webglFramebuffer:null;mt.bindFramebuffer(O.FRAMEBUFFER,Te);let ue=O.fenceSync(O.SYNC_GPU_COMMANDS_COMPLETE,0);return O.flush(),await kd(O,ue,4),O.bindBuffer(O.PIXEL_PACK_BUFFER,Kt),O.getBufferSubData(O.PIXEL_PACK_BUFFER,0,ft),O.bindBuffer(O.PIXEL_PACK_BUFFER,null),O.deleteBuffer(Kt),O.deleteSync(ue),ft}else throw new Error("THREE.WebGLRenderer.readRenderTargetPixelsAsync: requested read bounds are out of range.")},this.copyFramebufferToTexture=function(E,F=null,X=0){let k=Math.pow(2,-X),V=Math.floor(E.image.width*k),ft=Math.floor(E.image.height*k),Et=F!==null?F.x:0,ut=F!==null?F.y:0;Ot.setTexture2D(E,0),O.copyTexSubImage2D(O.TEXTURE_2D,X,0,0,Et,ut,V,ft),mt.unbindTexture()},this.copyTextureToTexture=function(E,F,X=null,k=null,V=0,ft=0){let Et,ut,yt,Rt,zt,Zt,Tt,Kt,Te,ue=E.isCompressedTexture?E.mipmaps[ft]:E.image;if(X!==null)Et=X.max.x-X.min.x,ut=X.max.y-X.min.y,yt=X.isBox3?X.max.z-X.min.z:1,Rt=X.min.x,zt=X.min.y,Zt=X.isBox3?X.min.z:0;else{let $t=Math.pow(2,-V);Et=Math.floor(ue.width*$t),ut=Math.floor(ue.height*$t),E.isDataArrayTexture?yt=ue.depth:E.isData3DTexture?yt=Math.floor(ue.depth*$t):yt=1,Rt=0,zt=0,Zt=0}k!==null?(Tt=k.x,Kt=k.y,Te=k.z):(Tt=0,Kt=0,Te=0);let ce=st.convert(F.format),Ke=st.convert(F.type),At;F.isData3DTexture?(Ot.setTexture3D(F,0),At=O.TEXTURE_3D):F.isDataArrayTexture||F.isCompressedArrayTexture?(Ot.setTexture2DArray(F,0),At=O.TEXTURE_2D_ARRAY):(Ot.setTexture2D(F,0),At=O.TEXTURE_2D),mt.activeTexture(O.TEXTURE0),mt.pixelStorei(O.UNPACK_FLIP_Y_WEBGL,F.flipY),mt.pixelStorei(O.UNPACK_PREMULTIPLY_ALPHA_WEBGL,F.premultiplyAlpha),mt.pixelStorei(O.UNPACK_ALIGNMENT,F.unpackAlignment);let ni=mt.getParameter(O.UNPACK_ROW_LENGTH),te=mt.getParameter(O.UNPACK_IMAGE_HEIGHT),Gn=mt.getParameter(O.UNPACK_SKIP_PIXELS),Je=mt.getParameter(O.UNPACK_SKIP_ROWS),Lo=mt.getParameter(O.UNPACK_SKIP_IMAGES);mt.pixelStorei(O.UNPACK_ROW_LENGTH,ue.width),mt.pixelStorei(O.UNPACK_IMAGE_HEIGHT,ue.height),mt.pixelStorei(O.UNPACK_SKIP_PIXELS,Rt),mt.pixelStorei(O.UNPACK_SKIP_ROWS,zt),mt.pixelStorei(O.UNPACK_SKIP_IMAGES,Zt);let Sn=E.isDataArrayTexture||E.isData3DTexture,Wn=F.isDataArrayTexture||F.isData3DTexture;if(E.isDepthTexture){let $t=Nt.get(E),Ve=Nt.get(F),Ri=Nt.get($t.__renderTarget),ye=Nt.get(Ve.__renderTarget);mt.bindFramebuffer(O.READ_FRAMEBUFFER,Ri.__webglFramebuffer),mt.bindFramebuffer(O.DRAW_FRAMEBUFFER,ye.__webglFramebuffer);for(let ze=0;ze<yt;ze++)Sn&&(O.framebufferTextureLayer(O.READ_FRAMEBUFFER,O.COLOR_ATTACHMENT0,Nt.get(E).__webglTexture,V,Zt+ze),O.framebufferTextureLayer(O.DRAW_FRAMEBUFFER,O.COLOR_ATTACHMENT0,Nt.get(F).__webglTexture,ft,Te+ze)),O.blitFramebuffer(Rt,zt,Rt+Et,zt+ut,Tt,Kt,Tt+Et,Kt+ut,O.DEPTH_BUFFER_BIT,O.NEAREST);mt.bindFramebuffer(O.READ_FRAMEBUFFER,null),mt.bindFramebuffer(O.DRAW_FRAMEBUFFER,null)}else if(V!==0||E.isRenderTargetTexture||Nt.has(E)){let $t=Nt.get(E),Ve=Nt.get(F);mt.bindFramebuffer(O.READ_FRAMEBUFFER,L),mt.bindFramebuffer(O.DRAW_FRAMEBUFFER,B);for(let Ri=0;Ri<yt;Ri++)Sn?O.framebufferTextureLayer(O.READ_FRAMEBUFFER,O.COLOR_ATTACHMENT0,$t.__webglTexture,V,Zt+Ri):O.framebufferTexture2D(O.READ_FRAMEBUFFER,O.COLOR_ATTACHMENT0,O.TEXTURE_2D,$t.__webglTexture,V),Wn?O.framebufferTextureLayer(O.DRAW_FRAMEBUFFER,O.COLOR_ATTACHMENT0,Ve.__webglTexture,ft,Te+Ri):O.framebufferTexture2D(O.DRAW_FRAMEBUFFER,O.COLOR_ATTACHMENT0,O.TEXTURE_2D,Ve.__webglTexture,ft),V!==0?O.blitFramebuffer(Rt,zt,Rt+Et,zt+ut,Tt,Kt,Tt+Et,Kt+ut,O.COLOR_BUFFER_BIT,O.NEAREST):Wn?O.copyTexSubImage3D(At,ft,Tt,Kt,Te+Ri,Rt,zt,Et,ut):O.copyTexSubImage2D(At,ft,Tt,Kt,Rt,zt,Et,ut);mt.bindFramebuffer(O.READ_FRAMEBUFFER,null),mt.bindFramebuffer(O.DRAW_FRAMEBUFFER,null)}else Wn?E.isDataTexture||E.isData3DTexture?O.texSubImage3D(At,ft,Tt,Kt,Te,Et,ut,yt,ce,Ke,ue.data):F.isCompressedArrayTexture?O.compressedTexSubImage3D(At,ft,Tt,Kt,Te,Et,ut,yt,ce,ue.data):O.texSubImage3D(At,ft,Tt,Kt,Te,Et,ut,yt,ce,Ke,ue):E.isDataTexture?O.texSubImage2D(O.TEXTURE_2D,ft,Tt,Kt,Et,ut,ce,Ke,ue.data):E.isCompressedTexture?O.compressedTexSubImage2D(O.TEXTURE_2D,ft,Tt,Kt,ue.width,ue.height,ce,ue.data):O.texSubImage2D(O.TEXTURE_2D,ft,Tt,Kt,Et,ut,ce,Ke,ue);mt.pixelStorei(O.UNPACK_ROW_LENGTH,ni),mt.pixelStorei(O.UNPACK_IMAGE_HEIGHT,te),mt.pixelStorei(O.UNPACK_SKIP_PIXELS,Gn),mt.pixelStorei(O.UNPACK_SKIP_ROWS,Je),mt.pixelStorei(O.UNPACK_SKIP_IMAGES,Lo),ft===0&&F.generateMipmaps===!0&&F.mipmapsAutoUpdate===!0&&O.generateMipmap(At),mt.unbindTexture()},this.initRenderTarget=function(E){Nt.get(E).__webglFramebuffer===void 0&&Ot.setupRenderTarget(E)},this.initTexture=function(E){E.isCubeTexture?Ot.setTextureCube(E,0):E.isData3DTexture?Ot.setTexture3D(E,0):E.isDataArrayTexture||E.isCompressedArrayTexture?Ot.setTexture2DArray(E,0):Ot.setTexture2D(E,0),mt.unbindTexture()},this.resetState=function(){G=0,z=0,tt=null,mt.reset(),lt.reset()},typeof __THREE_DEVTOOLS__<"u"&&__THREE_DEVTOOLS__.dispatchEvent(new CustomEvent("observe",{detail:this}))}get coordinateSystem(){return Ei}get outputColorSpace(){return this._outputColorSpace}set outputColorSpace(t){this._outputColorSpace=t;let e=this.getContext();e.drawingBufferColorSpace=qt._getDrawingBufferColorSpace(t),e.unpackColorSpace=qt._getUnpackColorSpace()}};var C1=[15432046,9231257,9285362];function zh(n){if(!n?.center)return[];let t=[{position:n.center,kind:"centre"}];for(let e=0;e<3;e++){let i=(n.max[e]-n.min[e])/2,s=Math.max(.32,i),r=[...n.center];r[e]+=s,t.push({position:r,kind:"stretch",axis:e,extent:i})}return t}var ar=class{constructor(){this.group=new ge,this.key=null}clear(){for(let t of this.group.children)t.geometry.dispose(),t.material.dispose();this.group.clear()}update(t){if(!t){this.group.children.length&&this.clear(),this.key=null;return}if(t.key===this.key)return;this.key=t.key,this.clear();let e=(s,r,o=1)=>{if(!s?.length)return;let a=new Float32Array(s.length*3);s.forEach((h,u)=>a.set(h,u*3));let l=new Yt;l.setAttribute("position",new re(a,3));let c=new gi({color:r,opacity:o,transparent:o<1,depthTest:!1,toneMapped:!1});this.group.add(new un(l,c))},i=new St().fromArray(t.color||[.4,.9,.8]);if(e(t.outer,i,.28),e(t.surface,i,.9),t["handles?"]!==!1)for(let{position:s,kind:r,axis:o}of zh(t)){let a=r==="centre",l=a?16767920:C1[o];a||e([t.center,s],l);let c=a?new dn(.11,12,8):new be(.13,.13,.13),h=new Lt(c,new ci({color:l,depthTest:!1,toneMapped:!1}));h.position.fromArray(s),this.group.add(h)}}dispose(){this.clear(),this.key=null}};var Ye=(n,t,e)=>Math.max(t,Math.min(e,n)),Bl=n=>n*Math.PI/180,xo=n=>Array.isArray(n)&&n.length===3&&n.every(Number.isFinite);function Tf(n){return[n?.width,n?.height,n?.depth].map((t,e)=>Number.isFinite(t)&&t>0?t:[12,5,10][e])}function kh(n){let t=Math.hypot(...n);return t>1e-9?n.map(e=>e/t):[0,-1,0]}function zn(n,t){let e=n[t]??n.channels?.[t];return Number.isFinite(e)&&e>=0&&e<=1?e:void 0}function xn(n,t,e){let i=e+"-deg";if(Number.isFinite(t.pose?.[i]))return t.pose[i];let s=zn(t,e);if(s===void 0&&Number.isFinite(t[i]))return t[i];let r=s??0,o=n.calibration?.[e];if(Array.isArray(o)&&o.length===2&&o[1]!==0)return(r*255-o[0])/o[1]*180;let a=o?.["min-deg"]??(e==="pan"?-180:-90),l=o?.["max-deg"]??(e==="pan"?180:90);return a+r*(l-a)}function kn(n,t=[0,0,0]){let[e,i,s]=n;for(let r=0;r<3;r++){let o=Bl(Number(t[r])||0),a=Math.cos(o),l=Math.sin(o);r===0&&([i,s]=[i*a-s*l,i*l+s*a]),r===1&&([e,s]=[e*a+s*l,-e*l+s*a]),r===2&&([e,i]=[e*a-i*l,e*l+i*a])}return[e,i,s]}function Hh(n,t={}){if(xo(t.pose?.direction))return t.pose.direction;if(!(zn(t,"pan")!==void 0&&zn(t,"tilt")!==void 0||Number.isFinite(t.pose?.["pan-deg"])&&Number.isFinite(t.pose?.["tilt-deg"]))){if(xo(t.direction)&&Math.hypot(...t.direction)>1e-9)return kh(t.direction);if(xo(t.aim)&&xo(n.position))return kh(t.aim.map((r,o)=>r-n.position[o]))}let i=Bl(xn(n,t,"pan")),s=Bl(xn(n,t,"tilt"));return kh(kn([Math.sin(i)*Math.cos(s),Math.sin(s),Math.cos(i)*Math.cos(s)],n["rotation-deg"]))}function us(n={}){return xo(n["emitted-color"])?n["emitted-color"].map(t=>Ye(t,0,8)):[0,0,0]}function Vh(n,t={}){let e=n["zoom-range-deg"],i=Ye(Number(t.zoom??t.channels?.zoom)||0,0,1),s=o=>Number.isFinite(o)&&o>0&&o<180,r=Array.isArray(e)&&e.length===2&&e.every(s)?e[0]+i*(e[1]-e[0]):s(n["beam-angle"])?n["beam-angle"]:12+i*34;return Bl(r/2)}function Af(n){let t=n?.fixtures||[];return JSON.stringify({room:n?.room,surfaces:n?.surfaces,reflections:t.filter(e=>e["pseudo-fixture"]).length,fixtures:t.map(e=>({id:e.id,type:e["fixture-type"],aperture:e["lens-radius-m"],diffuser:e["diffuser-radius-m"]&&[e["diffuser-radius-m"],e["pixel-layout-id"],e["pixel-index"],e.position]})),trusses:(n?.trusses||[]).map(e=>({id:e.id,length:e.length}))})}var P1=(n,t,e,i)=>{n.updateMatrixWorld();let s=new Xi;return s.setFromCamera(new et((e-t.left)*2/t.width-1,1-(i-t.top)*2/t.height),n),s.ray};function I1(n){let t=Number(n?.width)||12,e=Number(n?.height)||5,i=Number(n?.depth)||10;return{id:"ceiling",point:[0,e,0],normal:[0,-1,0],"u-axis":[1,0,0],size:[t,i]}}function D1(n,t){let e=new R().fromArray(t.point),i=new R().fromArray(t.normal).normalize(),s=i.dot(n.direction);if(s>=-1e-8)return null;let r=e.clone().sub(n.origin).dot(i)/s;if(!(r>0))return null;let o=n.at(r,new R),a=o.clone().sub(e),l=new R().fromArray(t["u-axis"]).projectOnPlane(i).normalize(),c=new R().crossVectors(i,l);return Math.abs(a.dot(l))>t.size[0]/2+1e-8||Math.abs(a.dot(c))>t.size[1]/2+1e-8?null:{distance:r,point:o.toArray(),normal:i.toArray(),kind:"surface","surface-id":t.id}}function L1(n,t,e){let i=Math.min(40,Number(t.length)||1)/2,s=new R(...kn([1,0,0],t["rotation-deg"])),r=new R().fromArray(t.position),o=n.origin.clone().sub(r),a=n.direction.dot(s),l=n.direction.dot(o),c=s.dot(o),h=1-a*a,u=Math.max(-i,Math.min(i,h>1e-9?(c-a*l)/h:c)),d=r.clone().addScaledVector(s,u),f=n.direction.dot(d.clone().sub(n.origin)),x=n.at(Math.max(0,f),new R).distanceTo(d);return!(f>0)||x>e?null:{distance:f-e,point:d.toArray(),normal:kn([0,-1,0],t["rotation-deg"]),kind:"truss","truss-id":String(t.id)}}function Rf(n,t,e,i,s,r=.25){if(!(e.width>0&&e.height>0))return null;let o=P1(t,e,i,s),a=[];for(let u of[...n?.["receiver-surfaces"]||[],I1(n?.rig?.room)]){let d=D1(o,u);d&&a.push(d)}for(let u of(n?.rig?.trusses||[]).slice(0,24)){if(!Array.isArray(u.position)||u.position.length!==3)continue;let d=L1(o,u,r);d&&a.push(d)}let l=a.sort((u,d)=>u.distance-d.distance)[0];if(!l)return null;let{distance:c,...h}=l;return h}var Cf=n=>({forward:kn([0,0,1],n),up:kn([0,1,0],n)});var _o=new Set(["aim","place","calibrate"]),yo=n=>({xz:1,xy:2,yz:0})[n]??1,N1=n=>JSON.stringify([n?.["runtime-id"],n?.["show-id"],n?.epoch]),we=n=>Array.isArray(n)&&n.length===3&&n.every(t=>Number.isFinite(t)&&Math.abs(t)<=1e4);function lr(n,t,e,i,s="xz",r=0,o=0){if(!(t.width>0&&t.height>0)||!Number.isFinite(r))return null;n.updateMatrixWorld();let a=new Xi;a.setFromCamera(new et(2*(e-t.left)/t.width-1,1-2*(i-t.top)/t.height),n);let l=yo(s),c=new R().setComponent(l,1),h=new R;if(!a.ray.intersectPlane(new si(c,-r),h))return null;let u=h.toArray().map((d,f)=>f===l?r:o>0?Math.round(d/o)*o:d);return we(u)?u:null}function U1(n,t,e,i,s){if(!(e.width>0&&e.height>0))return null;t.updateMatrixWorld();let r=new Xi;r.setFromCamera(new et((i-e.left)*2/e.width-1,1-(s-e.top)*2/e.height),t);let{origin:o,direction:a}=r.ray,l=1/0,c=null;for(let h of n?.["receiver-surfaces"]||[]){let u=new R().fromArray(h.point),d=new R().fromArray(h.normal).normalize(),f=d.dot(a);if(f>=-1e-8)continue;let p=u.clone().sub(o).dot(d)/f;if(p<=0||p>=l)continue;let x=r.ray.at(p,new R),m=x.clone().sub(u),g=new R().fromArray(h["u-axis"]).projectOnPlane(d).normalize(),_=new R().crossVectors(d,g);Math.abs(m.dot(g))<=h.size[0]/2+1e-8&&Math.abs(m.dot(_))<=h.size[1]/2+1e-8&&(l=p,c={point:x.toArray(),"surface-id":h.id})}return c}function vo(n,t,e){if(!we(e))return null;let i=new R(...e).project(n);return i.z>=-1&&i.z<=1?{x:t.left+(i.x+1)*t.width/2,y:t.top+(1-i.y)*t.height/2,depth:i.z}:null}var F1=(n,t,e,i)=>{let s=i.x-e.x,r=i.y-e.y,o=Math.max(0,Math.min(1,((n-e.x)*s+(t-e.y)*r)/(s*s+r*r||1)));return Math.hypot(n-e.x-o*s,t-e.y-o*r)};function O1(n,t,e,i,s){t.updateMatrixWorld();let r=n?.editor||{},o=[],a=(l,c,h=24)=>{let u=vo(t,e,l);if(!u)return;let d=Math.hypot(u.x-i,u.y-s);d<=h&&o.push({...c,position:l,score:d/h+u.depth*.001})};if(r.tool==="sculpt-volume")for(let l of zh(n["sculpt-guide"]))a(l.position,l,22);else if(r.tool==="volume-select")for(let l of n["spatial-volumes"]||[]){let c=n["volume-guides"]?.[l.id],h=(c?.["moving?"]?l.signal?.position||l.position:c?.center)||l.signal?.position||l.position||(we(l.min)&&we(l.max)?l.min.map((u,d)=>(u+l.max[d])/2):null);h&&a(h,{kind:"volumes",id:l.id},32)}else if(r.tool==="pixels"&&r["pixel-mode"]==="pixel"){let l=r["pixel-indices"]||[];(r["pixel-positions"]||[]).forEach((c,h)=>a(c,{kind:"pixels",index:l[h]},22)),a(r["pixel-position"],{kind:"pixels",index:r["selected-pixel"]},22)}else r.tool==="draw"||r.tool==="pixels"?(r.points||[]).slice(0,256).forEach((l,c)=>a(l,{kind:"points",index:r["point-indices"]?.[c]??c},22)):((n?.rig?.fixtures||[]).slice(0,2048).filter(l=>l["pixel-layout-id"]==null).forEach(l=>a(l.position,{kind:"fixtures",id:String(l.id)})),(n?.rig?.trusses||[]).slice(0,24).forEach(l=>{if(!we(l.position))return;let c=kn([Math.min(40,l.length||1)/2,0,0],l["rotation-deg"]),h=vo(t,e,l.position.map((d,f)=>d-c[f])),u=vo(t,e,l.position.map((d,f)=>d+c[f]));if(h&&u){let d=F1(i,s,h,u);d<=18&&o.push({kind:"trusses",id:String(l.id),position:l.position,score:d/18+h.depth*.001+.2})}}));return o.sort((l,c)=>l.score-c.score)[0]||null}var zl=class{constructor(t){this.engine=t,this.canvas=t.canvas,this.abort=new AbortController,this.sequence=0;let e={signal:this.abort.signal,capture:!0};this.canvas.addEventListener("pointerdown",i=>this.down(i),e),this.canvas.addEventListener("pointermove",i=>this.move(i),e),this.canvas.addEventListener("pointerup",i=>this.finish(i,!1),e),this.canvas.addEventListener("pointercancel",i=>this.finish(i,!0),e),this.canvas.addEventListener("lostpointercapture",i=>this.finish(i,!0),e),this.canvas.addEventListener("keydown",i=>{i.key==="Escape"&&this.snapshot?.editor&&(this.cancel(),this.emit("navigate"))},e),globalThis.addEventListener?.("blur",()=>this.cancel(),{signal:this.abort.signal}),globalThis.document?.addEventListener("visibilitychange",()=>{document.hidden&&this.cancel()},{signal:this.abort.signal})}emit(t,e={}){this.canvas.dispatchEvent(new CustomEvent("tolglow-editor",{detail:{...e,type:t,identity:this.identity,token:this.gesture?.token}}))}update(t){let e=N1(t),i=t?.editor,s=this.snapshot?.editor;(this.identity!==e||i?.tool!==s?.tool||i?.["layout-id"]!==s?.["layout-id"]||i?.["pixel-mode"]!==s?.["pixel-mode"]||i?.plane!==s?.plane)&&this.cancel(),this.identity=e,this.snapshot=t;let r=!!(t?.editor&&t.editor.tool!=="navigate");r&&!this.interacting?(this.previousInteraction=!!this.engine.getNavigationState?.().interacting,this.engine.setInteractionActive?.(!0)):!r&&this.interacting&&this.engine.setInteractionActive?.(this.previousInteraction),this.interacting=r,this.canvas.style.cursor=r?["draw","pixels",..._o].includes(t.editor.tool)?"crosshair":"grab":""}down(t){let e=this.snapshot?.editor;if(_o.has(this.gesture?.tool)&&t.pointerId!==this.gesture.pointer){this.gesture.dragged=!0,t.preventDefault(),t.stopImmediatePropagation();return}if(!e||e.tool==="navigate"||t.button!==0||this.gesture)return;if(t.preventDefault(),t.stopImmediatePropagation(),this.canvas.focus(),_o.has(e.tool)){this.gesture={tool:e.tool,token:++this.sequence,pointer:t.pointerId,x:t.clientX,y:t.clientY,dragged:!1},this.canvas.setPointerCapture(t.pointerId);return}if(e.tool==="throw"){let a=lr(this.engine.camera,this.canvas.getBoundingClientRect(),t.clientX,t.clientY,e.plane,e["plane-value"],e.snap);if(!a){this.emit("unreachable");return}this.gesture={tool:"throw",token:++this.sequence,pointer:t.pointerId,point:a,start:t.timeStamp,x:t.clientX,y:t.clientY,plane:e.plane,value:e["plane-value"]},this.canvas.setPointerCapture(t.pointerId);return}let i=this.canvas.getBoundingClientRect(),s=O1(this.snapshot,this.engine.camera,i,t.clientX,t.clientY);if(e.tool==="sculpt-volume"){if(!s)return;let a=s.position[yo(e.plane)],l=lr(this.engine.camera,i,t.clientX,t.clientY,e.plane,a,e.snap),c=vo(this.engine.camera,i,s.position),h=[...s.position];s.axis!=null&&(h[s.axis]+=1);let u=vo(this.engine.camera,i,h);if(!l||!c||!u){this.emit("unreachable");return}let d=u.x-c.x,f=u.y-c.y;if(s.kind==="stretch"&&d*d+f*f<4){this.emit("unreachable");return}this.gesture={token:++this.sequence,tool:e.tool,pointer:t.pointerId,x:t.clientX,y:t.clientY,axis:s.axis,extent:s.extent,value:a,plane:e.plane,snap:e.snap,dx:d,dy:f},this.canvas.setPointerCapture(t.pointerId),this.emit("sculpt-begin",{point:l});return}if(e.tool==="volume-select"){s&&this.emit("select-volume",{"volume-id":s.id});return}if(e.tool==="pixels"&&!s&&!e["append?"])return;if(e.tool!=="draw"&&e.tool!=="pixels"&&!s){this.emit("select",{selection:null});return}let r=s?s.position[yo(e.plane)]:e["plane-value"],o=lr(this.engine.camera,i,t.clientX,t.clientY,e.plane,r,e.snap);if(!o){this.emit("unreachable");return}this.gesture={token:++this.sequence,pointer:t.pointerId,hit:s,point:o,x:t.clientX,value:r,tool:e.tool,offset:s?s.position.map((a,l)=>a-o[l]):[0,0,0]},this.canvas.setPointerCapture(t.pointerId),this.emit("begin",{selection:s,point:o})}move(t){let e=this.gesture;if(!e||t.pointerId!==e.pointer||(t.preventDefault(),t.stopImmediatePropagation(),e.tool==="throw"))return;if(e.tool==="sculpt-volume"){if(e.axis!=null){let r=((t.clientX-e.x)*e.dx+(t.clientY-e.y)*e.dy)/(e.dx*e.dx+e.dy*e.dy),o=e.snap>0?Math.round(r/e.snap)*e.snap:r;this.emit("sculpt-move",{axis:e.axis,delta:o,"half-extent":e.extent})}else{let r=lr(this.engine.camera,this.canvas.getBoundingClientRect(),t.clientX,t.clientY,e.plane,e.value,e.snap);r&&this.emit("sculpt-move",{point:r})}return}if(_o.has(e.tool)){Math.hypot(t.clientX-e.x,t.clientY-e.y)>10&&(e.dragged=!0);return}let i=this.snapshot.editor,s=lr(this.engine.camera,this.canvas.getBoundingClientRect(),t.clientX,t.clientY,i.plane,e.value,i.snap);s&&this.emit("move",{point:e.hit&&["move","draw","pixels"].includes(e.tool)?s.map((r,o)=>r+e.offset[o]):s,angle:(t.clientX-e.x)*.5})}finish(t,e){if(!this.gesture||t.pointerId!==this.gesture.pointer)return;t.preventDefault(),t.stopImmediatePropagation();let i=this.gesture;if(_o.has(i.tool)){if(!e&&!i.dragged&&Math.hypot(t.clientX-i.x,t.clientY-i.y)<=10){let r=this.canvas.getBoundingClientRect();if(i.tool==="place"){let o=Rf(this.snapshot,this.engine.camera,r,t.clientX,t.clientY);this.emit(o?"place":"missed-place",o||{})}else{let o=U1(this.snapshot,this.engine.camera,r,t.clientX,t.clientY);this.emit(o?"surface":"missed-surface",o||{})}}}else if(i.tool==="throw"){if(!e&&Math.hypot(t.clientX-i.x,t.clientY-i.y)>=8){let r=lr(this.engine.camera,this.canvas.getBoundingClientRect(),t.clientX,t.clientY,i.plane,i.value);if(r){let o=Math.max(.08,(t.timeStamp-i.start)/1e3),a=r.map((c,h)=>(c-i.point[h])/o),l=Math.min(1,50/(Math.hypot(...a)||1));this.emit("throw",{point:i.point,velocity:a.map(c=>c*l)})}}}else i.tool==="sculpt-volume"?this.emit(e?"sculpt-cancel":"sculpt-end"):this.emit(e?"cancel":"end");let s=i.pointer;this.gesture=null,this.canvas.hasPointerCapture?.(s)&&this.canvas.releasePointerCapture(s)}cancel(){if(!this.gesture)return;this.emit(this.gesture.tool==="sculpt-volume"?"sculpt-cancel":"cancel");let t=this.gesture.pointer;this.gesture=null,this.canvas.hasPointerCapture?.(t)&&this.canvas.releasePointerCapture(t)}dispose(){this.cancel(),this.abort.abort(),this.interacting&&this.engine.setInteractionActive?.(this.previousInteraction),this.interacting=!1,this.canvas.style.cursor=""}};function B1(n){return we(n.signal?.position)?n.signal.position:we(n.position)?n.position:n.shape==="box"&&we(n.min)&&we(n.max)?n.min.map((t,e)=>(t+n.max[e])/2):null}function z1(n){let t=B1(n),e=n.signal?.radius??n.radius;if(!t)return null;if(n.shape==="box"&&we(n.min)&&we(n.max))return{position:t,scale:n.max.map((r,o)=>Math.max(0,r-n.min[o]))};if(!["ball","sphere","cylinder"].includes(n.shape)||!Number.isFinite(e)||e<=0)return null;if(n.shape==="cylinder")return{position:t,scale:[e,n.height,e]};let i=Math.min(.5,Math.max(0,n.signal?.deformation||0)),s=1/Math.sqrt(1-i);return{position:t,scale:[e*s,e*(1-i),e*s]}}function k1(n){if(n==="box"||n==="cylinder"){let e=n==="box"?new be(1,1,1):new hi(1,1,1,40),i=new es(e);return e.dispose(),i}let t=[];for(let e=0;e<3;e++)for(let i=0;i<48;i++)for(let s of[i,i+1]){let r=[0,0,0];r[(e+1)%3]=Math.cos(s*Math.PI/24),r[(e+2)%3]=Math.sin(s*Math.PI/24),t.push(new R(...r))}return new Yt().setFromPoints(t)}var kl=class{constructor(){this.group=new ge,this.byId=new Map,this.fields=new Map}remove(t){let e=this.byId.get(t);e.geometry.dispose(),e.material.dispose(),this.group.remove(e),this.byId.delete(t)}update(t=[],e){let i=new Set,s=new Set;for(let r of t){let o=e?.[r.id];if(o){let c=this.fields.get(r.id);c||(c=new ar,this.fields.set(r.id,c),this.group.add(c.group)),c.update(o),o["moving?"]&&c.group.position.fromArray(r.signal?.position||r.position),s.add(r.id);continue}let a=z1(r);if(!a)continue;i.add(r.id);let l=this.byId.get(r.id);l&&l.userData.shape!==r.shape&&(this.remove(r.id),l=null),l||(l=new un(k1(r.shape),new gi({color:r.shape==="ball"?16760184:9427403,transparent:!0,opacity:.84,depthTest:!1,toneMapped:!1})),l.userData.shape=r.shape,this.group.add(l),this.byId.set(r.id,l)),l.position.fromArray(a.position),l.scale.fromArray(a.scale),we(r.signal?.rotation)&&l.rotation.set(...r.signal.rotation)}for(let r of this.byId.keys())i.has(r)||this.remove(r);for(let[r,o]of this.fields)s.has(r)||(o.dispose(),this.group.remove(o.group),this.fields.delete(r))}dispose(){this.update([])}};function H1(n){return`${n.columns}x${n.rows}|${n.outline===!1?"closed":"open"}`}function V1(n,t){let e=n+1,i=[];for(let s=0;s<=n;s++)i.push(s);for(let s=1;s<=t;s++)i.push(s*e+n);for(let s=n-1;s>=0;s--)i.push(t*e+s);for(let s=t-1;s>0;s--)i.push(s*e);return i}function G1(n){return!Array.isArray(n)||n.length<3?[0,0,0,0]:[...n.slice(0,3).map(t=>{let e=Math.min(1,Math.max(0,Number(t)||0));return e<=.0031308?12.92*e:1.055*Math.pow(e,1/2.4)-.055}),1]}function W1(n){let t=new Float32Array(n.length*3);return n.forEach((e,i)=>{let s=.35+.65*Math.min(1,Math.max(0,Number(e)||0));t.set([s,s*.86,s*.32],i*3)}),t}function Hl(n,t){return Array.isArray(n)&&n.length%t===0&&n.every(Number.isFinite)}function X1(){return new ae({transparent:!0,depthTest:!1,depthWrite:!1,toneMapped:!1,uniforms:{size:{value:16}},vertexShader:`attribute vec3 markerColor; attribute vec4 headColor; varying vec3 vColor; varying vec4 vHead;
      uniform float size;
      void main() { vColor = markerColor; vHead = headColor;
        gl_Position = projectionMatrix * modelViewMatrix * vec4(position, 1.0); gl_PointSize = size; }`,fragmentShader:`varying vec3 vColor; varying vec4 vHead;
      void main() { float r = length(gl_PointCoord - .5) * 2.; if (r > 1. || (r <= .62 && vHead.a < .5)) discard;
        gl_FragColor = vec4(r > .62 ? vColor : vHead.rgb, 1.); }`})}async function q1(n){let t=await fetch(n,{cache:"no-store"});if(!t.ok)throw new Error(`Video frame ${t.status}`);return createImageBitmap(await t.blob())}var Vl=class{constructor(t=q1){this.group=new ge,this.byId=new Map,this.textures=new Map,this.load=t}texture(t){if(!t)return null;let e=this.textures.get(t);return e||(e={texture:null,users:0,failed:!1},this.textures.set(t,e),this.load(t).then(i=>{if(this.textures.get(t)!==e){i.close?.();return}let s=new We(i);s.flipY=!1,s.colorSpace=Re,s.needsUpdate=!0,e.texture=s;for(let r of this.byId.values())r.wantUrl===t&&this.show(r,t,s);this.releaseTextures()}).catch(()=>{e.failed=!0})),e}show(t,e,i){t.shownUrl=e,t.mesh.material.map=i,t.mesh.material.color.set(16777215),t.mesh.material.needsUpdate=!0}releaseTextures(){let t=new Set;for(let e of this.byId.values())t.add(e.wantUrl),t.add(e.shownUrl);for(let[e,i]of this.textures)t.has(e)||(i.texture?.image?.close?.(),i.texture?.dispose(),this.textures.delete(e))}remove(t){let e=this.byId.get(t);for(let i of[e.mesh,e.edge,e.markers])i.geometry.dispose(),i.material.dispose();this.group.remove(e.root),this.byId.delete(t)}build(t){let e=new ge,i=new Lt(new Yt,new ci({color:5792622,side:De,transparent:!0,opacity:.5,toneMapped:!1,depthWrite:!1})),s=new Nr(new Yt,new gi({color:9427403,transparent:!0,opacity:.7,toneMapped:!1})),r=new ts(new Yt,X1());return e.add(i,s,r),this.group.add(e),{root:e,mesh:i,edge:s,markers:r,wantUrl:null,shownUrl:null}}shape(t,e){let i=H1(e),s=e.positions.length/3;if(t.key!==i||t.mesh.geometry.getAttribute("position")?.count!==s){t.key=i,t.mesh.geometry.dispose(),t.edge.geometry.dispose();let c=t.mesh.geometry=new Yt;c.setAttribute("position",new re(new Float32Array(s*3),3)),c.setAttribute("uv",new re(new Float32Array(s*2),2)),c.setIndex(e.indices),t.border=e.outline===!1?[]:V1(e.columns,e.rows),t.edge.geometry=new Yt,t.edge.geometry.setAttribute("position",new re(new Float32Array(t.border.length*3),3))}let r=t.mesh.geometry,o=r.getAttribute("position"),a=r.getAttribute("uv");o.array.set(e.positions),a.array.set(e.uvs),o.needsUpdate=a.needsUpdate=!0,r.computeBoundingSphere();let l=t.edge.geometry.getAttribute("position");t.border.forEach((c,h)=>l.array.set(e.positions.slice(c*3,c*3+3),h*3)),l.needsUpdate=!0,t.edge.geometry.computeBoundingSphere()}update(t=[],e=null){let i=new Set;for(let s of t||[]){if(!s||!Hl(s.positions,3)||!Hl(s.uvs,2))continue;let r=String(s.id);i.add(r);let o=this.byId.get(r);if(o||(o=this.build(s),this.byId.set(r,o)),o.positions!==s.positions&&(o.positions=s.positions,this.shape(o,s)),o.contacts!==s.contacts){o.contacts=s.contacts;let a=(s.contacts||[]).filter(u=>Hl(u,4)),l=o.markers.geometry;l.getAttribute("position")?.count!==a.length&&(l.dispose(),l=o.markers.geometry=new Yt,l.setAttribute("position",new re(new Float32Array(a.length*3),3)),l.setAttribute("markerColor",new re(new Float32Array(a.length*3),3)),l.setAttribute("headColor",new re(new Float32Array(a.length*4),4)));let c=l.getAttribute("position"),h=l.getAttribute("markerColor");a.forEach((u,d)=>c.array.set(u.slice(0,3),d*3)),h.array.set(W1(a.map(u=>u[3]))),c.needsUpdate=h.needsUpdate=!0,l.computeBoundingSphere(),o.ids=(s["contact-ids"]||[]).filter((u,d)=>Hl(s.contacts[d],4)),o.colored=null}if(o.colored!==e){o.colored=e;let a=o.markers.geometry.getAttribute("headColor");(o.ids||[]).forEach((l,c)=>a.array.set(G1(e?.[l]?.["emitted-color"]),c*4)),a.needsUpdate=!0}if(o.wantUrl!==s.texture){o.wantUrl=s.texture;let a=this.texture(s.texture);a?.texture?this.show(o,s.texture,a.texture):s.texture||(o.shownUrl=null,o.mesh.material.map=null,o.mesh.material.color.set(5792622),o.mesh.material.needsUpdate=!0)}}for(let s of[...this.byId.keys()])i.has(s)||this.remove(s);this.releaseTextures()}dispose(){this.update([])}};var Gl=class{constructor(){this.scene=new Xe,this.rig=new ge,this.volumes=new kl,this.sculpt=new ar,this.videoSurfaces=new Vl,this.scene.add(this.rig,this.volumes.group,this.sculpt.group,this.videoSurfaces.group),this.key=null}clear(){this.rig.traverse(t=>{t.geometry?.dispose(),t.material?.dispose()}),this.rig.clear()}update(t){this.volumes.update(t?.["spatial-volumes"],t?.["volume-guides"]),this.sculpt.update(t?.["sculpt-guide"]),this.videoSurfaces.update(t?.["video-surfaces"],t?.output?.values);let e=t?.editor,i=e?.selection;if(!e){this.key!==null&&(this.clear(),this.key=null);return}this.markerRig!==t.rig&&(this.markerRig=t.rig,this.markers=(t.rig?.fixtures||[]).slice(0,2048).filter(c=>c["pixel-layout-id"]==null).map(c=>c.position).filter(we),this.key=null);let s=t?.rig?.[i?.kind]?.find(c=>String(c.id)===String(i.id)),r=this.markers;if(this.key===e&&this.item===s)return;this.key=e,this.item=s,this.clear();let o=(c,h,u=1)=>{let d=new Yt().setFromPoints(c.map(f=>new R(...f)));this.rig.add(new Qn(d,new gi({color:h,transparent:u<1,opacity:u,depthTest:!1,toneMapped:!1})))},a=(c,h,u)=>{if(!c.length)return;let d=new Yt().setFromPoints(c.map(f=>new R(...f)));this.rig.add(new ts(d,new Vs({color:h,size:u,sizeAttenuation:!1,depthTest:!1,toneMapped:!1})))};a(r,10663625,6),e.tool==="pixels"&&(a(e["pixel-positions"]||[],7923154,5),e["pixel-mode"]==="pixel"&&we(e["pixel-position"])&&a([e["pixel-position"]],16763264,12));let l=(e.points||[]).filter(we).slice(0,256);if(l.length>1&&o(e["closed?"]?[...l,l[0]]:l,11979177),e["pixel-mode"]!=="pixel"&&l.forEach((c,h)=>{let u=new Lt(new dn(h===e["selected-point"]?.11:.075,10,6),new ci({color:h===e["selected-point"]?16763264:11979177,depthTest:!1,toneMapped:!1}));u.position.fromArray(c),this.rig.add(u)}),s&&we(s.position)){let c=new be(i.kind==="trusses"?Math.min(40,s.length||1):.7,.9,.65),h=new es(c);c.dispose();let u=new un(h,new gi({color:16763264,depthTest:!1,toneMapped:!1}));if(u.position.fromArray(s.position),u.rotation.set(...(s["rotation-deg"]||[0,0,0]).map(d=>d*Math.PI/180),"ZYX"),this.rig.add(u),i.kind==="fixtures"&&["mount","place","calibrate"].includes(e.tool)){let{forward:d,up:f}=Cf(s["rotation-deg"]||[0,0,0]),p=(x,m)=>s.position.map((g,_)=>g+x[_]*m);o([s.position,p(d,1.6)],16763264),a([p(d,1.6)],16763264,11),o([s.position,p(f,.55)],7910629)}i.kind==="fixtures"&&e.tool==="calibrate"&&we(e["calibration-target"])&&o([s.position,e["calibration-target"]],7923154,.8)}if(e.tool==="calibrate"&&(a((e["calibration-points"]||[]).filter(we).slice(0,64),11979177,9),we(e["calibration-target"])&&a([e["calibration-target"]],7923154,13)),e.tool==="draw"||e.tool==="pixels"){let c=yo(e.plane),h=[0,1,2].filter(d=>d!==c),u=Number(e["plane-value"])||0;for(let d=-10;d<=10;d++)for(let f=0;f<2;f++){let p=[0,0,0],x=[0,0,0];p[c]=x[c]=u,p[h[f]]=x[h[f]]=d,p[h[1-f]]=-10,x[h[1-f]]=10,o([p,x],11979177,.14)}}}render(t,e){if(!this.rig.children.length&&!this.volumes.group.children.length&&!this.sculpt.group.children.length&&!this.videoSurfaces.group.children.length)return;let i=t.autoClear,s=t.getRenderTarget();try{t.setRenderTarget(null),t.autoClear=!1,t.clearDepth(),t.render(this.scene,e)}finally{t.autoClear=i,t.setRenderTarget(s)}}dispose(){this.clear(),this.volumes.dispose(),this.sculpt.dispose(),this.videoSurfaces.dispose()}};var Pf={type:"change"},Wh={type:"start"},Df={type:"end"},Wl=new hn,If=new si,Y1=Math.cos(70*nr.DEG2RAD),Fe=new R,di=2*Math.PI,ee={NONE:-1,ROTATE:0,DOLLY:1,PAN:2,TOUCH_ROTATE:3,TOUCH_PAN:4,TOUCH_DOLLY_PAN:5,TOUCH_DOLLY_ROTATE:6},Gh=1e-6,Xl=class extends Jr{constructor(t,e=null){super(t,e),this.state=ee.NONE,this.target=new R,this.cursor=new R,this.minDistance=0,this.maxDistance=1/0,this.minZoom=0,this.maxZoom=1/0,this.minTargetRadius=0,this.maxTargetRadius=1/0,this.minPolarAngle=0,this.maxPolarAngle=Math.PI,this.minAzimuthAngle=-1/0,this.maxAzimuthAngle=1/0,this.enableDamping=!1,this.dampingFactor=.05,this.enableZoom=!0,this.zoomSpeed=1,this.enableRotate=!0,this.rotateSpeed=1,this.keyRotateSpeed=1,this.enablePan=!0,this.panSpeed=1,this.screenSpacePanning=!0,this.keyPanSpeed=7,this.zoomToCursor=!1,this.autoRotate=!1,this.autoRotateSpeed=2,this.keys={LEFT:"ArrowLeft",UP:"ArrowUp",RIGHT:"ArrowRight",BOTTOM:"ArrowDown"},this.mouseButtons={LEFT:Un.ROTATE,MIDDLE:Un.DOLLY,RIGHT:Un.PAN},this.touches={ONE:Oi.ROTATE,TWO:Oi.DOLLY_PAN},this.target0=this.target.clone(),this.position0=this.object.position.clone(),this.zoom0=this.object.zoom,this._cursorStyle="auto",this._domElementKeyEvents=null,this._lastPosition=new R,this._lastQuaternion=new Qe,this._lastTargetPosition=new R,this._quat=new Qe().setFromUnitVectors(t.up,new R(0,1,0)),this._quatInverse=this._quat.clone().invert(),this._spherical=new Ks,this._sphericalDelta=new Ks,this._scale=1,this._panOffset=new R,this._rotateStart=new et,this._rotateEnd=new et,this._rotateDelta=new et,this._panStart=new et,this._panEnd=new et,this._panDelta=new et,this._dollyStart=new et,this._dollyEnd=new et,this._dollyDelta=new et,this._dollyDirection=new R,this._mouse=new et,this._performCursorZoom=!1,this._pointers=[],this._pointerPositions={},this._controlActive=!1,this._onPointerMove=K1.bind(this),this._onPointerDown=Z1.bind(this),this._onPointerUp=J1.bind(this),this._onContextMenu=nv.bind(this),this._onMouseWheel=Q1.bind(this),this._onKeyDown=tv.bind(this),this._onTouchStart=ev.bind(this),this._onTouchMove=iv.bind(this),this._onMouseDown=$1.bind(this),this._onMouseMove=j1.bind(this),this._interceptControlDown=sv.bind(this),this._interceptControlUp=rv.bind(this),this.domElement!==null&&this.connect(this.domElement),this.update()}set cursorStyle(t){this._cursorStyle=t,t==="grab"?this.domElement.style.cursor="grab":this.domElement.style.cursor="auto"}get cursorStyle(){return this._cursorStyle}connect(t){super.connect(t),this.domElement.addEventListener("pointerdown",this._onPointerDown),this.domElement.addEventListener("pointercancel",this._onPointerUp),this.domElement.addEventListener("contextmenu",this._onContextMenu),this.domElement.addEventListener("wheel",this._onMouseWheel,{passive:!1}),this.domElement.ownerDocument.addEventListener("keydown",this._interceptControlDown,{passive:!0,capture:!0}),this.domElement.style.touchAction="none"}disconnect(){this.state=ee.NONE,this.domElement.removeEventListener("pointerdown",this._onPointerDown),this.domElement.ownerDocument.removeEventListener("pointermove",this._onPointerMove),this.domElement.ownerDocument.removeEventListener("pointerup",this._onPointerUp),this.domElement.removeEventListener("pointercancel",this._onPointerUp),this.domElement.removeEventListener("wheel",this._onMouseWheel),this.domElement.removeEventListener("contextmenu",this._onContextMenu),this.stopListenToKeyEvents(),this.domElement.ownerDocument.removeEventListener("keydown",this._interceptControlDown,{capture:!0}),this.domElement.ownerDocument.removeEventListener("keyup",this._interceptControlUp,{capture:!0}),this._controlActive=!1,this._pointers.length=0,this._pointerPositions={},this.domElement.style.touchAction="",this.domElement.style.cursor="auto"}dispose(){this.disconnect()}getPolarAngle(){return this._spherical.phi}getAzimuthalAngle(){return this._spherical.theta}getDistance(){return this.object.position.distanceTo(this.target)}listenToKeyEvents(t){t.addEventListener("keydown",this._onKeyDown),this._domElementKeyEvents=t}stopListenToKeyEvents(){this._domElementKeyEvents!==null&&(this._domElementKeyEvents.removeEventListener("keydown",this._onKeyDown),this._domElementKeyEvents=null)}saveState(){this.target0.copy(this.target),this.position0.copy(this.object.position),this.zoom0=this.object.zoom}reset(){this.target.copy(this.target0),this.object.position.copy(this.position0),this.object.zoom=this.zoom0,this.object.updateProjectionMatrix(),this.dispatchEvent(Pf),this.update(),this.state=ee.NONE}pan(t,e){this._pan(t,e),this.update()}dollyIn(t){this._dollyIn(t),this.update()}dollyOut(t){this._dollyOut(t),this.update()}rotateLeft(t){this._rotateLeft(t),this.update()}rotateUp(t){this._rotateUp(t),this.update()}update(t=null){let e=this.object.position;Fe.copy(e).sub(this.target),Fe.applyQuaternion(this._quat),this._spherical.setFromVector3(Fe),this.autoRotate&&this.state===ee.NONE&&this._rotateLeft(this._getAutoRotationAngle(t)),this.enableDamping?(this._spherical.theta+=this._sphericalDelta.theta*this.dampingFactor,this._spherical.phi+=this._sphericalDelta.phi*this.dampingFactor):(this._spherical.theta+=this._sphericalDelta.theta,this._spherical.phi+=this._sphericalDelta.phi);let i=this.minAzimuthAngle,s=this.maxAzimuthAngle;isFinite(i)&&isFinite(s)&&(i<-Math.PI?i+=di:i>Math.PI&&(i-=di),s<-Math.PI?s+=di:s>Math.PI&&(s-=di),i<=s?this._spherical.theta=Math.max(i,Math.min(s,this._spherical.theta)):this._spherical.theta=this._spherical.theta>(i+s)/2?Math.max(i,this._spherical.theta):Math.min(s,this._spherical.theta)),this._spherical.phi=Math.max(this.minPolarAngle,Math.min(this.maxPolarAngle,this._spherical.phi)),this._spherical.makeSafe(),this.enableDamping===!0?this.target.addScaledVector(this._panOffset,this.dampingFactor):this.target.add(this._panOffset),this.target.sub(this.cursor),this.target.clampLength(this.minTargetRadius,this.maxTargetRadius),this.target.add(this.cursor);let r=!1;if(this.zoomToCursor&&this._performCursorZoom||this.object.isOrthographicCamera)this._spherical.radius=this._clampDistance(this._spherical.radius);else{let o=this._spherical.radius;this._spherical.radius=this._clampDistance(this._spherical.radius*this._scale),r=o!=this._spherical.radius}if(Fe.setFromSpherical(this._spherical),Fe.applyQuaternion(this._quatInverse),e.copy(this.target).add(Fe),this.object.lookAt(this.target),this.enableDamping===!0?(this._sphericalDelta.theta*=1-this.dampingFactor,this._sphericalDelta.phi*=1-this.dampingFactor,this._panOffset.multiplyScalar(1-this.dampingFactor)):(this._sphericalDelta.set(0,0,0),this._panOffset.set(0,0,0)),this.zoomToCursor&&this._performCursorZoom){let o=null;if(this.object.isPerspectiveCamera){let a=Fe.length();o=this._clampDistance(a*this._scale);let l=a-o;this.object.position.addScaledVector(this._dollyDirection,l),this.object.updateMatrixWorld(),r=!!l}else if(this.object.isOrthographicCamera){let a=new R(this._mouse.x,this._mouse.y,0);a.unproject(this.object);let l=this.object.zoom;this.object.zoom=Math.max(this.minZoom,Math.min(this.maxZoom,this.object.zoom/this._scale)),this.object.updateProjectionMatrix(),r=l!==this.object.zoom;let c=new R(this._mouse.x,this._mouse.y,0);c.unproject(this.object),this.object.position.sub(c).add(a),this.object.updateMatrixWorld(),o=Fe.length()}else console.warn("WARNING: OrbitControls.js encountered an unknown camera type - zoom to cursor disabled."),this.zoomToCursor=!1;o!==null&&(this.screenSpacePanning?this.target.set(0,0,-1).transformDirection(this.object.matrix).multiplyScalar(o).add(this.object.position):(Wl.origin.copy(this.object.position),Wl.direction.set(0,0,-1).transformDirection(this.object.matrix),Math.abs(this.object.up.dot(Wl.direction))<Y1?this.object.lookAt(this.target):(If.setFromNormalAndCoplanarPoint(this.object.up,this.target),Wl.intersectPlane(If,this.target))))}else if(this.object.isOrthographicCamera){let o=this.object.zoom;this.object.zoom=Math.max(this.minZoom,Math.min(this.maxZoom,this.object.zoom/this._scale)),o!==this.object.zoom&&(this.object.updateProjectionMatrix(),r=!0)}return this._scale=1,this._performCursorZoom=!1,r||this._lastPosition.distanceToSquared(this.object.position)>Gh||8*(1-this._lastQuaternion.dot(this.object.quaternion))>Gh||this._lastTargetPosition.distanceToSquared(this.target)>Gh?(this.dispatchEvent(Pf),this._lastPosition.copy(this.object.position),this._lastQuaternion.copy(this.object.quaternion),this._lastTargetPosition.copy(this.target),!0):!1}_getAutoRotationAngle(t){return t!==null?di/60*this.autoRotateSpeed*t:di/60/60*this.autoRotateSpeed}_getZoomScale(t){let e=Math.abs(t*.01);return Math.pow(.95,this.zoomSpeed*e)}_rotateLeft(t){this._sphericalDelta.theta-=t}_rotateUp(t){this._sphericalDelta.phi-=t}_panLeft(t,e){Fe.setFromMatrixColumn(e,0),Fe.multiplyScalar(-t),this._panOffset.add(Fe)}_panUp(t,e){this.screenSpacePanning===!0?Fe.setFromMatrixColumn(e,1):(Fe.setFromMatrixColumn(e,0),Fe.crossVectors(this.object.up,Fe)),Fe.multiplyScalar(t),this._panOffset.add(Fe)}_pan(t,e){let i=this.domElement;if(this.object.isPerspectiveCamera){let s=this.object.position;Fe.copy(s).sub(this.target);let r=Fe.length();r*=Math.tan(this.object.fov/2*Math.PI/180),this._panLeft(2*t*r/i.clientHeight,this.object.matrix),this._panUp(2*e*r/i.clientHeight,this.object.matrix)}else this.object.isOrthographicCamera?(this._panLeft(t*(this.object.right-this.object.left)/this.object.zoom/i.clientWidth,this.object.matrix),this._panUp(e*(this.object.top-this.object.bottom)/this.object.zoom/i.clientHeight,this.object.matrix)):(console.warn("WARNING: OrbitControls.js encountered an unknown camera type - pan disabled."),this.enablePan=!1)}_dollyOut(t){this.object.isPerspectiveCamera||this.object.isOrthographicCamera?this._scale/=t:(console.warn("WARNING: OrbitControls.js encountered an unknown camera type - dolly/zoom disabled."),this.enableZoom=!1)}_dollyIn(t){this.object.isPerspectiveCamera||this.object.isOrthographicCamera?this._scale*=t:(console.warn("WARNING: OrbitControls.js encountered an unknown camera type - dolly/zoom disabled."),this.enableZoom=!1)}_updateZoomParameters(t,e){if(!this.zoomToCursor)return;this._performCursorZoom=!0;let i=this.domElement.getBoundingClientRect(),s=t-i.left,r=e-i.top,o=i.width,a=i.height;this._mouse.x=s/o*2-1,this._mouse.y=-(r/a)*2+1,this._dollyDirection.set(this._mouse.x,this._mouse.y,1).unproject(this.object).sub(this.object.position).normalize()}_clampDistance(t){return Math.max(this.minDistance,Math.min(this.maxDistance,t))}_handleMouseDownRotate(t){this._rotateStart.set(t.clientX,t.clientY)}_handleMouseDownDolly(t){this._updateZoomParameters(t.clientX,t.clientY),this._dollyStart.set(t.clientX,t.clientY)}_handleMouseDownPan(t){this._panStart.set(t.clientX,t.clientY)}_handleMouseMoveRotate(t){this._rotateEnd.set(t.clientX,t.clientY),this._rotateDelta.subVectors(this._rotateEnd,this._rotateStart).multiplyScalar(this.rotateSpeed);let e=this.domElement;this._rotateLeft(di*this._rotateDelta.x/e.clientHeight),this._rotateUp(di*this._rotateDelta.y/e.clientHeight),this._rotateStart.copy(this._rotateEnd),this.update()}_handleMouseMoveDolly(t){this._dollyEnd.set(t.clientX,t.clientY),this._dollyDelta.subVectors(this._dollyEnd,this._dollyStart),this._dollyDelta.y>0?this._dollyOut(this._getZoomScale(this._dollyDelta.y)):this._dollyDelta.y<0&&this._dollyIn(this._getZoomScale(this._dollyDelta.y)),this._dollyStart.copy(this._dollyEnd),this.zoomToCursor&&(this._performCursorZoom=!0,this._dollyDirection.set(this._mouse.x,this._mouse.y,1).unproject(this.object).sub(this.object.position).normalize()),this.update()}_handleMouseMovePan(t){this._panEnd.set(t.clientX,t.clientY),this._panDelta.subVectors(this._panEnd,this._panStart).multiplyScalar(this.panSpeed),this._pan(this._panDelta.x,this._panDelta.y),this._panStart.copy(this._panEnd),this.update()}_handleMouseWheel(t){this._updateZoomParameters(t.clientX,t.clientY),t.deltaY<0?this._dollyIn(this._getZoomScale(t.deltaY)):t.deltaY>0&&this._dollyOut(this._getZoomScale(t.deltaY)),this.update()}_handleKeyDown(t){let e=!1;switch(t.code){case this.keys.UP:t.ctrlKey||t.metaKey||t.shiftKey?this.enableRotate&&this._rotateUp(di*this.keyRotateSpeed/this.domElement.clientHeight):this.enablePan&&this._pan(0,this.keyPanSpeed),e=!0;break;case this.keys.BOTTOM:t.ctrlKey||t.metaKey||t.shiftKey?this.enableRotate&&this._rotateUp(-di*this.keyRotateSpeed/this.domElement.clientHeight):this.enablePan&&this._pan(0,-this.keyPanSpeed),e=!0;break;case this.keys.LEFT:t.ctrlKey||t.metaKey||t.shiftKey?this.enableRotate&&this._rotateLeft(di*this.keyRotateSpeed/this.domElement.clientHeight):this.enablePan&&this._pan(this.keyPanSpeed,0),e=!0;break;case this.keys.RIGHT:t.ctrlKey||t.metaKey||t.shiftKey?this.enableRotate&&this._rotateLeft(-di*this.keyRotateSpeed/this.domElement.clientHeight):this.enablePan&&this._pan(-this.keyPanSpeed,0),e=!0;break}e&&(t.preventDefault(),this.update())}_handleTouchStartRotate(t){if(this._pointers.length===1)this._rotateStart.set(t.pageX,t.pageY);else{let e=this._getSecondPointerPosition(t),i=.5*(t.pageX+e.x),s=.5*(t.pageY+e.y);this._rotateStart.set(i,s)}}_handleTouchStartPan(t){if(this._pointers.length===1)this._panStart.set(t.pageX,t.pageY);else{let e=this._getSecondPointerPosition(t),i=.5*(t.pageX+e.x),s=.5*(t.pageY+e.y);this._panStart.set(i,s)}}_handleTouchStartDolly(t){let e=this._getSecondPointerPosition(t),i=t.pageX-e.x,s=t.pageY-e.y,r=Math.sqrt(i*i+s*s);this._dollyStart.set(0,r)}_handleTouchStartDollyPan(t){this.enableZoom&&this._handleTouchStartDolly(t),this.enablePan&&this._handleTouchStartPan(t)}_handleTouchStartDollyRotate(t){this.enableZoom&&this._handleTouchStartDolly(t),this.enableRotate&&this._handleTouchStartRotate(t)}_handleTouchMoveRotate(t){if(this._pointers.length==1)this._rotateEnd.set(t.pageX,t.pageY);else{let i=this._getSecondPointerPosition(t),s=.5*(t.pageX+i.x),r=.5*(t.pageY+i.y);this._rotateEnd.set(s,r)}this._rotateDelta.subVectors(this._rotateEnd,this._rotateStart).multiplyScalar(this.rotateSpeed);let e=this.domElement;this._rotateLeft(di*this._rotateDelta.x/e.clientHeight),this._rotateUp(di*this._rotateDelta.y/e.clientHeight),this._rotateStart.copy(this._rotateEnd)}_handleTouchMovePan(t){if(this._pointers.length===1)this._panEnd.set(t.pageX,t.pageY);else{let e=this._getSecondPointerPosition(t),i=.5*(t.pageX+e.x),s=.5*(t.pageY+e.y);this._panEnd.set(i,s)}this._panDelta.subVectors(this._panEnd,this._panStart).multiplyScalar(this.panSpeed),this._pan(this._panDelta.x,this._panDelta.y),this._panStart.copy(this._panEnd)}_handleTouchMoveDolly(t){let e=this._getSecondPointerPosition(t),i=t.pageX-e.x,s=t.pageY-e.y,r=Math.sqrt(i*i+s*s);this._dollyEnd.set(0,r),this._dollyDelta.set(0,Math.pow(this._dollyEnd.y/this._dollyStart.y,this.zoomSpeed)),this._dollyOut(this._dollyDelta.y),this._dollyStart.copy(this._dollyEnd);let o=(t.pageX+e.x)*.5,a=(t.pageY+e.y)*.5;this._updateZoomParameters(o,a)}_handleTouchMoveDollyPan(t){this.enableZoom&&this._handleTouchMoveDolly(t),this.enablePan&&this._handleTouchMovePan(t)}_handleTouchMoveDollyRotate(t){this.enableZoom&&this._handleTouchMoveDolly(t),this.enableRotate&&this._handleTouchMoveRotate(t)}_addPointer(t){this._pointers.push(t.pointerId)}_removePointer(t){delete this._pointerPositions[t.pointerId];for(let e=0;e<this._pointers.length;e++)if(this._pointers[e]==t.pointerId){this._pointers.splice(e,1);return}}_isTrackingPointer(t){for(let e=0;e<this._pointers.length;e++)if(this._pointers[e]==t.pointerId)return!0;return!1}_trackPointer(t){let e=this._pointerPositions[t.pointerId];e===void 0&&(e=new et,this._pointerPositions[t.pointerId]=e),e.set(t.pageX,t.pageY)}_getSecondPointerPosition(t){let e=t.pointerId===this._pointers[0]?this._pointers[1]:this._pointers[0];return this._pointerPositions[e]}_customWheelEvent(t){let e=t.deltaMode,i={clientX:t.clientX,clientY:t.clientY,deltaY:t.deltaY};switch(e){case 1:i.deltaY*=16;break;case 2:i.deltaY*=100;break}return t.ctrlKey&&!this._controlActive&&(i.deltaY*=10),i}};function Z1(n){this.enabled!==!1&&(this._pointers.length===0&&(this.domElement.setPointerCapture(n.pointerId),this.domElement.ownerDocument.addEventListener("pointermove",this._onPointerMove),this.domElement.ownerDocument.addEventListener("pointerup",this._onPointerUp)),!this._isTrackingPointer(n)&&(this._addPointer(n),n.pointerType==="touch"?this._onTouchStart(n):this._onMouseDown(n),this._cursorStyle==="grab"&&(this.domElement.style.cursor="grabbing")))}function K1(n){this.enabled!==!1&&(n.pointerType==="touch"?this._onTouchMove(n):this._onMouseMove(n))}function J1(n){switch(this._removePointer(n),this._pointers.length){case 0:this.domElement.releasePointerCapture(n.pointerId),this.domElement.ownerDocument.removeEventListener("pointermove",this._onPointerMove),this.domElement.ownerDocument.removeEventListener("pointerup",this._onPointerUp),this.dispatchEvent(Df),this.state=ee.NONE,this._cursorStyle==="grab"&&(this.domElement.style.cursor="grab");break;case 1:let t=this._pointers[0],e=this._pointerPositions[t];this._onTouchStart({pointerId:t,pageX:e.x,pageY:e.y});break}}function $1(n){let t;switch(n.button){case 0:t=this.mouseButtons.LEFT;break;case 1:t=this.mouseButtons.MIDDLE;break;case 2:t=this.mouseButtons.RIGHT;break;default:t=-1}switch(t){case Un.DOLLY:if(this.enableZoom===!1)return;this._handleMouseDownDolly(n),this.state=ee.DOLLY;break;case Un.ROTATE:if(n.ctrlKey||n.metaKey||n.shiftKey){if(this.enablePan===!1)return;this._handleMouseDownPan(n),this.state=ee.PAN}else{if(this.enableRotate===!1)return;this._handleMouseDownRotate(n),this.state=ee.ROTATE}break;case Un.PAN:if(n.ctrlKey||n.metaKey||n.shiftKey){if(this.enableRotate===!1)return;this._handleMouseDownRotate(n),this.state=ee.ROTATE}else{if(this.enablePan===!1)return;this._handleMouseDownPan(n),this.state=ee.PAN}break;default:this.state=ee.NONE}this.state!==ee.NONE&&this.dispatchEvent(Wh)}function j1(n){switch(this.state){case ee.ROTATE:if(this.enableRotate===!1)return;this._handleMouseMoveRotate(n);break;case ee.DOLLY:if(this.enableZoom===!1)return;this._handleMouseMoveDolly(n);break;case ee.PAN:if(this.enablePan===!1)return;this._handleMouseMovePan(n);break}}function Q1(n){this.enabled===!1||this.enableZoom===!1||this.state!==ee.NONE||(n.preventDefault(),this.dispatchEvent(Wh),this._handleMouseWheel(this._customWheelEvent(n)),this.dispatchEvent(Df))}function tv(n){this.enabled!==!1&&this._handleKeyDown(n)}function ev(n){switch(this._trackPointer(n),this._pointers.length){case 1:switch(this.touches.ONE){case Oi.ROTATE:if(this.enableRotate===!1){this.state=ee.NONE;break}this._handleTouchStartRotate(n),this.state=ee.TOUCH_ROTATE;break;case Oi.PAN:if(this.enablePan===!1){this.state=ee.NONE;break}this._handleTouchStartPan(n),this.state=ee.TOUCH_PAN;break;default:this.state=ee.NONE}break;case 2:switch(this.touches.TWO){case Oi.DOLLY_PAN:if(this.enableZoom===!1&&this.enablePan===!1)return;this._handleTouchStartDollyPan(n),this.state=ee.TOUCH_DOLLY_PAN;break;case Oi.DOLLY_ROTATE:if(this.enableZoom===!1&&this.enableRotate===!1)return;this._handleTouchStartDollyRotate(n),this.state=ee.TOUCH_DOLLY_ROTATE;break;default:this.state=ee.NONE}break;default:this.state=ee.NONE}this.state!==ee.NONE&&this.dispatchEvent(Wh)}function iv(n){switch(this._trackPointer(n),this.state){case ee.TOUCH_ROTATE:if(this.enableRotate===!1)return;this._handleTouchMoveRotate(n),this.update();break;case ee.TOUCH_PAN:if(this.enablePan===!1)return;this._handleTouchMovePan(n),this.update();break;case ee.TOUCH_DOLLY_PAN:if(this.enableZoom===!1&&this.enablePan===!1)return;this._handleTouchMoveDollyPan(n),this.update();break;case ee.TOUCH_DOLLY_ROTATE:if(this.enableZoom===!1&&this.enableRotate===!1)return;this._handleTouchMoveDollyRotate(n),this.update();break;default:this.state=ee.NONE}}function nv(n){this.enabled!==!1&&n.preventDefault()}function sv(n){n.key==="Control"&&(this._controlActive=!0,this.domElement.ownerDocument.addEventListener("keyup",this._interceptControlUp,{passive:!0,capture:!0}))}function rv(n){n.key==="Control"&&(this._controlActive=!1,this.domElement.ownerDocument.removeEventListener("keyup",this._interceptControlUp,{passive:!0,capture:!0}))}function Lf(n,t){let e=new Xl(n,t);return e.enableDamping=!0,e.dampingFactor=.1,e.minDistance=1,e.maxDistance=45,e.maxPolarAngle=Math.PI*.6,e.enablePan=!0,e.screenSpacePanning=!0,e.touches.ONE=Oi.ROTATE,e.touches.TWO=Oi.DOLLY_PAN,e}function Xh(n,t,e,i){let[s,r,o]=e;if(i==="overview")n.position.set(s*.58,r*1.25,o*.77),t.target.set(0,r*.3,-o*.08);else if(i==="audience")n.position.set(s*.09,r*.48,o*.76),t.target.set(0,r*.38,-o*.28);else throw new Error("Unknown preview camera");t.update()}function Nf(n,t,e,i){return i==="Home"?(Xh(n,t,e,"audience"),!0):["+","=","-"].includes(i)?(t.dollyIn(i==="-"?1.1:.9),!0):!1}function Uf(n){let[t,e]=n.values();return{x:(t.x+e.x)/2,y:(t.y+e.y)/2,span:Math.hypot(e.x-t.x,e.y-t.y)}}var ql=class{points=new Map;pair=null;kind=null;add(t,e,i){this.points.set(t,{x:e,y:i}),this.rebase()}remove(t){this.points.delete(t),this.rebase()}rebase(){this.pair=this.points.size===2?Uf(this.points):null,this.kind=null}sample(){if(!this.pair)return null;let t=Uf(this.points),e=t.x-this.pair.x,i=t.y-this.pair.y,s=Math.abs(t.span-this.pair.span)/2,r=Math.hypot(e,i);if(!this.kind){if(Math.max(r,s)<4)return null;this.kind=s>r?"pinch":"walk"}let o=Math.log(Math.max(1,t.span)/Math.max(1,this.pair.span));return this.pair=t,{kind:this.kind,x:e,y:i,zoom:o}}};var qh={KeyW:"forward",KeyK:"forward",ArrowUp:"forward",KeyS:"backward",KeyJ:"backward",ArrowDown:"backward",KeyA:"left",KeyH:"left",ArrowLeft:"left",KeyD:"right",KeyL:"right",ArrowRight:"right",KeyQ:"down",KeyE:"up"},ov=new Set(Object.values(qh)),Ff=[];function av(n,t,e){let i=t.lengthSq()?13:19,s=-Math.expm1(-i*e),r=n.clone().sub(t).multiplyScalar(s/i).addScaledVector(t,e);return n.lerp(t,s),r}var Yh=n=>Array.isArray(n)&&n.length===3&&n.every(Number.isFinite),Bf=n=>[n?.[0]||12,n?.[1]||5,n?.[2]||10].map(t=>Ye(t,1,100)),lv=n=>n?.isContentEditable||/^(INPUT|TEXTAREA|SELECT)$/.test(n?.tagName||""),Of=(n,t,e)=>`${n},${t},${e}`;function zf(n){return new Map(n.map(t=>[String(t.id),t]))}function cv(n,t=zf(n?.rig?.fixtures||[]),e){let i=n?.output?.values||{},s=[];for(let r of e||Object.keys(i)){let o=t.get(r)?.position;if(!Yh(o))continue;let a=us(i[r]),l=a[0]*.2126+a[1]*.7152+a[2]*.0722;l>1e-4&&s.push({id:r,position:o.slice(),energy:l})}return s.sort((r,o)=>o.energy-r.energy||r.id.localeCompare(o.id)).slice(0,16)}function kf(n,t,e=[],i=n){let[s,r,o]=Bf(t),a=[-s/2+.25,.2,-o/2+.25],l=[s/2-.25,r-.2,o/2-.25],c=n.map((h,u)=>Ye(h,Math.min(a[u],i[u]),Math.max(l[u],i[u])));for(let h of e){if(!Yh(h.position))continue;let u=c.map((f,p)=>f-h.position[p]),d=Math.hypot(...u);if(d<.45){let f=d>1e-5?u.map(p=>p/d):[0,-1,0];c=h.position.map((p,x)=>p+f[x]*.45)}}return c.map((h,u)=>Ye(h,Math.min(a[u],i[u]),Math.max(l[u],i[u])))}function hv(n,t,e=0){let[i,s,r]=Bf(t);if(!n)return{position:[i*.28,s*.55,r*.38],target:[0,s*.4,-r*.12]};let[o,a,l]=n.position,c=Math.atan2(-l||1,-o)+Math.sin(e)*.32,h=Ye(Math.min(i,r)*.22,1.1,2.4);return{position:kf([o+Math.cos(c)*h,Ye(a-.65,.5,s-.4),l+Math.sin(c)*h],t,[],[0,s/2,0]),target:n.position.slice()}}var Yl=class{constructor({camera:t,controls:e,canvas:i,room:s,snapshot:r,visibleLights:o,active:a,reducedMotion:l,changed:c=()=>{}}){Object.assign(this,{camera:t,controls:e,canvas:i,room:s,snapshot:r,visibleLights:o,active:a,reducedMotion:l,changed:c}),this.mode="orbit",this.speed=2,this.keys=new Set,this.buttons=new Set,this.listeners=new Set,this.velocity=new R,this.swipe=new et,this.look=new et,this.gesture=new ql,this.zoom=0,this.interacting=!1,this.disposed=!1,this.clock=0,this.subjectId=null,this.visitStarted=0,this.abort=new AbortController;let h=this.abort.signal;i.addEventListener("keydown",u=>this.keydown(u),{signal:h}),i.addEventListener("keyup",u=>{this.keys.delete(u.code),this.boost=u.shiftKey},{signal:h}),i.addEventListener("blur",()=>this.clearInput(),{signal:h}),window.addEventListener("blur",()=>this.clearInput(),{signal:h}),document.addEventListener("visibilitychange",()=>{this.clearInput(),this.lastTime=null},{signal:h}),i.addEventListener("pointerdown",u=>this.pointerDown(u),{signal:h,capture:!0}),i.addEventListener("pointermove",u=>this.pointerMove(u),{signal:h,capture:!0});for(let u of["pointerup","pointercancel","lostpointercapture"])i.addEventListener(u,d=>this.pointerUp(d),{signal:h,capture:!0});i.addEventListener("wheel",u=>this.wheel(u),{signal:h,capture:!0,passive:!1}),this.animate=u=>{this.frame=null,!(this.disposed||!this.active())&&(this.tick(u),this.refreshActivity())},this.refreshActivity()}refreshActivity(){this.disposed||!this.active()?(this.frame!=null&&cancelAnimationFrame(this.frame),this.frame=null,this.lastTime=null,this.clearInput(),this.controls.enabled=!1):this.frame==null&&(this.controls.enabled=this.mode==="orbit"&&!this.interacting,this.frame=requestAnimationFrame(this.animate))}state(){return{mode:this.mode,speed:this.speed,interacting:this.interacting,"reduced-motion":!!this.reducedMotion(),"tour-fixture":this.subjectId,"tour-waiting":this.mode==="tour"&&!this.subjectId}}notify(){let t=this.state(),e=JSON.stringify(t);if(e!==this.lastNotice){this.lastNotice=e;for(let i of this.listeners)i(t)}}subscribe(t){return this.listeners.add(t),t(this.state()),()=>this.listeners.delete(t)}setMode(t,e=!1){if(!["orbit","walk","tour"].includes(t))throw new Error("Unknown camera navigation mode");this.clearInput(e),this.mode=t,this.lastTime=null,this.subjectId=null,this.visitStarted=this.clock,this.controls.enabled=t==="orbit"&&!this.interacting,this.notify()}setSpeed(t){if(!Number.isFinite(t)||t<.25||t>8)throw new Error("Camera speed must be 0.25\u20138 metres per second");this.speed=t,this.notify()}setInteractionActive(t){this.interacting=!!t,this.clearInput(),this.lastTime=null,this.controls.enabled=this.mode==="orbit"&&!this.interacting,this.notify()}clearInput(t=!1){if(!t){let e=[...this.gesture.points.keys()];this.gesture.points.clear(),this.gesture.rebase();for(let i of e)this.canvas.hasPointerCapture?.(i)&&this.canvas.releasePointerCapture(i)}this.velocity.set(0,0,0),this.swipe.set(0,0),this.look.set(0,0),this.zoom=0,this.keys.clear(),this.buttons.clear(),this.boost=!1,this.pointer&&this.canvas.hasPointerCapture?.(this.pointer.id)&&this.canvas.releasePointerCapture(this.pointer.id),this.pointer=null}move(t,e){if(!ov.has(t))throw new Error("Unknown camera direction");if(!e){this.buttons.delete(t);return}this.interacting||!this.active()||(this.mode!=="walk"&&this.setMode("walk"),this.buttons.add(t))}keydown(t){if(!(lv(t.target)||t.altKey||t.metaKey||t.ctrlKey||this.interacting||!this.active())){if(qh[t.code]){t.preventDefault(),this.mode!=="walk"&&this.setMode("walk"),this.keys.add(t.code),this.boost=t.shiftKey;return}if(t.key==="Escape"){this.setMode("orbit");return}["+","=","-","Home"].includes(t.key)&&(this.mode!=="orbit"&&this.setMode("orbit"),Nf(this.camera,this.controls,this.room(),t.key)&&(t.preventDefault(),this.changed()))}}queueSwipe(t,e){let i=this.speed*2/Math.max(1,this.canvas.clientHeight);this.swipe.x+=t*i,this.swipe.y-=e*i}wheel(t){if(!this.active()||this.interacting||t.altKey||t.metaKey||!t.ctrlKey&&t.deltaMode!==0&&this.mode==="orbit")return;if(t.preventDefault(),t.stopImmediatePropagation(),t.ctrlKey){this.mode==="tour"&&this.setMode("orbit"),this.zoom-=Ye(t.deltaY*.01,-1,1);return}this.mode!=="walk"&&this.setMode("walk");let e=t.deltaMode===1?16:t.deltaMode===2?this.canvas.clientHeight:1;this.queueSwipe(t.deltaX*e,t.deltaY*e)}pointerDown(t){if(!(this.interacting||!this.active()||t.button!==0)){if(this.canvas.focus({preventScroll:!0}),this.mode==="tour"&&this.setMode("walk"),t.pointerType==="touch"){t.preventDefault(),t.stopImmediatePropagation(),this.gesture.add(t.pointerId,t.clientX,t.clientY),this.canvas.setPointerCapture(t.pointerId);return}this.mode!=="walk"||this.pointer||(t.preventDefault(),t.stopImmediatePropagation(),this.pointer={id:t.pointerId,x:t.clientX,y:t.clientY},this.canvas.setPointerCapture(t.pointerId))}}pointerMove(t){let e=this.gesture.points.get(t.pointerId),i=e||(this.pointer?.id===t.pointerId?this.pointer:null);!i||this.interacting||(t.preventDefault(),t.stopImmediatePropagation(),(!e||this.gesture.points.size===1)&&(this.look.x+=t.clientX-i.x,this.look.y+=t.clientY-i.y),i.x=t.clientX,i.y=t.clientY)}pointerUp(t){let e=this.gesture.points.has(t.pointerId);if(!e&&this.pointer?.id!==t.pointerId)return;t.stopImmediatePropagation();let i=t.type!=="pointerup";e?(i||this.sampleGesture(),this.gesture.remove(t.pointerId)):this.pointer=null,i&&(this.swipe.set(0,0),this.zoom=0,this.look.set(0,0)),this.canvas.hasPointerCapture?.(t.pointerId)&&this.canvas.releasePointerCapture(t.pointerId)}sampleGesture(){let t=this.gesture.sample();if(t){if(t.kind==="pinch"){this.zoom+=t.zoom;return}this.mode!=="walk"&&this.setMode("walk",!0),this.queueSwipe(t.x,t.y)}}updateView(t,e){let i=this.reducedMotion()?1:-Math.expm1(-24*t);if(Math.abs(this.zoom)>1e-4){let o=this.zoom*i;this.zoom-=o,this.mode==="orbit"?this.controls.dollyIn(Math.exp(-o)):this.translate(this.camera.getWorldDirection(new R).multiplyScalar(o*this.speed),e),this.changed()}if(this.look.lengthSq()<1e-4)return;let s=this.look.x*i,r=this.look.y*i;if(this.look.multiplyScalar(1-i),this.mode==="orbit"){let o=2*Math.PI/Math.max(1,this.canvas.clientHeight);this.controls.rotateLeft(s*o),this.controls.rotateUp(r*o)}else{let o=new Ti().setFromQuaternion(this.camera.quaternion,"YXZ");o.y-=s*.004,o.x=Ye(o.x-r*.004,-Math.PI*.48,Math.PI*.48),this.camera.quaternion.setFromEuler(o),this.controls.target.copy(this.camera.position).add(this.camera.getWorldDirection(new R).multiplyScalar(2))}this.changed()}walk(t,e){if(!this.keys.size&&!this.buttons.size&&this.velocity.lengthSq()<1e-7&&this.swipe.lengthSq()<1e-9)return;let i=new Set([...this.buttons,...Array.from(this.keys,h=>qh[h])]),s=new R(Number(i.has("right"))-Number(i.has("left")),Number(i.has("up"))-Number(i.has("down")),Number(i.has("forward"))-Number(i.has("backward")));s.normalize().multiplyScalar(this.speed*(this.boost?2:1));let r=av(this.velocity,s,t),o=this.reducedMotion()?1:-Math.expm1(-24*t);r.x+=this.swipe.x*o,r.z+=this.swipe.y*o,this.swipe.multiplyScalar(1-o);let a=this.camera.getWorldDirection(new R);a.y=0,a.normalize();let l=new R().crossVectors(a,this.camera.up).normalize(),c=a.multiplyScalar(r.z).addScaledVector(l,r.x);c.y=r.y,this.translate(c,e),this.changed()}tick(t){let e=this.active()&&!this.interacting;!e&&this.wasActive&&this.clearInput(),this.wasActive=e;let i=e&&this.lastTime!=null?Ye((t-this.lastTime)/1e3,0,.05):0;if(this.lastTime=e?t:null,this.controls.enabled=e&&this.mode==="orbit",!e||(this.sampleGesture(),!i))return;if(this.mode==="orbit"){this.updateView(i,Ff),this.controls.update(i);return}let s=this.snapshot(),r=s?.rig?.fixtures||Ff;if(this.updateView(i,r),this.mode==="walk"){this.walk(i,r);return}if(this.reducedMotion())return;this.clock+=i;let o=this.visibleLights?.();(s!==this.subjectSnapshot||o!==this.subjectLights)&&(r!==this.subjectFixtures&&(this.subjectFixtures=r,this.fixtureIndex=zf(r)),this.subjectLights=o,this.subjectSnapshot=s,this.subjects=cv(s,this.fixtureIndex,o));let a=this.subjects||[],l=a.find(f=>f.id===this.subjectId);if(!l||this.clock-this.visitStarted>=8){let f=l?(a.indexOf(l)+1)%a.length:0;l=a[f],this.subjectId=l?.id||null,this.visitStarted=this.clock,this.notify()}let c=hv(l,this.room(),this.clock*.22),u=new R(...c.position).sub(this.camera.position),d=u.length();d>0&&u.multiplyScalar(Math.min(1,this.speed*i/d)),this.translate(u,r),this.controls.target.lerp(new R(...c.target),1-Math.exp(-i*2)),this.camera.lookAt(this.controls.target),this.changed()}translate(t,e){if(e!==this.collisionFixtures){this.collisionFixtures=e,this.collisionGrid=new Map;for(let r of e)if(Yh(r.position)){let o=Of(...r.position.map(Math.floor));this.collisionGrid.has(o)||this.collisionGrid.set(o,[]),this.collisionGrid.get(o).push(r)}}let i=Math.max(1,Math.ceil(t.length()/.15)),s=t.clone().divideScalar(i);for(let r=0;r<i;r++){let o=this.camera.position.clone(),a=o.clone().add(s),[l,c,h]=a.toArray().map(Math.floor),u=[];for(let p=-1;p<=1;p++)for(let x=-1;x<=1;x++)for(let m=-1;m<=1;m++)u.push(...this.collisionGrid.get(Of(l+p,c+x,h+m))||[]);let f=new R(...kf(a.toArray(),this.room(),u,o.toArray())).sub(o).clampLength(0,s.length());this.camera.position.copy(o).add(f),this.controls.target.add(this.camera.position.clone().sub(o))}}dispose(){this.disposed||(this.disposed=!0,this.clearInput(),this.abort.abort(),cancelAnimationFrame(this.frame),this.listeners.clear())}};var Zl=class extends Xe{constructor(){super(),this.name="RoomEnvironment",this.position.y=-3.5;let t=new be;t.deleteAttribute("uv");let e=new Pe({side:qe}),i=new Pe,s=new qr(16777215,900,28,2);s.position.set(.418,16.199,.3),this.add(s);let r=new Lt(t,e);r.position.set(-.757,13.219,.717),r.scale.set(31.713,28.305,28.591),this.add(r);let o=new jn(t,i,6),a=new ve;a.position.set(-10.906,2.009,1.846),a.rotation.set(0,-.195,0),a.scale.set(2.328,7.905,4.651),a.updateMatrix(),o.setMatrixAt(0,a.matrix),a.position.set(-5.607,-.754,-.758),a.rotation.set(0,.994,0),a.scale.set(1.97,1.534,3.955),a.updateMatrix(),o.setMatrixAt(1,a.matrix),a.position.set(6.167,.857,7.803),a.rotation.set(0,.561,0),a.scale.set(3.927,6.285,3.687),a.updateMatrix(),o.setMatrixAt(2,a.matrix),a.position.set(-2.017,.018,6.124),a.rotation.set(0,.333,0),a.scale.set(2.002,4.566,2.064),a.updateMatrix(),o.setMatrixAt(3,a.matrix),a.position.set(2.291,-.756,-2.621),a.rotation.set(0,-.286,0),a.scale.set(1.546,1.552,1.496),a.updateMatrix(),o.setMatrixAt(4,a.matrix),a.position.set(-2.193,-.369,-5.547),a.rotation.set(0,.516,0),a.scale.set(3.875,3.487,2.986),a.updateMatrix(),o.setMatrixAt(5,a.matrix),this.add(o);let l=new Lt(t,cr(50));l.position.set(-16.116,14.37,8.208),l.scale.set(.1,2.428,2.739),this.add(l);let c=new Lt(t,cr(50));c.position.set(-16.109,18.021,-8.207),c.scale.set(.1,2.425,2.751),this.add(c);let h=new Lt(t,cr(17));h.position.set(14.904,12.198,-1.832),h.scale.set(.15,4.265,6.331),this.add(h);let u=new Lt(t,cr(43));u.position.set(-.462,8.89,14.52),u.scale.set(4.38,5.441,.088),this.add(u);let d=new Lt(t,cr(20));d.position.set(3.235,11.486,-12.541),d.scale.set(2.5,2,.1),this.add(d);let f=new Lt(t,cr(100));f.position.set(0,20,0),f.scale.set(1,.1,1),this.add(f)}dispose(){let t=new Set;this.traverse(e=>{e.isMesh&&(t.add(e.geometry),t.add(e.material))});for(let e of t)e.dispose()}};function cr(n){return new Wr({color:0,emissive:16777215,emissiveIntensity:n})}var Hn={name:"CopyShader",uniforms:{tDiffuse:{value:null},opacity:{value:1}},vertexShader:`

		varying vec2 vUv;

		void main() {

			vUv = uv;
			gl_Position = projectionMatrix * modelViewMatrix * vec4( position, 1.0 );

		}`,fragmentShader:`

		uniform float opacity;

		uniform sampler2D tDiffuse;

		varying vec2 vUv;

		void main() {

			vec4 texel = texture2D( tDiffuse, vUv );
			gl_FragColor = opacity * texel;


		}`};var li=class{constructor(){this.isPass=!0,this.enabled=!0,this.needsSwap=!0,this.clear=!1,this.renderToScreen=!1}setSize(){}render(){console.error("THREE.Pass: .render() must be implemented in derived pass.")}dispose(){}},uv=new rs(-1,1,1,-1,0,1),Zh=class extends Yt{constructor(){super(),this.setAttribute("position",new jt([-1,3,0,-1,-1,0,3,-1,0],3)),this.setAttribute("uv",new jt([0,2,0,0,2,0],2))}},dv=new Zh,Ji=class{constructor(t){this._mesh=new Lt(dv,t)}dispose(){this._mesh.geometry.dispose()}render(t){t.render(this._mesh,uv)}get material(){return this._mesh.material}set material(t){this._mesh.material=t}};var ds=class extends li{constructor(t,e="tDiffuse"){super(),this.textureID=e,this.uniforms=null,this.material=null,t instanceof ae?(this.uniforms=t.uniforms,this.material=t):t&&(this.uniforms=ki.clone(t.uniforms),this.material=new ae({name:t.name!==void 0?t.name:"unspecified",defines:Object.assign({},t.defines),uniforms:this.uniforms,vertexShader:t.vertexShader,fragmentShader:t.fragmentShader})),this._fsQuad=new Ji(this.material)}render(t,e,i){this.uniforms[this.textureID]&&(this.uniforms[this.textureID].value=i.texture),this._fsQuad.material=this.material,this.renderToScreen?(t.setRenderTarget(null),this._fsQuad.render(t)):(t.setRenderTarget(e),this.clear&&t.clear(t.autoClearColor,t.autoClearDepth,t.autoClearStencil),this._fsQuad.render(t))}dispose(){this.material.dispose(),this._fsQuad.dispose()}};var Mo=class extends li{constructor(t,e){super(),this.scene=t,this.camera=e,this.clear=!0,this.needsSwap=!1,this.inverse=!1}render(t,e,i){let s=t.getContext(),r=t.state;r.buffers.color.setMask(!1),r.buffers.depth.setMask(!1),r.buffers.color.setLocked(!0),r.buffers.depth.setLocked(!0);let o,a;this.inverse?(o=0,a=1):(o=1,a=0),r.buffers.stencil.setTest(!0),r.buffers.stencil.setOp(s.REPLACE,s.REPLACE,s.REPLACE),r.buffers.stencil.setFunc(s.ALWAYS,o,4294967295),r.buffers.stencil.setClear(a),r.buffers.stencil.setLocked(!0),t.setRenderTarget(i),this.clear&&t.clear(),t.render(this.scene,this.camera),t.setRenderTarget(e),this.clear&&t.clear(),t.render(this.scene,this.camera),r.buffers.color.setLocked(!1),r.buffers.depth.setLocked(!1),r.buffers.color.setMask(!0),r.buffers.depth.setMask(!0),r.buffers.stencil.setLocked(!1),r.buffers.stencil.setFunc(s.EQUAL,1,4294967295),r.buffers.stencil.setOp(s.KEEP,s.KEEP,s.KEEP),r.buffers.stencil.setLocked(!0)}},Kl=class extends li{constructor(){super(),this.needsSwap=!1}render(t){t.state.buffers.stencil.setLocked(!1),t.state.buffers.stencil.setTest(!1)}};var Jl=class{constructor(t,e){if(this.renderer=t,this._pixelRatio=t.getPixelRatio(),e===void 0){let i=t.getSize(new et);this._width=i.width,this._height=i.height,e=new he(this._width*this._pixelRatio,this._height*this._pixelRatio,{type:de}),e.texture.name="EffectComposer.rt1"}else this._width=e.width,this._height=e.height;this.renderTarget1=e,this.renderTarget2=e.clone(),this.renderTarget2.texture.name="EffectComposer.rt2",this.writeBuffer=this.renderTarget1,this.readBuffer=this.renderTarget2,this.renderToScreen=!0,this.passes=[],this.copyPass=new ds(Hn),this.copyPass.material.blending=yi,this.timer=new Kr}swapBuffers(){let t=this.readBuffer;this.readBuffer=this.writeBuffer,this.writeBuffer=t}addPass(t){this.passes.push(t),t.setSize(this._width*this._pixelRatio,this._height*this._pixelRatio)}insertPass(t,e){this.passes.splice(e,0,t),t.setSize(this._width*this._pixelRatio,this._height*this._pixelRatio)}removePass(t){let e=this.passes.indexOf(t);e!==-1&&this.passes.splice(e,1)}isLastEnabledPass(t){for(let e=t+1;e<this.passes.length;e++)if(this.passes[e].enabled)return!1;return!0}render(t){this.timer.update(),t===void 0&&(t=this.timer.getDelta());let e=this.renderer.getRenderTarget(),i=!1;for(let s=0,r=this.passes.length;s<r;s++){let o=this.passes[s];if(o.enabled!==!1){if(o.renderToScreen=this.renderToScreen&&this.isLastEnabledPass(s),o.render(this.renderer,this.writeBuffer,this.readBuffer,t,i),o.needsSwap){if(i){let a=this.renderer.getContext(),l=this.renderer.state.buffers.stencil;l.setFunc(a.NOTEQUAL,1,4294967295),this.copyPass.render(this.renderer,this.writeBuffer,this.readBuffer,t),l.setFunc(a.EQUAL,1,4294967295)}this.swapBuffers()}Mo!==void 0&&(o instanceof Mo?i=!0:o instanceof Kl&&(i=!1))}}this.renderer.setRenderTarget(e)}reset(t){if(t===void 0){let e=this.renderer.getSize(new et);this._pixelRatio=this.renderer.getPixelRatio(),this._width=e.width,this._height=e.height,t=this.renderTarget1.clone(),t.setSize(this._width*this._pixelRatio,this._height*this._pixelRatio)}this.renderTarget1.dispose(),this.renderTarget2.dispose(),this.renderTarget1=t,this.renderTarget2=t.clone(),this.writeBuffer=this.renderTarget1,this.readBuffer=this.renderTarget2}setSize(t,e){this._width=t,this._height=e;let i=this._width*this._pixelRatio,s=this._height*this._pixelRatio;this.renderTarget1.setSize(i,s),this.renderTarget2.setSize(i,s);for(let r=0;r<this.passes.length;r++)this.passes[r].setSize(i,s)}setPixelRatio(t){this._pixelRatio=t,this.setSize(this._width,this._height)}dispose(){this.renderTarget1.dispose(),this.renderTarget2.dispose(),this.copyPass.dispose()}};var $l=class extends li{constructor(t,e=1){super();let i=Hn;this.map=t,this.opacity=e,this.needsSwap=!1,this.uniforms=ki.clone(i.uniforms),this.material=new ae({uniforms:this.uniforms,vertexShader:i.vertexShader,fragmentShader:i.fragmentShader,depthTest:!1,depthWrite:!1,premultipliedAlpha:!0}),this._fsQuad=new Ji(null)}render(t,e,i){let s=t.autoClear;t.autoClear=!1,this._fsQuad.material=this.material,this.uniforms.opacity.value=this.opacity,this.uniforms.tDiffuse.value=this.map,this.material.transparent=this.opacity<1,t.setRenderTarget(this.renderToScreen?null:i),this.clear&&t.clear(),this._fsQuad.render(t),t.autoClear=s}dispose(){this.material.dispose(),this._fsQuad.dispose()}};var Hf={name:"LuminosityHighPassShader",uniforms:{tDiffuse:{value:null},luminosityThreshold:{value:1},smoothWidth:{value:1},defaultColor:{value:new St(0)},defaultOpacity:{value:0}},vertexShader:`

		varying vec2 vUv;

		void main() {

			vUv = uv;

			gl_Position = projectionMatrix * modelViewMatrix * vec4( position, 1.0 );

		}`,fragmentShader:`

		uniform sampler2D tDiffuse;
		uniform vec3 defaultColor;
		uniform float defaultOpacity;
		uniform float luminosityThreshold;
		uniform float smoothWidth;

		varying vec2 vUv;

		void main() {

			vec4 texel = texture2D( tDiffuse, vUv );

			float v = luminance( texel.xyz );

			vec4 outputColor = vec4( defaultColor.rgb, defaultOpacity );

			float alpha = smoothstep( luminosityThreshold, luminosityThreshold + smoothWidth, v );

			gl_FragColor = mix( outputColor, texel, alpha );

		}`};var hr=class n extends li{constructor(t,e=1,i,s){super(),this.strength=e,this.radius=i,this.threshold=s,this.resolution=t!==void 0?new et(t.x,t.y):new et(256,256),this.clearColor=new St(0,0,0),this.needsSwap=!1,this.renderTargetsHorizontal=[],this.renderTargetsVertical=[],this.nMips=5;let r=Math.round(this.resolution.x/2),o=Math.round(this.resolution.y/2);this.renderTargetBright=new he(r,o,{type:de,depthBuffer:!1}),this.renderTargetBright.texture.name="UnrealBloomPass.bright",this.renderTargetBright.texture.generateMipmaps=!1;for(let h=0;h<this.nMips;h++){let u=new he(r,o,{type:de,depthBuffer:!1});u.texture.name="UnrealBloomPass.h"+h,u.texture.generateMipmaps=!1,this.renderTargetsHorizontal.push(u);let d=new he(r,o,{type:de,depthBuffer:!1});d.texture.name="UnrealBloomPass.v"+h,d.texture.generateMipmaps=!1,this.renderTargetsVertical.push(d),r=Math.round(r/2),o=Math.round(o/2)}let a=Hf;this.highPassUniforms=ki.clone(a.uniforms),this.highPassUniforms.luminosityThreshold.value=s,this.highPassUniforms.smoothWidth.value=.01,this.materialHighPassFilter=new ae({uniforms:this.highPassUniforms,vertexShader:a.vertexShader,fragmentShader:a.fragmentShader}),this.separableBlurMaterials=[];let l=[6,10,14,18,22];r=Math.round(this.resolution.x/2),o=Math.round(this.resolution.y/2);for(let h=0;h<this.nMips;h++)this.separableBlurMaterials.push(this._getSeparableBlurMaterial(l[h])),this.separableBlurMaterials[h].uniforms.invSize.value=new et(1/r,1/o),r=Math.round(r/2),o=Math.round(o/2);this.compositeMaterial=this._getCompositeMaterial(this.nMips),this.compositeMaterial.uniforms.blurTexture1.value=this.renderTargetsVertical[0].texture,this.compositeMaterial.uniforms.blurTexture2.value=this.renderTargetsVertical[1].texture,this.compositeMaterial.uniforms.blurTexture3.value=this.renderTargetsVertical[2].texture,this.compositeMaterial.uniforms.blurTexture4.value=this.renderTargetsVertical[3].texture,this.compositeMaterial.uniforms.blurTexture5.value=this.renderTargetsVertical[4].texture,this.compositeMaterial.uniforms.bloomStrength.value=e,this.compositeMaterial.uniforms.bloomRadius.value=.1;let c=[1,.8,.6,.4,.2];this.compositeMaterial.uniforms.bloomFactors.value=c,this.bloomTintColors=[new R(1,1,1),new R(1,1,1),new R(1,1,1),new R(1,1,1),new R(1,1,1)],this.compositeMaterial.uniforms.bloomTintColors.value=this.bloomTintColors,this.copyUniforms=ki.clone(Hn.uniforms),this.blendMaterial=new ae({uniforms:this.copyUniforms,vertexShader:Hn.vertexShader,fragmentShader:Hn.fragmentShader,premultipliedAlpha:!0,blending:$r,depthTest:!1,depthWrite:!1,transparent:!0}),this._oldClearColor=new St,this._oldClearAlpha=1,this._basic=new ci,this._fsQuad=new Ji(null)}dispose(){for(let t=0;t<this.renderTargetsHorizontal.length;t++)this.renderTargetsHorizontal[t].dispose();for(let t=0;t<this.renderTargetsVertical.length;t++)this.renderTargetsVertical[t].dispose();this.renderTargetBright.dispose();for(let t=0;t<this.separableBlurMaterials.length;t++)this.separableBlurMaterials[t].dispose();this.compositeMaterial.dispose(),this.blendMaterial.dispose(),this._basic.dispose(),this._fsQuad.dispose()}setSize(t,e){let i=Math.round(t/2),s=Math.round(e/2);this.renderTargetBright.setSize(i,s);for(let r=0;r<this.nMips;r++)this.renderTargetsHorizontal[r].setSize(i,s),this.renderTargetsVertical[r].setSize(i,s),this.separableBlurMaterials[r].uniforms.invSize.value=new et(1/i,1/s),i=Math.round(i/2),s=Math.round(s/2)}render(t,e,i,s,r){t.getClearColor(this._oldClearColor),this._oldClearAlpha=t.getClearAlpha();let o=t.autoClear;t.autoClear=!1,t.setClearColor(this.clearColor,0),r&&t.state.buffers.stencil.setTest(!1),this.renderToScreen&&(this._fsQuad.material=this._basic,this._basic.map=i.texture,t.setRenderTarget(null),t.clear(),this._fsQuad.render(t)),this.highPassUniforms.tDiffuse.value=i.texture,this.highPassUniforms.luminosityThreshold.value=this.threshold,this._fsQuad.material=this.materialHighPassFilter,t.setRenderTarget(this.renderTargetBright),t.clear(),this._fsQuad.render(t);let a=this.renderTargetBright;for(let l=0;l<this.nMips;l++)this._fsQuad.material=this.separableBlurMaterials[l],this.separableBlurMaterials[l].uniforms.colorTexture.value=a.texture,this.separableBlurMaterials[l].uniforms.direction.value=n.BlurDirectionX,t.setRenderTarget(this.renderTargetsHorizontal[l]),t.clear(),this._fsQuad.render(t),this.separableBlurMaterials[l].uniforms.colorTexture.value=this.renderTargetsHorizontal[l].texture,this.separableBlurMaterials[l].uniforms.direction.value=n.BlurDirectionY,t.setRenderTarget(this.renderTargetsVertical[l]),t.clear(),this._fsQuad.render(t),a=this.renderTargetsVertical[l];this._fsQuad.material=this.compositeMaterial,this.compositeMaterial.uniforms.bloomStrength.value=this.strength,this.compositeMaterial.uniforms.bloomRadius.value=this.radius,this.compositeMaterial.uniforms.bloomTintColors.value=this.bloomTintColors,t.setRenderTarget(this.renderTargetsHorizontal[0]),t.clear(),this._fsQuad.render(t),this._fsQuad.material=this.blendMaterial,this.copyUniforms.tDiffuse.value=this.renderTargetsHorizontal[0].texture,r&&t.state.buffers.stencil.setTest(!0),this.renderToScreen?(t.setRenderTarget(null),this._fsQuad.render(t)):(t.setRenderTarget(i),this._fsQuad.render(t)),t.setClearColor(this._oldClearColor,this._oldClearAlpha),t.autoClear=o}_getSeparableBlurMaterial(t){let e=[],i=t/3;for(let o=0;o<t;o++)e.push(.39894*Math.exp(-.5*o*o/(i*i))/i);let s=[],r=[];for(let o=1;o<t;o+=2){let a=e[o],l=o+1<t?e[o+1]:0,c=a+l;s.push((o*a+(o+1)*l)/c),r.push(c)}return new ae({defines:{KERNEL_PAIRS:s.length},uniforms:{colorTexture:{value:null},invSize:{value:new et(.5,.5)},direction:{value:new et(.5,.5)},centerWeight:{value:e[0]},gaussianOffsets:{value:s},gaussianWeights:{value:r}},vertexShader:`

				varying vec2 vUv;

				void main() {

					vUv = uv;
					gl_Position = projectionMatrix * modelViewMatrix * vec4( position, 1.0 );

				}`,fragmentShader:`

				#include <common>

				varying vec2 vUv;

				uniform sampler2D colorTexture;
				uniform vec2 invSize;
				uniform vec2 direction;
				uniform float centerWeight;
				uniform float gaussianOffsets[KERNEL_PAIRS];
				uniform float gaussianWeights[KERNEL_PAIRS];

				void main() {

					vec3 diffuseSum = texture2D( colorTexture, vUv ).rgb * centerWeight;

					for ( int i = 0; i < KERNEL_PAIRS; i ++ ) {

						vec2 uvOffset = direction * invSize * gaussianOffsets[ i ];
						vec3 sample1 = texture2D( colorTexture, vUv + uvOffset ).rgb;
						vec3 sample2 = texture2D( colorTexture, vUv - uvOffset ).rgb;
						diffuseSum += ( sample1 + sample2 ) * gaussianWeights[ i ];

					}

					gl_FragColor = vec4( diffuseSum, 1.0 );

				}`})}_getCompositeMaterial(t){return new ae({defines:{NUM_MIPS:t},uniforms:{blurTexture1:{value:null},blurTexture2:{value:null},blurTexture3:{value:null},blurTexture4:{value:null},blurTexture5:{value:null},bloomStrength:{value:1},bloomFactors:{value:null},bloomTintColors:{value:null},bloomRadius:{value:0}},vertexShader:`

				varying vec2 vUv;

				void main() {

					vUv = uv;
					gl_Position = projectionMatrix * modelViewMatrix * vec4( position, 1.0 );

				}`,fragmentShader:`

				varying vec2 vUv;

				uniform sampler2D blurTexture1;
				uniform sampler2D blurTexture2;
				uniform sampler2D blurTexture3;
				uniform sampler2D blurTexture4;
				uniform sampler2D blurTexture5;
				uniform float bloomStrength;
				uniform float bloomRadius;
				uniform float bloomFactors[NUM_MIPS];
				uniform vec3 bloomTintColors[NUM_MIPS];

				float lerpBloomFactor( const in float factor ) {

					float mirrorFactor = 1.2 - factor;
					return mix( factor, mirrorFactor, bloomRadius );

				}

				void main() {

					// 3.0 for backwards compatibility with previous alpha-based intensity
					vec3 bloom = 3.0 * bloomStrength * (
						lerpBloomFactor( bloomFactors[ 0 ] ) * bloomTintColors[ 0 ] * texture2D( blurTexture1, vUv ).rgb +
						lerpBloomFactor( bloomFactors[ 1 ] ) * bloomTintColors[ 1 ] * texture2D( blurTexture2, vUv ).rgb +
						lerpBloomFactor( bloomFactors[ 2 ] ) * bloomTintColors[ 2 ] * texture2D( blurTexture3, vUv ).rgb +
						lerpBloomFactor( bloomFactors[ 3 ] ) * bloomTintColors[ 3 ] * texture2D( blurTexture4, vUv ).rgb +
						lerpBloomFactor( bloomFactors[ 4 ] ) * bloomTintColors[ 4 ] * texture2D( blurTexture5, vUv ).rgb
					);

					float bloomAlpha = max( bloom.r, max( bloom.g, bloom.b ) );
					gl_FragColor = vec4( bloom, bloomAlpha );

				}`})}};hr.BlurDirectionX=new et(1,0);hr.BlurDirectionY=new et(0,1);var Kh={name:"FXAAShader",uniforms:{tDiffuse:{value:null},resolution:{value:new et(1/1024,1/512)}},vertexShader:`

		varying vec2 vUv;

		void main() {

			vUv = uv;
			gl_Position = projectionMatrix * modelViewMatrix * vec4( position, 1.0 );

		}`,fragmentShader:`

		uniform sampler2D tDiffuse;
		uniform vec2 resolution;
		varying vec2 vUv;

		#define EDGE_STEP_COUNT 6
		#define EDGE_GUESS 8.0
		#define EDGE_STEPS 1.0, 1.5, 2.0, 2.0, 2.0, 4.0
		const float edgeSteps[EDGE_STEP_COUNT] = float[EDGE_STEP_COUNT]( EDGE_STEPS );

		float _ContrastThreshold = 0.0312;
		float _RelativeThreshold = 0.063;
		float _SubpixelBlending = 1.0;

		vec4 Sample( sampler2D  tex2D, vec2 uv ) {

			return texture( tex2D, uv );

		}

		float SampleLuminance( sampler2D tex2D, vec2 uv ) {

			return dot( Sample( tex2D, uv ).rgb, vec3( 0.3, 0.59, 0.11 ) );

		}

		float SampleLuminance( sampler2D tex2D, vec2 texSize, vec2 uv, float uOffset, float vOffset ) {

			uv += texSize * vec2(uOffset, vOffset);
			return SampleLuminance(tex2D, uv);

		}

		struct LuminanceData {

			float m, n, e, s, w;
			float ne, nw, se, sw;
			float highest, lowest, contrast;

		};

		LuminanceData SampleLuminanceNeighborhood( sampler2D tex2D, vec2 texSize, vec2 uv ) {

			LuminanceData l;
			l.m = SampleLuminance( tex2D, uv );
			l.n = SampleLuminance( tex2D, texSize, uv,  0.0,  1.0 );
			l.e = SampleLuminance( tex2D, texSize, uv,  1.0,  0.0 );
			l.s = SampleLuminance( tex2D, texSize, uv,  0.0, -1.0 );
			l.w = SampleLuminance( tex2D, texSize, uv, -1.0,  0.0 );

			l.ne = SampleLuminance( tex2D, texSize, uv,  1.0,  1.0 );
			l.nw = SampleLuminance( tex2D, texSize, uv, -1.0,  1.0 );
			l.se = SampleLuminance( tex2D, texSize, uv,  1.0, -1.0 );
			l.sw = SampleLuminance( tex2D, texSize, uv, -1.0, -1.0 );

			l.highest = max( max( max( max( l.n, l.e ), l.s ), l.w ), l.m );
			l.lowest = min( min( min( min( l.n, l.e ), l.s ), l.w ), l.m );
			l.contrast = l.highest - l.lowest;
			return l;

		}

		bool ShouldSkipPixel( LuminanceData l ) {

			float threshold = max( _ContrastThreshold, _RelativeThreshold * l.highest );
			return l.contrast < threshold;

		}

		float DeterminePixelBlendFactor( LuminanceData l ) {

			float f = 2.0 * ( l.n + l.e + l.s + l.w );
			f += l.ne + l.nw + l.se + l.sw;
			f *= 1.0 / 12.0;
			f = abs( f - l.m );
			f = clamp( f / l.contrast, 0.0, 1.0 );

			float blendFactor = smoothstep( 0.0, 1.0, f );
			return blendFactor * blendFactor * _SubpixelBlending;

		}

		struct EdgeData {

			bool isHorizontal;
			float pixelStep;
			float oppositeLuminance, gradient;

		};

		EdgeData DetermineEdge( vec2 texSize, LuminanceData l ) {

			EdgeData e;
			float horizontal =
				abs( l.n + l.s - 2.0 * l.m ) * 2.0 +
				abs( l.ne + l.se - 2.0 * l.e ) +
				abs( l.nw + l.sw - 2.0 * l.w );
			float vertical =
				abs( l.e + l.w - 2.0 * l.m ) * 2.0 +
				abs( l.ne + l.nw - 2.0 * l.n ) +
				abs( l.se + l.sw - 2.0 * l.s );
			e.isHorizontal = horizontal >= vertical;

			float pLuminance = e.isHorizontal ? l.n : l.e;
			float nLuminance = e.isHorizontal ? l.s : l.w;
			float pGradient = abs( pLuminance - l.m );
			float nGradient = abs( nLuminance - l.m );

			e.pixelStep = e.isHorizontal ? texSize.y : texSize.x;

			if (pGradient < nGradient) {

				e.pixelStep = -e.pixelStep;
				e.oppositeLuminance = nLuminance;
				e.gradient = nGradient;

			} else {

				e.oppositeLuminance = pLuminance;
				e.gradient = pGradient;

			}

			return e;

		}

		float DetermineEdgeBlendFactor( sampler2D  tex2D, vec2 texSize, LuminanceData l, EdgeData e, vec2 uv ) {

			vec2 uvEdge = uv;
			vec2 edgeStep;
			if (e.isHorizontal) {

				uvEdge.y += e.pixelStep * 0.5;
				edgeStep = vec2( texSize.x, 0.0 );

			} else {

				uvEdge.x += e.pixelStep * 0.5;
				edgeStep = vec2( 0.0, texSize.y );

			}

			float edgeLuminance = ( l.m + e.oppositeLuminance ) * 0.5;
			float gradientThreshold = e.gradient * 0.25;

			vec2 puv = uvEdge + edgeStep * edgeSteps[0];
			float pLuminanceDelta = SampleLuminance( tex2D, puv ) - edgeLuminance;
			bool pAtEnd = abs( pLuminanceDelta ) >= gradientThreshold;

			for ( int i = 1; i < EDGE_STEP_COUNT && !pAtEnd; i++ ) {

				puv += edgeStep * edgeSteps[i];
				pLuminanceDelta = SampleLuminance( tex2D, puv ) - edgeLuminance;
				pAtEnd = abs( pLuminanceDelta ) >= gradientThreshold;

			}

			if ( !pAtEnd ) {

				puv += edgeStep * EDGE_GUESS;

			}

			vec2 nuv = uvEdge - edgeStep * edgeSteps[0];
			float nLuminanceDelta = SampleLuminance( tex2D, nuv ) - edgeLuminance;
			bool nAtEnd = abs( nLuminanceDelta ) >= gradientThreshold;

			for ( int i = 1; i < EDGE_STEP_COUNT && !nAtEnd; i++ ) {

				nuv -= edgeStep * edgeSteps[i];
				nLuminanceDelta = SampleLuminance( tex2D, nuv ) - edgeLuminance;
				nAtEnd = abs( nLuminanceDelta ) >= gradientThreshold;

			}

			if ( !nAtEnd ) {

				nuv -= edgeStep * EDGE_GUESS;

			}

			float pDistance, nDistance;
			if ( e.isHorizontal ) {

				pDistance = puv.x - uv.x;
				nDistance = uv.x - nuv.x;

			} else {

				pDistance = puv.y - uv.y;
				nDistance = uv.y - nuv.y;

			}

			float shortestDistance;
			bool deltaSign;
			if ( pDistance <= nDistance ) {

				shortestDistance = pDistance;
				deltaSign = pLuminanceDelta >= 0.0;

			} else {

				shortestDistance = nDistance;
				deltaSign = nLuminanceDelta >= 0.0;

			}

			if ( deltaSign == ( l.m - edgeLuminance >= 0.0 ) ) {

				return 0.0;

			}

			return 0.5 - shortestDistance / ( pDistance + nDistance );

		}

		vec4 ApplyFXAA( sampler2D  tex2D, vec2 texSize, vec2 uv ) {

			LuminanceData luminance = SampleLuminanceNeighborhood( tex2D, texSize, uv );
			if ( ShouldSkipPixel( luminance ) ) {

				return Sample( tex2D, uv );

			}

			float pixelBlend = DeterminePixelBlendFactor( luminance );
			EdgeData edge = DetermineEdge( texSize, luminance );
			float edgeBlend = DetermineEdgeBlendFactor( tex2D, texSize, luminance, edge, uv );
			float finalBlend = max( pixelBlend, edgeBlend );

			if (edge.isHorizontal) {

				uv.y += edge.pixelStep * finalBlend;

			} else {

				uv.x += edge.pixelStep * finalBlend;

			}

			return Sample( tex2D, uv );

		}

		void main() {

			gl_FragColor = ApplyFXAA( tDiffuse, resolution.xy, vUv );

		}`};var So={name:"OutputShader",uniforms:{tDiffuse:{value:null},toneMappingExposure:{value:1}},vertexShader:`
		precision highp float;

		uniform mat4 modelViewMatrix;
		uniform mat4 projectionMatrix;

		attribute vec3 position;
		attribute vec2 uv;

		varying vec2 vUv;

		void main() {

			vUv = uv;
			gl_Position = projectionMatrix * modelViewMatrix * vec4( position, 1.0 );

		}`,fragmentShader:`

		precision highp float;

		uniform sampler2D tDiffuse;

		#include <tonemapping_pars_fragment>
		#include <colorspace_pars_fragment>

		varying vec2 vUv;

		void main() {

			gl_FragColor = texture2D( tDiffuse, vUv );

			// tone mapping

			#ifdef LINEAR_TONE_MAPPING

				gl_FragColor.rgb = LinearToneMapping( gl_FragColor.rgb );

			#elif defined( REINHARD_TONE_MAPPING )

				gl_FragColor.rgb = ReinhardToneMapping( gl_FragColor.rgb );

			#elif defined( CINEON_TONE_MAPPING )

				gl_FragColor.rgb = CineonToneMapping( gl_FragColor.rgb );

			#elif defined( ACES_FILMIC_TONE_MAPPING )

				gl_FragColor.rgb = ACESFilmicToneMapping( gl_FragColor.rgb );

			#elif defined( AGX_TONE_MAPPING )

				gl_FragColor.rgb = AgXToneMapping( gl_FragColor.rgb );

			#elif defined( NEUTRAL_TONE_MAPPING )

				gl_FragColor.rgb = NeutralToneMapping( gl_FragColor.rgb );

			#elif defined( CUSTOM_TONE_MAPPING )

				gl_FragColor.rgb = CustomToneMapping( gl_FragColor.rgb );

			#endif

			// color space

			#ifdef SRGB_TRANSFER

				gl_FragColor = sRGBTransferOETF( gl_FragColor );

			#endif

		}`};var jl=class extends li{constructor(){super(),this.isOutputPass=!0,this.uniforms=ki.clone(So.uniforms),this.material=new In({name:So.name,uniforms:this.uniforms,vertexShader:So.vertexShader,fragmentShader:So.fragmentShader}),this._fsQuad=new Ji(this.material),this._outputColorSpace=null,this._toneMapping=null}render(t,e,i){this.uniforms.tDiffuse.value=i.texture,this.uniforms.toneMappingExposure.value=t.toneMappingExposure,(this._outputColorSpace!==t.outputColorSpace||this._toneMapping!==t.toneMapping)&&(this._outputColorSpace=t.outputColorSpace,this._toneMapping=t.toneMapping,this.material.defines={},qt.getTransfer(this._outputColorSpace)===Qt&&(this.material.defines.SRGB_TRANSFER=""),this._toneMapping===jr?this.material.defines.LINEAR_TONE_MAPPING="":this._toneMapping===Qr?this.material.defines.REINHARD_TONE_MAPPING="":this._toneMapping===to?this.material.defines.CINEON_TONE_MAPPING="":this._toneMapping===as?this.material.defines.ACES_FILMIC_TONE_MAPPING="":this._toneMapping===io?this.material.defines.AGX_TONE_MAPPING="":this._toneMapping===no?this.material.defines.NEUTRAL_TONE_MAPPING="":this._toneMapping===eo&&(this.material.defines.CUSTOM_TONE_MAPPING=""),this.material.needsUpdate=!0),this.renderToScreen===!0?(t.setRenderTarget(null),this._fsQuad.render(t)):(t.setRenderTarget(e),this.clear&&t.clear(t.autoClearColor,t.autoClearDepth,t.autoClearStencil),this._fsQuad.render(t))}dispose(){this.material.dispose(),this._fsQuad.dispose()}};function Vf(){let n=new jl;return n.uniforms.extendedDisplay={value:!1},n.material.fragmentShader=`uniform bool extendedDisplay;
`+n.material.fragmentShader.replace("gl_FragColor.rgb = ACESFilmicToneMapping( gl_FragColor.rgb );",`if (extendedDisplay) {
       vec3 exposed = max(vec3(0.0), gl_FragColor.rgb * toneMappingExposure);
       gl_FragColor.rgb = exposed / (1.0 + max(exposed.r, max(exposed.g, exposed.b)) * 0.25);
     } else {
       gl_FragColor.rgb = ACESFilmicToneMapping(gl_FragColor.rgb);
     }`),n}var Ql=class extends ds{constructor(){super(Kh)}setSize(t,e){this.uniforms.resolution.value.set(1/t,1/e)}},tc=class extends ds{constructor(){super({uniforms:{tDiffuse:{value:null},grain:{value:.0015},phase:{value:0}},vertexShader:Kh.vertexShader,fragmentShader:`
        uniform sampler2D tDiffuse;
        uniform float grain;
        uniform float phase;
        varying vec2 vUv;
        void main() {
          vec3 color = texture2D(tDiffuse, vUv).rgb;
          vec2 pixel = gl_FragCoord.xy;
          float dither = fract(52.9829189 * fract(dot(pixel, vec2(0.06711056, 0.00583715)))) - 0.5;
          float noise = fract(52.9829189 * fract(dot(pixel + phase, vec2(0.06711056, 0.00583715)))) - 0.5;
          gl_FragColor = vec4(color + dither / 255.0 + noise * grain, 1.0);
        }`})}};var $i={low:{dpr:.75,maxPixels:18e4,shadows:1,shadowSize:256,fps:10,fixtures:32,fullLights:4,fog:"phone"},mobile:{dpr:2,maxPixels:15e5,shadows:2,shadowSize:512,fps:120,fixtures:64,fullLights:8},desktop:{dpr:2,maxPixels:6e6,shadows:4,shadowSize:1024,fps:120,fixtures:128,fullLights:8},high:{dpr:4,maxPixels:8294400,shadows:4,shadowSize:2048,fps:120,fixtures:128,fullLights:16}};function Gf(n,t=null){let e=t??n?.fullLights??16;return Math.max(0,Math.min(16,Math.floor(Number(e)||0)))}function Wf(n,t,e,i){let s=n==="low"&&t==="auto"&&!e&&i?$i.mobile.fps:$i[n].fps;return Math.min(s,i||s)}function ec(n,t,e){return $i[n==="low"&&t==="auto"&&!e?"mobile":n]}function Xf(n,t,e,i,s,r){let o=Math.min(e||1,Math.sqrt(8294400/(n*t)),r/Math.max(n,t)),a=Math.min(o,i.dpr,Math.sqrt(i.maxPixels/(n*t)))*s,l=c=>[n,t].map(h=>Math.max(1,Math.round(h*c)));return{scene:l(a),display:l(o)}}function qf(n){let t=i=>Array.from(n.getInternalformatParameter(n.RENDERBUFFER,i,n.SAMPLES)),e=t(n.DEPTH_COMPONENT24);return Math.max(0,...t(n.RGBA16F).filter(i=>i<=4&&e.includes(i)))}function Yf(n,t,e,i){if(n==="low")return 0;for(let s of[i,2])if(s>=2&&s<=i&&t*e*s<=16e6)return s;return 0}function Zf(n,t){let e=Object.keys($i);return e[Math.max(0,Math.min(e.length-1,e.indexOf(n)+t))]}function Kf(){let n=globalThis.WebGL2RenderingContext?.prototype;return!!(globalThis.matchMedia?.("(dynamic-range: high)").matches&&typeof n?.drawingBufferStorage=="function"&&typeof n?.drawingBufferToneMapping=="function")}function Jf(n,t,e,i){if(!i||!n.getExtension("EXT_color_buffer_float"))return"sdr";try{if(n.drawingBufferStorage(n.RGBA16F,t,e),n.drawingBufferFormat===n.RGBA16F&&n.drawingBufferToneMapping({mode:"extended"})?.mode==="extended")return"extended"}catch{}if(n.drawingBufferFormat===n.RGBA16F){n.drawingBufferStorage(n.RGBA8,t,e);try{n.drawingBufferToneMapping({mode:"standard"})}catch{}}return"sdr"}var bo=class{constructor(t){this.gl=t,this.extension=t.getExtension("EXT_disjoint_timer_query_webgl2")}sample(){let t=this.gl;if(!this.extension){let s=this.elapsed;return this.elapsed=null,s}if(!this.query)return null;let e=t.getParameter(this.extension.GPU_DISJOINT_EXT);if(!e&&!t.getQueryParameter(this.query,t.QUERY_RESULT_AVAILABLE))return null;let i=e?null:t.getQueryParameter(this.query,t.QUERY_RESULT)/1e6;return this.dispose(),i}begin(){return this.extension?this.query||(this.query=this.gl.createQuery(),!this.query)?!1:(this.gl.beginQuery(this.extension.TIME_ELAPSED_EXT,this.query),!0):this.fence?!1:(this.started=performance.now(),!0)}end(){let t=this.gl;if(this.extension){t.endQuery(this.extension.TIME_ELAPSED_EXT);return}if(this.fence=t.fenceSync(t.SYNC_GPU_COMMANDS_COMPLETE,0),!this.fence)return;t.flush();let e=()=>{let i=t.clientWaitSync(this.fence,0,0),s=performance.now()-this.started;if(i===t.TIMEOUT_EXPIRED&&s<3e4){this.timeout=setTimeout(e,2);return}this.elapsed=i===t.CONDITION_SATISFIED||i===t.ALREADY_SIGNALED?s:null,t.deleteSync(this.fence),this.fence=null};this.timeout=setTimeout(e,0)}dispose(){this.query&&this.gl.deleteQuery(this.query),this.fence&&this.gl.deleteSync(this.fence),clearTimeout(this.timeout),this.query=this.fence=this.elapsed=null}};var jf=(n,t)=>{if(typeof document>"u")return new OffscreenCanvas(n,t);let e=document.createElement("canvas");return e.width=n,e.height=t,e};function ur(n,t){let e=Math.sin(n*127.1+t*311.7)*43758.5453123;return e-Math.floor(e)}function $f(n,t){let e=jf(n,n),i=e.getContext("2d"),s=i.createImageData(n,n);for(let a=0;a<n;a++)for(let l=0;l<n;l++){let c=ur(l,a),h=ur(Math.floor(l/24),Math.floor(a/24)),u=t==="floor"?108+c*28+h*10:139+c*28+h*10;t==="floor"&&(l<2||a<2)&&(u*=.62);let d=(a*n+l)*4;s.data[d]=u,s.data[d+1]=u*.98,s.data[d+2]=u*.94,s.data[d+3]=255}i.putImageData(s,0,0),i.strokeStyle="rgba(26,26,25,.12)",i.lineWidth=1;for(let a=0;a<40;a++){let l=ur(a,6)*n,c=ur(a,8)*n;i.beginPath(),i.moveTo(l,c),i.lineTo(l+ur(a,2)*80,c+ur(a,1)*5),i.stroke()}let r=new Gs(e);r.wrapS=r.wrapT=cn,r.colorSpace=Re,r.anisotropy=4;let o=r.clone();return o.colorSpace=zi,{map:r,bump:o}}function fv(){let n=jf(64,64),t=n.getContext("2d");t.fillStyle="#353a3c",t.fillRect(0,0,64,64),t.fillStyle="#080a0b";for(let i=0;i<64;i+=8)for(let s=0;s<64;s+=8)t.beginPath(),t.arc(s+(i%16?4:0),i,2.5,0,Math.PI*2),t.fill();let e=new Gs(n);return e.wrapS=e.wrapT=cn,e.repeat.set(3,5),e.colorSpace=Re,e}function Qf(){let n=$f(512,"floor"),t=$f(512,"wall");n.map.repeat.set(5,4),n.bump.repeat.copy(n.map.repeat),t.map.repeat.set(3,2),t.bump.repeat.copy(t.map.repeat);let e={floor:new Pe({color:12960184,map:n.map,bumpMap:n.bump,bumpScale:.028,roughness:.42,metalness:.12}),wall:new Pe({color:11972772,map:t.map,bumpMap:t.bump,bumpScale:.035,roughness:.91,metalness:.02}),metal:new Pe({color:9147033,roughness:.28,metalness:.88}),blackMetal:new Pe({color:2303786,roughness:.34,metalness:.75}),housing:new Pe({color:1777186,roughness:.43,metalness:.32}),rubber:new Pe({color:1118997,roughness:.85,metalness:0}),curtain:new Pe({color:2632495,roughness:1,metalness:0,side:De}),stage:new Pe({color:2105894,roughness:.73,metalness:.14}),grille:new Pe({color:9606807,map:fv(),roughness:.7,metalness:.55}),yellow:new Pe({color:10189117,roughness:.65,metalness:.05})};return e.floor.userData.meanMapReflectance=.22,e.wall.userData.meanMapReflectance=.34,e.wall.shadowSide=De,e}function tp(n){let t=new Set;for(let e of Object.values(n)){for(let i of Object.values(e))i?.isTexture&&t.add(i);e.dispose()}for(let e of t)e.dispose()}function fs(n){let t=n["lens-radius-m"];return Number.isFinite(t)&&t>=0?t:.119}function Jh(n,t,e,i){let s=new R(...t).normalize(),r=new R(...n),o=i/Math.tan(e);return{optical:r,virtual:r.clone().addScaledVector(s,-o),axis:s,apexDistance:o,radius:i}}function $h(n,t,e){n.onBeforeCompile=i=>{i.uniforms.tolglowApexDepth=t,i.uniforms.tolglowPatchRadius=e,i.fragmentShader=`uniform float tolglowApexDepth[18]; uniform float tolglowPatchRadius[18];
`+i.fragmentShader;let s=Ht.lights_fragment_begin.replace("getSpotLightInfo( spotLight, geometryPosition, directLight );",`getSpotLightInfo( spotLight, geometryPosition, directLight );
       directLight.color *= step(tolglowApexDepth[i], dot(spotLight.position - geometryPosition, spotLight.direction));
       { float patchD2 = dot(spotLight.position - geometryPosition, spotLight.position - geometryPosition);
       directLight.color *= patchD2 / max(1.e-9, patchD2 + tolglowPatchRadius[i]*tolglowPatchRadius[i]); }`);i.fragmentShader=i.fragmentShader.replace("#include <lights_fragment_begin>",s)},n.customProgramCacheKey=()=>"tolglow-finite-aperture-patch-v2"}var wo=new R;function Ai(n,t,e,i,s,r){let o=2*Math.PI*s/4,a=Math.max(r-2*s,0),l=Math.PI/4;wo.copy(t),wo[i]=0,wo.normalize();let c=.5*o/(o+a),h=1-wo.angleTo(n)/l;return Math.sign(wo[e])===1?h*c:a/(o+a)+c+c*(1-h)}var dr=class n extends be{constructor(t=1,e=1,i=1,s=2,r=.1){let o=s*2+1;if(r=Math.min(t/2,e/2,i/2,r),super(1,1,1,o,o,o),this.type="RoundedBoxGeometry",this.parameters={width:t,height:e,depth:i,segments:s,radius:r},o===1)return;let a=this.toNonIndexed();this.index=null,this.attributes.position=a.attributes.position,this.attributes.normal=a.attributes.normal,this.attributes.uv=a.attributes.uv;let l=new R,c=new R,h=new R(t,e,i).divideScalar(2).subScalar(r),u=this.attributes.position.array,d=this.attributes.normal.array,f=this.attributes.uv.array,p=u.length/6,x=new R,m=.5/o;for(let g=0,_=0;g<u.length;g+=3,_+=2)switch(l.fromArray(u,g),c.copy(l),c.x-=Math.sign(c.x)*m,c.y-=Math.sign(c.y)*m,c.z-=Math.sign(c.z)*m,c.normalize(),u[g+0]=h.x*Math.sign(l.x)+c.x*r,u[g+1]=h.y*Math.sign(l.y)+c.y*r,u[g+2]=h.z*Math.sign(l.z)+c.z*r,d[g+0]=c.x,d[g+1]=c.y,d[g+2]=c.z,Math.floor(g/p)){case 0:x.set(1,0,0),f[_+0]=Ai(x,c,"z","y",r,i),f[_+1]=1-Ai(x,c,"y","z",r,e);break;case 1:x.set(-1,0,0),f[_+0]=1-Ai(x,c,"z","y",r,i),f[_+1]=1-Ai(x,c,"y","z",r,e);break;case 2:x.set(0,1,0),f[_+0]=1-Ai(x,c,"x","z",r,t),f[_+1]=Ai(x,c,"z","x",r,i);break;case 3:x.set(0,-1,0),f[_+0]=1-Ai(x,c,"x","z",r,t),f[_+1]=1-Ai(x,c,"z","x",r,i);break;case 4:x.set(0,0,1),f[_+0]=1-Ai(x,c,"x","y",r,t),f[_+1]=1-Ai(x,c,"y","x",r,e);break;case 5:x.set(0,0,-1),f[_+0]=Ai(x,c,"x","y",r,t),f[_+1]=1-Ai(x,c,"y","x",r,e);break}}static fromJSON(t){return new n(t.width,t.height,t.depth,t.segments,t.radius)}};function Me(n,t,e,i,s=0){let r=s?new dr(...t,2,s):new be(...t),o=new Lt(r,i);return o.position.set(...e),o.castShadow=o.receiveShadow=!0,n.add(o),o}function jh(n,t,e,i,s,r=8){let o=new R().subVectors(new R(...e),new R(...t)),a=new Lt(new hi(i,i,o.length(),r),s);return a.position.set(...t).addScaledVector(o,.5),a.quaternion.setFromUnitVectors(new R(0,1,0),o.normalize()),a.castShadow=a.receiveShadow=!0,n.add(a),a}function ep(n,t){let e=new ge;e.name=`truss:${n.id}`,e.position.fromArray(n.position);let i=(n["rotation-deg"]||[0,0,0]).map(p=>p*Math.PI/180);e.rotation.set(...i,"ZYX");let s=Math.min(40,Math.max(.3,n.length)),r=s/2,o=.17,a=[];for(let p of[-o,o])for(let x of[-o,o])a.push([[-r,p,x],[r,p,x],.025]);let l=Math.ceil(s/.55),c=s/l;for(let p=0;p<l;p++){let x=-r+p*c,m=x+c,g=p%2?-1:1;for(let _ of[-o,o])a.push([[x,-o*g,_],[m,o*g,_],.012]),a.push([[x,_,-o*g],[m,_,o*g],.012])}for(let p=0;p<=l;p++){let x=-r+p*c;a.push([[x,-o,-o],[x,o,-o],.012],[[x,-o,o],[x,o,o],.012],[[x,-o,-o],[x,-o,o],.012],[[x,o,-o],[x,o,o],.012])}let h=new jn(new hi(1,1,1,8),t.metal,a.length),u=new ve,d=new R,f=new R(0,1,0);return a.forEach(([p,x,m],g)=>{d.set(x[0]-p[0],x[1]-p[1],x[2]-p[2]);let _=d.length();u.position.set((p[0]+x[0])/2,(p[1]+x[1])/2,(p[2]+x[2])/2),u.quaternion.setFromUnitVectors(f,d.normalize()),u.scale.set(m,_,m),u.updateMatrix(),h.setMatrixAt(g,u.matrix)}),h.castShadow=h.receiveShadow=!0,e.add(h),e}function ip(n,t){let e=fs({}),i=new ge;i.name=`fixture:${n.id}`,i.position.fromArray(n.position||[0,0,0]);let s=Me(i,[.52,.15,.36],[0,.62,0],t.housing,.025);Me(i,[.44,.035,.27],[0,.71,0],t.blackMetal,.012);for(let d of[-.17,.17])Me(i,[.055,.05,.11],[d,.73,0],t.rubber,.008);let r=new ge;i.add(r);for(let d of[-.22,.22]){Me(r,[.065,.36,.11],[d,.39,0],t.housing,.02);let f=new Lt(new hi(.055,.055,.085,16),t.metal);f.rotation.z=Math.PI/2,f.position.set(d,.23,0),r.add(f)}let o=new ge;i.add(o),Me(o,[.34,.36,.3],[0,.24,0],t.housing,.055);let a=new Lt(new hi(e*1.22,e*1.01,.16,32),t.blackMetal);a.position.y=.081,a.castShadow=a.receiveShadow=!0,o.add(a);let l=new Gr({color:2109754,metalness:.35,roughness:.1,clearcoat:1,clearcoatRoughness:.04,emissive:new St(0,0,0),emissiveIntensity:1}),c=new Lt(new Br(e,40),l);c.rotation.x=Math.PI/2,o.add(c);let h=new Lt(new Vr(e+.014,.012,8,40),t.metal);h.rotation.x=Math.PI/2,h.position.y=-.002,o.add(h);for(let d=0;d<6;d++)Me(o,[.27,.012,.012],[0,.17+d*.034,.156],t.rubber);Me(s,[.12,.035,.004],[0,-.015,.182],t.rubber,.003);let u={root:i,head:o,yoke:r,lensMaterial:l,setAperture(d){let f=fs(d),p=f/e;this.lensRadius=f,a.scale.set(p,1,p),c.scale.set(p,p,1);let x=(f+.014)/(e+.014);h.scale.set(x,x,1)},dispose(){l.dispose()}};return u.setAperture(n),u}function Qh(n){let t=new Set;n.traverse(e=>{e.geometry&&t.add(e.geometry),e.userData.ownsReceiverMaterial&&e.material.dispose()});for(let e of t)e.dispose()}function pv(n,t){if(t?.method!=="bracketed-neutral-card"||!Number.isFinite(t.estimate)||t.estimate<0)return n;let e=n.clone();return e.map=null,e.metalness=0,e.roughness=1,e.color.setScalar(Math.min(1,t.estimate)),e.userData.meanMapReflectance=1,e.onBeforeCompile=n.onBeforeCompile,e.customProgramCacheKey=n.customProgramCacheKey,e}function np(n,t,e=[]){let[i,s,r]=n,o=new ge;o.name="venue-context";let a=new Lt(new _i(i,r),t.floor);a.userData.receiver="floor",a.rotation.x=-Math.PI/2,a.receiveShadow=!0,o.add(a),Me(o,[i+.2,.16,r+.2],[0,-.09,0],t.blackMetal),Me(o,[i,s,.18],[0,s/2,-r/2-.09],t.wall).userData.receiver="back";for(let _ of[-i/2,i/2]){Me(o,[.18,s,r],[_+Math.sign(_)*.09,s/2,0],t.wall).userData.receiver=_<0?"left":"right";for(let y of[-r*.4,r*.12,r*.4])Me(o,[.2,s,.2],[_-Math.sign(_)*.13,s/2,y],t.blackMetal,.008),Me(o,[.36,.08,.36],[_-Math.sign(_)*.13,.04,y],t.metal,.008);jh(o,[_-Math.sign(_)*.22,s-.25,-r/2],[_-Math.sign(_)*.22,s-.25,r/2],.075,t.blackMetal,12)}let l=i*.68,c=Math.min(2.3,r*.27),h=-r/2+c/2+.16;Me(o,[l,.42,c],[0,.21,h],t.stage,.025).userData.receiver="stage",Me(o,[l,.025,.045],[0,.435,h+c/2-.02],t.stage,.008);for(let _=-l/2+1;_<l/2;_+=Math.max(1.2,l/64))Me(o,[.008,.003,c-.08],[_,.423,h],t.rubber);for(let _=0;_<2;_++)Me(o,[.85,.14*(_+1),.34],[l/2+.35,.07*(_+1),h+c/2-_*.34],t.stage,.015);let u=i*.7,d=s*.72,f=new _i(u,d,120,12),p=f.attributes.position;for(let _=0;_<p.count;_++){let y=p.getX(_),S=p.getY(_);p.setZ(_,.08*Math.sin(y*19)+.018*Math.sin(y*37+S*.5))}f.computeVertexNormals();let x=new Lt(f,t.curtain);x.position.set(0,.45+d/2,-r/2+.15),x.castShadow=x.receiveShadow=!0,o.add(x);for(let _ of[-1,1]){let y=_*i*.395,S=-r*.31;for(let T=0;T<2;T++){Me(o,[.68,.86,.58],[y,.43+T*.88,S],t.housing,.035),Me(o,[.59,.72,.014],[y,.43+T*.88,S+.3],t.grille,.01);for(let w of[-.25,.25])Me(o,[.02,.07,.018],[y+w,.18+T*.88,S+.315],t.metal)}Me(o,[.85,.5,.8],[y,.25,S+1],t.rubber,.035),Me(o,[.72,.37,.015],[y,.26,S+1.41],t.grille,.012)}for(let _ of[-r*.38,r*.05,r*.4])Me(o,[i,.17,.1],[0,s-.12,_],t.blackMetal),jh(o,[-i/2,s-.3,_+.14],[i/2,s-.3,_+.14],.025,t.metal);let m=new Ws([new R(-i*.4,.025,-r*.1),new R(-i*.39,.025,r*.2),new R(-i*.28,.025,r*.35),new R(-i*.1,.025,r*.38)]),g=new Lt(new is(m,40,.012,5,!1),t.rubber);o.add(g);for(let _ of e){let{id:y,point:S,normal:T,size:w}=_;if(!Array.isArray(w)||!w.every(A=>Number.isFinite(A)&&A>0))continue;let v=new R(...T).normalize(),M=new R(..._["u-axis"]);M.addScaledVector(v,-M.dot(v)).normalize();let D=pv(t.wall,_.reflectance),P=new Lt(new _i(...w),D);D!==t.wall&&(P.userData.ownsReceiverMaterial=!0),P.position.fromArray(S),P.quaternion.setFromRotationMatrix(new Gt().makeBasis(M,v.clone().cross(M),v)),P.userData.receiver=String(y),P.receiveShadow=P.castShadow=!0,o.add(P)}return o}function mv(n,t){if(n.length<2)return null;let e=n.length-1,i=Math.min(256,e),s=Array.from({length:i+1},(f,p)=>new R(...n[Math.round(e*p/i)].position)),r=new Hr;for(let f=1;f<s.length;f++)s[f-1].equals(s[f])||r.add(new Xs(s[f-1],s[f]));if(!r.curves.length)return null;let o=i*2,a=12,l=new is(r,o,n[0]["diffuser-radius-m"],a),c=new re(new Float32Array(l.attributes.position.count*3),3);c.setUsage(Yi),l.setAttribute("color",c);let h=new Pe({color:8488587,roughness:.72,vertexColors:!0});h.onBeforeCompile=f=>{f.fragmentShader=f.fragmentShader.replace("#include <color_fragment>","").replace("#include <emissivemap_fragment>","totalEmissiveRadiance += 5.0 * vColor.rgb;")};let u=new Lt(l,h);u.name="diffuser:"+n[0]["pixel-layout-id"],u.receiveShadow=!0,t.add(u);let d=n.map(f=>({id:String(f.id),fixture:f}));return{rows:d,update(f){let p=d.map(({id:x})=>us(f[x]));for(let x=0;x<=o;x++){let m=e*x/o,g=Math.floor(m),_=m-g,y=p[Math.max(0,g-1)],S=p[g],T=p[Math.min(e,g+1)],w=p[Math.min(e,g+2)],v=S.map((M,D)=>(1-_)*(.16*y[D]+.68*M+.16*T[D])+_*(.16*M+.68*T[D]+.16*w[D]));for(let M=0;M<=a;M++)c.setXYZ(x*(a+1)+M,...v)}c.needsUpdate=!0},dispose(){h.dispose()}}}function sp(n,t){let e=new Map;for(let i of n){let s=i["pixel-layout-id"],r=e.get(s);r?r.push(i):e.set(s,[i])}return[...e.values()].map(i=>mv(i.sort((s,r)=>s["pixel-index"]-r["pixel-index"]),t)).filter(Boolean)}var{exp:gv,max:tu}=Math;function eu(n,t,e){return gv(-(tu(0,n)*tu(0,t)*tu(0,e)))}var Eo=`float fogTransmission(float extinction, float density, float distance) {
  return exp((-((max(0.0, extinction) * max(0.0, density)) * max(0.0, distance))));
}`;var rp=(n,t,e)=>Math.max(t,Math.min(e,n)),iu=n=>Number.isFinite(n)&&n>=0,op=n=>Array.isArray(n)&&n.length===3&&n.every(Number.isFinite);var ap=(n,t)=>n===t||Array.isArray(n)&&Array.isArray(t)&&n.length===t.length&&n.every((e,i)=>e===t[i]);function xv(n,t){return n===t||n["extinction-per-m"]===t["extinction-per-m"]&&n["reference-path-m"]===t["reference-path-m"]&&n["fog-level"]===t["fog-level"]&&n["volume-m3"]===t["volume-m3"]&&n["mixed-m3"]===t["mixed-m3"]&&n["mixed-density"]===t["mixed-density"]&&n.sources.length===t.sources.length&&n.sources.every((e,i)=>{let s=t.sources[i];return e["fixture-id"]===s["fixture-id"]&&e["flow-m3-s"]===s["flow-m3-s"]&&e["local-m3"]===s["local-m3"]&&e["emitted-m3"]===s["emitted-m3"]&&ap(e.position,s.position)&&ap(e.direction,s.direction)})}function nu(n,t,e){return{position:[...n.position],direction:[...n.direction],flow:n["flow-m3-s"],time:t,mass:e}}function lp(n,t){let e=n.mass+t.mass,i=t.mass/e,s=(r,o)=>r+i*(o-r);return{mass:e,time:s(n.time,t.time),flow:s(n.flow,t.flow),position:n.position.map((r,o)=>s(r,t.position[o])),direction:n.direction.map((r,o)=>s(r,t.direction[o]))}}function _v(n,t,e){let i=n["local-m3"],s=n["emitted-m3"];if(!i)return{emitted:s,parcels:[]};if(!t||s<t.emitted){let c=n["flow-m3-s"]>0?[.15,1,2.8]:[1,2.8,4.5];return{emitted:s,parcels:c.map((h,u)=>nu(n,e-h*1e3,i*[.25,.45,.3][u])).reverse()}}let r=Math.min(i,s-t.emitted),o=i-r,a=t.parcels,l=a.reduce((c,h)=>c+h.mass,0);if(!o)a.length=0;else if(l)for(let c of a)c.mass*=o/l;else a.push(nu(n,e-1500,o));if(r){let c=nu(n,e,r),h=a.at(-1);h&&Math.floor(e/180)===Math.floor(h.time/180)?a[a.length-1]=lp(h,c):a.push(c)}return a.length>8&&a.splice(0,2,lp(a[0],a[1])),{emitted:s,parcels:a}}function vv(n,t,e){let i=Math.max(0,(e-t.time)*.001),s=n.grid,r=n.spacing,o=n.room,a=Math.hypot(...t.direction)||1,l=t.direction.map(A=>A/a),h=Math.min(4.5,.45+Math.sqrt(t.flow))*(1-Math.exp(-i/1.4))+.12,u=.12+.15*Math.sqrt(i)+.035*Math.min(i,15),d=u+.3*Math.exp(-i/3),f=Math.min(i,12),p=t.position[0]*.9+t.position[2]*.65,x=[.07*f+.1*Math.sin(p+f)*Math.min(1,f),.11*f,.05*f+.09*Math.sin(p+f*.6)*Math.min(1,f)],m=[-o[0]/2,0,-o[2]/2],g=t.position.map((A,C)=>rp(A+h*l[C]+x[C],m[C],m[C]+o[C])),_=r.map(A=>Math.max(u,.48*A)),y=_.map(A=>3*Math.max(A,d)),S=(A,C)=>rp(Math.floor((C-m[A])/r[A]),0,s[A]-1),T=g.map((A,C)=>S(C,A-y[C])),w=g.map((A,C)=>S(C,A+y[C])),v=n.advected,M=Math.max(d,.48*Math.hypot(...r)),D=0;for(let A=T[2];A<=w[2];A++)for(let C=T[1];C<=w[1];C++)for(let U=T[0];U<=w[0];U++){let L=(U+.5)*r[0]+m[0]-g[0],B=(C+.5)*r[1]+m[1]-g[1],G=(A+.5)*r[2]+m[2]-g[2],z=L*l[0]+B*l[1]+G*l[2],tt=Math.exp(-.5*(((L-z*l[0])/_[0])**2+((B-z*l[1])/_[1])**2+((G-z*l[2])/_[2])**2+(z/M)**2));v[U+s[0]*(C+s[1]*A)]=tt,D+=tt}if(!D)return;let P=t.mass/(D*r[0]*r[1]*r[2]);for(let A=T[2];A<=w[2];A++)for(let C=T[1];C<=w[1];C++)for(let U=T[0];U<=w[0];U++){let L=U+s[0]*(C+s[1]*A);n.data[L]+=P*v[L]}}var ic=class{constructor(){this.sources=new Map,this.sample=null,this.rebase=!0,this.paintAt=null}suspend(){this.rebase=!0}accept(t,e){let i=e["sample-time-ms"];if(!Number.isFinite(i)||!iu(e["mixed-density"])||!Array.isArray(e.sources))return!1;let s=JSON.stringify([t,e.epoch]),r=this.sample?.["sample-time-ms"];if(s===this.epoch&&(i<r||!this.rebase&&i===r&&xv(e,this.sample)))return!1;let o=this.rebase||s!==this.epoch||i-r>1e3,a=new Map,l=this.localVolume,c=this.sample?.["mixed-density"];this.localVolume=0;for(let h of e.sources){let u=h["fixture-id"];u==null||!op(h.position)||!op(h.direction)||!["local-m3","emitted-m3","flow-m3-s"].every(d=>iu(h[d]))||(this.localVolume+=h["local-m3"],a.set(u,_v(h,o?null:this.sources.get(u),i)))}return this.sources=a,this.epoch=s,this.sample=e,this.rebase=!1,this.elapsed=0,this.dirty||=o||this.localVolume>0||l>0||c!==e["mixed-density"],!0}paint(t,e){if(!this.sample)return!1;this.elapsed=Math.min(.1,this.elapsed+Math.max(0,e));let i=this.sample["sample-time-ms"]+1e3*this.elapsed;if(!this.dirty&&this.field===t&&(!this.localVolume||Math.abs(i-this.paintAt)<49.99))return!1;t.data.fill(this.sample["mixed-density"]),t.temperature.fill(0),t.sources=[];for(let{parcels:s}of this.sources.values())for(let r of s)vv(t,r,i);return t.time=i*.001%1e4,t.steps++,this.dirty=!1,this.paintAt=i,this.field=t,!0}get extinctionPerM(){let t=this.sample?.["extinction-per-m"];return iu(t)?t:1.25}stats(){return{sampleTimeMs:this.sample?.["sample-time-ms"],mixedDensity:this.sample?.["mixed-density"],extinctionPerM:this.extinctionPerM,referencePathM:this.sample?.["reference-path-m"],fogLevel:this.sample?.["fog-level"],mixedVolumeM3:this.sample?.["mixed-m3"],roomVolumeM3:this.sample?.["volume-m3"],localVolumeM3:this.localVolume,foggerCount:this.sources.size}}};var cp=[[0,0],[.68,0],[-.68,0],[0,.68],[0,-.68]],yv=n=>Array.isArray(n)&&n.length===3&&n.every(Number.isFinite),su=n=>Math.max(n.r,n.g,n.b);function Mv(n,t,e,i,s=1.25){if(t<=0||s<=0)return 1;let r=6,[o,a,l]=n.room,[c,h,u]=n.grid,d=0;for(let f=0;f<r;f++){let p=(f+.5)/r,x=(e.x+(i.x-e.x)*p)/o+.5,m=(e.y+(i.y-e.y)*p)/a,g=(e.z+(i.z-e.z)*p)/l+.5;d+=n.sample(x*c-.5,m*h-.5,g*u-.5)}return eu(s,t*d/r,e.distanceTo(i))}function Sv(n,t,e){if(!e||!["hit","center","normal","major-axis","minor-axis"].every(d=>yv(e[d])))return;let i=e["major-radius-m"],s=e["minor-radius-m"];if(!(i>0&&s>0&&Number.isFinite(i*s)))return;let r=new R(...e.normal).normalize(),o=-t.direction.dot(r);if(o<=1e-5)return;let a=new R(...e.hit),l=a.clone().sub(t.position).dot(r)/-o;if(!(l>0&&l<t.distance-t.apexDistance))return;let c=t.position.clone().addScaledVector(t.direction,l),h=new R(...e.center).add(c.clone().sub(a)),u=t.intensity*o/(l+t.apexDistance)**2;return{id:n.id,fixture:n.root,surface:String(e["surface-id"]),origin:t.position,center:h,normal:r,major:new R(...e["major-axis"]).multiplyScalar(i),minor:new R(...e["minor-axis"]).multiplyScalar(s),radius:Math.sqrt(i*s),power:u*i*s,color:t.color}}var nc=class{constructor(t,e,i,s=[]){if(this.chunks=[],this.targets=[],e){t.updateMatrixWorld(!0);let r=new Map(s.map(o=>[o.root,o]));for(let o of t.children){let a=[];if(o.traverse(l=>{l.isMesh&&a.push(l)}),!!a.length)if(r.has(o))this.chunks.push({meshes:a,root:o,fixture:r.get(o),sphere:new ri});else for(let l of a){let c=l.isInstancedMesh?l:l.geometry;c.boundingSphere||c.computeBoundingSphere(),this.chunks.push({meshes:[l],mesh:l,local:c.boundingSphere,sphere:new ri})}}}this.ray=new Xi,this.hits=[],this.lights=Array.from({length:e},()=>{let r=new ss(0,0,15,Math.PI*.47,1,2);return r.castShadow=!0,r.shadow.autoUpdate=!1,r.shadow.needsUpdate=!0,r.shadow.mapSize.set(i,i),r.shadow.camera.near=.015,r.shadow.bias=-1e-4,r.shadow.normalBias=.005,t.add(r,r.target),r}),this.patches=[]}addCandidate(t,e,i,s){if(!this.lights.length||!e.fixture["pseudo-fixture"])return;let r=Sv(e,i,s);r&&t.push(r)}sample(t,e){let i=e.clone().sub(t.origin),s=i.length();this.ray.set(t.origin,i.normalize()),this.ray.near=.003,this.ray.far=s+.04,this.hits.length=0,this.targets.length=0;for(let{root:o,meshes:a,sphere:l}of this.chunks)o===t.fixture||t.origin.distanceToSquared(l.center)>(l.radius+this.ray.far)**2||this.ray.ray.intersectsSphere(l)&&this.targets.push(...a);let[r]=this.ray.intersectObjects(this.targets,!1,this.hits);if(!(r?.object.userData.receiver!==t.surface||Math.abs(r.distance-s)>.04))return r.object.material}resolve(t){if(this.patches.length=0,!!t.length){for(let e of this.chunks)e.fixture?(e.sphere.center.setFromMatrixPosition(e.root.matrixWorld),e.sphere.radius=Math.max(1,1.5*fs(e.fixture.fixture)+.3)):e.sphere.copy(e.local).applyMatrix4(e.mesh.matrixWorld);t.sort((e,i)=>su(i.color)*i.power-su(e.color)*e.power);for(let e of t){let i=new R,s=new St(0,0,0),r=0;for(let[o,a]of cp){let l=e.center.clone().addScaledVector(e.major,o).addScaledVector(e.minor,a),c=this.sample(e,l);if(!c)continue;let h=c.color.clone().multiplyScalar((1-c.metalness)*(c.userData.meanMapReflectance??1));s.add(h),i.add(l),r++}if(!(!r||su(s)<=0)&&(e.position=i.divideScalar(r),e.reflected=s.multiplyScalar(1/cp.length).multiply(e.color),this.patches.push(e),this.patches.length===this.lights.length))break}for(let e=0;e<this.patches.length;e++)this.lights[e].shadow.needsUpdate=!0}}illuminate(t,e,i){this.lights.forEach((s,r)=>{let o=this.patches[r];if(!o){s.intensity=0,s.color.setRGB(0,0,0);return}s.color.copy(o.reflected),s.intensity=o.power*Mv(t,e,o.origin,o.position,i),s.userData.patchRadius=o.radius,s.position.copy(o.position).addScaledVector(o.normal,.018),s.target.position.copy(s.position).add(o.normal),s.target.updateMatrixWorld()})}dispose(){for(let t of this.lights)t.dispose()}};var oc=class extends Lt{constructor(t,e,i=!1,s=!1,r=1e4){let o=new Yt;super(o,e),this.isMarchingCubes=!0;let a=this,l=new Float32Array(36),c=new Float32Array(36),h=new Float32Array(36);this.enableUvs=i,this.enableColors=s,this.init=function(_){this.resolution=_,this.isolation=80,this.size=_,this.size2=this.size*this.size,this.size3=this.size2*this.size,this.halfsize=this.size/2,this.delta=2/this.size,this.yd=this.size,this.zd=this.size2,this.field=new Float32Array(this.size3),this.normal_cache=new Float32Array(this.size3*3),this.palette=new Float32Array(this.size3*3),this.count=0;let y=r*3;this.positionArray=new Float32Array(y*3);let S=new re(this.positionArray,3);S.setUsage(Yi),o.setAttribute("position",S),this.normalArray=new Float32Array(y*3);let T=new re(this.normalArray,3);if(T.setUsage(Yi),o.setAttribute("normal",T),this.enableUvs){this.uvArray=new Float32Array(y*2);let w=new re(this.uvArray,2);w.setUsage(Yi),o.setAttribute("uv",w)}if(this.enableColors){this.colorArray=new Float32Array(y*3);let w=new re(this.colorArray,3);w.setUsage(Yi),o.setAttribute("color",w)}o.boundingSphere=new ri(new R,1)};function u(_,y,S){return _+(y-_)*S}function d(_,y,S,T,w,v,M,D,P,A){let C=(S-M)/(D-M),U=a.normal_cache;l[y+0]=T+C*a.delta,l[y+1]=w,l[y+2]=v,c[y+0]=u(U[_+0],U[_+3],C),c[y+1]=u(U[_+1],U[_+4],C),c[y+2]=u(U[_+2],U[_+5],C),h[y+0]=u(a.palette[P*3+0],a.palette[A*3+0],C),h[y+1]=u(a.palette[P*3+1],a.palette[A*3+1],C),h[y+2]=u(a.palette[P*3+2],a.palette[A*3+2],C)}function f(_,y,S,T,w,v,M,D,P,A){let C=(S-M)/(D-M),U=a.normal_cache;l[y+0]=T,l[y+1]=w+C*a.delta,l[y+2]=v;let L=_+a.yd*3;c[y+0]=u(U[_+0],U[L+0],C),c[y+1]=u(U[_+1],U[L+1],C),c[y+2]=u(U[_+2],U[L+2],C),h[y+0]=u(a.palette[P*3+0],a.palette[A*3+0],C),h[y+1]=u(a.palette[P*3+1],a.palette[A*3+1],C),h[y+2]=u(a.palette[P*3+2],a.palette[A*3+2],C)}function p(_,y,S,T,w,v,M,D,P,A){let C=(S-M)/(D-M),U=a.normal_cache;l[y+0]=T,l[y+1]=w,l[y+2]=v+C*a.delta;let L=_+a.zd*3;c[y+0]=u(U[_+0],U[L+0],C),c[y+1]=u(U[_+1],U[L+1],C),c[y+2]=u(U[_+2],U[L+2],C),h[y+0]=u(a.palette[P*3+0],a.palette[A*3+0],C),h[y+1]=u(a.palette[P*3+1],a.palette[A*3+1],C),h[y+2]=u(a.palette[P*3+2],a.palette[A*3+2],C)}function x(_){let y=_*3;a.normal_cache[y]===0&&(a.normal_cache[y+0]=a.field[_-1]-a.field[_+1],a.normal_cache[y+1]=a.field[_-a.yd]-a.field[_+a.yd],a.normal_cache[y+2]=a.field[_-a.zd]-a.field[_+a.zd])}function m(_,y,S,T,w){let v=T+1,M=T+a.yd,D=T+a.zd,P=v+a.yd,A=v+a.zd,C=T+a.yd+a.zd,U=v+a.yd+a.zd,L=0,B=a.field[T],G=a.field[v],z=a.field[M],tt=a.field[P],W=a.field[D],J=a.field[A],j=a.field[C],xt=a.field[U];B<w&&(L|=1),G<w&&(L|=2),z<w&&(L|=8),tt<w&&(L|=4),W<w&&(L|=16),J<w&&(L|=32),j<w&&(L|=128),xt<w&&(L|=64);let Mt=bv[L];if(Mt===0)return 0;let q=a.delta,ot=_+q,gt=y+q,wt=S+q;Mt&1&&(x(T),x(v),d(T*3,0,w,_,y,S,B,G,T,v)),Mt&2&&(x(v),x(P),f(v*3,3,w,ot,y,S,G,tt,v,P)),Mt&4&&(x(M),x(P),d(M*3,6,w,_,gt,S,z,tt,M,P)),Mt&8&&(x(T),x(M),f(T*3,9,w,_,y,S,B,z,T,M)),Mt&16&&(x(D),x(A),d(D*3,12,w,_,y,wt,W,J,D,A)),Mt&32&&(x(A),x(U),f(A*3,15,w,ot,y,wt,J,xt,A,U)),Mt&64&&(x(C),x(U),d(C*3,18,w,_,gt,wt,j,xt,C,U)),Mt&128&&(x(D),x(C),f(D*3,21,w,_,y,wt,W,j,D,C)),Mt&256&&(x(T),x(D),p(T*3,24,w,_,y,S,B,W,T,D)),Mt&512&&(x(v),x(A),p(v*3,27,w,ot,y,S,G,J,v,A)),Mt&1024&&(x(P),x(U),p(P*3,30,w,ot,gt,S,tt,xt,P,U)),Mt&2048&&(x(M),x(C),p(M*3,33,w,_,gt,S,z,j,M,C)),L<<=4;let rt,Dt,ie,bt=0,Bt=0;for(;rc[L+Bt]!=-1;)rt=L+Bt,Dt=rt+1,ie=rt+2,g(l,c,h,3*rc[rt],3*rc[Dt],3*rc[ie]),Bt+=3,bt++;return bt}function g(_,y,S,T,w,v){let M=a.count*3;if(a.positionArray[M+0]=_[T],a.positionArray[M+1]=_[T+1],a.positionArray[M+2]=_[T+2],a.positionArray[M+3]=_[w],a.positionArray[M+4]=_[w+1],a.positionArray[M+5]=_[w+2],a.positionArray[M+6]=_[v],a.positionArray[M+7]=_[v+1],a.positionArray[M+8]=_[v+2],a.material.flatShading===!0){let D=(y[T+0]+y[w+0]+y[v+0])/3,P=(y[T+1]+y[w+1]+y[v+1])/3,A=(y[T+2]+y[w+2]+y[v+2])/3;a.normalArray[M+0]=D,a.normalArray[M+1]=P,a.normalArray[M+2]=A,a.normalArray[M+3]=D,a.normalArray[M+4]=P,a.normalArray[M+5]=A,a.normalArray[M+6]=D,a.normalArray[M+7]=P,a.normalArray[M+8]=A}else a.normalArray[M+0]=y[T+0],a.normalArray[M+1]=y[T+1],a.normalArray[M+2]=y[T+2],a.normalArray[M+3]=y[w+0],a.normalArray[M+4]=y[w+1],a.normalArray[M+5]=y[w+2],a.normalArray[M+6]=y[v+0],a.normalArray[M+7]=y[v+1],a.normalArray[M+8]=y[v+2];if(a.enableUvs){let D=a.count*2;a.uvArray[D+0]=_[T+0],a.uvArray[D+1]=_[T+2],a.uvArray[D+2]=_[w+0],a.uvArray[D+3]=_[w+2],a.uvArray[D+4]=_[v+0],a.uvArray[D+5]=_[v+2]}a.enableColors&&(a.colorArray[M+0]=S[T+0],a.colorArray[M+1]=S[T+1],a.colorArray[M+2]=S[T+2],a.colorArray[M+3]=S[w+0],a.colorArray[M+4]=S[w+1],a.colorArray[M+5]=S[w+2],a.colorArray[M+6]=S[v+0],a.colorArray[M+7]=S[v+1],a.colorArray[M+8]=S[v+2]),a.count+=3}this.addBall=function(_,y,S,T,w,v){let M=Math.sign(T);T=Math.abs(T);let D=v!=null,P=new St(_,y,S);if(D)try{P=v instanceof St?v:Array.isArray(v)?new St(Math.min(Math.abs(v[0]),1),Math.min(Math.abs(v[1]),1),Math.min(Math.abs(v[2]),1)):new St(v)}catch{P=new St(_,y,S)}let A=this.size*Math.sqrt(T/w),C=S*this.size,U=y*this.size,L=_*this.size,B=Math.floor(C-A);B<1&&(B=1);let G=Math.floor(C+A);G>this.size-1&&(G=this.size-1);let z=Math.floor(U-A);z<1&&(z=1);let tt=Math.floor(U+A);tt>this.size-1&&(tt=this.size-1);let W=Math.floor(L-A);W<1&&(W=1);let J=Math.floor(L+A);J>this.size-1&&(J=this.size-1);let j,xt,Mt,q,ot,gt,wt,rt,Dt,ie,bt;for(Mt=B;Mt<G;Mt++)for(ot=this.size2*Mt,rt=Mt/this.size-S,Dt=rt*rt,xt=z;xt<tt;xt++)for(q=ot+this.size*xt,wt=xt/this.size-y,ie=wt*wt,j=W;j<J;j++)if(gt=j/this.size-_,bt=T/(1e-6+gt*gt+ie+Dt)-w,bt>0){this.field[q+j]+=bt*M;let Bt=Math.sqrt((j-L)*(j-L)+(xt-U)*(xt-U)+(Mt-C)*(Mt-C))/A,Jt=1-Bt*Bt*Bt*(Bt*(Bt*6-15)+10);this.palette[(q+j)*3+0]+=P.r*Jt,this.palette[(q+j)*3+1]+=P.g*Jt,this.palette[(q+j)*3+2]+=P.b*Jt}},this.addPlaneX=function(_,y){let S=this.size,T=this.yd,w=this.zd,v=this.field,M,D,P,A,C,U,L,B=S*Math.sqrt(_/y);for(B>S&&(B=S),M=0;M<B;M++)if(U=M/S,A=U*U,C=_/(1e-4+A)-y,C>0)for(D=0;D<S;D++)for(L=M+D*T,P=0;P<S;P++)v[w*P+L]+=C},this.addPlaneY=function(_,y){let S=this.size,T=this.yd,w=this.zd,v=this.field,M,D,P,A,C,U,L,B,G=S*Math.sqrt(_/y);for(G>S&&(G=S),D=0;D<G;D++)if(U=D/S,A=U*U,C=_/(1e-4+A)-y,C>0)for(L=D*T,M=0;M<S;M++)for(B=L+M,P=0;P<S;P++)v[w*P+B]+=C},this.addPlaneZ=function(_,y){let S=this.size,T=this.yd,w=this.zd,v=this.field,M,D,P,A,C,U,L,B,G=S*Math.sqrt(_/y);for(G>S&&(G=S),P=0;P<G;P++)if(U=P/S,A=U*U,C=_/(1e-4+A)-y,C>0)for(L=w*P,D=0;D<S;D++)for(B=L+D*T,M=0;M<S;M++)v[B+M]+=C},this.setCell=function(_,y,S,T){let w=this.size2*S+this.size*y+_;this.field[w]=T},this.getCell=function(_,y,S){let T=this.size2*S+this.size*y+_;return this.field[T]},this.blur=function(_=1){let y=this.field,S=y.slice(),T=this.size,w=this.size2;for(let v=0;v<T;v++)for(let M=0;M<T;M++)for(let D=0;D<T;D++){let P=w*D+T*M+v,A=S[P],C=1;for(let U=-1;U<=1;U+=2){let L=U+v;if(!(L<0||L>=T))for(let B=-1;B<=1;B+=2){let G=B+M;if(!(G<0||G>=T))for(let z=-1;z<=1;z+=2){let tt=z+D;if(tt<0||tt>=T)continue;let W=w*tt+T*G+L,J=S[W];C++,A+=_*(J-A)/C}}}y[P]=A}},this.reset=function(){for(let _=0;_<this.size3;_++)this.normal_cache[_*3]=0,this.field[_]=0,this.palette[_*3]=this.palette[_*3+1]=this.palette[_*3+2]=0},this.update=function(){this.count=0;let _=this.size-2;for(let y=1;y<_;y++){let S=this.size2*y,T=(y-this.halfsize)/this.halfsize;for(let w=1;w<_;w++){let v=S+this.size*w,M=(w-this.halfsize)/this.halfsize;for(let D=1;D<_;D++){let P=(D-this.halfsize)/this.halfsize,A=v+D;m(P,M,T,A,this.isolation)}}}this.geometry.setDrawRange(0,this.count),o.getAttribute("position").needsUpdate=!0,o.getAttribute("normal").needsUpdate=!0,this.enableUvs&&(o.getAttribute("uv").needsUpdate=!0),this.enableColors&&(o.getAttribute("color").needsUpdate=!0),this.count/3>r&&console.warn("THREE.MarchingCubes: Geometry buffers too small for rendering. Please create an instance with a higher poly count.")},this.init(t)}},bv=new Int32Array([0,265,515,778,1030,1295,1541,1804,2060,2309,2575,2822,3082,3331,3593,3840,400,153,915,666,1430,1183,1941,1692,2460,2197,2975,2710,3482,3219,3993,3728,560,825,51,314,1590,1855,1077,1340,2620,2869,2111,2358,3642,3891,3129,3376,928,681,419,170,1958,1711,1445,1196,2988,2725,2479,2214,4010,3747,3497,3232,1120,1385,1635,1898,102,367,613,876,3180,3429,3695,3942,2154,2403,2665,2912,1520,1273,2035,1786,502,255,1013,764,3580,3317,4095,3830,2554,2291,3065,2800,1616,1881,1107,1370,598,863,85,348,3676,3925,3167,3414,2650,2899,2137,2384,1984,1737,1475,1226,966,719,453,204,4044,3781,3535,3270,3018,2755,2505,2240,2240,2505,2755,3018,3270,3535,3781,4044,204,453,719,966,1226,1475,1737,1984,2384,2137,2899,2650,3414,3167,3925,3676,348,85,863,598,1370,1107,1881,1616,2800,3065,2291,2554,3830,4095,3317,3580,764,1013,255,502,1786,2035,1273,1520,2912,2665,2403,2154,3942,3695,3429,3180,876,613,367,102,1898,1635,1385,1120,3232,3497,3747,4010,2214,2479,2725,2988,1196,1445,1711,1958,170,419,681,928,3376,3129,3891,3642,2358,2111,2869,2620,1340,1077,1855,1590,314,51,825,560,3728,3993,3219,3482,2710,2975,2197,2460,1692,1941,1183,1430,666,915,153,400,3840,3593,3331,3082,2822,2575,2309,2060,1804,1541,1295,1030,778,515,265,0]),rc=new Int32Array([-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,0,8,3,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,0,1,9,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,1,8,3,9,8,1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,1,2,10,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,0,8,3,1,2,10,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,9,2,10,0,2,9,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,2,8,3,2,10,8,10,9,8,-1,-1,-1,-1,-1,-1,-1,3,11,2,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,0,11,2,8,11,0,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,1,9,0,2,3,11,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,1,11,2,1,9,11,9,8,11,-1,-1,-1,-1,-1,-1,-1,3,10,1,11,10,3,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,0,10,1,0,8,10,8,11,10,-1,-1,-1,-1,-1,-1,-1,3,9,0,3,11,9,11,10,9,-1,-1,-1,-1,-1,-1,-1,9,8,10,10,8,11,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,4,7,8,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,4,3,0,7,3,4,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,0,1,9,8,4,7,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,4,1,9,4,7,1,7,3,1,-1,-1,-1,-1,-1,-1,-1,1,2,10,8,4,7,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,3,4,7,3,0,4,1,2,10,-1,-1,-1,-1,-1,-1,-1,9,2,10,9,0,2,8,4,7,-1,-1,-1,-1,-1,-1,-1,2,10,9,2,9,7,2,7,3,7,9,4,-1,-1,-1,-1,8,4,7,3,11,2,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,11,4,7,11,2,4,2,0,4,-1,-1,-1,-1,-1,-1,-1,9,0,1,8,4,7,2,3,11,-1,-1,-1,-1,-1,-1,-1,4,7,11,9,4,11,9,11,2,9,2,1,-1,-1,-1,-1,3,10,1,3,11,10,7,8,4,-1,-1,-1,-1,-1,-1,-1,1,11,10,1,4,11,1,0,4,7,11,4,-1,-1,-1,-1,4,7,8,9,0,11,9,11,10,11,0,3,-1,-1,-1,-1,4,7,11,4,11,9,9,11,10,-1,-1,-1,-1,-1,-1,-1,9,5,4,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,9,5,4,0,8,3,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,0,5,4,1,5,0,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,8,5,4,8,3,5,3,1,5,-1,-1,-1,-1,-1,-1,-1,1,2,10,9,5,4,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,3,0,8,1,2,10,4,9,5,-1,-1,-1,-1,-1,-1,-1,5,2,10,5,4,2,4,0,2,-1,-1,-1,-1,-1,-1,-1,2,10,5,3,2,5,3,5,4,3,4,8,-1,-1,-1,-1,9,5,4,2,3,11,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,0,11,2,0,8,11,4,9,5,-1,-1,-1,-1,-1,-1,-1,0,5,4,0,1,5,2,3,11,-1,-1,-1,-1,-1,-1,-1,2,1,5,2,5,8,2,8,11,4,8,5,-1,-1,-1,-1,10,3,11,10,1,3,9,5,4,-1,-1,-1,-1,-1,-1,-1,4,9,5,0,8,1,8,10,1,8,11,10,-1,-1,-1,-1,5,4,0,5,0,11,5,11,10,11,0,3,-1,-1,-1,-1,5,4,8,5,8,10,10,8,11,-1,-1,-1,-1,-1,-1,-1,9,7,8,5,7,9,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,9,3,0,9,5,3,5,7,3,-1,-1,-1,-1,-1,-1,-1,0,7,8,0,1,7,1,5,7,-1,-1,-1,-1,-1,-1,-1,1,5,3,3,5,7,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,9,7,8,9,5,7,10,1,2,-1,-1,-1,-1,-1,-1,-1,10,1,2,9,5,0,5,3,0,5,7,3,-1,-1,-1,-1,8,0,2,8,2,5,8,5,7,10,5,2,-1,-1,-1,-1,2,10,5,2,5,3,3,5,7,-1,-1,-1,-1,-1,-1,-1,7,9,5,7,8,9,3,11,2,-1,-1,-1,-1,-1,-1,-1,9,5,7,9,7,2,9,2,0,2,7,11,-1,-1,-1,-1,2,3,11,0,1,8,1,7,8,1,5,7,-1,-1,-1,-1,11,2,1,11,1,7,7,1,5,-1,-1,-1,-1,-1,-1,-1,9,5,8,8,5,7,10,1,3,10,3,11,-1,-1,-1,-1,5,7,0,5,0,9,7,11,0,1,0,10,11,10,0,-1,11,10,0,11,0,3,10,5,0,8,0,7,5,7,0,-1,11,10,5,7,11,5,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,10,6,5,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,0,8,3,5,10,6,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,9,0,1,5,10,6,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,1,8,3,1,9,8,5,10,6,-1,-1,-1,-1,-1,-1,-1,1,6,5,2,6,1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,1,6,5,1,2,6,3,0,8,-1,-1,-1,-1,-1,-1,-1,9,6,5,9,0,6,0,2,6,-1,-1,-1,-1,-1,-1,-1,5,9,8,5,8,2,5,2,6,3,2,8,-1,-1,-1,-1,2,3,11,10,6,5,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,11,0,8,11,2,0,10,6,5,-1,-1,-1,-1,-1,-1,-1,0,1,9,2,3,11,5,10,6,-1,-1,-1,-1,-1,-1,-1,5,10,6,1,9,2,9,11,2,9,8,11,-1,-1,-1,-1,6,3,11,6,5,3,5,1,3,-1,-1,-1,-1,-1,-1,-1,0,8,11,0,11,5,0,5,1,5,11,6,-1,-1,-1,-1,3,11,6,0,3,6,0,6,5,0,5,9,-1,-1,-1,-1,6,5,9,6,9,11,11,9,8,-1,-1,-1,-1,-1,-1,-1,5,10,6,4,7,8,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,4,3,0,4,7,3,6,5,10,-1,-1,-1,-1,-1,-1,-1,1,9,0,5,10,6,8,4,7,-1,-1,-1,-1,-1,-1,-1,10,6,5,1,9,7,1,7,3,7,9,4,-1,-1,-1,-1,6,1,2,6,5,1,4,7,8,-1,-1,-1,-1,-1,-1,-1,1,2,5,5,2,6,3,0,4,3,4,7,-1,-1,-1,-1,8,4,7,9,0,5,0,6,5,0,2,6,-1,-1,-1,-1,7,3,9,7,9,4,3,2,9,5,9,6,2,6,9,-1,3,11,2,7,8,4,10,6,5,-1,-1,-1,-1,-1,-1,-1,5,10,6,4,7,2,4,2,0,2,7,11,-1,-1,-1,-1,0,1,9,4,7,8,2,3,11,5,10,6,-1,-1,-1,-1,9,2,1,9,11,2,9,4,11,7,11,4,5,10,6,-1,8,4,7,3,11,5,3,5,1,5,11,6,-1,-1,-1,-1,5,1,11,5,11,6,1,0,11,7,11,4,0,4,11,-1,0,5,9,0,6,5,0,3,6,11,6,3,8,4,7,-1,6,5,9,6,9,11,4,7,9,7,11,9,-1,-1,-1,-1,10,4,9,6,4,10,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,4,10,6,4,9,10,0,8,3,-1,-1,-1,-1,-1,-1,-1,10,0,1,10,6,0,6,4,0,-1,-1,-1,-1,-1,-1,-1,8,3,1,8,1,6,8,6,4,6,1,10,-1,-1,-1,-1,1,4,9,1,2,4,2,6,4,-1,-1,-1,-1,-1,-1,-1,3,0,8,1,2,9,2,4,9,2,6,4,-1,-1,-1,-1,0,2,4,4,2,6,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,8,3,2,8,2,4,4,2,6,-1,-1,-1,-1,-1,-1,-1,10,4,9,10,6,4,11,2,3,-1,-1,-1,-1,-1,-1,-1,0,8,2,2,8,11,4,9,10,4,10,6,-1,-1,-1,-1,3,11,2,0,1,6,0,6,4,6,1,10,-1,-1,-1,-1,6,4,1,6,1,10,4,8,1,2,1,11,8,11,1,-1,9,6,4,9,3,6,9,1,3,11,6,3,-1,-1,-1,-1,8,11,1,8,1,0,11,6,1,9,1,4,6,4,1,-1,3,11,6,3,6,0,0,6,4,-1,-1,-1,-1,-1,-1,-1,6,4,8,11,6,8,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,7,10,6,7,8,10,8,9,10,-1,-1,-1,-1,-1,-1,-1,0,7,3,0,10,7,0,9,10,6,7,10,-1,-1,-1,-1,10,6,7,1,10,7,1,7,8,1,8,0,-1,-1,-1,-1,10,6,7,10,7,1,1,7,3,-1,-1,-1,-1,-1,-1,-1,1,2,6,1,6,8,1,8,9,8,6,7,-1,-1,-1,-1,2,6,9,2,9,1,6,7,9,0,9,3,7,3,9,-1,7,8,0,7,0,6,6,0,2,-1,-1,-1,-1,-1,-1,-1,7,3,2,6,7,2,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,2,3,11,10,6,8,10,8,9,8,6,7,-1,-1,-1,-1,2,0,7,2,7,11,0,9,7,6,7,10,9,10,7,-1,1,8,0,1,7,8,1,10,7,6,7,10,2,3,11,-1,11,2,1,11,1,7,10,6,1,6,7,1,-1,-1,-1,-1,8,9,6,8,6,7,9,1,6,11,6,3,1,3,6,-1,0,9,1,11,6,7,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,7,8,0,7,0,6,3,11,0,11,6,0,-1,-1,-1,-1,7,11,6,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,7,6,11,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,3,0,8,11,7,6,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,0,1,9,11,7,6,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,8,1,9,8,3,1,11,7,6,-1,-1,-1,-1,-1,-1,-1,10,1,2,6,11,7,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,1,2,10,3,0,8,6,11,7,-1,-1,-1,-1,-1,-1,-1,2,9,0,2,10,9,6,11,7,-1,-1,-1,-1,-1,-1,-1,6,11,7,2,10,3,10,8,3,10,9,8,-1,-1,-1,-1,7,2,3,6,2,7,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,7,0,8,7,6,0,6,2,0,-1,-1,-1,-1,-1,-1,-1,2,7,6,2,3,7,0,1,9,-1,-1,-1,-1,-1,-1,-1,1,6,2,1,8,6,1,9,8,8,7,6,-1,-1,-1,-1,10,7,6,10,1,7,1,3,7,-1,-1,-1,-1,-1,-1,-1,10,7,6,1,7,10,1,8,7,1,0,8,-1,-1,-1,-1,0,3,7,0,7,10,0,10,9,6,10,7,-1,-1,-1,-1,7,6,10,7,10,8,8,10,9,-1,-1,-1,-1,-1,-1,-1,6,8,4,11,8,6,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,3,6,11,3,0,6,0,4,6,-1,-1,-1,-1,-1,-1,-1,8,6,11,8,4,6,9,0,1,-1,-1,-1,-1,-1,-1,-1,9,4,6,9,6,3,9,3,1,11,3,6,-1,-1,-1,-1,6,8,4,6,11,8,2,10,1,-1,-1,-1,-1,-1,-1,-1,1,2,10,3,0,11,0,6,11,0,4,6,-1,-1,-1,-1,4,11,8,4,6,11,0,2,9,2,10,9,-1,-1,-1,-1,10,9,3,10,3,2,9,4,3,11,3,6,4,6,3,-1,8,2,3,8,4,2,4,6,2,-1,-1,-1,-1,-1,-1,-1,0,4,2,4,6,2,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,1,9,0,2,3,4,2,4,6,4,3,8,-1,-1,-1,-1,1,9,4,1,4,2,2,4,6,-1,-1,-1,-1,-1,-1,-1,8,1,3,8,6,1,8,4,6,6,10,1,-1,-1,-1,-1,10,1,0,10,0,6,6,0,4,-1,-1,-1,-1,-1,-1,-1,4,6,3,4,3,8,6,10,3,0,3,9,10,9,3,-1,10,9,4,6,10,4,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,4,9,5,7,6,11,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,0,8,3,4,9,5,11,7,6,-1,-1,-1,-1,-1,-1,-1,5,0,1,5,4,0,7,6,11,-1,-1,-1,-1,-1,-1,-1,11,7,6,8,3,4,3,5,4,3,1,5,-1,-1,-1,-1,9,5,4,10,1,2,7,6,11,-1,-1,-1,-1,-1,-1,-1,6,11,7,1,2,10,0,8,3,4,9,5,-1,-1,-1,-1,7,6,11,5,4,10,4,2,10,4,0,2,-1,-1,-1,-1,3,4,8,3,5,4,3,2,5,10,5,2,11,7,6,-1,7,2,3,7,6,2,5,4,9,-1,-1,-1,-1,-1,-1,-1,9,5,4,0,8,6,0,6,2,6,8,7,-1,-1,-1,-1,3,6,2,3,7,6,1,5,0,5,4,0,-1,-1,-1,-1,6,2,8,6,8,7,2,1,8,4,8,5,1,5,8,-1,9,5,4,10,1,6,1,7,6,1,3,7,-1,-1,-1,-1,1,6,10,1,7,6,1,0,7,8,7,0,9,5,4,-1,4,0,10,4,10,5,0,3,10,6,10,7,3,7,10,-1,7,6,10,7,10,8,5,4,10,4,8,10,-1,-1,-1,-1,6,9,5,6,11,9,11,8,9,-1,-1,-1,-1,-1,-1,-1,3,6,11,0,6,3,0,5,6,0,9,5,-1,-1,-1,-1,0,11,8,0,5,11,0,1,5,5,6,11,-1,-1,-1,-1,6,11,3,6,3,5,5,3,1,-1,-1,-1,-1,-1,-1,-1,1,2,10,9,5,11,9,11,8,11,5,6,-1,-1,-1,-1,0,11,3,0,6,11,0,9,6,5,6,9,1,2,10,-1,11,8,5,11,5,6,8,0,5,10,5,2,0,2,5,-1,6,11,3,6,3,5,2,10,3,10,5,3,-1,-1,-1,-1,5,8,9,5,2,8,5,6,2,3,8,2,-1,-1,-1,-1,9,5,6,9,6,0,0,6,2,-1,-1,-1,-1,-1,-1,-1,1,5,8,1,8,0,5,6,8,3,8,2,6,2,8,-1,1,5,6,2,1,6,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,1,3,6,1,6,10,3,8,6,5,6,9,8,9,6,-1,10,1,0,10,0,6,9,5,0,5,6,0,-1,-1,-1,-1,0,3,8,5,6,10,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,10,5,6,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,11,5,10,7,5,11,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,11,5,10,11,7,5,8,3,0,-1,-1,-1,-1,-1,-1,-1,5,11,7,5,10,11,1,9,0,-1,-1,-1,-1,-1,-1,-1,10,7,5,10,11,7,9,8,1,8,3,1,-1,-1,-1,-1,11,1,2,11,7,1,7,5,1,-1,-1,-1,-1,-1,-1,-1,0,8,3,1,2,7,1,7,5,7,2,11,-1,-1,-1,-1,9,7,5,9,2,7,9,0,2,2,11,7,-1,-1,-1,-1,7,5,2,7,2,11,5,9,2,3,2,8,9,8,2,-1,2,5,10,2,3,5,3,7,5,-1,-1,-1,-1,-1,-1,-1,8,2,0,8,5,2,8,7,5,10,2,5,-1,-1,-1,-1,9,0,1,5,10,3,5,3,7,3,10,2,-1,-1,-1,-1,9,8,2,9,2,1,8,7,2,10,2,5,7,5,2,-1,1,3,5,3,7,5,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,0,8,7,0,7,1,1,7,5,-1,-1,-1,-1,-1,-1,-1,9,0,3,9,3,5,5,3,7,-1,-1,-1,-1,-1,-1,-1,9,8,7,5,9,7,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,5,8,4,5,10,8,10,11,8,-1,-1,-1,-1,-1,-1,-1,5,0,4,5,11,0,5,10,11,11,3,0,-1,-1,-1,-1,0,1,9,8,4,10,8,10,11,10,4,5,-1,-1,-1,-1,10,11,4,10,4,5,11,3,4,9,4,1,3,1,4,-1,2,5,1,2,8,5,2,11,8,4,5,8,-1,-1,-1,-1,0,4,11,0,11,3,4,5,11,2,11,1,5,1,11,-1,0,2,5,0,5,9,2,11,5,4,5,8,11,8,5,-1,9,4,5,2,11,3,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,2,5,10,3,5,2,3,4,5,3,8,4,-1,-1,-1,-1,5,10,2,5,2,4,4,2,0,-1,-1,-1,-1,-1,-1,-1,3,10,2,3,5,10,3,8,5,4,5,8,0,1,9,-1,5,10,2,5,2,4,1,9,2,9,4,2,-1,-1,-1,-1,8,4,5,8,5,3,3,5,1,-1,-1,-1,-1,-1,-1,-1,0,4,5,1,0,5,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,8,4,5,8,5,3,9,0,5,0,3,5,-1,-1,-1,-1,9,4,5,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,4,11,7,4,9,11,9,10,11,-1,-1,-1,-1,-1,-1,-1,0,8,3,4,9,7,9,11,7,9,10,11,-1,-1,-1,-1,1,10,11,1,11,4,1,4,0,7,4,11,-1,-1,-1,-1,3,1,4,3,4,8,1,10,4,7,4,11,10,11,4,-1,4,11,7,9,11,4,9,2,11,9,1,2,-1,-1,-1,-1,9,7,4,9,11,7,9,1,11,2,11,1,0,8,3,-1,11,7,4,11,4,2,2,4,0,-1,-1,-1,-1,-1,-1,-1,11,7,4,11,4,2,8,3,4,3,2,4,-1,-1,-1,-1,2,9,10,2,7,9,2,3,7,7,4,9,-1,-1,-1,-1,9,10,7,9,7,4,10,2,7,8,7,0,2,0,7,-1,3,7,10,3,10,2,7,4,10,1,10,0,4,0,10,-1,1,10,2,8,7,4,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,4,9,1,4,1,7,7,1,3,-1,-1,-1,-1,-1,-1,-1,4,9,1,4,1,7,0,8,1,8,7,1,-1,-1,-1,-1,4,0,3,7,4,3,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,4,8,7,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,9,10,8,10,11,8,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,3,0,9,3,9,11,11,9,10,-1,-1,-1,-1,-1,-1,-1,0,1,10,0,10,8,8,10,11,-1,-1,-1,-1,-1,-1,-1,3,1,10,11,3,10,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,1,2,11,1,11,9,9,11,8,-1,-1,-1,-1,-1,-1,-1,3,0,9,3,9,11,1,2,9,2,11,9,-1,-1,-1,-1,0,2,11,8,0,11,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,3,2,11,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,2,3,8,2,8,10,10,8,9,-1,-1,-1,-1,-1,-1,-1,9,10,2,0,9,2,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,2,3,8,2,8,10,0,1,8,1,10,8,-1,-1,-1,-1,1,10,2,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,1,3,8,9,1,8,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,0,9,1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,0,3,8,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1]);var wv=`
varying vec3 worldPoint;
varying vec3 viewPoint;
varying vec3 viewNormal;
varying vec3 localPoint;
void main(){
  worldPoint=(modelMatrix*vec4(position,1.0)).xyz;
  viewPoint=(modelViewMatrix*vec4(position,1.0)).xyz;
  localPoint=position;
  viewNormal=normalize(normalMatrix*normal);
  gl_Position=projectionMatrix*vec4(viewPoint,1.0);
}`,Ev=`
uniform sampler2D sceneColor, opaqueDepth, atmosphere;
uniform samplerCube roomEnvironment;
uniform vec2 resolution;
uniform mat4 lens;
uniform vec3 tint, sourcePos[4], sourceColor[4], sourceDir[4];
uniform vec4 coneCos;
uniform float sizeM, clockS, velocity, nearZ, farZ, workLight, fogAmount, strength;
varying vec3 worldPoint,viewPoint,viewNormal,localPoint;
float eyeDepth(float z){return (nearZ*farZ)/(farZ-(farZ-nearZ)*z);}
vec2 uvOf(vec3 p){vec4 q=lens*vec4(p,1.0);return q.xy/max(q.w,0.001)*0.5+0.5;}
vec2 bounded(vec2 uv){return clamp(uv,0.5/resolution,1.0-0.5/resolution);}
void main(){
  vec2 uv=gl_FragCoord.xy/resolution;
  float opaque=eyeDepth(texture2D(opaqueDepth,uv).r), depth=-viewPoint.z;
  if(depth>opaque+0.004) discard;
  vec3 V=normalize(-viewPoint), N=normalize(viewNormal);
  // Subpixel waves are filtered by screen-space derivatives to keep highlights stable in motion.
  vec3 p=localPoint*2.4;
  float phase=clockS*(0.42+min(velocity,4.0)*0.14);
  float aa=1.0/(1.0+4.0*length(fwidth(p)));
  vec3 ripples=vec3(sin(p.y*2.1+phase)*cos(p.z*1.7-phase),cos(p.x*1.9-phase)*sin(p.z+phase),sin(p.x+p.y-phase));
  N=normalize(N+aa*(0.012+0.008*min(velocity,3.0))*(ripples-N*dot(ripples,N)));
  if(dot(N,V)<0.0) N=-N;
  float facing=max(0.0,dot(N,V));
  float fresnel=0.037+0.963*pow(1.0-facing,5.0);
  float thickness=min(sizeM*mix(0.24,1.72,facing),max(0.02,opaque-depth));
  vec3 T=refract(-V,N,1.0/1.46);
  vec2 shift=uvOf(viewPoint+T*thickness)-uv;
  // Suppress samples that would drag an opaque foreground object through the surface.
  float sampleDepth=eyeDepth(texture2D(opaqueDepth,bounded(uv+shift)).r);
  shift*=smoothstep(-0.015,max(0.025,sizeM*0.1),sampleDepth-depth);
  shift*=smoothstep(0.0,0.035,min(min(uv.x,uv.y),min(1.0-uv.x,1.0-uv.y)));
  vec3 transmitted=vec3(texture2D(sceneColor,bounded(uv+shift*1.023)).r,
                         texture2D(sceneColor,bounded(uv+shift)).g,
                         texture2D(sceneColor,bounded(uv+shift*0.977)).b);
  vec3 absorption=exp(-thickness*(0.018+0.13*(1.0-clamp(tint,0.0,1.0))));
  vec3 R=reflect(-V,N);
  vec3 reflected=texture2D(sceneColor,bounded(uvOf(viewPoint+R*sizeM*1.4))).rgb;
  vec3 worldN=normalize((vec4(N,0.0)*viewMatrix).xyz);
  vec3 worldV=normalize(cameraPosition-worldPoint);
  vec3 specular=vec3(0.0);
  float lobe=clamp(105.0/(1.0+18.0*length(fwidth(N))),22.0,105.0);
  for(int i=0;i<4;i++){
    vec3 lightDelta=sourcePos[i]-worldPoint;
    vec3 L=lightDelta/max(0.01,length(lightDelta));
    float alignment=dot(-L,sourceDir[i]);
    float beam=smoothstep(coneCos[i]-0.06,coneCos[i]+0.025,alignment);
    float h=max(0.0,dot(worldN,normalize(L+worldV)));
    specular+=sourceColor[i]*beam*(pow(h,lobe)*3.5+pow(h,12.0)*0.12)/(1.0+dot(lightDelta,lightDelta)*0.065);
  }
  // A broad ceiling and floor reflection from the room work light gives the lens
  // a readable silhouette even before performers launch a directional fixture.
  vec3 wr=reflect(-worldV,worldN);
  float ceiling=smoothstep(0.4,0.92,wr.y);
  float strip=exp(-abs(wr.y-0.27-0.12*wr.x)*18.0);
  vec3 room=workLight*(mix(vec3(0.11,0.085,0.064),vec3(0.7,0.83,1.0),ceiling)+strip*vec3(2.4,2.25,1.95));
  // The same high dynamic range room lights and panels illuminate the fixture
  // materials. Polished glass resolves their shapes in its front and back surfaces.
  vec3 environment=textureCube(roomEnvironment,wr).rgb;
  vec3 refractedWorld=normalize((vec4(T,0.0)*viewMatrix).xyz);
  vec3 backReflection=textureCube(roomEnvironment,reflect(refractedWorld,-worldN)).rgb;
  float internal=0.045+0.095*pow(1.0-facing,1.65);
  vec3 reflectedLight=reflected+room+environment*(workLight*2.2);
  vec3 color=mix(transmitted*absorption,reflectedLight,fresnel)+specular
             +backReflection*workLight*internal*absorption;
  // Thin blue/amber edges arise from slightly different ray directions in glass.
  vec3 film=0.58+0.42*cos(6.28*(facing*1.3+0.08*localPoint.y+0.035*sin(phase+localPoint.x))+
                              vec3(0.0,2.1,4.2));
  float lightLevel=workLight+dot(specular+reflected*0.3,vec3(0.2126,0.7152,0.0722));
  vec3 edgeTint=mix(vec3(0.84,0.93,1.0),clamp(tint,0.0,1.0),0.65);
  color+=min(2.0,lightLevel)*pow(1.0-facing,2.2)*(edgeTint*1.35+film*0.48);
  float edgeAA=smoothstep(0.0,max(0.003,fwidth(facing)*0.58),facing);
  if(fogAmount>0.0){
    vec4 fog=texture2D(atmosphere,uv);
    float fraction=clamp(depth/max(opaque,0.01),0.0,1.0);
    float front=pow(clamp(fog.a,0.001,1.0),fraction);
    color=mix(transmitted,color,front)+fog.rgb*(1.0-front)*fresnel*0.3;
  }
  vec3 original=texture2D(sceneColor,uv).rgb;
  gl_FragColor=vec4(mix(original,color,clamp(strength,0.0,1.0)*edgeAA),1.0);
}`;function Tv({resolution:n,field:t,padding:e}){let i=new ci,s=new oc(n,i,!1,!1,18e3);s.field.set(t),s.isolation=0,s.update();let r=new Yt,o=Math.min(s.count,s.geometry.attributes.position.count)*3;for(let a of["position","normal"]){let l=s.geometry.attributes[a].array.slice(0,o);r.setAttribute(a,new re(l,3))}return s.geometry.dispose(),i.dispose(),r.scale(e,e,e),r.computeBoundingSphere(),r}function Av(n){switch(n.kind){case"sphere":return new dn(1,48,32);case"box":return new be(2,2,2);case"cylinder":return new hi(1,1,2,48);case"field":return Tv(n);case"rounded-box":{let[t,e,i]=n.size,s=Math.max(0,n.rounding)*Math.min(t,e,i)/2;return(s>0?new dr(t,e,i,5,s):new be(t,e,i)).scale(2/t,2/e,2/i)}default:throw new Error("Unknown optical surface")}}var Oe=n=>({value:n}),ru=()=>Array.from({length:4},()=>new R);function Rv(n,t,e,i){let s=new ae({vertexShader:wv,fragmentShader:Ev,side:De,uniforms:{...t,tint:Oe(new St),sizeM:Oe(1),strength:Oe(1),velocity:Oe(0)}}),r=new Lt(n,s);return r.position.copy(e),{mesh:r,start:e.clone(),end:e.clone(),at:i,period:25,speed:0}}function Cv(n,t,e,i){let{mesh:s}=n;if(!n.end.equals(e)){let u=i-n.at;n.speed=Math.min(6,e.distanceTo(n.end)/(Math.max(1,u)/1e3)),n.start.copy(u>220?e:s.position),n.end.copy(e),n.at=i,n.period=nr.clamp(u,8,80)}let r=nr.clamp(t.deformation||0,0,.5),o=1/Math.sqrt(1-r),a=s.scale.fromArray(t["half-size"]);a.set(a.x*o,a.y*(1-r),a.z*o);let{sizeM:l,tint:c,strength:h}=s.material.uniforms;l.value=Math.max(.02,Math.min(a.x,a.y,a.z)),c.value.fromArray(t.color||[.65,.85,1]),h.value=t.strength??1}function Pv(n,t){let e=t.filter(i=>i.intensity>0).sort((i,s)=>s.intensity-i.intensity).slice(0,4);for(let i=0;i<4;i++){let s=e[i],r=n.sourceColor.value[i];if(!s){r.set(0,0,0);continue}let o=n.sourcePos.value[i].setFromMatrixPosition(s.matrixWorld);n.sourceDir.value[i].setFromMatrixPosition(s.target.matrixWorld).sub(o).normalize(),r.set(s.color.r,s.color.g,s.color.b).multiplyScalar(s.intensity/50),n.coneCos.value.setComponent(i,Math.cos(s.angle))}}var ac=class extends li{constructor(t){super(),this.engine=t,this.enabled=!1,this.needsSwap=!1,this.scene=new Xe,this.geometry=new Map,this.instances=new Map,this.target=new R,this.uniforms={sceneColor:Oe(null),roomEnvironment:Oe(null),opaqueDepth:Oe(null),atmosphere:Oe(null),resolution:Oe(new et(1,1)),lens:Oe(new Gt),clockS:Oe(0),workLight:Oe(0),fogAmount:Oe(0),nearZ:Oe(.1),farZ:Oe(100),sourcePos:Oe(ru()),sourceColor:Oe(ru()),sourceDir:Oe(ru()),coneCos:Oe(new ne)}}setSize(t,e){this.uniforms.resolution.value.set(t,e)}update(t,e,i=performance.now()){let s=new Set,r=new Set;for(let o of(e||[]).slice(0,12)){let a=t?.[o.mesh];if(!a)continue;this.geometry.has(o.mesh)||this.geometry.set(o.mesh,Av(a));let l=this.geometry.get(o.mesh),c=this.target.fromArray(o.position),h=this.instances.get(o.id);h||(h=Rv(l,this.uniforms,c,i),this.instances.set(o.id,h),this.scene.add(h.mesh)),h.mesh.geometry=l,Cv(h,o,c,i),s.add(o.mesh),r.add(o.id)}for(let[o,{mesh:a}]of this.instances)r.has(o)||(this.scene.remove(a),a.material.dispose(),this.instances.delete(o));for(let[o,a]of this.geometry)s.has(o)||(a.dispose(),this.geometry.delete(o));this.enabled=r.size>0}render(t,e,i){let{camera:s,clock:r,compositeTarget:o,sceneTarget:a,fog:l,fogAmount:c,workLight:h,spots:u,environmentTarget:d}=this.engine,f=this.uniforms,p=performance.now();for(let g of this.instances.values()){let _=Math.max(0,p-g.at);g.mesh.position.lerpVectors(g.start,g.end,Math.min(1,_/g.period)),g.mesh.material.uniforms.velocity.value=g.speed*Math.exp(-Math.max(0,_-g.period)/220)}f.sceneColor.value=o.texture,f.opaqueDepth.value=a.depthTexture,f.atmosphere.value=l.target.texture,f.roomEnvironment.value=d.texture,f.clockS.value=r,f.fogAmount.value=c,f.workLight.value=h,f.lens.value.copy(s.projectionMatrix),f.nearZ.value=s.near,f.farZ.value=s.far,Pv(f,u||[]);let x=t.autoClear,m=t.getRenderTarget();try{t.setRenderTarget(i),t.autoClear=!1,t.clearDepth(),t.render(this.scene,s)}finally{t.autoClear=x,t.setRenderTarget(m)}}dispose(){this.update({},[])}};var ps=(n,t,e)=>Math.max(t,Math.min(e,n)),lc=(n,t)=>{if(!Number.isFinite(n))throw new Error(`Invalid ${t}`);return n},Ao=Object.freeze({high:Object.freeze({grid:[48,24,48],scale:.67,pixels:14e5,steps:72,shadows:4,mode:"raymarch"}),desktop:Object.freeze({grid:[32,18,32],scale:.5,pixels:52e4,steps:48,shadows:4,mode:"raymarch"}),mobile:Object.freeze({grid:[24,14,24],scale:.34,pixels:3e5,steps:32,shadows:2,mode:"raymarch"}),phone:Object.freeze({grid:[20,12,20],scale:.33,pixels:12e4,steps:24,shadows:1,mode:"haze"})});function hp(n,t,e){let i=Math.max(1,n*t);return Math.min(e.scale,e.pixels?Math.sqrt(e.pixels/i):e.scale)}var To=class n{constructor({grid:t=Ao.desktop.grid,room:e=[12,5,10],initialHaze:i=.07}={}){if(!Array.isArray(t)||t.length!==3||t.some(r=>!Number.isInteger(r)||r<4||r>64))throw new Error("Invalid fog grid");this.grid=[...t],this.initialHaze=ps(lc(i,"haze"),0,1);let s=t[0]*t[1]*t[2];this.data=new Float32Array(s),this.temperature=new Float32Array(s),this.advected=new Float32Array(s),this.advectedHeat=new Float32Array(s),this.next=new Float32Array(s),this.nextHeat=new Float32Array(s),this.steps=0,this.time=0,this.accumulator=0,this.droppedSeconds=0,this.sources=[],this.setRoom(e)}setRoom(t){if(!Array.isArray(t)||t.length!==3||t.some(e=>!Number.isFinite(e)||e<=0))throw new Error("Invalid fog room");return this.room?.every((e,i)=>e===t[i])?!1:(this.room=[...t],this.spacing=t.map((e,i)=>e/this.grid[i]),this.clear(this.initialHaze),!0)}clear(t=0){lc(t,"haze"),this.data.fill(ps(t,0,6)),this.temperature.fill(0),this.sources=[],this.accumulator=0,this.time=0,this.steps=0,this.droppedSeconds=0}copyFrom(t){if(!(t instanceof n))throw new Error("Expected an owned fog field");this.setRoom(t.room);let[e,i,s]=this.grid,[r,o,a]=t.grid;for(let l=0;l<s;l++)for(let c=0;c<i;c++)for(let h=0;h<e;h++){let u=h+c*e+l*e*i,d=(h+.5)*r/e-.5,f=(c+.5)*o/i-.5,p=(l+.5)*a/s-.5;this.data[u]=t.sample(d,f,p),this.temperature[u]=t.sample(d,f,p,t.temperature)}this.sources=t.sources.map(l=>({...l,position:[...l.position],direction:[...l.direction]}));for(let l of["time","steps","accumulator","droppedSeconds"])this[l]=t[l]}burst({position:t=[-this.room[0]*.34,.22,-this.room[2]*.24],direction:e=[.7,.12,.7],duration:i=2.8,strength:s=1.2}={}){if(![t,e].every(c=>Array.isArray(c)&&c.length===3&&c.every(Number.isFinite)))throw new Error("Invalid fog source");let r=Math.hypot(...e);if(r<1e-6)throw new Error("Fog source needs a direction");let o=e.map(c=>c/r),a={position:[...t],direction:o,remaining:ps(lc(i,"burst duration"),0,8),strength:ps(lc(s,"burst strength"),0,8)},l=this.sources.find(c=>c.position.every((h,u)=>h===t[u]));l?(l.remaining=Math.min(8,l.remaining+a.remaining),l.strength=a.strength):(this.sources.length>=4&&this.sources.shift(),this.sources.push(a))}sample(t,e,i,s=this.data){let[r,o,a]=this.grid;t=ps(t,0,r-1),e=ps(e,0,o-1),i=ps(i,0,a-1);let l=Math.floor(t),c=Math.floor(e),h=Math.floor(i),u=Math.min(l+1,r-1),d=Math.min(c+1,o-1),f=Math.min(h+1,a-1),p=t-l,x=e-c,m=i-h,g=r*o,_=l+c*r+h*g,y=u+c*r+h*g,S=l+d*r+h*g,T=u+d*r+h*g,w=(f-h)*g,v=(s[_]*(1-p)+s[y]*p)*(1-x)+(s[S]*(1-p)+s[T]*p)*x,M=(s[_+w]*(1-p)+s[y+w]*p)*(1-x)+(s[S+w]*(1-p)+s[T+w]*p)*x;return v*(1-m)+M*m}advance(t){if(!Number.isFinite(t)||t<0)throw new Error("Fog dt must be finite and nonnegative");let e=Math.min(t,.1);this.droppedSeconds+=t-e,this.accumulator+=e;let i=0;for(;this.accumulator+1e-10>=.05&&i<2;)this.step(.05),this.accumulator=Math.max(0,this.accumulator-.05),i++;return i}step(t){let[e,i,s]=this.grid,[r,o,a]=this.spacing,l=e*i,c=this.data,h=this.temperature,u=this.advected,d=this.advectedHeat,f=this.time,p=this.room[0],x=this.room[2],m=Array.from({length:e},(D,P)=>{let A=(P+.5)*r-p/2;return[A,Math.cos(A*1.5+f*.43),Math.cos(A*1.5-f*.43),Math.cos(A*4.3-f*.8)]}),g=Array.from({length:i},(D,P)=>{let A=(P+.5)*o;return[A,Math.cos(A*1.7+f*.43),Math.sin(A*1.7+f*.31),Math.cos(A*4.1-f*.8),Math.sin(A*4.1+f*.7)]}),_=Array.from({length:s},(D,P)=>{let A=(P+.5)*a-x/2;return[A,Math.sin(A*1.3-f*.31),Math.sin(A*1.3+f*.31),Math.sin(A*3.7+f*.7)]});for(let D=0;D<s;D++)for(let P=0;P<i;P++)for(let A=0;A<e;A++){let C=A+P*e+D*l,[U,L,B,G]=m[A],[z,tt,W,J,j]=g[P],[xt,Mt,q,ot]=_[D],gt=Math.min(1,h[C])*.17,wt=.12+.11*(tt-Mt)+gt*(J-ot),rt=h[C]*.38+.035+.08*(q-L)+gt*(ot-G),Dt=.025+.11*(B-W)+gt*(G-j),ie=0;for(let bt of this.sources){let Bt=U-bt.position[0],Jt=z-bt.position[1],Wt=xt-bt.position[2],fe=Bt*bt.direction[0]+Jt*bt.direction[1]+Wt*bt.direction[2];if(fe<-.2||fe>1.8)continue;let Se=.2+Math.max(0,fe)*.24,Ze=Math.max(0,Bt*Bt+Jt*Jt+Wt*Wt-fe*fe),pe=Math.exp(-Ze/(2*Se*Se)-((fe-.45)/.85)**2),fi=Math.sin(fe*8-f*5.5+z*3),Ee=.72+.28*fi*fi,He=pe*Ee*bt.strength;ie+=He,wt+=bt.direction[0]*He*.9,rt+=bt.direction[1]*He*.9,Dt+=bt.direction[2]*He*.9}u[C]=Math.min(6,this.sample(A-t*wt/r,P-t*rt/o,D-t*Dt/a,c)+ie*t*3.2),d[C]=Math.min(4,this.sample(A-t*wt/r,P-t*rt/o,D-t*Dt/a,h)+ie*t*2.4)}let y=t*.025/(r*r),S=t*.025/(o*o),T=t*.025/(a*a),w=Math.min(1,.45/(y+S+T));y*=w,S*=w,T*=w;let v=Math.exp(-t/100),M=Math.exp(-t/4.5);for(let D=0;D<s;D++)for(let P=0;P<i;P++)for(let A=0;A<e;A++){let C=A+P*e+D*l,U=A?C-1:C,L=A<e-1?C+1:C,B=P?C-e:C,G=P<i-1?C+e:C,z=D?C-l:C,tt=D<s-1?C+l:C;this.next[C]=Math.max(0,(u[C]+y*(u[U]+u[L]-2*u[C])+S*(u[B]+u[G]-2*u[C])+T*(u[z]+u[tt]-2*u[C]))*v),this.nextHeat[C]=Math.max(0,(d[C]+y*(d[U]+d[L]-2*d[C])+S*(d[B]+d[G]-2*d[C])+T*(d[z]+d[tt]-2*d[C]))*M)}[this.data,this.next]=[this.next,this.data],[this.temperature,this.nextHeat]=[this.nextHeat,this.temperature];for(let D of this.sources)D.remaining-=t;this.sources=this.sources.filter(D=>D.remaining>1e-8),this.time+=t,this.steps++}stats(){let t=0,e=0,i=0;for(let s=0;s<this.data.length;s++)t+=this.data[s],e=Math.max(e,this.data[s]),i+=this.temperature[s];return{cells:this.data.length,steps:this.steps,time:this.time,droppedSeconds:this.droppedSeconds,meanDensity:t/this.data.length,maximumDensity:e,heat:i/this.data.length,activeSources:this.sources.length,massEstimate:t*this.spacing[0]*this.spacing[1]*this.spacing[2]}}};var Iv=`out vec2 vUv;
void main(){vUv=uv;gl_Position=vec4(position.xy,0.0,1.0);}`,fp=`precision highp float;
precision highp sampler3D;
#include <packing>
${Eo}
in vec2 vUv;
uniform sampler2D sceneDepth;
uniform sampler3D densityTexture;
uniform mat4 inverseProjection, cameraWorld;
uniform vec3 eye, room;
uniform vec3 detailMin, detailMax;
uniform float amount, time, mixedDensity, extinctionPerM;
uniform int raySteps, lightCount;
uniform vec3 lightPosition[16], lightDirection[16], lightColor[16];
uniform vec3 lightShape[16]; // half-angle cosine, intensity, maximum distance
uniform vec4 lightProfile[16]; // inner cosine, optical tangent, halo tangent increment, halo cosine
uniform float lightStart[16]; // virtual apex to optical lens plane
uniform vec3 ambientSky,ambientGround;
uniform int lightShadow[16];
uniform highp sampler2DShadow shadow0, shadow1, shadow2, shadow3;
uniform mat4 shadowMatrix[4];
uniform vec2 shadowTexel[4];
uniform float shadowBias[4];

float hash(vec3 p){p=fract(p*0.3183099+vec3(.11,.23,.37));p*=17.;return fract(p.x*p.y*p.z*(p.x+p.y+p.z));}
float noise(vec3 p){vec3 i=floor(p),f=fract(p);f=f*f*(3.-2.*f);
return mix(mix(mix(hash(i),hash(i+vec3(1,0,0)),f.x),mix(hash(i+vec3(0,1,0)),hash(i+vec3(1,1,0)),f.x),f.y),
 mix(mix(hash(i+vec3(0,0,1)),hash(i+vec3(1,0,1)),f.x),mix(hash(i+vec3(0,1,1)),hash(i+vec3(1,1,1)),f.x),f.y),f.z);}
vec2 boxHit(vec3 ro,vec3 rd,vec3 lower,vec3 upper){
 vec3 safe=vec3(abs(rd.x)<1e-7?(rd.x<0.?-1e-7:1e-7):rd.x,abs(rd.y)<1e-7?(rd.y<0.?-1e-7:1e-7):rd.y,abs(rd.z)<1e-7?(rd.z<0.?-1e-7:1e-7):rd.z);
 vec3 a=(lower-ro)/safe,b=(upper-ro)/safe,lo=min(a,b),hi=max(a,b);
 return vec2(max(max(lo.x,lo.y),lo.z),min(min(hi.x,hi.y),hi.z));
}
vec2 roomHit(vec3 ro,vec3 rd){
 return boxHit(ro,rd,vec3(-room.x*.5,0.,-room.z*.5),vec3(room.x*.5,room.y,room.z*.5));
}
float shadowCompare(int slot,vec2 uv,float depth){
 if(slot==0)return texture(shadow0,vec3(uv,depth));
 if(slot==1)return texture(shadow1,vec3(uv,depth));
 if(slot==2)return texture(shadow2,vec3(uv,depth));
 return texture(shadow3,vec3(uv,depth));
}
float visibility(int slot,vec3 position){
 if(slot<0)return 1.;
 vec4 coordinate=shadowMatrix[slot]*vec4(position,1.);coordinate.xyz/=coordinate.w;
 if(coordinate.w<=0.||coordinate.z<0.||coordinate.z>1.||any(lessThan(coordinate.xy,vec2(0.)))||any(greaterThan(coordinate.xy,vec2(1.))))return 1.;
 float depth=coordinate.z+shadowBias[slot];vec2 texel=shadowTexel[slot]*.65;
 return .25*(shadowCompare(slot,coordinate.xy+vec2(-texel.x,-texel.y),depth)+
 shadowCompare(slot,coordinate.xy+vec2(texel.x,-texel.y),depth)+
 shadowCompare(slot,coordinate.xy+vec2(-texel.x,texel.y),depth)+
 shadowCompare(slot,coordinate.xy+texel,depth));
}
// A uniform bulk medium scatters the same share of incident light per steradian.
const float bulkPhase = 1./12.5663706;
vec2 coneHit(int i,float angleCos,vec3 ro,vec3 rd,float lo,float hi){
 vec3 q=ro-lightPosition[i];float axial=dot(q,lightDirection[i]),slope=dot(rd,lightDirection[i]);
 float plane=lightStart[i];
 if(abs(slope)<1e-7){if(axial<plane)return vec2(1.,0.);}
 else if(slope>0.)lo=max(lo,(plane-axial)/slope);else hi=min(hi,(plane-axial)/slope);
 float range=lightShape[i].z;
 if(range>0.){float b=dot(q,rd),discriminant=b*b-dot(q,q)+range*range;
  if(discriminant<0.)return vec2(1.,0.);float root=sqrt(discriminant);lo=max(lo,-b-root);hi=min(hi,-b+root);}
 float cos2=angleCos*angleCos;
 float a=slope*slope-cos2,b=2.*(axial*slope-cos2*dot(q,rd)),c=axial*axial-cos2*dot(q,q);
 if(abs(a)<1e-7){if(abs(b)<1e-7){if(c<0.)return vec2(1.,0.);}else if(b>0.)lo=max(lo,-c/b);else hi=min(hi,-c/b);}
 else{float discriminant=b*b-4.*a*c;
  if(discriminant<0.){if(a<0.)return vec2(1.,0.);}
  else{float root=sqrt(discriminant),first=(-b-root)/(2.*a),last=(-b+root)/(2.*a);
   if(first>last){float swap=first;first=last;last=swap;}
   if(a<0.){lo=max(lo,first);hi=min(hi,last);}else if(slope>0.)lo=max(lo,last);else hi=min(hi,first);}}
 return vec2(lo,hi);
}
vec3 incident(int i,vec3 position,float density,bool halo){
  vec3 delta=position-lightPosition[i];float distance=length(delta);vec3 travel=delta/max(distance,.001);
  float cosine=dot(travel,lightDirection[i]);float outer=lightShape[i].x;
  float inner=lightProfile[i].x;
  float spot=smoothstep(outer,max(outer+.00001,inner),cosine);
  vec3 shadowPoint=position;
  float axial=dot(delta,lightDirection[i]);
  float path=max(0.,axial-lightStart[i]);
  if(!halo && lightStart[i]>0.){
   // The lens fills its clear aperture. Develop the angular penumbra
   // downstream instead of shaping the illuminated disk at the glass.
   float radius=lightStart[i]*lightProfile[i].y;
   float radial=length(delta-axial*lightDirection[i]);
   float disk=1.-smoothstep(.88*radius,radius,radial);
   spot=mix(disk,spot,smoothstep(0.,2.*radius,path));
  }
  if(halo){
   // Forward scattering can illuminate the room just outside the optical
   // field. The additional width starts at zero at the actual lens.
   vec3 lateral=delta-axial*lightDirection[i];
   float coreRadius=axial*lightProfile[i].y;
   float width=path*lightProfile[i].z;
   float radial=length(lateral), outside=radial-coreRadius;
   // The optical core already contributes here. Start the halo continuously
   // outside the core so it cannot count the same light twice at intersections.
   if(width<=0.||outside<=0.||outside>=width)return vec3(0.);
   spot=smoothstep(0.,width*.15,outside)*.26*(1.-fogTransmission(extinctionPerM,density,path))*(1.-smoothstep(0.,width,outside));
   // Sample the shadow on the illuminated edge that feeds this halo.
   shadowPoint=lightPosition[i]+axial*lightDirection[i]+lateral*min(1.,.98*coreRadius/max(radial,.001));
  }
  float range=lightShape[i].z;
  if(spot<.00001||(range>0.&&distance>=range))return vec3(0.);
  float falloff=range>0.?pow(clamp(1.-pow(distance/range,4.),0.,1.),2.):1.;
  float attenuation=falloff/max(.25,distance*distance);
  // Coarse incident-path extinction as well as the independently integrated
  // viewer-path extinction. Density is measured in the same grid for both.
  vec3 optical=lightPosition[i]+lightStart[i]*lightDirection[i];
  vec3 midpoint=mix(optical,shadowPoint,.5);
  vec3 uvw=vec3(midpoint.x/room.x+.5,midpoint.y/room.y,midpoint.z/room.z+.5);
  float incidentDensity=amount*texture(densityTexture,clamp(uvw,vec3(0.),vec3(1.))).r;
  float transmission=fogTransmission(extinctionPerM,incidentDensity,length(shadowPoint-optical));
  return lightColor[i]*lightShape[i].y*spot*attenuation*transmission*bulkPhase*visibility(lightShadow[i],shadowPoint);
}
float densityAt(vec3 position){
 vec3 coordinate=vec3(position.x/room.x+.5,position.y/room.y,position.z/room.z+.5);
 float density=texture(densityTexture,clamp(coordinate,vec3(0.),vec3(1.))).r;
 // Resolved density owns the plume. Coherent advected detail erodes dense jet
 // material into billows; it cannot create fog where the field is empty.
 float localDensity=max(0.,density-mixedDensity);
 float detailWeight=smoothstep(.10,.7,localDensity),structure=1.;
 if(detailWeight>.001){
  vec3 drift=vec3(-time*.13,-time*.04,time*.02);
  vec3 p=position*1.6+drift;
  float warp=noise(p*.55+vec3(3.1,7.9,1.2));p+=vec3(warp,-warp,warp*.6)*1.4;
  float billow=.57*noise(p)+.29*noise(p*2.03+vec3(7.1,1.9,3.7))+.14*noise(p*4.07);
  float folds=smoothstep(.36,.64,billow);
  structure=mix(1.,.08+2.6*folds*folds,detailWeight);
 }
 return (density+(structure-1.)*localDensity)*amount;
}
void integrateFog(out vec4 volumeOutput){
 if(amount<=0.){volumeOutput=vec4(0.,0.,0.,1.);return;}
 vec4 farView=inverseProjection*vec4(vUv*2.-1.,1.,1.);farView/=farView.w;
 vec3 farWorld=(cameraWorld*farView).xyz;vec3 ray=normalize(farWorld-eye);
 float depth=texture(sceneDepth,vUv).r;
 vec4 surfaceView=inverseProjection*vec4(vUv*2.-1.,depth*2.-1.,1.);surfaceView/=surfaceView.w;
 vec3 surfaceWorld=(cameraWorld*surfaceView).xyz;
 vec2 interval=roomHit(eye,ray);float start=max(0.,interval.x),finish=min(interval.y,length(surfaceWorld-eye));
 if(finish<=start){volumeOutput=vec4(0.,0.,0.,1.);return;}
 vec4 lightIntervals[16];int rayLights[16];int rayLightCount=0;
 for(int i=0;i<16;i++){
  if(i>=lightCount)break;
  if(dot(lightColor[i],lightColor[i])<=.000001 || lightShape[i].y<=0.)continue;
  vec4 spans=vec4(coneHit(i,lightShape[i].x,eye,ray,start,finish),
                  coneHit(i,lightProfile[i].w,eye,ray,start,finish));
  // Most screen rays cross very few beams. Cull once, before marching, while
  // retaining each exact clipped interval (including sub-step narrow shafts).
  if(spans.y<=spans.x && spans.w<=spans.z)continue;
  lightIntervals[rayLightCount]=spans;rayLights[rayLightCount]=i;rayLightCount++;
 }
 float baseStep=(finish-start)/float(raySteps);
 vec2 detailSpan=vec2(1.,0.);
 if(all(lessThan(detailMin,detailMax)))detailSpan=boxHit(eye,ray,detailMin,detailMax);
 // Desktop has fewer spare evaluations than dense base intervals can occupy.
 // Distribute those subdivisions along the whole plume instead of refining
 // only its near end. Integer differences keep the total at most 72.
 bool hasDetail=detailSpan.y>detailSpan.x;
 int detailFirst=hasDetail?int(clamp(floor((detailSpan.x-start)/baseStep),0.,float(raySteps))):0;
 int detailLast=hasDetail?int(clamp(ceil((detailSpan.y-start)/baseStep),0.,float(raySteps))):0;
 int detailCount=max(0,detailLast-detailFirst);
 int regularExtra=max(0,72/raySteps-1);
 int extraLimit=max(0,72-raySteps);
 int distribute=raySteps>36?min(extraLimit,detailCount):regularExtra*detailCount;
 // Keep density offsets local to each interval, with the optics sampled at
 // its midpoint. This avoids locking the lit plume texture to march planes.
 float jitter=mix(.25,.75,hash(vec3(gl_FragCoord.xy,13.)));
 vec3 radiance=vec3(0.);float transmission=1.;
 int baseIndex=0,subdivision=0,subdivisions=1;
 float stepSize=baseStep;
 for(int n=0;n<72;n++){
  if(baseIndex>=raySteps||transmission<.002)break;
  float baseBegin=start+float(baseIndex)*baseStep;
  if(subdivision==0){
   int local=baseIndex-detailFirst;
   subdivisions=1;
   if(local>=0 && local<detailCount && distribute>0){
    subdivisions+=regularExtra>0?regularExtra:(local+1)*distribute/detailCount-local*distribute/detailCount;
   }
   stepSize=baseStep/float(subdivisions);
  }
  float begin=baseBegin+float(subdivision)*stepSize;
  float distance=begin+(.2+.6*jitter)*stepSize;vec3 position=eye+ray*distance;
  float density=densityAt(position);
  float segment=fogTransmission(extinctionPerM,density,stepSize);
  // A bounded hemisphere approximation makes illuminated plumes visible outside
  // direct spots. Caller owns worklight/bounce estimates; this is not GI.
  // Sky irradiance also reaches material near the floor. A height-only mix
  // incorrectly made a warm white plume look like black smoke at its source.
  vec3 ambient=mix(ambientGround,ambientSky,.65+.15*clamp(position.y/room.y,0.,1.));
  if(density*extinctionPerM>.12){
   // Three coarse canopy samples approximate environment occlusion inside a
   // dense plume. Multiple scattering retains a bright floor, unlike opaque
   // smoke. This is a local lighting estimate, not a second fluid simulation.
   vec3 coordinate=vec3(position.x/room.x+.5,position.y/room.y,position.z/room.z+.5);
   float canopy=0.;
   canopy+=texture(densityTexture,clamp(coordinate+vec3(.15,.3,.05)/room,vec3(0.),vec3(1.))).r*.3;
   canopy+=texture(densityTexture,clamp(coordinate+vec3(.32,.7,.12)/room,vec3(0.),vec3(1.))).r*.4;
   canopy+=texture(densityTexture,clamp(coordinate+vec3(.55,1.3,.2)/room,vec3(0.),vec3(1.))).r*.6;
   ambient*=.78+.22*fogTransmission(extinctionPerM,canopy*amount,.56);
  }
  radiance+=transmission*(1.-segment)*.92*ambient;
  for(int light=0;light<16;light++){
   if(light>=rayLightCount)break;
   int i=rayLights[light];
   for(int layer=0;layer<2;layer++){
    bool halo=layer==1;
    vec2 span=halo?lightIntervals[light].zw:lightIntervals[light].xy;
    float lo=max(begin,span.x),hi=min(begin+stepSize,span.y);
    if(hi<=lo)continue;
    vec3 samplePosition=eye+ray*((lo+hi)*.5);
    // A span covering the whole interval samples exactly the step's own point,
    // so reuse its density instead of evaluating the detail noise again.
    float localDensity=lo<=begin&&hi>=begin+stepSize?density:densityAt(eye+ray*(lo+(.2+.6*jitter)*(hi-lo)));
    if(localDensity<=0.)continue;
    float foreground=fogTransmission(extinctionPerM,density,lo-begin);
    radiance+=transmission*foreground*(1.-fogTransmission(extinctionPerM,localDensity,hi-lo))*.92*
       incident(i,samplePosition,localDensity,halo);
   }
  }
  transmission*=segment;
  subdivision++;
  if(subdivision>=subdivisions){baseIndex++;subdivision=0;}
 }
 volumeOutput=vec4(radiance,transmission);
}`,Dv=`${fp}
out vec4 volumeOutput;
void main(){integrateFog(volumeOutput);}`,Lv=`${fp}
out vec4 compositeOutput;
uniform sampler2D sceneColor,volume;
uniform vec2 volumeSize;
uniform float nearPlane,farPlane;
float viewDepth(float z){return (nearPlane*farPlane)/(farPlane-(farPlane-nearPlane)*z);}
void main(){
 vec4 color=texture(sceneColor,vUv);
 if(amount<=0.){compositeOutput=color;return;}
 float center=viewDepth(texture(sceneDepth,vUv).r);
 vec2 p=vUv*volumeSize-.5,base=floor(p),fraction=fract(p);
 vec4 fog=vec4(0.);float total=0.;
 for(int y=0;y<2;y++)for(int x=0;x<2;x++){
  vec2 uv=(base+vec2(float(x),float(y))+.5)/volumeSize;
  float neighbor=viewDepth(texture(sceneDepth,uv).r);
  float spatial=(x==0?1.-fraction.x:fraction.x)*(y==0?1.-fraction.y:fraction.y);
  float weight=spatial*exp(-abs(neighbor-center)/max(.03,center*.012));
  fog+=texture(volume,uv)*weight;total+=weight;
 }
 // All neighboring samples can lie behind a thin foreground surface. Integrate
 // this pixel up to its actual depth instead of discarding the medium in front.
 if(total>.00001)fog/=total;else integrateFog(fog);
 compositeOutput=vec4(color.rgb*fog.a+fog.rgb,color.a);
}`,Nv=`precision highp float;
precision highp sampler3D;
${Eo}
in vec2 vUv;
out vec4 compositeOutput;
uniform sampler2D sceneColor, sceneDepth;
uniform sampler3D densityTexture;
uniform mat4 inverseProjection, cameraWorld;
uniform vec3 eye, room, ambientSky, ambientGround;
uniform float amount, extinctionPerM, time, nearPlane, farPlane;
uniform sampler2D volume;
uniform vec2 volumeSize;
uniform bool beamLayer;
float viewDepth(float z){return (nearPlane*farPlane)/(farPlane-(farPlane-nearPlane)*z);}
// Faked beams are drawn into the reduced raster; upsample them with the same
// opaque-depth weights as the raymarch composite.
vec3 beams(float depth){
 if(!beamLayer)return vec3(0.);
 float center=viewDepth(depth);
 vec2 p=vUv*volumeSize-.5,base=floor(p),fraction=fract(p);
 vec3 sum=vec3(0.);float total=0.;
 for(int y=0;y<2;y++)for(int x=0;x<2;x++){
  vec2 uv=(base+vec2(float(x),float(y))+.5)/volumeSize;
  float spatial=(x==0?1.-fraction.x:fraction.x)*(y==0?1.-fraction.y:fraction.y);
  float weight=spatial*exp(-abs(viewDepth(texture(sceneDepth,uv).r)-center)/max(.03,center*.012));
  sum+=texture(volume,uv).rgb*weight;total+=weight;
 }
 return total>.00001?sum/total:vec3(0.);
}
void main(){
 vec4 color=texture(sceneColor,vUv);
 if(amount<=0.){compositeOutput=color;return;}
 vec4 farView=inverseProjection*vec4(vUv*2.-1.,1.,1.);farView/=farView.w;
 vec3 ray=normalize((cameraWorld*farView).xyz-eye);
 float depth=texture(sceneDepth,vUv).r;
 vec4 surfaceView=inverseProjection*vec4(vUv*2.-1.,depth*2.-1.,1.);surfaceView/=surfaceView.w;
 vec3 surfaceWorld=(cameraWorld*surfaceView).xyz;
 vec3 safe=vec3(abs(ray.x)<1e-7?1e-7:ray.x,abs(ray.y)<1e-7?1e-7:ray.y,abs(ray.z)<1e-7?1e-7:ray.z);
 vec3 a=(vec3(-room.x*.5,0.,-room.z*.5)-eye)/safe,b=(vec3(room.x*.5,room.y,room.z*.5)-eye)/safe;
 vec3 lo=min(a,b),hi=max(a,b);
 float start=max(0.,max(max(lo.x,lo.y),lo.z)),finish=min(min(min(hi.x,hi.y),hi.z),length(surfaceWorld-eye));
 if(finish<=start){compositeOutput=color;return;}
 // Interleaved gradient noise spreads the tap offsets evenly across
 // neighbouring pixels, so eight taps read as a fine dither, not grain.
 float stepSize=(finish-start)/8.,jitter=fract(52.9829189*fract(dot(gl_FragCoord.xy,vec2(.06711056,.00583715))));
 vec3 radiance=vec3(0.);float transmission=1.;
 for(int i=0;i<8;i++){
  vec3 p=eye+ray*(start+(float(i)+jitter)*stepSize);
  vec3 uvw=vec3(p.x/room.x+.5,p.y/room.y,p.z/room.z+.5);
  float density=amount*texture(densityTexture,clamp(uvw,vec3(0.),vec3(1.))).r;
  float segment=fogTransmission(extinctionPerM,density,stepSize);
  vec3 ambient=mix(ambientGround,ambientSky,.65+.15*clamp(p.y/room.y,0.,1.));
  radiance+=transmission*(1.-segment)*.92*ambient;
  transmission*=segment;
 }
 compositeOutput=vec4(color.rgb*transmission+radiance+beams(depth),color.a);
}`,up=()=>globalThis.performance?.now?.()??Date.now(),Uv=()=>{let n=new Ui(new Uint32Array([4294967295]),1,1,Di,oi);return n.internalFormat="DEPTH_COMPONENT24",n.compareFunction=er,n.minFilter=n.magFilter=oe,n.needsUpdate=!0,n};function dp(n){let t=new Bs(new Uint16Array(n.data.length),...n.grid);return Object.assign(t,{format:On,type:de,internalFormat:"R16F",minFilter:oe,magFilter:oe,unpackAlignment:1})}var cc=class{constructor(t,{quality:e="desktop"}={}){if(!Ao[e])throw new Error("Unknown fog quality");this.renderer=t,this.quality=e,this.budget=Ao[e],this.disposed=!1,this.field=new To({grid:this.budget.grid}),this.field.burst(),this.amount=.5,this.density=dp(this.field),this.packed=this.density.image.data,this.dummyShadow=Uv(),this.target=new he(1,1,{type:de,format:ai,depthBuffer:!1,stencilBuffer:!1,minFilter:oe,magFilter:oe}),this.lights=[],this.updateMs=0,this.uploads=0,this.width=1,this.height=1;let i=r=>Array.from({length:r},()=>new R);this.uniforms={sceneDepth:{value:null},densityTexture:{value:this.density},inverseProjection:{value:new Gt},cameraWorld:{value:new Gt},eye:{value:new R},room:{value:new R(...this.field.room)},detailMin:{value:new R},detailMax:{value:new R},amount:{value:this.amount},time:{value:0},mixedDensity:{value:0},extinctionPerM:{value:1.25},raySteps:{value:this.budget.steps},lightCount:{value:0},lightPosition:{value:i(16)},lightDirection:{value:i(16)},lightColor:{value:i(16)},lightShape:{value:i(16)},lightShadow:{value:new Int32Array(16).fill(-1)},lightProfile:{value:Array.from({length:16},()=>new ne)},lightStart:{value:new Float32Array(16)},ambientSky:{value:new R},ambientGround:{value:new R},shadowMatrix:{value:Array.from({length:4},()=>new Gt)},shadowTexel:{value:Array.from({length:4},()=>new et(1,1))},shadowBias:{value:new Float32Array(4)}};for(let r=0;r<4;r++)this.uniforms[`shadow${r}`]={value:this.dummyShadow};this.compositeUniforms={...this.uniforms,sceneColor:{value:null},volume:{value:this.target.texture},volumeSize:{value:new et(1,1)},nearPlane:{value:.08},farPlane:{value:80},beamLayer:{value:!1}};let s=(r,o)=>new ae({vertexShader:Iv,fragmentShader:r,uniforms:o,glslVersion:gn,depthTest:!1,depthWrite:!1,blending:yi,toneMapped:!1});this.volumeMaterial=s(Dv,this.uniforms),this.compositeMaterial=s(Lv,this.compositeUniforms),this.hazeMaterial=s(Nv,this.compositeUniforms),this.mode=this.budget.mode||"raymarch",this.quad=new Lt(new _i(2,2),this.volumeMaterial),this.quad.frustumCulled=!1,this.scene=new Xe,this.scene.add(this.quad),this.camera=new fn,this.upload()}upload(){let[t,e,i]=this.field.grid,[s,r,o]=this.field.room,a=this.showAtmosphere?this.showAtmosphere.sample?.["mixed-density"]||0:this.field.initialHaze,l=Hs.toHalfFloat(a+.22),c=t,h=e,u=i,d=-1,f=-1,p=-1;for(let g=0;g<this.packed.length;g++){let _=Hs.toHalfFloat(Math.min(65504,this.field.data[g]));if(this.packed[g]=_,_<l)continue;let y=g%t,S=Math.floor(g/t)%e,T=Math.floor(g/(t*e));c=Math.min(c,y),h=Math.min(h,S),u=Math.min(u,T),d=Math.max(d,y),f=Math.max(f,S),p=Math.max(p,T)}let x=this.uniforms.detailMin.value,m=this.uniforms.detailMax.value;d<0?(x.set(1,1,1),m.set(-1,-1,-1)):(x.set((Math.max(0,c-1)/t-.5)*s,Math.max(0,h-1)/e*r,(Math.max(0,u-1)/i-.5)*o),m.set((Math.min(t,d+2)/t-.5)*s,Math.min(e,f+2)/e*r,(Math.min(i,p+2)/i-.5)*o)),this.density.needsUpdate=!0,this.uploads++}setQuality(t){let e=Ao[t];if(!e)throw new Error("Unknown fog quality");if(this.disposed||this.quality===t)return;if(e.grid.every((r,o)=>r===this.budget.grid[o])){this.quality=t,this.budget=e,this.mode=e.mode||"raymarch",this.uniforms.raySteps.value=e.steps;return}let i=new To({grid:e.grid});i.copyFrom(this.field);let s=dp(i);this.density.dispose(),this.field=i,this.density=s,this.packed=s.image.data,this.quality=t,this.budget=e,this.mode=e.mode||"raymarch",this.uniforms.densityTexture.value=s,this.uniforms.raySteps.value=e.steps,this.upload()}setRoom(t){this.field.setRoom(t)&&(this.uniforms.room.value.fromArray(t),this.showAtmosphere?(this.showAtmosphere.suspend(),this.field.clear()):this.field.burst(),this.upload())}get extinctionPerM(){return this.showAtmosphere?.extinctionPerM??1.25}setAmount(t){if(!Number.isFinite(t))throw new Error("Invalid fog amount");this.amount=Math.max(0,Math.min(1,t))}setAmbient({color:t,intensity:e=0,groundColor:i=t}={}){if(!Number.isFinite(e)||e<0)throw new Error("Invalid atmosphere ambient intensity");let s=(r,o)=>{if(!o)return r.value.set(0,0,0);if(![o.r,o.g,o.b].every(a=>Number.isFinite(a)&&a>=0))throw new Error("Invalid atmosphere ambient color");r.value.set(o.r,o.g,o.b).multiplyScalar(e)};s(this.uniforms.ambientSky,t),s(this.uniforms.ambientGround,i)}setAtmosphere(t,e){e!=null?(this.showAtmosphere||(this.showAtmosphere=new ic,this.field.clear(0),this.upload()),this.showAtmosphere.accept(t,e)):this.showAtmosphere&&(this.showAtmosphere=null,this.field.clear(this.field.initialHaze),this.field.burst(),this.upload())}burst(t){return this.showAtmosphere?!1:(this.field.burst(t),!0)}setSize(t,e){if(![t,e].every(s=>Number.isFinite(s)&&s>0))return;let i=hp(t,e,this.budget);this.width=Math.max(1,Math.floor(t*i)),this.height=Math.max(1,Math.floor(e*i)),this.sceneWidth=t,this.sceneHeight=e,this.target.setSize(this.width,this.height),this.compositeUniforms.volumeSize.value.set(this.width,this.height)}update(t,e=[]){if(this.disposed)return;let i=up();this.lights=this.mode==="haze"?[]:e.slice(0,16),(this.showAtmosphere?this.showAtmosphere.paint(this.field,t):this.field.advance(t))&&this.upload(),this.updateMs=up()-i}render(t,e,i=null,s=null){if(this.disposed)return;if(!t?.depthTexture||!t?.texture)throw new Error("Fog requires opaque scene color and depth");let r=this.renderer,o=r.getRenderTarget(),a=r.autoClear,l=this.uniforms,c=this.compositeUniforms;l.sceneDepth.value=t.depthTexture,l.inverseProjection.value.copy(e.projectionMatrixInverse),l.cameraWorld.value.copy(e.matrixWorld),l.eye.value.setFromMatrixPosition(e.matrixWorld);let h=this.showAtmosphere,u=h&&!h.localVolume&&!h.sample?.["mixed-density"]?0:this.amount;l.extinctionPerM.value=this.extinctionPerM,l.mixedDensity.value=h?.sample?.["mixed-density"]||0,l.amount.value=u,l.time.value=this.field.time,l.lightCount.value=this.lights.length,l.lightShadow.value.fill(-1);let d=0;for(let f=0;f<4;f++)l[`shadow${f}`].value=this.dummyShadow;this.lights.forEach((f,p)=>{l.lightPosition.value[p].copy(f.virtualPosition||f.position),l.lightDirection.value[p].copy(f.direction).normalize(),l.lightColor.value[p].set(f.color.r,f.color.g,f.color.b),l.lightShape.value[p].set(Math.cos(f.angle),Math.max(0,f.intensity??1),Math.max(0,f.distance??0));let x=Math.max(0,Math.min(1,f.penumbra??.55)),m=Math.max(f.angle,Math.min(1.54,f.angle+.23)),g=Math.tan(f.angle);if(l.lightProfile.value[p].set(Math.cos(f.angle*(1-x)),g,Math.tan(m)-g,Math.cos(m)),l.lightStart.value[p]=Math.max(0,f.apexDistance??0),f.shadow?.map?.depthTexture&&d<this.budget.shadows){if(f.shadow.map.depthTexture.compareFunction!==er)throw new Error("Fog requires forward-depth PCF comparison shadow textures");let _=d++;l.lightShadow.value[p]=_,l[`shadow${_}`].value=f.shadow.map.depthTexture,l.shadowMatrix.value[_].copy(f.shadow.matrix),l.shadowTexel.value[_].set(1/f.shadow.map.width,1/f.shadow.map.height),l.shadowBias.value[_]=f.shadow.bias??-2e-4}}),this.shadowCount=d,this.visibleAmount=u,c.sceneColor.value=t.texture,c.sceneDepth.value=t.depthTexture,c.nearPlane.value=e.near,c.farPlane.value=e.far,c.amount.value=u;try{if(r.autoClear=!0,this.mode==="haze"){if(c.beamLayer.value=!1,u>0&&s){let f=r.getClearColor(new St),p=r.getClearAlpha();r.setClearColor(0,1),r.setRenderTarget(this.target),r.clear(!0,!1,!1),r.setClearColor(f,p),c.beamLayer.value=!!s(this.target)}r.setRenderTarget(i),this.quad.material=this.hazeMaterial,r.render(this.scene,this.camera)}else u>0&&(r.setRenderTarget(this.target),this.quad.material=this.volumeMaterial,r.render(this.scene,this.camera),s?.(this.target)),r.setRenderTarget(i),this.quad.material=this.compositeMaterial,r.render(this.scene,this.camera)}finally{r.setRenderTarget(o),r.autoClear=a}}stats(){return{quality:this.quality,mode:this.mode,targetSize:this.mode==="haze"?[this.sceneWidth,this.sceneHeight]:[this.width,this.height],raySteps:this.mode==="haze"?8:this.budget.steps,lightCount:this.lights.length,shadowCount:this.shadowCount??0,cpuUpdateMs:this.updateMs,uploads:this.uploads,atmosphere:this.showAtmosphere?.stats(),...this.field.stats()}}dispose(){this.disposed||(this.disposed=!0,this.target.dispose(),this.density.dispose(),this.dummyShadow.dispose(),this.volumeMaterial.dispose(),this.compositeMaterial.dispose(),this.hazeMaterial.dispose(),this.quad.geometry.dispose(),this.scene.clear(),this.lights=[])}};var hc=class{constructor(t,e,i=document,s=window){this.focused=i.hasFocus(),this.visibleDocument=!i.hidden,this.external=!1,this.externalOwners=new Set,this.inViewport=!1,this.pageActive=!0,this.active=!1,this.abort=new AbortController;let r=this.abort.signal,o=()=>{let l=this.pageActive&&(this.external||this.inViewport&&this.visibleDocument&&this.focused);l!==this.active&&(this.active=l,t.dispatchEvent?.(new CustomEvent("tolglow-preview-activity",{detail:l})),e(l))};t.addEventListener?.("tolglow-preview-external",l=>{if(l.detail&&typeof l.detail=="object"){let{owner:c,active:h}=l.detail;h?this.externalOwners.add(c):this.externalOwners.delete(c)}else l.detail===!0?this.externalOwners.add("pip"):this.externalOwners.delete("pip");this.external=this.externalOwners.size>0,o()},{signal:r});let a=l=>{this.focused=l,o()};s.addEventListener("focus",()=>a(!0),{signal:r}),s.addEventListener("blur",()=>a(!1),{signal:r}),i.addEventListener("visibilitychange",()=>{this.visibleDocument=!i.hidden,o()},{signal:r}),s.addEventListener("pagehide",()=>{this.pageActive=!1,o()},{signal:r}),s.addEventListener("pageshow",()=>{this.pageActive=!0,this.visibleDocument=!i.hidden,this.focused=i.hasFocus(),o()},{signal:r}),this.observer=new s.IntersectionObserver(l=>{let c=l.at(-1);!c||r.aborted||(this.inViewport=c.isIntersecting&&c.intersectionRatio>0,o())},{threshold:[0,Number.EPSILON]}),this.observer.observe(t)}dispose(){this.abort.abort(),this.observer.disconnect(),this.active=!1}};function ou(n,t){let e=new Set(t),i=new Set(n),s=t.filter(o=>!i.has(o)),r=0;return n.map(o=>e.has(o)?o:s[r++])}function pp(n,t,e,i,s,r){let o=new Set(s),a=[];for(let[p,x]of Object.entries(t)){let m=n.get(p),g=x?.["emitted-color"];if(!m||!Array.isArray(g)||g.length!==3||!g.every(Number.isFinite))continue;let _=Math.max(...g)*(m["relative-brightness"]??1);if(_<=0)continue;let y=m.channels?.pan!=null&&m.channels?.tilt!=null;a.push({id:p,level:_,moving:y})}a.sort((p,x)=>+(x.id===e)-+(p.id===e)||Number(x.moving)-Number(p.moving)||x.level-p.level||Number(o.has(x.id))-Number(o.has(p.id))||(p.id<x.id?-1:1));let l=Array.from(new Set([a[0],a.find(p=>!p.moving),...a].filter(Boolean).map(p=>p.id))),c=new Set;n.has(e)&&c.add(e);for(let p of l)c.add(p);for(let p of i)c.add(p);let h=ou(i,Array.from(c).slice(0,i.length)),u=new Set(h),d=new Set(l.filter(p=>u.has(p)));for(let p of s)u.has(p)&&d.add(p);for(let p of h)d.add(p);let f=Array.from(d).slice(0,s.length);return{meshes:h,lights:[...ou(s.slice(0,r),f.slice(0,r)),...ou(s.slice(r),f.slice(r))]}}var Fv=Object.freeze({}),mp=n=>Array.isArray(n)&&n.length===3&&n.every(Number.isFinite),uc=(n,t,e)=>n+(t-n)*e;function Ov(n,t,e,i){if(!n||!t||t["cut?"]||n.strobe>0||t.strobe>0||n.channels?.strobe>0||t.channels?.strobe>0||n.shutter!==t.shutter||n.channels?.shutter!==t.channels?.shutter)return t;let s={...t};for(let l of["intensity","dimmer"])Number.isFinite(n[l])&&Number.isFinite(t[l])&&(s[l]=uc(n[l],t[l],i));let r=n["emitted-color"],o=t["emitted-color"];mp(r)&&mp(o)&&(s["emitted-color"]=o.map((l,c)=>uc(r[c],l,i)));let a=["pan","tilt"];if(a.every(l=>zn(n,l)!==void 0&&zn(t,l)!==void 0&&Math.abs(xn(e,t,l)-xn(e,n,l))<180)){for(let l of a)s[l]=uc(zn(n,l),zn(t,l),i),s[l+"-deg"]=uc(xn(e,n,l),xn(e,t,l),i);s.pose={"pan-deg":s["pan-deg"],"tilt-deg":s["tilt-deg"]},delete s.direction,delete s.aim}return s}var dc=class{constructor(){this.mode="hold"}setMode(t){if(!["hold","interpolate"].includes(t))throw new RangeError("Unknown preview lighting mode");this.mode!==t&&(this.mode=t,this.previous=null)}accept(t,e,i){let s=this.latest,r=t["time-ms"]-s?.["time-ms"],o=this.mode==="interpolate"&&s&&this.rigKey===e&&bn(t)===bn(s)&&t.revision===s.revision&&!t["blackout?"]&&!s["blackout?"]&&t.master!==0&&s.master!==0&&!t["cut?"]&&t["frame-number"]===s["frame-number"]+1&&r>0&&r<=50&&i>=this.receivedAt&&i-this.receivedAt<75;this.latest=t,this.previous=o?s:null,this.rigKey=e,this.period=r,this.receivedAt=i}values(t,e){let i=this.latest?.output?.values||Fv;if(!this.previous)return i;let s=Math.max(0,(e-this.receivedAt)/this.period);if(s>=1)return this.previous=null,i;let r=this.previous.output?.values||{},o=Object.create(null);for(let{id:a,fixture:l}of t)o[a]=Ov(r[a],i[a],l,s);return o}};var Bv=(n,t)=>n[0]*t[0]+n[1]*t[1]+n[2]*t[2],gp=(n,t)=>[n[0]-t[0],n[1]-t[1],n[2]-t[2]],xp=n=>Math.hypot(n[0],n[1],n[2]),_n=Object.freeze({margin:1.35,hold:.6,facing:1.5,minimumLevel:1e-4});function zv(n,t){let e=Math.max(.05,n.length??6),i=e/2,s=n.position.map((l,c)=>l+n.direction[c]*i),r=Math.hypot(i,e*Math.tan(Math.min(n.angle,1.4)));for(let l of t.planes||[])if(l[0]*s[0]+l[1]*s[1]+l[2]*s[2]+l[3]<-r)return 0;let o=Math.max(r*.5,xp(gp(s,t.eye)),.001),a=Math.min(1,r/(o*Math.max(.001,t.tanHalfFov)));return a*a}function kv(n,t,e=_n){let i=Math.max(0,n.level||0);if(i<=(e.minimumLevel??_n.minimumLevel))return 0;let s=zv(n,t);if(s<=0)return 0;let r=gp(t.eye,n.position),o=xp(r),a=o>1e-6?Math.max(0,Bv(n.direction,r)/o):0,l=1+(e.facing??_n.facing)*a**4,c=i*Math.sqrt(s)*l;return Number.isFinite(c)?c:0}function _p(n,t,e,i=null,s=0,r=_n){let o=r.margin??_n.margin,a=r.hold??_n.hold,l=i?.since||new Map,c=new Map,h=[];for(let A of n){let C=kv(A,t,r);c.set(A.id,C),(A.level||0)>(r.minimumLevel??_n.minimumLevel)&&h.push(A)}let u=Math.max(0,Math.floor(e)),d=A=>{let C=c.get(A.id);return!l.has(A.id)||C<=0?C:s-l.get(A.id)<a?1/0:C*o},p=h.filter(A=>c.get(A.id)>0||A.pinned).map(A=>({id:A.id,pinned:!!A.pinned,key:d(A),score:c.get(A.id)})).sort((A,C)=>Number(C.pinned)-Number(A.pinned)||C.key-A.key||C.score-A.score||(A.id<C.id?-1:A.id>C.id?1:0)).slice(0,u).map(A=>A.id);if(p.length<u){let A=new Set(p),C=h.filter(U=>!A.has(U.id)).sort((U,L)=>Number(l.has(L.id))-Number(l.has(U.id))||L.level-U.level||(U.id<L.id?-1:U.id>L.id?1:0));for(let U of C.slice(0,u-p.length))p.push(U.id)}let x=new Set(p),m=new Map;for(let A of p)m.set(A,l.has(A)?l.get(A):s);let g=h.map(A=>A.id).filter(A=>!x.has(A)),_=i?.shadowSince||new Map,y=Math.min(p.length,Math.max(0,Math.floor(r.shadowSlots??0))),S=new Set(n.filter(A=>A.pinned).map(A=>A.id)),T=(A,C)=>c.get(C)-c.get(A)||(A<C?-1:A>C?1:0),w=A=>{let C=c.get(A);return _.has(A)?s-_.get(A)<a?1/0:C*o:C},v=[...p].sort((A,C)=>Number(S.has(C))-Number(S.has(A))||w(C)-w(A)||T(A,C)).slice(0,y).sort(T),M=new Set(v),D=new Map(v.map(A=>[A,_.has(A)?_.get(A):s]));return{full:[...v,...p.filter(A=>!M.has(A)).sort(T)],faked:g,scores:c,state:{since:m,shadowSince:D}}}function vp(n,t,e,i){let s=Array.from({length:e},(u,d)=>n?.[d]??null),r=t.slice(0,e),o=r.slice(0,i),a=r.slice(i),l=(u,d,f)=>{let p=new Set(f);for(let g=u;g<d;g++)s[g]!=null&&!p.has(s[g])&&(s[g]=null);let x=new Set(s.slice(u,d)),m=f.filter(g=>!x.has(g));for(let g=u;g<d&&m.length;g++)s[g]==null&&(s[g]=m.shift());return m},c=new Set(o),h=new Set(a);for(let u=0;u<e;u++)(u<i?h.has(s[u]):c.has(s[u]))&&(s[u]=null);return l(0,Math.min(i,e),o),l(Math.min(i,e),e,a),s}function yp(n,t,e,i,s=.3,r=null){let o=s>0?Math.max(0,Math.min(1,i/s)):1,a=t.map((l,c)=>{let h=n?.[c];return h&&h.id!=null&&e.has(h.id)?{...h}:{id:null,weight:0}});for(let l=0;l<a.length;l++){let c=a[l],h=e.has(t[l])?t[l]:null;if(c.id===h){h!=null&&(c.weight=Math.min(1,c.weight+o));continue}if(c.id!=null&&c.weight>o){c.weight-=o;continue}let u=h!=null&&a.some((d,f)=>f!==l&&d.id===h);a[l]=u||h==null?{id:null,weight:0}:{id:h,weight:r?.has(h)?0:1}}return a}function Mp(n,t,e){let i=n,s=[],r=[0,1,2,3].map(o=>[i[o],i[o+4],i[o+8],i[o+12]]);for(let[o,a]of[[0,1],[0,-1],[1,1],[1,-1],[2,1],[2,-1]]){let l=r[3].map((h,u)=>h+a*r[o][u]),c=Math.hypot(l[0],l[1],l[2])||1;s.push(l.map(h=>h/c))}return{eye:t,planes:s,tanHalfFov:e}}var Hv=`precision highp float;
in vec3 position;
in vec2 uv;
in vec3 beamPosition, beamDirection;
in vec4 beamColor; // linear rgb \xD7 intensity \xD7 faked weight, penumbra
in vec4 beamShape; // aperture radius, tan(half-angle), length, apex distance
uniform mat4 viewMatrix, projectionMatrix;
out vec3 vWorld, vNormal, vColor, vLens;
out vec3 vBeam; // distance from the virtual apex, radius there, along 0..1
out vec2 vAround;
out float vPenumbra;
void main(){
  float along=clamp(.5-position.y,0.,1.);
  vec3 axis=normalize(beamDirection);
  vec3 helper=abs(axis.y)<.95?vec3(0.,1.,0.):vec3(1.,0.,0.);
  vec3 u=normalize(cross(helper,axis)),v=cross(axis,u);
  vec2 ring=normalize(position.xz);
  vec3 radial=u*ring.x+v*ring.y;
  float s=along*beamShape.z;
  float radius=beamShape.x+s*beamShape.y;
  vec3 world=beamPosition+axis*s+radial*radius;
  vWorld=world; vNormal=normalize(radial-axis*beamShape.y); vColor=beamColor.rgb; vPenumbra=beamColor.w;
  vLens=beamPosition;
  vBeam=vec3(beamShape.w+s,radius,along);
  // The cylinder's own u has a duplicated seam column, so it never wraps
  // across a triangle.
  vAround=vec2(uv.x,s);
  gl_Position=projectionMatrix*viewMatrix*vec4(world,1.);
}`,Vv=`precision highp float;
precision highp sampler3D;
${Eo}
in vec3 vWorld, vNormal, vColor, vBeam, vLens;
in vec2 vAround;
in float vPenumbra;
uniform vec3 cameraPosition, room;
uniform sampler2D sceneDepth, noiseTexture;
uniform sampler3D densityTexture;
uniform mat4 viewMatrix;
uniform vec2 resolution;
uniform float nearPlane, farPlane, amount, extinctionPerM, time;
out vec4 beamOutput;
float viewDepth(float z){return (nearPlane*farPlane)/(farPlane-(farPlane-nearPlane)*z);}
float hash(vec3 p){p=fract(p*.3183099+vec3(.11,.23,.37));p*=17.;return fract(p.x*p.y*p.z*(p.x+p.y+p.z));}
float valueNoise(vec3 p){vec3 i=floor(p),f=fract(p);f=f*f*(3.-2.*f);
 return mix(mix(mix(hash(i),hash(i+vec3(1,0,0)),f.x),mix(hash(i+vec3(0,1,0)),hash(i+vec3(1,1,0)),f.x),f.y),
  mix(mix(hash(i+vec3(0,0,1)),hash(i+vec3(1,0,1)),f.x),mix(hash(i+vec3(0,1,1)),hash(i+vec3(1,1,1)),f.x),f.y),f.z);}
float fbm(vec3 p){float sum=0.,weight=.5;for(int i=0;i<4;i++){sum+=weight*valueNoise(p);p=p*2.03+vec3(1.7,9.2,3.1);weight*=.5;}return sum/.9375;}
float densityAt(vec3 p){
  vec3 coordinate=vec3(p.x/room.x+.5,p.y/room.y,p.z/room.z+.5);
  return amount*texture(densityTexture,clamp(coordinate,vec3(0.),vec3(1.))).r;
}
const float bulkPhase=1./12.5663706;
void main(){
  if(amount<=0.){discard;}
  vec3 toEye=cameraPosition-vWorld;float eyeDistance=length(toEye);
  float facing=abs(dot(normalize(vNormal),toEye/eyeDistance));
  // Opaque geometry: soft intersection against the resolved scene depth.
  float fragmentDepth=-(viewMatrix*vec4(vWorld,1.)).z;
  float opaque=viewDepth(texture(sceneDepth,gl_FragCoord.xy/resolution).r);
  float soft=clamp((opaque-fragmentDepth)/.18,0.,1.);
  if(soft<=0.)discard;
  float near=smoothstep(.12,1.1,fragmentDepth);
  float density=densityAt(vWorld);
  if(density<=0.)discard;
  // Same extinction model as the raymarch: the incident path from the lens
  // (one midpoint tap) and the view path from the eye, clipped to the room
  // (three taps).
  float incidentLength=length(vWorld-vLens);
  float incident=fogTransmission(extinctionPerM,densityAt(mix(vLens,vWorld,.5)),incidentLength);
  vec3 ray=-toEye/eyeDistance;
  vec3 safe=vec3(abs(ray.x)<1e-7?1e-7:ray.x,abs(ray.y)<1e-7?1e-7:ray.y,abs(ray.z)<1e-7?1e-7:ray.z);
  vec3 a=(vec3(-room.x*.5,0.,-room.z*.5)-cameraPosition)/safe,b=(vec3(room.x*.5,room.y,room.z*.5)-cameraPosition)/safe;
  vec3 lo=min(a,b);
  float start=clamp(max(max(lo.x,lo.y),lo.z),0.,eyeDistance),span=eyeDistance-start;
  vec3 entry=cameraPosition+ray*start;
  float viewDensity=(densityAt(mix(entry,vWorld,1./6.))+densityAt(mix(entry,vWorld,.5))+densityAt(mix(entry,vWorld,5./6.)))/3.;
  float view=fogTransmission(extinctionPerM,viewDensity,span);
  // Drifting haze: two scrolling samples of a tiling grey noise plus a
  // world-space fbm, so neighbouring beams do not share one pattern.
  float n1=texture(noiseTexture,vec2(vAround.x*3.,vAround.y*.35-time*.06)).r;
  float n2=texture(noiseTexture,vec2(vAround.x*5.+.37,vAround.y*.6+time*.045)).r;
  float haze=fbm(vWorld*1.3+vec3(-time*.13,-time*.04,time*.02));
  float structure=.35+1.3*(.45*n1+.25*n2+.3*haze);
  // Chord through the cone at this silhouette point (r\xB7|n\xB7v| per face, two
  // faces), weighted by the spot profile at the chord's closest radius.
  float closest=sqrt(max(0.,1.-facing*facing));
  float profile=smoothstep(1.,1.-max(.02,vPenumbra),closest);
  float chord=vBeam.y*facing;
  float scatter=1.-fogTransmission(extinctionPerM,density,chord);
  float distance2=max(.25,vBeam.x*vBeam.x);
  float fade=1.-smoothstep(.72,1.,vBeam.z);
  vec3 radiance=vColor*bulkPhase*.92*scatter*profile/distance2*incident*view*structure*soft*near*fade;
  beamOutput=vec4(radiance,0.);
}`;function Gv(n=256){let t=new Uint8Array(n*n),e=16,i=new Float32Array(e*e),s=2654435769;for(let a=0;a<i.length;a++)s=s*1664525+1013904223>>>0,i[a]=s/2**32;let r=(a,l)=>i[l%e*e+a%e];for(let a=0;a<n;a++)for(let l=0;l<n;l++){let c=l*e/n,h=a*e/n,u=Math.floor(c),d=Math.floor(h),f=(c-u)**2*(3-2*(c-u)),p=(h-d)**2*(3-2*(h-d)),x=r(u,d)*(1-f)+r(u+1,d)*f,m=r(u,d+1)*(1-f)+r(u+1,d+1)*f,g=(l*7919+a*104729)%251/251;t[a*n+l]=Math.round(255*(.85*(x*(1-p)+m*p)+.15*g))}let o=new Ui(t,n,n,On,ti);return o.wrapS=o.wrapT=cn,o.minFilter=Bi,o.magFilter=oe,o.generateMipmaps=!0,o.needsUpdate=!0,o}function au(n,t,e){let i=[e[0]/2,e[1],e[2]/2],s=[-i[0],0,-i[2]],r=1/0;for(let o=0;o<3;o++){let a=t[o];if(Math.abs(a)<1e-9)continue;let c=((a>0?i[o]:s[o])-n[o])/a;c>=0&&(r=Math.min(r,c))}return Number.isFinite(r)?r:Math.max(...e)*2}var fc=class{constructor(t=128){this.capacity=t,this.count=0;let e=new hi(1,1,1,28,10,!0),i=new Zr;i.index=e.index,i.setAttribute("position",e.getAttribute("position")),i.setAttribute("uv",e.getAttribute("uv")),e.dispose();let s=(r,o)=>{let a=new $n(new Float32Array(t*o),o);return a.setUsage(Yi),i.setAttribute(r,a),a};this.attributes={position:s("beamPosition",3),direction:s("beamDirection",3),color:s("beamColor",4),shape:s("beamShape",4)},i.instanceCount=0,this.noise=Gv(),this.uniforms={sceneDepth:{value:null},noiseTexture:{value:this.noise},densityTexture:{value:null},room:{value:new R(12,5,10)},resolution:{value:new et(1,1)},nearPlane:{value:.08},farPlane:{value:100},amount:{value:0},extinctionPerM:{value:1.25},time:{value:0}},this.material=new In({vertexShader:Hv,fragmentShader:Vv,uniforms:this.uniforms,glslVersion:gn,transparent:!0,depthTest:!1,depthWrite:!1,side:De,blending:Xa,blendEquation:pn,blendSrc:js,blendDst:js,blendSrcAlpha:qa,blendDstAlpha:js,toneMapped:!1}),this.mesh=new Lt(i,this.material),this.mesh.frustumCulled=!1,this.scene=new Xe,this.scene.add(this.mesh)}update(t){let e=Math.min(this.capacity,t.length),i=this.attributes;for(let s=0;s<e;s++){let r=t[s];i.position.setXYZ(s,r.position[0],r.position[1],r.position[2]),i.direction.setXYZ(s,r.direction[0],r.direction[1],r.direction[2]),i.color.setXYZW(s,r.color[0],r.color[1],r.color[2],r.penumbra??.5),i.shape.setXYZW(s,r.radius,r.tangent,r.length,r.apexDistance)}for(let s of Object.values(i))s.clearUpdateRanges(),s.addUpdateRange(0,e*s.itemSize),s.needsUpdate=!0;this.mesh.geometry.instanceCount=this.count=e}render(t,e,i,s,{room:r,amount:o,extinctionPerM:a,time:l,densityTexture:c}){if(!this.count||!(o>0))return!1;let h=this.uniforms;h.sceneDepth.value=s.depthTexture,h.densityTexture.value=c,h.room.value.fromArray(r),h.resolution.value.set(i.width,i.height),h.nearPlane.value=e.near,h.farPlane.value=e.far,h.amount.value=o,h.extinctionPerM.value=a,h.time.value=l;let u=t.getRenderTarget(),d=t.autoClear;try{t.autoClear=!1,t.setRenderTarget(i),t.render(this.scene,e)}finally{t.setRenderTarget(u),t.autoClear=d}return!0}stats(){return{count:this.count,capacity:this.capacity}}dispose(){this.mesh.geometry.dispose(),this.material.dispose(),this.noise.dispose(),this.scene.clear(),this.count=0}};var Kw=Object.freeze(["floor","back","left","right","stage-top","stage-front"]),lu=4;function Sp([n,,t]){let e=Math.min(2.3,t*.27),i=-t/2+e/2+.16;return[n*.68/2,.42,i-e/2,i+e/2]}function Wv(n){return Math.max(...n)>12.8?512:256}var Xv=`out vec2 vUv;
void main(){vUv=uv;gl_Position=vec4(position.xy,0.,1.);}`,qv=`precision highp float;
in vec2 vUv;
uniform sampler2D lightTable;
uniform int lightCount;
uniform vec3 room;
uniform vec4 stage;
out vec4 irradiance;
// 3 x 2 tiles: floor, back, left / right, stage top, stage front.
void surface(vec2 uv,out vec3 position,out vec3 normal){
  vec2 tile=floor(uv*vec2(3.,2.)),local=fract(uv*vec2(3.,2.));
  float w=room.x,h=room.y,d=room.z;
  int index=int(tile.x+3.*tile.y);
  if(index==0){position=vec3((local.x-.5)*w,0.,(local.y-.5)*d);normal=vec3(0.,1.,0.);}
  else if(index==1){position=vec3((local.x-.5)*w,local.y*h,-.5*d);normal=vec3(0.,0.,1.);}
  else if(index==2){position=vec3(-.5*w,local.y*h,(local.x-.5)*d);normal=vec3(1.,0.,0.);}
  else if(index==3){position=vec3(.5*w,local.y*h,(local.x-.5)*d);normal=vec3(-1.,0.,0.);}
  else if(index==4){position=vec3((local.x*2.-1.)*stage.x,stage.y,mix(stage.z,stage.w,local.y));normal=vec3(0.,1.,0.);}
  else{position=vec3((local.x*2.-1.)*stage.x,local.y*stage.y,stage.w);normal=vec3(0.,0.,1.);}
}
void main(){
  vec3 position,normal;surface(vUv,position,normal);
  vec3 sum=vec3(0.);
  for(int i=0;i<128;i++){
    if(i>=lightCount)break;
    vec4 a=texelFetch(lightTable,ivec2(0,i),0); // virtual apex, apex distance
    vec4 b=texelFetch(lightTable,ivec2(1,i),0); // direction, outer cosine
    vec4 c=texelFetch(lightTable,ivec2(2,i),0); // colour \xD7 intensity, inner cosine
    vec4 e=texelFetch(lightTable,ivec2(3,i),0); // range
    vec3 delta=position-a.xyz;float distance=length(delta);vec3 travel=delta/max(distance,.001);
    float cosine=dot(travel,b.xyz);
    if(cosine<=b.w||dot(delta,b.xyz)<a.w)continue;
    float lambert=dot(normal,-travel);
    if(lambert<=0.)continue;
    float spot=smoothstep(b.w,max(b.w+.00001,c.w),cosine);
    float range=e.x;
    float falloff=range>0.?pow(clamp(1.-pow(distance/range,4.),0.,1.),2.):1.;
    sum+=c.rgb*spot*lambert*falloff/max(.25,distance*distance);
  }
  irradiance=vec4(sum,1.);
}`,pc=class{constructor(t,{capacity:e=128}={}){this.renderer=t,this.capacity=e,this.count=0,this.dirty=!0,this.redraws=0,this.tile=256,this.target=new he(3*this.tile,2*this.tile,{type:de,depthBuffer:!1,minFilter:oe,magFilter:oe,generateMipmaps:!1}),this.table=new Ui(new Float32Array(lu*e*4),lu,e,ai,ui),this.table.minFilter=this.table.magFilter=Ce,this.table.needsUpdate=!0,this.uniforms={lightTable:{value:this.table},lightCount:{value:0},room:{value:new R(12,5,10)},stage:{value:new ne(...Sp([12,5,10]))}},this.material=new ae({vertexShader:Xv,fragmentShader:qv,uniforms:this.uniforms,glslVersion:gn,depthTest:!1,depthWrite:!1,blending:yi,toneMapped:!1}),this.quad=new Lt(new _i(2,2),this.material),this.quad.frustumCulled=!1,this.scene=new Xe,this.scene.add(this.quad),this.camera=new fn,this.materialUniforms={tolglowFootprints:{value:this.target.texture},tolglowRoom:{value:new R(12,5,10)},tolglowStage:{value:this.uniforms.stage.value},tolglowFootprintStrength:{value:0}}}setRoom(t){this.uniforms.room.value.fromArray(t),this.materialUniforms.tolglowRoom.value.fromArray(t),this.uniforms.stage.value.set(...Sp(t));let e=Wv(t);e!==this.tile&&(this.tile=e,this.target.setSize(3*e,2*e)),this.dirty=!0}update(t){let e=Math.min(this.capacity,t.length),i=this.table.image.data,s=e!==this.count;for(let r=0;r<e;r++){let o=t[r],a=r*lu*4,l=[...o.virtualPosition,o.apexDistance,...o.direction,o.outerCos,...o.color,o.innerCos,o.range,0,0,0];for(let c=0;c<l.length;c++){let h=Math.fround(l[c]);i[a+c]!==h&&(i[a+c]=h,s=!0)}}this.count=e,s&&(this.table.needsUpdate=!0,this.dirty=!0),this.materialUniforms.tolglowFootprintStrength.value=e&&!this.disabled?1:0}render(){if(!this.dirty)return!1;this.dirty=!1,this.redraws++;let t=this.renderer,e=t.getRenderTarget();this.uniforms.lightCount.value=this.count;try{t.setRenderTarget(this.target),t.render(this.scene,this.camera)}finally{t.setRenderTarget(e)}return!0}dispose(){this.target.dispose(),this.table.dispose(),this.material.dispose(),this.quad.geometry.dispose(),this.scene.clear()}};function bp(n,t){let e=n.onBeforeCompile,i=n.customProgramCacheKey;n.onBeforeCompile=(s,r)=>{e?.call(n,s,r),Object.assign(s.uniforms,t),s.vertexShader=`varying vec3 tolglowWorld; varying vec3 tolglowWorldNormal;
`+s.vertexShader.replace("#include <project_vertex>",`#include <project_vertex>
       tolglowWorld = (modelMatrix * vec4(transformed, 1.0)).xyz;
       tolglowWorldNormal = normalize(mat3(modelMatrix) * objectNormal);`),s.fragmentShader=`varying vec3 tolglowWorld; varying vec3 tolglowWorldNormal;
uniform sampler2D tolglowFootprints; uniform vec3 tolglowRoom; uniform vec4 tolglowStage; uniform float tolglowFootprintStrength;
vec3 tolglowTile(float index, vec2 local){
  vec2 tile=vec2(mod(index,3.),floor(index/3.));
  vec2 texel=1./vec2(textureSize(tolglowFootprints,0));
  vec2 uv=(tile+clamp(local,0.,1.))/vec2(3.,2.);
  return texture(tolglowFootprints,clamp(uv,tile/vec2(3.,2.)+texel,(tile+1.)/vec2(3.,2.)-texel)).rgb;
}
vec3 tolglowFootprint(vec3 p, vec3 n){
  if(tolglowFootprintStrength<=0.) return vec3(0.);
  vec4 s=tolglowStage;
  bool onStage=abs(p.x)<=s.x+.02&&p.z>=s.z-.02&&p.z<=s.w+.02;
  if(n.y>.7&&onStage&&abs(p.y-s.y)<.12) return tolglowTile(4.,vec2(p.x/(2.*s.x)+.5,(p.z-s.z)/(s.w-s.z)))*tolglowFootprintStrength;
  if(n.z>.7&&abs(p.x)<=s.x+.02&&abs(p.z-s.w)<.08&&p.y<=s.y+.02) return tolglowTile(5.,vec2(p.x/(2.*s.x)+.5,p.y/s.y))*tolglowFootprintStrength;
  vec3 a=abs(n); vec2 local; float gap, index;
  if(a.y>=a.x&&a.y>=a.z){ if(n.y<0.) return vec3(0.);
    index=0.; local=vec2(p.x/tolglowRoom.x+.5,p.z/tolglowRoom.z+.5); gap=p.y; }
  else if(a.z>=a.x){ if(n.z<0.) return vec3(0.);
    index=1.; local=vec2(p.x/tolglowRoom.x+.5,p.y/tolglowRoom.y); gap=p.z+.5*tolglowRoom.z; }
  else if(n.x>0.){ index=2.; local=vec2(p.z/tolglowRoom.z+.5,p.y/tolglowRoom.y); gap=p.x+.5*tolglowRoom.x; }
  else { index=3.; local=vec2(p.z/tolglowRoom.z+.5,p.y/tolglowRoom.y); gap=.5*tolglowRoom.x-p.x; }
  float near=1.-smoothstep(.2,.5,abs(gap));
  if(near<=0.) return vec3(0.);
  return tolglowTile(index,local)*near*tolglowFootprintStrength;
}
`+s.fragmentShader.replace("#include <lights_fragment_end>",`#include <lights_fragment_end>
  reflectedLight.directDiffuse += tolglowFootprint(tolglowWorld, normalize(tolglowWorldNormal)) * BRDF_Lambert(material.diffuseColor);`)},n.customProgramCacheKey=()=>(i?i.call(n):"")+"+tolglow-footprints-v2"}var Zv=new R(0,-1,0),mc=16,cu=128,Kv=.3;function wp(n,t){let e=t.position||[0,0,0],i=(t["rotation-deg"]||[0,0,0]).map(s=>s*Math.PI/180);return!e.some((s,r)=>s!==n.position.getComponent(r))&&n.rotation.order==="ZYX"&&n.rotation.x===i[0]&&n.rotation.y===i[1]&&n.rotation.z===i[2]?!1:(n.position.fromArray(e),n.rotation.set(...i,"ZYX"),!0)}function Ep(n,t=[]){for(let e of n)e.castShadow&&(e.shadow.needsUpdate=!0);for(let e of t)e.castShadow&&(e.shadow.needsUpdate=!0)}var gc=class{constructor(t,e,i={}){if(this.displayHz=i.displayHz??(i.worker?null:60),this.platform=i,this.displayClock=new No,this.lighting=new dc,this.canvas=t,t.tolglowPreview=this,this.status=e,this.ready=!1,this.disposed=!1,this.room=[12,5,10],this.snapshot=null,this.key=null,this.fixtures=[],this.filmSettings={bloom:.28,exposure:.85,grain:.0015},this.quality=this.autoQuality(),this.options=$i[this.quality],this.qualityChoice="auto",this.fogOverride=null,this.workLight=.12,this.visible=!!i.worker,this.lost=!1,this.lastFrame=0,this.renderScale=1,this.scaleChangedAt=0,this.clock=0,this.fogAnimationUntil=0,this.draws=0,this.lastCPU=0,this.abort=new AbortController,this.reducedMotion=i.worker?{matches:!!i.reducedMotion}:matchMedia("(prefers-reduced-motion: reduce)"),this.whenReady=new Promise(r=>{this.resolveReady=r}),this.setStatus("Building venue and lighting shaders\u2026"),this.camera=new Ge(49,1,.08,100),this.controls=i.worker?{target:new R,update:()=>this.camera.lookAt(this.controls.target),dispose(){},touches:{}}:Lf(this.camera,this.canvas),this.setCamera("audience"),i.worker)try{this.initialize()}catch(r){this.fail(r);return}i.worker||(this.refresh=new Uo(r=>this.setDisplayHz(r)),this.resizeObserver=new ResizeObserver(()=>this.resize()),this.resizeObserver.observe(t),this.activity=new hc(t,r=>this.changeActivity(r)));let s=this.abort.signal;t.addEventListener("webglcontextlost",r=>{r.preventDefault(),this.lost=!0,this.ready=!1,this.setStatus("Graphics context lost \xB7 waiting for recovery")},{signal:s}),t.addEventListener("webglcontextrestored",()=>{try{this.restorePending=!0,this.visible&&(this.restoreContext(),this.scheduleFrame(0))}catch(r){this.fail(r)}},{signal:s}),i.worker||(this.navigation=new Yl({camera:this.camera,controls:this.controls,canvas:t,room:()=>this.room,snapshot:()=>this.snapshot,visibleLights:()=>this.litIds,active:()=>this.visible&&!this.paused&&!this.disposed,reducedMotion:()=>this.reducedMotion.matches}),this.reducedMotion.addEventListener("change",()=>this.navigation.notify(),{signal:s})),i.autoStart!==!1&&this.scheduleFrame(0)}get targetHz(){return Wf(this.quality,this.qualityChoice,this.softwareRenderer,this.displayHz)}setDisplayHz(t){!t||t===this.displayHz||(this.displayHz=t,this.overBudget=this.underBudget=0)}autoQuality(t=this.platform.viewport?.width??this.canvas.getBoundingClientRect().width){return this.softwareRenderer?"low":(this.platform.worker?this.platform.coarsePointer:matchMedia("(pointer: coarse)").matches)||t<(this.autoLayoutTier==="mobile"?740:660)?"mobile":"desktop"}setStatus(t){this.status&&this.status.textContent!==t&&(this.status.textContent=t)}initialize(){this.hdrRequested=!this.platform.worker&&Kf(),this.renderer=new Ul({canvas:this.canvas,alpha:this.hdrRequested,antialias:!1,powerPreference:"high-performance",preserveDrawingBuffer:!1});let t=this.renderer.getContext(),e=t.getExtension("WEBGL_debug_renderer_info");this.maxDimension=Math.min(t.getParameter(t.MAX_RENDERBUFFER_SIZE),t.getParameter(t.MAX_TEXTURE_SIZE)),this.samples=qf(t),this.gpuTimer=this.platform.worker?null:new bo(t),this.deviceRenderer=e?t.getParameter(e.UNMASKED_RENDERER_WEBGL):t.getParameter(t.RENDERER),this.softwareRenderer=/swiftshader|llvmpipe|softpipe|software rasterizer/i.test(this.deviceRenderer||""),this.quality=this.autoLayoutTier=this.autoQuality(),this.options=$i[this.quality],this.frameInterval=1e3/this.targetHz,this.renderer.outputColorSpace=Re,this.renderer.toneMapping=as,this.renderer.toneMappingExposure=.85,this.renderer.shadowMap.enabled=!0,this.renderer.shadowMap.type=os,this.renderer.debug.onShaderError=(i,s,r,o)=>{let a=i.getProgramInfoLog(s)||i.getShaderInfoLog(o)||i.getShaderInfoLog(r);throw new Error(`Preview shader rejected: ${String(a).slice(0,500)}`)},this.scene=new Xe,this.scene.background=new St(.0015,.002,.003),this.materials=Qf(),this.apertureUniform={value:new Float32Array(mc+2)},this.patchRadii={value:new Float32Array(mc+2)};for(let i of Object.values(this.materials))$h(i,this.apertureUniform,this.patchRadii);this.footprints=new pc(this.renderer,{capacity:cu});for(let i of["floor","wall","stage","curtain"])bp(this.materials[i],this.footprints.materialUniforms);this.beams=new fc(cu),this.refreshEnvironment(),this.hemisphere=new qs(12966384,3812384,0),this.scene.add(this.hemisphere),this.diffuseBounce=new qs(0,0,1),this.scene.add(this.diffuseBounce),this.workLamp=new Yr(16769988,0),this.workLamp.position.set(2,6,4),this.scene.add(this.workLamp),this.setWorkLight(this.workLight),this.sceneTarget=new he(1,1,{type:de,minFilter:oe,magFilter:oe,depthBuffer:!0}),this.sceneTarget.texture.colorSpace=Cn,this.sceneTarget.depthTexture=new Fi(1,1,oi),this.compositeTarget=new he(1,1,{type:de,depthBuffer:!1}),this.compositeTarget.texture.colorSpace=Cn,this.fog=new cc(this.renderer,{quality:this.options.fog||this.quality}),this.fog.setRoom(this.room),this.fog.setAmount(this.fogAmount),this.composer=new Jl(this.renderer,new he(1,1,{type:de,depthBuffer:!1})),this.inputPass=new $l(this.compositeTarget.texture),this.composer.addPass(this.inputPass),this.glass=new ac(this),this.composer.addPass(this.glass),this.bloom=new hr(new et(1,1),.28,.4,1.1),this.composer.addPass(this.bloom),this.outputPass=Vf(),this.composer.addPass(this.outputPass),this.edges=new Ql,this.composer.addPass(this.edges),this.film=new tc,this.composer.addPass(this.film),this.setFilm(this.filmSettings),this.rebuild({room:{width:12,height:5,depth:10},fixtures:[],trusses:[]}),this.renderer.compile(this.beams.scene,this.camera),this.resize()}refreshEnvironment(){this.environmentTarget?.dispose();let t=new Zl,e=new rr(this.renderer);this.environmentTarget=e.fromScene(t,.04),this.scene.environment=this.environmentTarget.texture,t.dispose(),e.dispose()}setFilm(t){for(let[e,i]of[["bloom",2],["exposure",3],["grain",.08]])Number.isFinite(t?.[e])&&(this.filmSettings[e]=Ye(t[e],0,i));this.film&&(this.renderer.toneMappingExposure=this.filmSettings.exposure,this.bloom.strength=this.filmSettings.bloom,this.bloom.enabled=this.bloom.strength>0,this.film.uniforms.grain.value=this.filmSettings.grain)}setWorkLight(t){this.workLight=Ye(Number(t)||0,0,1),this.hemisphere&&(this.hemisphere.intensity=this.workLight*1.8),this.workLamp&&(this.workLamp.intensity=this.workLight*3.6),this.scene&&(this.scene.environmentIntensity=this.workLight*1.25),this.ready&&this.updateStatus()}get fogAmount(){return this.fogOverride??(this.snapshot?.output?.atmosphere?1:.3)}setFog(t){this.fogOverride=t==null?null:Ye(Number(t)||0,0,1),this.fog?.setAmount(this.fogAmount)}burst(t){return!this.visible||!this.fog?.burst(t)?!1:(this.fogAnimationUntil=this.clock+8,!0)}setQuality(t="auto"){if(!["auto",...Object.keys($i)].includes(t))throw new Error("Unknown preview quality");let e=this.qualityChoice!==t;if(e&&(this.renderScale=1,this.overBudget=this.underBudget=0,this.scaleChangedAt=performance.now()),this.qualityChoice=t,!this.visible||!this.renderer)return;t==="auto"&&e&&(this.autoLayoutTier=this.autoQuality());let i=t==="auto"?e?this.autoLayoutTier:this.quality:t;if(i===this.quality){e&&this.resize();return}this.applyTier(i)}applyTier(t){this.fog.setQuality($i[t].fog||t),this.quality=t,this.options=$i[t],this.frameInterval=1e3/this.targetHz,this.width&&this.fog.setSize(this.width,this.height),this.snapshot&&this.update(this.snapshot,!0),this.resize()}setLightingMode(t){t!==this.lighting.mode&&(this.lighting.setMode(t),this.visible&&this.renderer&&this.snapshot&&this.applyLighting(this.snapshot.output?.values||{}))}setLightBudget(t=null){let e=t==null?null:Math.max(0,Math.min(mc,Math.floor(Number(t)||0)));e!==this.lightBudgetOverride&&(this.lightBudgetOverride=e,this.visible&&this.renderer&&this.snapshot?this.update(this.snapshot,!0):this.lightBudgetPending=!0)}get lightBudget(){return Gf(this.options,this.lightBudgetOverride)}update(t,e=!1,i=performance.now()){if(!t||this.disposed)return;if(this.platform.worker||(this.editorInput||=new zl(this),this.editorInput.update(t)),!this.visible||!this.renderer){this.snapshot=t;return}let s=bn(t);if(this.opticalShow!==s&&this.glass?.dispose(),this.opticalShow=s,this.glass){this.glass.update(t["glass-meshes"],t["glass-volumes"]);for(let l of[this.composer.readBuffer,this.composer.writeBuffer])l.depthBuffer!==this.glass.enabled&&(l.dispose(),l.depthBuffer=this.glass.enabled)}this.editorOverlay||=new Gl,this.editorOverlay.update(t),this.snapshot=t;let r=t.rig||{},o=r===this.lastRig?this.key:Af(r);r!==this.lastRig&&(this.fixtureById=new Map((r.fixtures||[]).map(l=>[String(l.id),l])),this.beamById=new Map([...this.fixtureById].filter(([,l])=>!(l["pixel-layout-id"]&&l["diffuser-radius-m"])))),this.lastRig=r,(e||this.key!==o)&&(this.ready=!1,this.rebuild(r),this.key=o);let a=!1;for(let l of this.trusses||[]){let c=(r.trusses||[]).find(h=>String(h.id)===l.id);c&&wp(l.root,c)&&(a=!0)}this.updateEmittedOutput(t,i,a)}updateEmittedOutput(t,e,i=!1){this.snapshot=t,this.fog?.setAtmosphere(this.opticalShow,t.output?.atmosphere),this.fog?.setAmount(this.fogAmount),this.lighting.accept(t,this.lastRig,e);let s=t.editor?.selection,r=s?.kind==="fixtures"?String(s.id):null,o=pp(this.beamById,t.output?.values||{},r,this.fixtures.map(a=>a.id),[],0);this.selectedFixture=r,this.fixtures.forEach((a,l)=>{let c=o.meshes[l],h=this.fixtureById.get(c);if(a.fixture!==h){let u=a.lensRadius;a.setAperture(h),a.lensRadius!==u&&(i=!0),a.fixture=h,a.id=c,a.root.name="fixture:"+c}wp(a.root,h)&&(i=!0)}),this.applyLighting(this.lighting.values(this.fixtureSamples,e),i)}assignLights(t){let e=performance.now()/1e3,i=this.lodTime==null?0:Math.max(0,Math.min(.1,e-this.lodTime));this.lodTime=e;for(let p of this.fixtures)p.light=null,p.fullWeight=0;if(!this.spots.length){this.fading=!1,this.lod={budget:0,full:0,faked:t.length};return}let s=this.camera;s.updateMatrixWorld();let r=(this.lodMatrix||=new Gt).multiplyMatrices(s.projectionMatrix,s.matrixWorldInverse),o=new R().setFromMatrixPosition(s.matrixWorld).toArray(),a=Mp(r.elements,o,Math.tan(s.fov*Math.PI/360)),l=t.map(p=>({id:p.id,level:p.beam.level,pinned:p.id===this.selectedFixture,position:p.beam.optics.optical.toArray(),direction:p.beam.optics.axis.toArray(),angle:p.beam.angle,length:p.beam.length})),c=this.spots.filter(p=>p.castShadow).length,h=_p(l,a,this.spots.length,this.lodState,e,{..._n,shadowSlots:c});this.lodState=h.state,this.desiredSlots=vp(this.desiredSlots,h.full,this.spots.length,c);let u=new Set(t.map(p=>p.id));this.slotStates=yp(this.slotStates,this.desiredSlots,u,i,Kv,this.fakedIds);let d=this.slotStates.map(p=>p.id);(d.length!==this.lightIds.length||d.some((p,x)=>p!==this.lightIds[x]))&&(this.lightIds=d);let f=new Map(this.fixtures.map(p=>[p.id,p]));this.slotStates.forEach((p,x)=>{let m=p.id!=null&&f.get(p.id);m&&(m.light=this.spots[x],m.fullWeight=p.weight)}),this.fading=this.slotStates.some(p=>p.id!=null&&p.weight<1),this.fakedIds=new Set(t.filter(p=>!(p.fullWeight>=1)).map(p=>p.id)),this.lod={budget:this.spots.length,full:h.full.length,faked:h.faked.length,fading:this.fading}}meshlessBeams(t,e){let i=[],s=this.lighting.latest?.output?.values||{};for(let[r,o]of this.beamById||[]){if(e.has(r))continue;let a=t[r]||s[r];if(!a)continue;let l=us(a),c=o["relative-brightness"]??1;if(!(c>0&&Math.max(...l)>0))continue;let h=Hh(o,a),u=Vh(o,a),d=Jh(o.position||[0,0,0],h,u,fs(o));if(i.push({id:r,fixture:o,beam:{rgb:l,brightness:c,angle:u,optics:d,direction:h,resolved:a,level:Math.max(...l)*c,penumbra:Ye(.72-(Number(a.focus)||0)*.52,.2,.8),length:au(d.optical.toArray(),d.axis.toArray(),this.room)}}),i.length>=cu)break}return i}applyLighting(t,e=!1){this.displayedValues=t;for(let x of this.diffuserTubes)x.update(t);this.lightEntries=[];let i=[],s=[],r=new Qe,o=new Qe,a=new R;for(let x of this.fixtures){let m=t[x.id]||{},g=Hh(x.fixture,m),_=us(m);r.setFromUnitVectors(Zv,a.fromArray(g)).premultiply(o.copy(x.root.quaternion).invert());let y=xn(x.fixture,m,"pan")*Math.PI/180;(!x.head.quaternion.equals(r)||x.yoke.rotation.y!==y)&&(e=!0,x.head.quaternion.copy(r),x.yoke.rotation.y=y);let S=x.fixture["relative-brightness"]??1;if(x.lensMaterial.emissive.setRGB(..._).multiplyScalar(8*S),x.beam=null,!(S>0&&Math.max(..._)>0))continue;let T=Vh(x.fixture,m),w=Jh(x.fixture.position,g,T,x.lensRadius);x.beam={rgb:_,brightness:S,angle:T,optics:w,direction:g,resolved:m,level:Math.max(..._)*S,penumbra:Ye(.72-(Number(m.focus)||0)*.52,.2,.8),length:au(w.optical.toArray(),w.axis.toArray(),this.room)},s.push(x)}this.assignLights(s);let l=this.meshlessBeams(t,new Set(this.fixtures.map(x=>x.id)));this.activeLights=s.length+l.length;let c=[...s,...l].map(x=>x.id);(c.length!==this.litIds?.length||c.some((x,m)=>x!==this.litIds[m]))&&(this.litIds=c);let h=this.fog?.mode==="haze",u=[],d=[];for(let x of this.spots)this.fixtures.some(m=>m.light===x)||x.intensity!==0&&(x.intensity=0,x.color.setRGB(0,0,0));for(let x of[...this.fixtures,...l]){let m=x.light,g=x.beam;if(m&&!g){m.intensity=0,m.color.setRGB(0,0,0);continue}if(!g)continue;let{angle:_,optics:y,rgb:S,brightness:T}=g,w=Math.max(...this.room)*2.5+y.apexDistance,v=145*T,M=m?x.fullWeight:0,D=h?1:1-M;if(D>0&&u.push({position:y.optical.toArray(),direction:y.axis.toArray(),color:S.map(U=>U*v*D),penumbra:g.penumbra,radius:y.radius,tangent:Math.tan(_),length:g.length,apexDistance:y.apexDistance}),M<1&&d.push({virtualPosition:y.virtual.toArray(),apexDistance:y.apexDistance,direction:y.axis.toArray(),outerCos:Math.cos(_),innerCos:Math.cos(_*(1-g.penumbra)),color:S.map(U=>U*v*(1-M)),range:w}),!m)continue;let P=y.apexDistance+.006,A=a.fromArray(g.direction).add(y.virtual);m.castShadow&&(m.angle!==_||m.distance!==w||m.shadow.camera.near!==P||!m.position.equals(y.virtual)||!m.target.position.equals(A))&&(m.shadow.needsUpdate=!0),m.color.setRGB(...S),m.intensity=v*M,m.angle=_,m.penumbra=g.penumbra,m.position.copy(y.virtual),m.distance=w,(m.shadow.camera.near!==P||m.shadow.camera.far!==w)&&(m.shadow.camera.near=P,m.shadow.camera.far=w,m.shadow.camera.updateProjectionMatrix()),m.userData.apexDistance=y.apexDistance,m.target.position.copy(A),m.target.updateMatrixWorld();let C={position:y.optical,virtualPosition:y.virtual,apexDistance:y.apexDistance,apertureRadius:y.radius,direction:y.axis,color:m.color,angle:m.angle,penumbra:m.penumbra,intensity:m.intensity,distance:m.distance,shadow:m.castShadow?m.shadow:null,pseudo:x.fixture["pseudo-fixture"]};this.lightEntries.push(C),this.reflections.addCandidate(i,x,C,g.resolved["beam-footprint"])}this.beams?.update(u),this.footprints?.update(d),this.fakedEntries=u;let f=new St(0,0,0),p=[...s,...l].filter(x=>!x.fixture["pseudo-fixture"]&&x.beam.optics.axis.y<0).sort((x,m)=>m.beam.level-x.beam.level||(x.id<m.id?-1:1)).slice(0,mc);for(let x of p){let m=x.beam,g=m.optics.optical,_=m.optics.axis,y=-g.y/_.y,S=g.x+y*_.x,T=g.z+y*_.z;if(y<=0||Math.abs(S)>this.room[0]/2||Math.abs(T)>this.room[2]/2)continue;let w=-_.y*.045*m.brightness/(1+y*y/24);f.r+=m.rgb[0]*w,f.g+=m.rgb[1]*w,f.b+=m.rgb[2]*w}this.diffuseBounce.color.copy(f).multiplyScalar(.14),this.diffuseBounce.groundColor.copy(f).multiplyScalar(.55),this.bounceColor=f,e&&Ep(this.spots,this.reflections?.lights),this.spots.length&&(i.length&&this.content.updateMatrixWorld(!0),this.reflections.resolve(i),this.reflections.illuminate(this.fog.field,this.fogAmount,this.fog.extinctionPerM),this.spotUniformOrder.forEach((x,m)=>{this.apertureUniform.value[m]=x.userData.apexDistance||0,this.patchRadii.value[m]=x.userData.patchRadius||0})),this.ready&&this.updateStatus()}rebuild(t){if(this.content){this.scene.remove(this.content),Qh(this.content);for(let r of this.fixtures)r.dispose();for(let r of this.diffuserTubes||[])r.dispose();for(let r of this.spots||[])r.dispose();this.reflections?.dispose()}this.room=Tf(t.room),this.camera.far=Math.max(100,3*Math.hypot(...this.room)),this.camera.updateProjectionMatrix(),this.fog?.setRoom(this.room),this.content=new ge,this.scene.add(this.content),this.content.add(np(this.room,this.materials,t.surfaces||[])),this.trusses=[];for(let r of(t.trusses||[]).slice(0,24))if(Array.isArray(r.position)&&r.position.every(Number.isFinite)&&Number.isFinite(r.length)&&r.length>0){let o=ep(r,this.materials);this.content.add(o),this.trusses.push({id:String(r.id),root:o})}this.spots=[],this.lightIds=[];let e=t.fixtures||[];this.diffuserTubes=sp(e.filter(r=>r["pixel-layout-id"]&&r["diffuser-radius-m"]),this.content),this.fixtures=e.filter(r=>!(r["pixel-layout-id"]&&r["diffuser-radius-m"])).slice(0,this.options.fixtures).map((r,o)=>{let a=ip(r,this.materials);$h(a.lensMaterial,this.apertureUniform,this.patchRadii),a.id=String(r.id),a.fixture=r;let l=(r["rotation-deg"]||[0,0,0]).map(c=>(Number(c)||0)*Math.PI/180);return a.root.rotation.set(...l,"ZYX"),this.content.add(a.root),a.light=null,a});for(let r=0;r<Math.min(this.lightBudget,this.fixtures.length);r++){let o=new ss(0,0,Math.max(...this.room)*2.5,Math.PI/20,.15,2);o.position.set(0,this.room[1],0),o.shadow.mapSize.set(this.options.shadowSize,this.options.shadowSize),o.shadow.camera.near=.06,o.shadow.camera.far=o.distance,o.shadow.bias=-8e-5,o.shadow.normalBias=.012,o.castShadow=r<this.options.shadows,o.shadow.autoUpdate=!1,o.shadow.needsUpdate=!0,this.spots.push(o),this.lightIds.push(null),this.content.add(o,o.target)}this.lodState=null,this.desiredSlots=null,this.slotStates=null,this.fakedIds=new Set,this.lodTime=null,this.lightBudgetPending=!1,this.footprints?.setRoom(this.room),this.fixtureSamples=this.fixtures.concat(this.diffuserTubes.flatMap(r=>r.rows));let i=e.filter(r=>r["pseudo-fixture"]).length,s=this.quality==="mobile"||this.quality==="low"?1:2;this.reflections=new nc(this.content,Math.min(s,i),Math.min(512,this.options.shadowSize),this.fixtures),this.spotUniformOrder=[...this.spots.filter(r=>r.castShadow),...this.reflections.lights,...this.spots.filter(r=>!r.castShadow)],this.totalFixtureCount=(t.fixtures||[]).length,this.lightEntries=[]}measureLoad(t,e){if(this.qualityChoice!=="auto"||this.draws<2||!this.visible)return;this.measuredFrameMs=t;let i=1e3/this.targetHz;if(this.overBudget=t>i*1.35?(this.overBudget||0)+1:0,this.underBudget=t<i*.65?(this.underBudget||0)+1:0,e-this.scaleChangedAt<1e3)return;let s=this.renderScale;if(this.overBudget>=4?s=Math.max(.5,Math.min(s*.85,s*Math.sqrt(i/t))):this.underBudget>=60&&(s=Math.min(1,s+.1)),s===this.renderScale&&!this.softwareRenderer){let r=this.overBudget>=8&&s===.5?-1:this.underBudget>=60&&s===1?1:0,o=Zf(this.quality,r);if(o!==this.quality){let a=ec(o,this.qualityChoice,this.softwareRenderer)===ec(this.quality,this.qualityChoice,this.softwareRenderer);this.renderScale=r<0?a?s:.65:.9,this.scaleChangedAt=e,this.overBudget=this.underBudget=0,this.applyTier(o);return}}s!==this.renderScale&&(this.renderScale=s,this.scaleChangedAt=e,this.overBudget=this.underBudget=0,this.resizePending=!0)}resize(){if(!this.visible||!this.renderer||this.disposed)return;this.resizePending=!1;let t=this.platform.worker?this.platform.viewport:this.canvas.getBoundingClientRect();if(!t.width||!t.height)return;if(this.qualityChoice==="auto"){let l=this.autoQuality(t.width);if(l!==this.autoLayoutTier&&(this.autoLayoutTier=l,this.renderScale=1,this.overBudget=this.underBudget=0,this.scaleChangedAt=performance.now(),l!==this.quality)){this.applyTier(l);return}}let e=this.platform.worker?t.pixelRatio:devicePixelRatio,{scene:[i,s],display:[r,o]}=Xf(t.width,t.height,e,ec(this.quality,this.qualityChoice,this.softwareRenderer),this.renderScale,this.maxDimension);this.camera.aspect=t.width/t.height,this.camera.updateProjectionMatrix(),(r!==this.displayWidth||o!==this.displayHeight)&&(this.renderer.setSize(r,o,!1),this.displayWidth=r,this.displayHeight=o,this.displayRange=Jf(this.renderer.getContext(),r,o,this.hdrRequested),this.outputPass.uniforms.extendedDisplay.value=this.displayRange==="extended");let a=Yf(this.quality,i,s,this.samples);i===this.width&&s===this.height&&this.sceneTarget.samples===a||(a!==this.sceneTarget.samples&&(this.sceneTarget.dispose(),this.sceneTarget.samples=a),this.sceneTarget.setSize(i,s),this.compositeTarget.setSize(i,s),this.composer.setSize(i,s),this.fog.setSize(i,s),this.width=i,this.height=s)}setCamera(t){this.camera&&(this.navigation?.setMode("orbit"),Xh(this.camera,this.controls,this.room,t),this.cameraName=t)}resetCamera(){this.setCamera("audience")}setNavigationMode(t){this.navigation?.setMode(t)}setNavigationSpeed(t){this.navigation?.setSpeed(t)}setInteractionActive(t){this.navigation?.setInteractionActive(t)}moveCamera(t,e){this.navigation?.move(t,e)}getNavigationState(){return this.navigation?.state()||null}onNavigationChange(t){return this.navigation?.subscribe(t)||(()=>{})}focusFixture(t){let e=this.fixtureById?.get(String(t));return e?(this.navigation?.setMode("orbit"),this.controls.target.fromArray(e.position||[0,0,0]),this.camera.position.copy(this.controls.target).add(new R(1.3,.5,2)),this.controls.update(),this.cameraName="fixture",!0):!1}changeActivity(t){if(this.refresh?.setActive(t&&!this.paused),this.visible=t,t||this.fog?.showAtmosphere?.suspend(),this.lastFrame=0,this.lighting.latest=this.lighting.previous=null,this.displayClock.cancel(),t||this.gpuTimer?.dispose(),this.navigation?.refreshActivity(),!(!t||this.disposed||this.error))try{this.renderer||this.initialize(),this.restorePending&&this.restoreContext(),this.setQuality(this.qualityChoice),this.snapshot&&this.update(this.snapshot,!!this.lightBudgetPending),this.resize(),this.scheduleFrame(0)}catch(e){this.fail(e)}}restoreContext(){this.restorePending=!1,this.displayWidth=this.displayHeight=null,this.gpuTimer?.dispose(),this.platform.worker||(this.gpuTimer=new bo(this.renderer.getContext())),this.resize(),this.refreshEnvironment(),this.fog?.upload(),this.footprints&&(this.footprints.dirty=!0),Ep(this.spots,this.reflections?.lights),this.lost=!1,this.ready=!1,this.lastFrame=0,this.setStatus("Restoring venue shaders\u2026")}scheduleFrame(t){this.disposed||this.paused||!this.visible||this.lost||this.platform.autoStart===!1||(this.nextFrameAt=performance.now()+t,this.displayClock.at(this.nextFrameAt,e=>this.animate(e)))}pause(){this.paused=!0,this.refresh?.setActive(!1),this.gpuTimer?.dispose(),this.navigation?.refreshActivity(),this.lighting.previous=null,this.displayClock.cancel()}resume(){this.disposed||(this.paused=!1,this.refresh?.setActive(this.visible),this.navigation?.refreshActivity(),this.lastFrame=0,this.scheduleFrame(0))}animate(t){if(this.disposed||this.paused||!this.visible||this.lost)return;let e=this.lastFrame?Math.min(.1,(t-this.lastFrame)/1e3):0;this.lastFrame=t,this.clock+=e;try{let i=this.gpuTimer?.sample(),s=this.gpuTimer?.begin();try{this.draw(e)}finally{s&&this.gpuTimer.end()}i!=null&&(this.lastGpuMs=i,this.measureLoad(Math.max(this.lastCPU,i||0),performance.now())),this.scheduleFrame(Math.max(0,t+this.frameInterval-performance.now()))}catch(i){this.fail(i)}}draw(t=0){if(this.disposed||this.lost||!this.visible||!this.renderer)return!1;this.resizePending&&this.resize();let e=performance.now();this.platform.worker&&this.controls.update();let i=!1;if(this.lighting.latest){let d=this.lighting.values(this.fixtureSamples,e);d!==this.displayedValues&&(this.applyLighting(d),i=!0)}this.camera.updateMatrixWorld();let s=this.camera.matrixWorld.elements,r=this.lodView||=new Float64Array(16).fill(NaN),o=!s.every((d,f)=>d===r[f]);o&&r.set(s),(o||this.fading)&&!i&&this.displayedValues&&(this.applyLighting(this.displayedValues),this.relights=(this.relights||0)+1);let a=!this.reducedMotion.matches||this.clock<this.fogAnimationUntil,l=this.hemisphere.color.clone().multiplyScalar(this.workLight*.8).add(this.workLamp.color.clone().multiplyScalar(this.workLight*.65));this.bounceColor&&l.add(this.bounceColor),this.fog.setAmbient?.({color:l,intensity:1,groundColor:l.clone().multiplyScalar(.5)}),this.fog.update(a?t:0,this.lightEntries||[]),this.reflections.illuminate(this.fog.field,this.fogAmount,this.fog.extinctionPerM),this.footprints.render(),this.renderer.setRenderTarget(this.sceneTarget),this.renderer.clear(),this.renderer.render(this.scene,this.camera),this.renderedShadows=this.spots.filter(d=>d.castShadow&&d.shadow.map).length,this.inputPass.enabled=this.glass.enabled;let c=this.glass.enabled?this.compositeTarget:this.composer.readBuffer;this.fog.render(this.sceneTarget,this.camera,c,d=>this.beams.render(this.renderer,this.camera,d,this.sceneTarget,{room:this.room,amount:this.fog.visibleAmount,extinctionPerM:this.fog.extinctionPerM,time:this.fog.field.time,densityTexture:this.fog.density})),this.film.uniforms.phase.value=this.reducedMotion.matches?0:Math.floor(this.clock*24)%1024,this.composer.render(t),this.editorOverlay?.render(this.renderer,this.camera),this.lastCPU=performance.now()-e,this.draws++,this.canvas.tolglowCaptureVideo?.(this.canvas),this.observedFrame?.(this.canvas,this.snapshot?.["show-id"],this.snapshot?.epoch,this.snapshot?.["frame-number"]??0);let h=1e3/this.targetHz,u=this.frameInterval||h;this.platform.worker||(this.frameInterval=Math.max(h,this.lastCPU*(this.softwareRenderer?4:1.5),u*.95)),this.ready||(this.ready=!0,this.resolveReady?.(!0)),this.updateStatus()}updateStatus(){let t=this.activeLights>this.spots.length?` \xB7 ${this.activeLights} lit \xB7 ${this.spots.length} full lights`:"",e=this.qualityChoice==="auto"&&this.softwareRenderer?"low \xB7 software GPU":this.quality;this.setStatus(`${e} \xB7 ${this.renderedShadows||0} shadows \xB7 ${this.workLight?"work light":"dark show"}${t}`)}getTelemetry(){return{lightingMode:this.lighting.mode,targetRefreshHz:this.targetHz,tierMaxHz:this.options.fps,displayHz:this.displayHz,ready:this.ready,failed:this.error||null,renderer:`Three.js r${"187"}`,quality:this.quality,deviceRenderer:this.deviceRenderer,softwareRenderer:this.softwareRenderer,pixels:[this.width,this.height],displayPixels:[this.displayWidth,this.displayHeight],sceneSamples:this.sceneTarget?.samples,edgeFilter:"FXAA",displayRange:this.displayRange,gpuTimeMs:this.lastGpuMs,film:{...this.filmSettings},measuredFrameMs:this.measuredFrameMs,renderScale:this.renderScale,frameCPUms:this.lastCPU,frameIntervalMs:this.frameInterval,idleBudgetMs:Math.max(0,this.frameInterval-this.lastCPU),nextFrameAt:this.nextFrameAt,frames:this.draws,geometryCount:this.renderer?.info.memory.geometries||0,textureCount:this.renderer?.info.memory.textures||0,programCount:this.renderer?.info.programs?.length||0,activeLights:this.activeLights||0,reflectedPatches:this.reflections?.patches.map(t=>({fixture:t.id,surface:t.surface,center:t.position.toArray(),majorRadius:t.major.length(),minorRadius:t.minor.length()}))||[],lightIds:this.litIds||[],slotIds:this.lightIds,fixtureCount:this.totalFixtureCount,visibleFixtures:this.fixtures.length,lightCap:this.spots?.length??0,lightLod:this.lod?{...this.lod,fakedBeams:this.beams?.count||0,footprints:this.footprints?.count||0}:null,shadowCount:this.renderedShadows||0,workLight:this.workLight,fogAmount:this.fogAmount,camera:this.camera?.position.toArray(),target:this.controls?.target.toArray(),fog:this.fog?.stats(),visible:this.visible,hidden:this.platform.worker?this.platform.hidden:document.hidden,lost:this.lost,epoch:this.snapshot?.epoch,revision:this.snapshot?.revision,frameNumber:this.snapshot?.["frame-number"]}}fail(t){this.error=String(t?.message||t),this.ready=!1,this.setStatus(`Preview unavailable: ${this.error}`),this.pause(),this.resolveReady?.(!1),console.error("Tolglow preview",t)}dispose(){if(!this.disposed){this.disposed=!0,this.ready=!1,this.pause(),this.abort.abort(),this.resizeObserver?.disconnect(),this.activity?.dispose(),this.editorInput?.dispose(),this.editorOverlay?.dispose(),this.navigation?.dispose(),this.controls?.dispose(),this.fog?.dispose(),this.beams?.dispose(),this.footprints?.dispose();for(let t of this.fixtures)t.dispose();for(let t of this.diffuserTubes||[])t.dispose();for(let t of this.spots||[])t.dispose();this.reflections?.dispose(),this.content&&Qh(this.content),this.glass?.dispose(),this.inputPass?.dispose(),this.bloom?.dispose(),this.outputPass?.dispose(),this.edges?.dispose(),this.film?.dispose(),this.gpuTimer?.dispose(),this.composer?.dispose(),this.sceneTarget?.dispose(),this.compositeTarget?.dispose(),this.environmentTarget?.dispose(),this.materials&&tp(this.materials),this.renderer?.dispose(),this.resolveReady?.(!1),this.canvas.tolglowPreview===this&&delete this.canvas.tolglowPreview}}};var pt,Ro,ms=0,fr=0,Jv=0,le,Io=!1,Po=0,mr=null,_c=!1,ji=!1,vn=0,Hi=null,pr=null,gs=null,Co=null,xs=null,hu=!1,ii=[],yn=n=>postMessage(n),Tp=()=>ji&&le?.generation===vn&&(!le.paused||_c),Ap=()=>ii.some(n=>n.fence);function Rp(){if(Io||!pt||pt.lost||Hi||!Tp())return!1;let n=pt.softwareRenderer||le.paused?1:2;return ii.length+ +(pr!==null)<n&&!(pt.resizePending&&Ap())}function uu(){Rp()&&!pt.displayClock.pending&&pt.displayClock.at(mr??performance.now(),Qv,pt.softwareRenderer?.5:Math.min(2.5,250/pt.targetHz))}function Cp(n){n.fence&&n.gl.deleteSync(n.fence),n.bitmap?.close()}function du(){clearTimeout(xs),xs=null;for(let n of ii)Cp(n);ii.length&&(ii.length=0,yn({type:"render-retired",rendering:!1}))}function Pp(){Io=!0,Hi=gs=null,pt?.displayClock.cancel(),du(),pt?.dispose()}function vc(n){Pp(),yn({type:"error",message:String(n?.message||n)})}function Ip(n){let t=performance.now();n.bitmap=Ro.transferToImageBitmap(),n.bitmapMs=performance.now()-t}function $v(n){xs!==null||hu||(!pt.softwareRenderer&&globalThis.scheduler?.yield&&n.yields<16&&performance.now()-n.submitted<Math.min(4,500/pt.targetHz)?(n.yields++,hu=!0,scheduler.yield().then(()=>{hu=!1,gr()},vc)):xs=setTimeout(()=>{xs=null,gr()},2))}function jv(){let n=ii[0];if(!n||n.fence||pr)return;ii.shift(),pr=n;let{bitmap:t,metadata:e,telemetry:i,time:s,started:r,duration:o,gpuWait:a,bitmapMs:l}=n;try{postMessage({type:"frame",...e,bitmap:t,rendering:ii.length>0,telemetry:{...i,frameIntervalMs:pt.frameInterval,workerFrameMs:o,rafDeliveryMs:r-s,readyForFrameMs:pt.lastReadyMs,gpuCompletionWaitMs:a,idleBudgetMs:Math.max(0,pt.frameInterval-o),bitmapTransferMs:l}},[t]),n.bitmap=null}catch(c){throw t.close(),c}}function gr(){if(!Io)try{if(pt.lost){du();return}for(let e of ii){if(!e.fence)continue;let{gl:i,fence:s}=e,r=i.clientWaitSync(s,0,0);if(r===i.WAIT_FAILED)throw new Error("Preview GPU frame wait failed");if(r!==i.ALREADY_SIGNALED&&r!==i.CONDITION_SATISFIED){if(performance.now()-e.submitted>3e4)throw new Error("Preview GPU frame did not complete");break}if(i.deleteSync(s),e.fence=null,e.gpuWait=performance.now()-e.submitted,ji&&e.metadata.generation===vn&&!e.bitmap&&Ip(e),e.duration=performance.now()-e.started,pt.softwareRenderer){let o=1e3/pt.targetHz,a=pt.frameInterval;pt.frameInterval=Math.max(o,Math.min(e.duration*2,a*1.5),a*.5)}}let n=!1;for(;ii[0]&&!ii[0].fence&&(!ji||ii[0].metadata.generation!==vn);)Cp(ii.shift()),n=!0;n&&yn({type:"render-retired",rendering:ii.length>0}),jv();let t=ii.find(e=>e.fence);if(t)$v(t);else if(clearTimeout(xs),xs=null,gs&&(Tp()&&pt.measureLoad(gs.elapsed,gs.now),gs=null),Hi){let e=Hi;Hi=null,xc(e)}uu()}catch(n){vc(n)}}function Qv(n){if(Rp())try{let t=performance.now(),e=++Jv;yn({type:"render-start",frameId:e,stateId:ms});let i=Po?Math.min(.1,(n-Po)/1e3):0,s=1e3/pt.targetHz;mr=wu(mr,n,s),pt.softwareRenderer||(pt.frameInterval=s),Po=n,pt.clock+=i,pt.draw(i),_c=!1;let r=performance.now(),o=pt.renderer.getContext(),a=o.fenceSync(o.SYNC_GPU_COMMANDS_COMPLETE,0);if(!a)throw new Error("Could not fence the preview GPU frame");let l=pt.snapshot,c={time:n,started:t,submitted:r,gl:o,fence:a,bitmap:null,yields:0,metadata:{frameId:e,stateId:ms,actionThrough:fr,generation:vn,runtimeId:l?.["runtime-id"],showId:l?.["show-id"],epoch:l?.epoch,showFrame:l?.["frame-number"]??0}};ii.push(c),o.flush(),pt.softwareRenderer||Ip(c),c.telemetry=pt.getTelemetry(),gr()}catch(t){vc(t)}}function xc(n,t=!1){if(!(n.id<=ms||Io)){if(ms=n.id,!ji||n.state.generation!==vn){yn({type:"state-ack",id:ms,actionThrough:fr});return}if(le=n.state,t){pt.updateEmittedOutput({...pt.snapshot,...le.snapshot},le.receivedAt-performance.timeOrigin),yn({type:"state-ack",id:ms,actionThrough:fr}),uu();return}if(!pt){Ro=new OffscreenCanvas(1,1);let e="",i={set textContent(r){e=r,yn({type:"status",text:r})},get textContent(){return e}};if(pt=new gc(Ro,i,{worker:!0,autoStart:!1,viewport:le.viewport,displayHz:le.displayHz,coarsePointer:le.coarsePointer,reducedMotion:le.reducedMotion}),pt.error)throw new Error(pt.error);let s=pt.abort.signal;Ro.addEventListener("webglcontextlost",()=>{pt.displayClock.cancel(),du()},{signal:s}),Ro.addEventListener("webglcontextrestored",()=>gr(),{signal:s})}if(pt.setDisplayHz(le.displayHz),pt.platform.viewport=le.viewport,pt.visible=!0,pt.restorePending&&pt.restoreContext(),pt.reducedMotion.matches=le.reducedMotion,pt.setQuality(le.quality),pt.setFilm(le.film),pt.setWorkLight(le.workLight),pt.setFog(le.fog),pt.setLightingMode(le.lightingMode),le.snapshot){let e=le.snapshot;for(let i of["sculpt-guide","spatial-volumes","volume-guides","glass-meshes","glass-volumes","video-surfaces"])Object.hasOwn(e,i)||(e[i]=pt.snapshot?.[i]);!e.rig&&pt.snapshot&&(e.rig=pt.snapshot.rig),Object.hasOwn(e,"editor")||(e.editor=pt.snapshot?.editor),pt.update(e,!1,le.receivedAt-performance.timeOrigin)}pt.camera.position.fromArray(le.camera.position),pt.controls.target.fromArray(le.camera.target),pt.camera.up.fromArray(le.camera.up),pt.camera.fov=le.camera.fov,pt.cameraName=le.camera.name,pt.controls.update(),pt.resize();for(let e of le.actions)e.id>fr&&(e.type==="burst"?pt.burst(e.options):e.type==="draw"&&(_c=!0),fr=e.id);yn({type:"state-ack",id:ms,actionThrough:fr}),le.paused&&(Po=0,mr=null,Co=null,pt.lighting.previous=null,pt.displayClock.cancel()),uu()}}self.onmessage=n=>{if(!Io)try{let t=n.data;if(t.type==="activity"){if(ji=t.active,vn=t.generation,_c=!1,Po=0,mr=Co=gs=null,Hi&&Hi.state.generation!==vn){let e=Hi;Hi=null,xc(e)}pt&&(pt.visible=ji,pt.platform.hidden=!ji,ji||pt.fog?.showAtmosphere?.suspend(),pt.displayClock.cancel(),pt.lighting.latest=pt.lighting.previous=null,gr())}else if(t.type==="state"){let e=ji&&t.state.generation===vn;e&&t.outputOnly&&!Hi&&pt?.snapshot&&!pt.lost&&!pt.softwareRenderer&&bn(t.state.snapshot)===bn(pt.snapshot)?xc(t,!0):e&&(Ap()||pt?.lost&&!pt.restorePending)?Hi=t:xc(t)}else if(t.type==="frame-ack"&&t.frameId===pr?.metadata.frameId){let e=pr;if(pr=null,ji&&e.metadata.generation===vn){let i=performance.now();pt.lastReadyMs=i-e.time,gs={elapsed:Math.min(pt.lastReadyMs,Co===null?1/0:i-Co),now:i},Co=i,pt.softwareRenderer&&(mr=i+pt.frameInterval)}gr()}else t.type==="dispose"&&(Pp(),close())}catch(t){vc(t)}};yn({type:"ready"});
/*! Bundled license information:

three/build/three.core.js:
three/build/three.module.js:
  (**
   * @license
   * Copyright 2010-2026 Three.js Authors
   * SPDX-License-Identifier: MIT
   *)
*/
