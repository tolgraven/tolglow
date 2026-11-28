function Bu(n,t,e){return n==null?t+e:n+Math.max(1,Math.floor((t-n)/e)+1)*e}var Oo=class{constructor({now:t=()=>performance.now(),raf:e=globalThis.requestAnimationFrame?.bind(globalThis),cancelRaf:i=globalThis.cancelAnimationFrame?.bind(globalThis),timer:s=globalThis.setTimeout.bind(globalThis),cancelTimer:r=globalThis.clearTimeout.bind(globalThis)}={}){Object.assign(this,{now:t,raf:e,cancelRaf:i,timer:s,cancelTimer:r}),this.pending=!1}cancel(){this.rafId!=null&&this.cancelRaf?.(this.rafId),this.timerId!=null&&this.cancelTimer(this.timerId),this.rafId=this.timerId=null,this.pending=!1}at(t,e,i=.5){this.cancel(),this.pending=!0;let s=o=>{if(this.rafId=this.timerId=null,o+i<t){r();return}this.pending=!1,e(o)},r=()=>{let o=Math.max(0,t-this.now());if(this.raf&&o<=18)try{this.rafId=this.raf(s);return}catch(a){if(a.name!=="NotSupportedError")throw a;this.raf=null}this.raf?this.timerId=this.timer(r,o-16):this.timerId=this.timer(()=>s(this.now()),Math.max(1,o))};r()}},Bo=class{constructor(t,e=globalThis){this.changed=t,this.host=e,this.active=!1,this.tick=this.tick.bind(this)}setActive(t){t===this.active||!this.host.requestAnimationFrame||(this.active=t,t?this.start():(this.host.cancelAnimationFrame(this.raf),this.host.clearTimeout(this.timer)))}start(){this.gaps=[],this.samples=0,this.previous=null,this.raf=this.host.requestAnimationFrame(this.tick)}tick(t){if(!this.active)return;this.samples++;let e=this.previous===null?0:t-this.previous;if(this.previous=t,e>=3&&e<=120&&this.gaps.push(e),this.gaps.length<16&&this.samples<40){this.raf=this.host.requestAnimationFrame(this.tick);return}if(this.gaps.length>=8){this.gaps.sort((o,a)=>o-a);let i=this.gaps[Math.floor(this.gaps.length/4)],s=this.gaps.map(o=>o/Math.max(1,Math.round(o/i))).sort((o,a)=>o-a),r=Math.round(1e3/s[Math.floor(s.length/2)]);(this.hz==null||Math.abs(this.hz-r)>=2)&&(this.hz=r,this.changed(r))}this.active&&(this.timer=this.host.setTimeout(()=>this.start(),3e4))}};var En=n=>JSON.stringify([n?.["runtime-id"]??null,n?.["show-id"]??null,n?.epoch??null]);var On={LEFT:0,MIDDLE:1,RIGHT:2,ROTATE:0,DOLLY:1,PAN:2},Bi={ROTATE:0,PAN:1,DOLLY_PAN:2,DOLLY_ROTATE:3},Sd=0,fh=1,bd=2;var ls=1,wd=2,js=3,Bn=0,Ze=1,Ue=2,wi=0,Qs=1,Qr=2,ph=3,mh=4,Za=5;var gn=100,Ed=101,Td=102,Ad=103,Rd=104,Ka=200,tr=201,Cd=202,Pd=203,gh=204,xh=205,Id=206,Dd=207,Ld=208,Nd=209,Ud=210,Fd=211,Od=212,Bd=213,zd=214,ma=0,ga=1,xa=2,Fs=3,_a=4,ya=5,va=6,Ma=7,Ja=0,kd=1,Hd=2,Yi=0,to=1,eo=2,io=3,cs=4,no=5,so=6,ro=7;var _h=300,hs=301,er=302,$a=303,ja=304,un=1e3,Wi=1001,Sa=1002,De=1003,Vd=1004;var oo=1005;var ce=1006,Qa=1007;var zi=1008;var ei=1009,yh=1010,vh=1011,ir=1012,tl=1013,hi=1014,gi=1015,xe=1016,el=1017,il=1018,us=1020,Mh=35902,Sh=35899,bh=1021,wh=1022,ui=1023,Li=1026,xn=1027,zn=1028,nl=1029,kn=1030,sl=1031;var rl=1033,ao=33776,lo=33777,co=33778,ho=33779,ol=35840,al=35841,ll=35842,cl=35843,hl=36196,ul=37492,dl=37496,fl=37488,pl=37489,uo=37490,ml=37491,gl=37808,xl=37809,_l=37810,yl=37811,vl=37812,Ml=37813,Sl=37814,bl=37815,wl=37816,El=37817,Tl=37818,Al=37819,Rl=37820,Cl=37821,Pl=36492,Il=36494,Dl=36495,Ll=36283,Nl=36284,fo=36285,Ul=36286;var Pr=2300,ba=2301,fa=2302,eh=2303,ih=2400,nh=2401,sh=2402;var Gd=3200;var po=0,Wd=1,ki="",Ie="srgb",In="srgb-linear",Ir="linear",ne="srgb";var pa=7680;var Xd=519,qd=512,Yd=513,Zd=514,nr=515,Kd=516,Jd=517,Eh=518,Th=519,$d=35044,Zi=35048;var _n="300 es",Ti=2e3,Os=2001;function Qp(n){for(let t=n.length-1;t>=0;--t)if(n[t]>=65535)return!0;return!1}function tm(n){return ArrayBuffer.isView(n)&&!(n instanceof DataView)}function Dr(n){return document.createElementNS("http://www.w3.org/1999/xhtml",n)}function jd(){let n=Dr("canvas");return n.style.display="block",n}var zu={},Bs=null;function Ah(...n){let t="THREE."+n.shift();Bs?Bs("log",t,...n):console.log(t,...n)}function Qd(n){let t=n[0];if(typeof t=="string"&&t.startsWith("TSL:")){let e=n[1];e&&e.isStackTrace?n[0]+=" "+e.getLocation():n[1]='Stack trace not available. Enable "THREE.Node.captureStackTrace" to capture stack traces.'}return n}function Lt(...n){n=Qd(n);let t="THREE."+n.shift();if(Bs)Bs("warn",t,...n);else{let e=n[0];e&&e.isStackTrace?console.warn(e.getError(t)):console.warn(t,...n)}}function Ot(...n){n=Qd(n);let t="THREE."+n.shift();if(Bs)Bs("error",t,...n);else{let e=n[0];e&&e.isStackTrace?console.error(e.getError(t)):console.error(t,...n)}}function $n(...n){let t=n.join(" ");t in zu||(zu[t]=!0,Lt(...n))}function tf(n,t,e){return new Promise(function(i,s){function r(){switch(n.clientWaitSync(t,n.SYNC_FLUSH_COMMANDS_BIT,0)){case n.WAIT_FAILED:s();break;case n.TIMEOUT_EXPIRED:setTimeout(r,e);break;default:i()}}setTimeout(r,e)})}var ef={[ma]:ma,[xa]:va,[_a]:_a,[Fs]:ya,[ga]:ga,[va]:xa,[Ma]:Ma,[ya]:Fs},Ni=class{addEventListener(t,e){this._listeners===void 0&&(this._listeners={});let i=this._listeners;i[t]===void 0&&(i[t]=[]),i[t].indexOf(e)===-1&&i[t].push(e)}hasEventListener(t,e){let i=this._listeners;return i===void 0?!1:i[t]!==void 0&&i[t].indexOf(e)!==-1}removeEventListener(t,e){let i=this._listeners;if(i===void 0)return;let s=i[t];if(s!==void 0){let r=s.indexOf(e);r!==-1&&s.splice(r,1)}}dispatchEvent(t){let e=this._listeners;if(e===void 0)return;let i=e[t.type];if(i!==void 0){t.target=this;let s=i.slice(0);for(let r=0,o=s.length;r<o;r++)s[r].call(this,t);t.target=null}}},je=["00","01","02","03","04","05","06","07","08","09","0a","0b","0c","0d","0e","0f","10","11","12","13","14","15","16","17","18","19","1a","1b","1c","1d","1e","1f","20","21","22","23","24","25","26","27","28","29","2a","2b","2c","2d","2e","2f","30","31","32","33","34","35","36","37","38","39","3a","3b","3c","3d","3e","3f","40","41","42","43","44","45","46","47","48","49","4a","4b","4c","4d","4e","4f","50","51","52","53","54","55","56","57","58","59","5a","5b","5c","5d","5e","5f","60","61","62","63","64","65","66","67","68","69","6a","6b","6c","6d","6e","6f","70","71","72","73","74","75","76","77","78","79","7a","7b","7c","7d","7e","7f","80","81","82","83","84","85","86","87","88","89","8a","8b","8c","8d","8e","8f","90","91","92","93","94","95","96","97","98","99","9a","9b","9c","9d","9e","9f","a0","a1","a2","a3","a4","a5","a6","a7","a8","a9","aa","ab","ac","ad","ae","af","b0","b1","b2","b3","b4","b5","b6","b7","b8","b9","ba","bb","bc","bd","be","bf","c0","c1","c2","c3","c4","c5","c6","c7","c8","c9","ca","cb","cc","cd","ce","cf","d0","d1","d2","d3","d4","d5","d6","d7","d8","d9","da","db","dc","dd","de","df","e0","e1","e2","e3","e4","e5","e6","e7","e8","e9","ea","eb","ec","ed","ee","ef","f0","f1","f2","f3","f4","f5","f6","f7","f8","f9","fa","fb","fc","fd","fe","ff"],ku=1234567,Ns=Math.PI/180,jn=180/Math.PI;function sr(){let n=Math.random()*4294967295|0,t=Math.random()*4294967295|0,e=Math.random()*4294967295|0,i=Math.random()*4294967295|0;return(je[n&255]+je[n>>8&255]+je[n>>16&255]+je[n>>24&255]+"-"+je[t&255]+je[t>>8&255]+"-"+je[t>>16&15|64]+je[t>>24&255]+"-"+je[e&63|128]+je[e>>8&255]+"-"+je[e>>16&255]+je[e>>24&255]+je[i&255]+je[i>>8&255]+je[i>>16&255]+je[i>>24&255]).toLowerCase()}function Vt(n,t,e){return Math.max(t,Math.min(e,n))}function Rh(n,t){return(n%t+t)%t}function em(n,t,e,i,s){return i+(n-t)*(s-i)/(e-t)}function im(n,t,e){return n!==t?(e-n)/(t-n):0}function Ar(n,t,e){return(1-e)*n+e*t}function nm(n,t,e,i){return Ar(n,t,1-Math.exp(-e*i))}function sm(n,t=1){return t-Math.abs(Rh(n,t*2)-t)}function rm(n,t,e){return n<=t?0:n>=e?1:(n=(n-t)/(e-t),n*n*(3-2*n))}function om(n,t,e){return n<=t?0:n>=e?1:(n=(n-t)/(e-t),n*n*n*(n*(n*6-15)+10))}function am(n,t){return n+Math.floor(Math.random()*(t-n+1))}function lm(n,t){return n+Math.random()*(t-n)}function cm(n){return n*(.5-Math.random())}function hm(n){n!==void 0&&(ku=n);let t=ku+=1831565813;return t=Math.imul(t^t>>>15,t|1),t^=t+Math.imul(t^t>>>7,t|61),((t^t>>>14)>>>0)/4294967296}function um(n){return n*Ns}function dm(n){return n*jn}function fm(n){return n>0&&Number.isInteger(n)&&2**Math.round(Math.log2(n))===n}function pm(n){return Math.pow(2,Math.ceil(Math.log(n)/Math.LN2))}function Ch(n){return Math.pow(2,Math.floor(Math.log(n)/Math.LN2))}function mm(n,t,e,i,s){let r=Math.cos,o=Math.sin,a=r(e/2),l=o(e/2),c=r((t+i)/2),h=o((t+i)/2),u=r((t-i)/2),d=o((t-i)/2),f=r((i-t)/2),m=o((i-t)/2);switch(s){case"XYX":n.set(a*h,l*u,l*d,a*c);break;case"YZY":n.set(l*d,a*h,l*u,a*c);break;case"ZXZ":n.set(l*u,l*d,a*h,a*c);break;case"XZX":n.set(a*h,l*m,l*f,a*c);break;case"YXY":n.set(l*f,a*h,l*m,a*c);break;case"ZYZ":n.set(l*m,l*f,a*h,a*c);break;default:Lt("MathUtils: .setQuaternionFromProperEuler() encountered an unknown order: "+s)}}function Ph(n){switch(n.constructor){case Uint32Array:return 4294967295;case Uint16Array:return 65535;case Uint8Array:case Uint8ClampedArray:return 255;case Int32Array:return 2147483647;case Int16Array:return 32767;case Int8Array:return 127;default:return}}function nf(n,t){if(t.constructor===Float32Array)return n;let e=Ph(t);if(e===void 0)throw new Error("THREE.MathUtils: Invalid component type.");return Math.max(n/e,-1)}function sf(n,t){if(t.constructor===Float32Array)return n;let e=Ph(t);if(e===void 0)throw new Error("THREE.MathUtils: Invalid component type.");return Math.round(n*e)}var rr={DEG2RAD:Ns,RAD2DEG:jn,generateUUID:sr,clamp:Vt,euclideanModulo:Rh,mapLinear:em,inverseLerp:im,lerp:Ar,damp:nm,pingpong:sm,smoothstep:rm,smootherstep:om,randInt:am,randFloat:lm,randFloatSpread:cm,seededRandom:hm,degToRad:um,radToDeg:dm,isPowerOfTwo:fm,ceilPowerOfTwo:pm,floorPowerOfTwo:Ch,setQuaternionFromProperEuler:mm,normalize:sf,denormalize:nf},et=class{static{this.prototype.isVector2=!0}constructor(t=0,e=0){this.x=t,this.y=e}get width(){return this.x}set width(t){this.x=t}get height(){return this.y}set height(t){this.y=t}set(t,e){return this.x=t,this.y=e,this}setScalar(t){return this.x=t,this.y=t,this}setX(t){return this.x=t,this}setY(t){return this.y=t,this}setComponent(t,e){switch(t){case 0:this.x=e;break;case 1:this.y=e;break;default:throw new Error("THREE.Vector2: index is out of range: "+t)}return this}getComponent(t){switch(t){case 0:return this.x;case 1:return this.y;default:throw new Error("THREE.Vector2: index is out of range: "+t)}}clone(){return new this.constructor(this.x,this.y)}copy(t){return this.x=t.x,this.y=t.y,this}add(t){return this.x+=t.x,this.y+=t.y,this}addScalar(t){return this.x+=t,this.y+=t,this}addVectors(t,e){return this.x=t.x+e.x,this.y=t.y+e.y,this}addScaledVector(t,e){return this.x+=t.x*e,this.y+=t.y*e,this}sub(t){return this.x-=t.x,this.y-=t.y,this}subScalar(t){return this.x-=t,this.y-=t,this}subVectors(t,e){return this.x=t.x-e.x,this.y=t.y-e.y,this}multiply(t){return this.x*=t.x,this.y*=t.y,this}multiplyScalar(t){return this.x*=t,this.y*=t,this}divide(t){return this.x/=t.x,this.y/=t.y,this}divideScalar(t){return this.multiplyScalar(1/t)}applyMatrix3(t){let e=this.x,i=this.y,s=t.elements;return this.x=s[0]*e+s[3]*i+s[6],this.y=s[1]*e+s[4]*i+s[7],this}min(t){return this.x=Math.min(this.x,t.x),this.y=Math.min(this.y,t.y),this}max(t){return this.x=Math.max(this.x,t.x),this.y=Math.max(this.y,t.y),this}clamp(t,e){return this.x=Vt(this.x,t.x,e.x),this.y=Vt(this.y,t.y,e.y),this}clampScalar(t,e){return this.x=Vt(this.x,t,e),this.y=Vt(this.y,t,e),this}clampLength(t,e){let i=this.length();return this.divideScalar(i||1).multiplyScalar(Vt(i,t,e))}floor(){return this.x=Math.floor(this.x),this.y=Math.floor(this.y),this}ceil(){return this.x=Math.ceil(this.x),this.y=Math.ceil(this.y),this}round(){return this.x=Math.round(this.x),this.y=Math.round(this.y),this}roundToZero(){return this.x=Math.trunc(this.x),this.y=Math.trunc(this.y),this}negate(){return this.x=-this.x,this.y=-this.y,this}dot(t){return this.x*t.x+this.y*t.y}cross(t){return this.x*t.y-this.y*t.x}lengthSq(){return this.x*this.x+this.y*this.y}length(){return Math.sqrt(this.x*this.x+this.y*this.y)}manhattanLength(){return Math.abs(this.x)+Math.abs(this.y)}normalize(){return this.divideScalar(this.length()||1)}angle(){return Math.atan2(-this.y,-this.x)+Math.PI}angleTo(t){let e=Math.sqrt(this.lengthSq()*t.lengthSq());if(e===0)return Math.PI/2;let i=this.dot(t)/e;return Math.acos(Vt(i,-1,1))}distanceTo(t){return Math.sqrt(this.distanceToSquared(t))}distanceToSquared(t){let e=this.x-t.x,i=this.y-t.y;return e*e+i*i}manhattanDistanceTo(t){return Math.abs(this.x-t.x)+Math.abs(this.y-t.y)}setLength(t){return this.normalize().multiplyScalar(t)}lerp(t,e){return this.x+=(t.x-this.x)*e,this.y+=(t.y-this.y)*e,this}lerpVectors(t,e,i){return this.x=t.x+(e.x-t.x)*i,this.y=t.y+(e.y-t.y)*i,this}equals(t){return t.x===this.x&&t.y===this.y}fromArray(t,e=0){return this.x=t[e],this.y=t[e+1],this}toArray(t=[],e=0){return t[e]=this.x,t[e+1]=this.y,t}fromBufferAttribute(t,e){return this.x=t.getX(e),this.y=t.getY(e),this}rotateAround(t,e){let i=Math.cos(e),s=Math.sin(e),r=this.x-t.x,o=this.y-t.y;return this.x=r*i-o*s+t.x,this.y=r*s+o*i+t.y,this}random(){return this.x=Math.random(),this.y=Math.random(),this}*[Symbol.iterator](){yield this.x,yield this.y}},ti=class{constructor(t=0,e=0,i=0,s=1){this.isQuaternion=!0,this._x=t,this._y=e,this._z=i,this._w=s}static slerpFlat(t,e,i,s,r,o,a){let l=i[s+0],c=i[s+1],h=i[s+2],u=i[s+3],d=r[o+0],f=r[o+1],m=r[o+2],x=r[o+3];if(u!==x||l!==d||c!==f||h!==m){let p=l*d+c*f+h*m+u*x;p<0&&(d=-d,f=-f,m=-m,x=-x,p=-p);let g=1-a;if(p<.9995){let y=Math.acos(p),v=Math.sin(y);g=Math.sin(g*y)/v,a=Math.sin(a*y)/v,l=l*g+d*a,c=c*g+f*a,h=h*g+m*a,u=u*g+x*a}else{l=l*g+d*a,c=c*g+f*a,h=h*g+m*a,u=u*g+x*a;let y=1/Math.sqrt(l*l+c*c+h*h+u*u);l*=y,c*=y,h*=y,u*=y}}t[e]=l,t[e+1]=c,t[e+2]=h,t[e+3]=u}static multiplyQuaternionsFlat(t,e,i,s,r,o){let a=i[s],l=i[s+1],c=i[s+2],h=i[s+3],u=r[o],d=r[o+1],f=r[o+2],m=r[o+3];return t[e]=a*m+h*u+l*f-c*d,t[e+1]=l*m+h*d+c*u-a*f,t[e+2]=c*m+h*f+a*d-l*u,t[e+3]=h*m-a*u-l*d-c*f,t}get x(){return this._x}set x(t){this._x=t,this._onChangeCallback()}get y(){return this._y}set y(t){this._y=t,this._onChangeCallback()}get z(){return this._z}set z(t){this._z=t,this._onChangeCallback()}get w(){return this._w}set w(t){this._w=t,this._onChangeCallback()}set(t,e,i,s){return this._x=t,this._y=e,this._z=i,this._w=s,this._onChangeCallback(),this}clone(){return new this.constructor(this._x,this._y,this._z,this._w)}copy(t){return this._x=t.x,this._y=t.y,this._z=t.z,this._w=t.w,this._onChangeCallback(),this}setFromEuler(t,e=!0){let i=t._x,s=t._y,r=t._z,o=t._order,a=Math.cos,l=Math.sin,c=a(i/2),h=a(s/2),u=a(r/2),d=l(i/2),f=l(s/2),m=l(r/2);switch(o){case"XYZ":this._x=d*h*u+c*f*m,this._y=c*f*u-d*h*m,this._z=c*h*m+d*f*u,this._w=c*h*u-d*f*m;break;case"YXZ":this._x=d*h*u+c*f*m,this._y=c*f*u-d*h*m,this._z=c*h*m-d*f*u,this._w=c*h*u+d*f*m;break;case"ZXY":this._x=d*h*u-c*f*m,this._y=c*f*u+d*h*m,this._z=c*h*m+d*f*u,this._w=c*h*u-d*f*m;break;case"ZYX":this._x=d*h*u-c*f*m,this._y=c*f*u+d*h*m,this._z=c*h*m-d*f*u,this._w=c*h*u+d*f*m;break;case"YZX":this._x=d*h*u+c*f*m,this._y=c*f*u+d*h*m,this._z=c*h*m-d*f*u,this._w=c*h*u-d*f*m;break;case"XZY":this._x=d*h*u-c*f*m,this._y=c*f*u-d*h*m,this._z=c*h*m+d*f*u,this._w=c*h*u+d*f*m;break;default:Lt("Quaternion: .setFromEuler() encountered an unknown order: "+o)}return e===!0&&this._onChangeCallback(),this}setFromAxisAngle(t,e){let i=e/2,s=Math.sin(i);return this._x=t.x*s,this._y=t.y*s,this._z=t.z*s,this._w=Math.cos(i),this._onChangeCallback(),this}setFromRotationMatrix(t){let e=t.elements,i=e[0],s=e[4],r=e[8],o=e[1],a=e[5],l=e[9],c=e[2],h=e[6],u=e[10],d=i+a+u;if(d>0){let f=.5/Math.sqrt(d+1);this._w=.25/f,this._x=(h-l)*f,this._y=(r-c)*f,this._z=(o-s)*f}else if(i>a&&i>u){let f=2*Math.sqrt(1+i-a-u);this._w=(h-l)/f,this._x=.25*f,this._y=(s+o)/f,this._z=(r+c)/f}else if(a>u){let f=2*Math.sqrt(1+a-i-u);this._w=(r-c)/f,this._x=(s+o)/f,this._y=.25*f,this._z=(l+h)/f}else{let f=2*Math.sqrt(1+u-i-a);this._w=(o-s)/f,this._x=(r+c)/f,this._y=(l+h)/f,this._z=.25*f}return this._onChangeCallback(),this}setFromUnitVectors(t,e){let i=t.dot(e)+1;return i<1e-8?(i=0,Math.abs(t.x)>Math.abs(t.z)?(this._x=-t.y,this._y=t.x,this._z=0,this._w=i):(this._x=0,this._y=-t.z,this._z=t.y,this._w=i)):(this._x=t.y*e.z-t.z*e.y,this._y=t.z*e.x-t.x*e.z,this._z=t.x*e.y-t.y*e.x,this._w=i),this.normalize()}angleTo(t){return 2*Math.acos(Math.abs(Vt(this.dot(t),-1,1)))}rotateTowards(t,e){let i=this.angleTo(t);if(i===0)return this;let s=Math.min(1,e/i);return this.slerp(t,s),this}identity(){return this.set(0,0,0,1)}invert(){return this.conjugate()}conjugate(){return this._x*=-1,this._y*=-1,this._z*=-1,this._onChangeCallback(),this}dot(t){return this._x*t._x+this._y*t._y+this._z*t._z+this._w*t._w}lengthSq(){return this._x*this._x+this._y*this._y+this._z*this._z+this._w*this._w}length(){return Math.sqrt(this._x*this._x+this._y*this._y+this._z*this._z+this._w*this._w)}normalize(){let t=this.length();return t===0?(this._x=0,this._y=0,this._z=0,this._w=1):(t=1/t,this._x=this._x*t,this._y=this._y*t,this._z=this._z*t,this._w=this._w*t),this._onChangeCallback(),this}multiply(t){return this.multiplyQuaternions(this,t)}premultiply(t){return this.multiplyQuaternions(t,this)}multiplyQuaternions(t,e){let i=t._x,s=t._y,r=t._z,o=t._w,a=e._x,l=e._y,c=e._z,h=e._w;return this._x=i*h+o*a+s*c-r*l,this._y=s*h+o*l+r*a-i*c,this._z=r*h+o*c+i*l-s*a,this._w=o*h-i*a-s*l-r*c,this._onChangeCallback(),this}slerp(t,e){let i=t._x,s=t._y,r=t._z,o=t._w,a=this.dot(t);a<0&&(i=-i,s=-s,r=-r,o=-o,a=-a);let l=1-e;if(a<.9995){let c=Math.acos(a),h=Math.sin(c);l=Math.sin(l*c)/h,e=Math.sin(e*c)/h,this._x=this._x*l+i*e,this._y=this._y*l+s*e,this._z=this._z*l+r*e,this._w=this._w*l+o*e,this._onChangeCallback()}else this._x=this._x*l+i*e,this._y=this._y*l+s*e,this._z=this._z*l+r*e,this._w=this._w*l+o*e,this.normalize();return this}slerpQuaternions(t,e,i){return this.copy(t).slerp(e,i)}random(){let t=2*Math.PI*Math.random(),e=2*Math.PI*Math.random(),i=Math.random(),s=Math.sqrt(1-i),r=Math.sqrt(i);return this.set(s*Math.sin(t),s*Math.cos(t),r*Math.sin(e),r*Math.cos(e))}equals(t){return t._x===this._x&&t._y===this._y&&t._z===this._z&&t._w===this._w}fromArray(t,e=0){return this._x=t[e],this._y=t[e+1],this._z=t[e+2],this._w=t[e+3],this._onChangeCallback(),this}toArray(t=[],e=0){return t[e]=this._x,t[e+1]=this._y,t[e+2]=this._z,t[e+3]=this._w,t}fromBufferAttribute(t,e){return this._x=t.getX(e),this._y=t.getY(e),this._z=t.getZ(e),this._w=t.getW(e),this._onChangeCallback(),this}toJSON(){return this.toArray()}_onChange(t){return this._onChangeCallback=t,this}_onChangeCallback(){}*[Symbol.iterator](){yield this._x,yield this._y,yield this._z,yield this._w}},C=class{static{this.prototype.isVector3=!0}constructor(t=0,e=0,i=0){this.x=t,this.y=e,this.z=i}set(t,e,i){return i===void 0&&(i=this.z),this.x=t,this.y=e,this.z=i,this}setScalar(t){return this.x=t,this.y=t,this.z=t,this}setX(t){return this.x=t,this}setY(t){return this.y=t,this}setZ(t){return this.z=t,this}setComponent(t,e){switch(t){case 0:this.x=e;break;case 1:this.y=e;break;case 2:this.z=e;break;default:throw new Error("THREE.Vector3: index is out of range: "+t)}return this}getComponent(t){switch(t){case 0:return this.x;case 1:return this.y;case 2:return this.z;default:throw new Error("THREE.Vector3: index is out of range: "+t)}}clone(){return new this.constructor(this.x,this.y,this.z)}copy(t){return this.x=t.x,this.y=t.y,this.z=t.z,this}add(t){return this.x+=t.x,this.y+=t.y,this.z+=t.z,this}addScalar(t){return this.x+=t,this.y+=t,this.z+=t,this}addVectors(t,e){return this.x=t.x+e.x,this.y=t.y+e.y,this.z=t.z+e.z,this}addScaledVector(t,e){return this.x+=t.x*e,this.y+=t.y*e,this.z+=t.z*e,this}sub(t){return this.x-=t.x,this.y-=t.y,this.z-=t.z,this}subScalar(t){return this.x-=t,this.y-=t,this.z-=t,this}subVectors(t,e){return this.x=t.x-e.x,this.y=t.y-e.y,this.z=t.z-e.z,this}multiply(t){return this.x*=t.x,this.y*=t.y,this.z*=t.z,this}multiplyScalar(t){return this.x*=t,this.y*=t,this.z*=t,this}multiplyVectors(t,e){return this.x=t.x*e.x,this.y=t.y*e.y,this.z=t.z*e.z,this}applyEuler(t){return this.applyQuaternion(Hu.setFromEuler(t))}applyAxisAngle(t,e){return this.applyQuaternion(Hu.setFromAxisAngle(t,e))}applyMatrix3(t){let e=this.x,i=this.y,s=this.z,r=t.elements;return this.x=r[0]*e+r[3]*i+r[6]*s,this.y=r[1]*e+r[4]*i+r[7]*s,this.z=r[2]*e+r[5]*i+r[8]*s,this}applyNormalMatrix(t){return this.applyMatrix3(t).normalize()}applyMatrix4(t){let e=this.x,i=this.y,s=this.z,r=t.elements,o=1/(r[3]*e+r[7]*i+r[11]*s+r[15]);return this.x=(r[0]*e+r[4]*i+r[8]*s+r[12])*o,this.y=(r[1]*e+r[5]*i+r[9]*s+r[13])*o,this.z=(r[2]*e+r[6]*i+r[10]*s+r[14])*o,this}applyQuaternion(t){let e=this.x,i=this.y,s=this.z,r=t.x,o=t.y,a=t.z,l=t.w,c=2*(o*s-a*i),h=2*(a*e-r*s),u=2*(r*i-o*e);return this.x=e+l*c+o*u-a*h,this.y=i+l*h+a*c-r*u,this.z=s+l*u+r*h-o*c,this}project(t){return this.applyMatrix4(t.matrixWorldInverse).applyMatrix4(t.projectionMatrix)}unproject(t){return this.applyMatrix4(t.projectionMatrixInverse).applyMatrix4(t.matrixWorld)}transformDirection(t){let e=this.x,i=this.y,s=this.z,r=t.elements;return this.x=r[0]*e+r[4]*i+r[8]*s,this.y=r[1]*e+r[5]*i+r[9]*s,this.z=r[2]*e+r[6]*i+r[10]*s,this.normalize()}divide(t){return this.x/=t.x,this.y/=t.y,this.z/=t.z,this}divideScalar(t){return this.multiplyScalar(1/t)}min(t){return this.x=Math.min(this.x,t.x),this.y=Math.min(this.y,t.y),this.z=Math.min(this.z,t.z),this}max(t){return this.x=Math.max(this.x,t.x),this.y=Math.max(this.y,t.y),this.z=Math.max(this.z,t.z),this}clamp(t,e){return this.x=Vt(this.x,t.x,e.x),this.y=Vt(this.y,t.y,e.y),this.z=Vt(this.z,t.z,e.z),this}clampScalar(t,e){return this.x=Vt(this.x,t,e),this.y=Vt(this.y,t,e),this.z=Vt(this.z,t,e),this}clampLength(t,e){let i=this.length();return this.divideScalar(i||1).multiplyScalar(Vt(i,t,e))}floor(){return this.x=Math.floor(this.x),this.y=Math.floor(this.y),this.z=Math.floor(this.z),this}ceil(){return this.x=Math.ceil(this.x),this.y=Math.ceil(this.y),this.z=Math.ceil(this.z),this}round(){return this.x=Math.round(this.x),this.y=Math.round(this.y),this.z=Math.round(this.z),this}roundToZero(){return this.x=Math.trunc(this.x),this.y=Math.trunc(this.y),this.z=Math.trunc(this.z),this}negate(){return this.x=-this.x,this.y=-this.y,this.z=-this.z,this}dot(t){return this.x*t.x+this.y*t.y+this.z*t.z}lengthSq(){return this.x*this.x+this.y*this.y+this.z*this.z}length(){return Math.sqrt(this.x*this.x+this.y*this.y+this.z*this.z)}manhattanLength(){return Math.abs(this.x)+Math.abs(this.y)+Math.abs(this.z)}normalize(){return this.divideScalar(this.length()||1)}setLength(t){return this.normalize().multiplyScalar(t)}lerp(t,e){return this.x+=(t.x-this.x)*e,this.y+=(t.y-this.y)*e,this.z+=(t.z-this.z)*e,this}lerpVectors(t,e,i){return this.x=t.x+(e.x-t.x)*i,this.y=t.y+(e.y-t.y)*i,this.z=t.z+(e.z-t.z)*i,this}cross(t){return this.crossVectors(this,t)}crossVectors(t,e){let i=t.x,s=t.y,r=t.z,o=e.x,a=e.y,l=e.z;return this.x=s*l-r*a,this.y=r*o-i*l,this.z=i*a-s*o,this}projectOnVector(t){let e=t.lengthSq();if(e===0)return this.set(0,0,0);let i=t.dot(this)/e;return this.copy(t).multiplyScalar(i)}projectOnPlane(t){return Pc.copy(this).projectOnVector(t),this.sub(Pc)}reflect(t){return this.sub(Pc.copy(t).multiplyScalar(2*this.dot(t)))}angleTo(t){let e=Math.sqrt(this.lengthSq()*t.lengthSq());if(e===0)return Math.PI/2;let i=this.dot(t)/e;return Math.acos(Vt(i,-1,1))}distanceTo(t){return Math.sqrt(this.distanceToSquared(t))}distanceToSquared(t){let e=this.x-t.x,i=this.y-t.y,s=this.z-t.z;return e*e+i*i+s*s}manhattanDistanceTo(t){return Math.abs(this.x-t.x)+Math.abs(this.y-t.y)+Math.abs(this.z-t.z)}setFromSpherical(t){return this.setFromSphericalCoords(t.radius,t.phi,t.theta)}setFromSphericalCoords(t,e,i){let s=Math.sin(e)*t;return this.x=s*Math.sin(i),this.y=Math.cos(e)*t,this.z=s*Math.cos(i),this}setFromCylindrical(t){return this.setFromCylindricalCoords(t.radius,t.theta,t.y)}setFromCylindricalCoords(t,e,i){return this.x=t*Math.sin(e),this.y=i,this.z=t*Math.cos(e),this}setFromMatrixPosition(t){let e=t.elements;return this.x=e[12],this.y=e[13],this.z=e[14],this}setFromMatrixScale(t){let e=this.setFromMatrixColumn(t,0).length(),i=this.setFromMatrixColumn(t,1).length(),s=this.setFromMatrixColumn(t,2).length();return this.x=e,this.y=i,this.z=s,this}setFromMatrixColumn(t,e){return this.fromArray(t.elements,e*4)}setFromMatrix3Column(t,e){return this.fromArray(t.elements,e*3)}setFromEuler(t){return this.x=t._x,this.y=t._y,this.z=t._z,this}setFromColor(t){return this.x=t.r,this.y=t.g,this.z=t.b,this}equals(t){return t.x===this.x&&t.y===this.y&&t.z===this.z}fromArray(t,e=0){return this.x=t[e],this.y=t[e+1],this.z=t[e+2],this}toArray(t=[],e=0){return t[e]=this.x,t[e+1]=this.y,t[e+2]=this.z,t}fromBufferAttribute(t,e){return this.x=t.getX(e),this.y=t.getY(e),this.z=t.getZ(e),this}random(){return this.x=Math.random(),this.y=Math.random(),this.z=Math.random(),this}randomDirection(){let t=Math.random()*Math.PI*2,e=Math.random()*2-1,i=Math.sqrt(1-e*e);return this.x=i*Math.cos(t),this.y=e,this.z=i*Math.sin(t),this}*[Symbol.iterator](){yield this.x,yield this.y,yield this.z}},Pc=new C,Hu=new ti,zt=class{static{this.prototype.isMatrix3=!0}constructor(t,e,i,s,r,o,a,l,c){this.elements=[1,0,0,0,1,0,0,0,1],t!==void 0&&this.set(t,e,i,s,r,o,a,l,c)}set(t,e,i,s,r,o,a,l,c){let h=this.elements;return h[0]=t,h[1]=s,h[2]=a,h[3]=e,h[4]=r,h[5]=l,h[6]=i,h[7]=o,h[8]=c,this}identity(){return this.set(1,0,0,0,1,0,0,0,1),this}copy(t){let e=this.elements,i=t.elements;return e[0]=i[0],e[1]=i[1],e[2]=i[2],e[3]=i[3],e[4]=i[4],e[5]=i[5],e[6]=i[6],e[7]=i[7],e[8]=i[8],this}extractBasis(t,e,i){return t.setFromMatrix3Column(this,0),e.setFromMatrix3Column(this,1),i.setFromMatrix3Column(this,2),this}setFromMatrix4(t){let e=t.elements;return this.set(e[0],e[4],e[8],e[1],e[5],e[9],e[2],e[6],e[10]),this}multiply(t){return this.multiplyMatrices(this,t)}premultiply(t){return this.multiplyMatrices(t,this)}multiplyMatrices(t,e){let i=t.elements,s=e.elements,r=this.elements,o=i[0],a=i[3],l=i[6],c=i[1],h=i[4],u=i[7],d=i[2],f=i[5],m=i[8],x=s[0],p=s[3],g=s[6],y=s[1],v=s[4],S=s[7],T=s[2],b=s[5],_=s[8];return r[0]=o*x+a*y+l*T,r[3]=o*p+a*v+l*b,r[6]=o*g+a*S+l*_,r[1]=c*x+h*y+u*T,r[4]=c*p+h*v+u*b,r[7]=c*g+h*S+u*_,r[2]=d*x+f*y+m*T,r[5]=d*p+f*v+m*b,r[8]=d*g+f*S+m*_,this}multiplyScalar(t){let e=this.elements;return e[0]*=t,e[3]*=t,e[6]*=t,e[1]*=t,e[4]*=t,e[7]*=t,e[2]*=t,e[5]*=t,e[8]*=t,this}determinant(){let t=this.elements,e=t[0],i=t[1],s=t[2],r=t[3],o=t[4],a=t[5],l=t[6],c=t[7],h=t[8];return e*o*h-e*a*c-i*r*h+i*a*l+s*r*c-s*o*l}invert(){let t=this.elements,e=t[0],i=t[1],s=t[2],r=t[3],o=t[4],a=t[5],l=t[6],c=t[7],h=t[8],u=h*o-a*c,d=a*l-h*r,f=c*r-o*l,m=e*u+i*d+s*f;if(m===0)return this.set(0,0,0,0,0,0,0,0,0);let x=1/m;return t[0]=u*x,t[1]=(s*c-h*i)*x,t[2]=(a*i-s*o)*x,t[3]=d*x,t[4]=(h*e-s*l)*x,t[5]=(s*r-a*e)*x,t[6]=f*x,t[7]=(i*l-c*e)*x,t[8]=(o*e-i*r)*x,this}transpose(){let t,e=this.elements;return t=e[1],e[1]=e[3],e[3]=t,t=e[2],e[2]=e[6],e[6]=t,t=e[5],e[5]=e[7],e[7]=t,this}getNormalMatrix(t){return this.setFromMatrix4(t).invert().transpose()}transposeIntoArray(t){let e=this.elements;return t[0]=e[0],t[1]=e[3],t[2]=e[6],t[3]=e[1],t[4]=e[4],t[5]=e[7],t[6]=e[2],t[7]=e[5],t[8]=e[8],this}setUvTransform(t,e,i,s,r,o,a){let l=Math.cos(r),c=Math.sin(r);return this.set(i*l,i*c,-i*(l*o+c*a)+o+t,-s*c,s*l,-s*(-c*o+l*a)+a+e,0,0,1),this}scale(t,e){return $n("Matrix3: .scale() is deprecated. Use .makeScale() instead."),this.premultiply(Ic.makeScale(t,e)),this}rotate(t){return $n("Matrix3: .rotate() is deprecated. Use .makeRotation() instead."),this.premultiply(Ic.makeRotation(-t)),this}translate(t,e){return $n("Matrix3: .translate() is deprecated. Use .makeTranslation() instead."),this.premultiply(Ic.makeTranslation(t,e)),this}makeTranslation(t,e){return t.isVector2?this.set(1,0,t.x,0,1,t.y,0,0,1):this.set(1,0,t,0,1,e,0,0,1),this}makeRotation(t){let e=Math.cos(t),i=Math.sin(t);return this.set(e,-i,0,i,e,0,0,0,1),this}makeScale(t,e){return this.set(t,0,0,0,e,0,0,0,1),this}equals(t){let e=this.elements,i=t.elements;for(let s=0;s<9;s++)if(e[s]!==i[s])return!1;return!0}fromArray(t,e=0){for(let i=0;i<9;i++)this.elements[i]=t[i+e];return this}toArray(t=[],e=0){let i=this.elements;return t[e]=i[0],t[e+1]=i[1],t[e+2]=i[2],t[e+3]=i[3],t[e+4]=i[4],t[e+5]=i[5],t[e+6]=i[6],t[e+7]=i[7],t[e+8]=i[8],t}clone(){return new this.constructor().fromArray(this.elements)}},Ic=new zt,Vu=new zt().set(.4123908,.3575843,.1804808,.212639,.7151687,.0721923,.0193308,.1191948,.9505322),Gu=new zt().set(3.2409699,-1.5373832,-.4986108,-.9692436,1.8759675,.0415551,.0556301,-.203977,1.0569715);function gm(){let n={enabled:!0,workingColorSpace:In,spaces:{},convert:function(s,r,o){return this.enabled===!1||r===o||!r||!o||(this.spaces[r].transfer===ne&&(s.r=hn(s.r),s.g=hn(s.g),s.b=hn(s.b)),this.spaces[r].primaries!==this.spaces[o].primaries&&(s.applyMatrix3(this.spaces[r].toXYZ),s.applyMatrix3(this.spaces[o].fromXYZ)),this.spaces[o].transfer===ne&&(s.r=Us(s.r),s.g=Us(s.g),s.b=Us(s.b))),s},workingToColorSpace:function(s,r){return this.convert(s,this.workingColorSpace,r)},colorSpaceToWorking:function(s,r){return this.convert(s,r,this.workingColorSpace)},getPrimaries:function(s){return this.spaces[s].primaries},getTransfer:function(s){return s===ki?Ir:this.spaces[s].transfer},getToneMappingMode:function(s){return this.spaces[s].outputColorSpaceConfig.toneMappingMode||"standard"},getLuminanceCoefficients:function(s,r=this.workingColorSpace){return s.fromArray(this.spaces[r].luminanceCoefficients)},define:function(s){Object.assign(this.spaces,s)},_getMatrix:function(s,r,o){return s.copy(this.spaces[o].fromXYZ).multiply(this.spaces[r].toXYZ)},_getDrawingBufferColorSpace:function(s){return this.spaces[s].outputColorSpaceConfig.drawingBufferColorSpace},_getUnpackColorSpace:function(s=this.workingColorSpace){return this.spaces[s].workingColorSpaceConfig.unpackColorSpace}},t=[.64,.33,.3,.6,.15,.06],e=[.2126,.7152,.0722],i=[.3127,.329];return n.define({[In]:{primaries:t,whitePoint:i,transfer:Ir,toXYZ:Vu,fromXYZ:Gu,luminanceCoefficients:e,workingColorSpaceConfig:{unpackColorSpace:Ie},outputColorSpaceConfig:{drawingBufferColorSpace:Ie}},[Ie]:{primaries:t,whitePoint:i,transfer:ne,toXYZ:Vu,fromXYZ:Gu,luminanceCoefficients:e,outputColorSpaceConfig:{drawingBufferColorSpace:Ie}}}),n}var Zt=gm();function hn(n){return n<.04045?n*.0773993808:Math.pow(n*.9478672986+.0521327014,2.4)}function Us(n){return n<.0031308?n*12.92:1.055*Math.pow(n,.41666)-.055}var vs,wa=class{static getDataURL(t,e="image/png"){if(/^data:/i.test(t.src)||typeof HTMLCanvasElement>"u")return t.src;let i;if(t instanceof HTMLCanvasElement)i=t;else{vs===void 0&&(vs=Dr("canvas")),vs.width=t.width,vs.height=t.height;let s=vs.getContext("2d");t instanceof ImageData?s.putImageData(t,0,0):s.drawImage(t,0,0,t.width,t.height),i=vs}return i.toDataURL(e)}static sRGBToLinear(t){if(typeof HTMLImageElement<"u"&&t instanceof HTMLImageElement||typeof HTMLCanvasElement<"u"&&t instanceof HTMLCanvasElement||typeof ImageBitmap<"u"&&t instanceof ImageBitmap){let e=Dr("canvas");e.width=t.width,e.height=t.height;let i=e.getContext("2d");i.drawImage(t,0,0,t.width,t.height);let s=i.getImageData(0,0,t.width,t.height),r=s.data;for(let o=0;o<r.length;o++)r[o]=hn(r[o]/255)*255;return i.putImageData(s,0,0),e}else if(t.data){let e=t.data.slice(0);for(let i=0;i<e.length;i++)e instanceof Uint8Array||e instanceof Uint8ClampedArray?e[i]=Math.floor(hn(e[i]/255)*255):e[i]=hn(e[i]);return{data:e,width:t.width,height:t.height}}else return Lt("ImageUtils.sRGBToLinear(): Unsupported image type. No color space conversion applied."),t}},xm=0,zs=class{constructor(t=null){this.isTextureSource=!0,Object.defineProperty(this,"id",{value:xm++}),this.uuid=sr(),this.data=t,this.dataReady=!0,this.version=0}getSize(t){let e=this.data;return typeof HTMLVideoElement<"u"&&e instanceof HTMLVideoElement?t.set(e.videoWidth,e.videoHeight,0):typeof VideoFrame<"u"&&e instanceof VideoFrame?t.set(e.displayWidth,e.displayHeight,0):e!==null?t.set(e.width||0,e.height||0,e.depth||0):t.set(0,0,0),t}set needsUpdate(t){t===!0&&this.version++}toJSON(t){let e=t===void 0||typeof t=="string";if(!e&&t.images[this.uuid]!==void 0)return t.images[this.uuid];let i={uuid:this.uuid,url:""},s=this.data;if(s!==null){let r;if(Array.isArray(s)){r=[];for(let o=0,a=s.length;o<a;o++)s[o].isDataTexture?r.push(Dc(s[o].image)):r.push(Dc(s[o]))}else r=Dc(s);i.url=r}return e||(t.images[this.uuid]=i),i}};function Dc(n){return typeof HTMLImageElement<"u"&&n instanceof HTMLImageElement||typeof HTMLCanvasElement<"u"&&n instanceof HTMLCanvasElement||typeof ImageBitmap<"u"&&n instanceof ImageBitmap?wa.getDataURL(n):n.data?{data:Array.from(n.data),width:n.width,height:n.height,type:n.data.constructor.name}:(Lt("Texture: Unable to serialize Texture."),{})}var _m=0,Lc=new C,qe=class n extends Ni{constructor(t=n.DEFAULT_IMAGE,e=n.DEFAULT_MAPPING,i=Wi,s=Wi,r=ce,o=zi,a=ui,l=ei,c=n.DEFAULT_ANISOTROPY,h=ki){super(),this.isTexture=!0,Object.defineProperty(this,"id",{value:_m++}),this.uuid=sr(),this.name="",this.source=new zs(t),this.mipmaps=[],this.mapping=e,this.channel=0,this.wrapS=i,this.wrapT=s,this.magFilter=r,this.minFilter=o,this.anisotropy=c,this.format=a,this.internalFormat=null,this.type=l,this.offset=new et(0,0),this.repeat=new et(1,1),this.center=new et(0,0),this.rotation=0,this.matrixAutoUpdate=!0,this.matrix=new zt,this.generateMipmaps=!0,this.mipmapsAutoUpdate=!0,this.premultiplyAlpha=!1,this.flipY=!0,this.unpackAlignment=4,this.colorSpace=h,this.userData={},this.updateRanges=[],this.version=0,this.onUpdate=null,this.renderTarget=null,this.isRenderTargetTexture=!1,this.isArrayTexture=!!(t&&t.depth&&t.depth>1),this.pmremVersion=0,this.isPMREMTexture=!1,this.normalized=!1}get width(){return this.source.getSize(Lc).x}get height(){return this.source.getSize(Lc).y}get depth(){return this.source.getSize(Lc).z}get image(){return this.source.data}set image(t){this.source.data=t}updateMatrix(){this.matrix.setUvTransform(this.offset.x,this.offset.y,this.repeat.x,this.repeat.y,this.rotation,this.center.x,this.center.y)}addUpdateRange(t,e){this.updateRanges.push({start:t,count:e})}clearUpdateRanges(){this.updateRanges.length=0}clone(){return new this.constructor().copy(this)}copy(t){return this.name=t.name,this.source=t.source,this.mipmaps=t.mipmaps.slice(0),this.mapping=t.mapping,this.channel=t.channel,this.wrapS=t.wrapS,this.wrapT=t.wrapT,this.magFilter=t.magFilter,this.minFilter=t.minFilter,this.anisotropy=t.anisotropy,this.format=t.format,this.internalFormat=t.internalFormat,this.type=t.type,this.normalized=t.normalized,this.offset.copy(t.offset),this.repeat.copy(t.repeat),this.center.copy(t.center),this.rotation=t.rotation,this.matrixAutoUpdate=t.matrixAutoUpdate,this.matrix.copy(t.matrix),this.generateMipmaps=t.generateMipmaps,this.mipmapsAutoUpdate=t.mipmapsAutoUpdate,this.premultiplyAlpha=t.premultiplyAlpha,this.flipY=t.flipY,this.unpackAlignment=t.unpackAlignment,this.colorSpace=t.colorSpace,this.renderTarget=t.renderTarget,this.isRenderTargetTexture=t.isRenderTargetTexture,this.isPMREMTexture=t.isPMREMTexture,this.isArrayTexture=t.isArrayTexture,this.userData=JSON.parse(JSON.stringify(t.userData)),this.needsUpdate=!0,this}setValues(t){for(let e in t){let i=t[e];if(i===void 0){Lt(`Texture.setValues(): parameter '${e}' has value of undefined.`);continue}let s=this[e];if(s===void 0){Lt(`Texture.setValues(): property '${e}' does not exist.`);continue}s&&i&&s.isVector2&&i.isVector2||s&&i&&s.isVector3&&i.isVector3||s&&i&&s.isMatrix3&&i.isMatrix3?s.copy(i):this[e]=i}}toJSON(t){let e=t===void 0||typeof t=="string";if(!e&&t.textures[this.uuid]!==void 0)return t.textures[this.uuid];let i={metadata:{version:4.7,type:"Texture",generator:"Texture.toJSON"},uuid:this.uuid,name:this.name,image:this.source.toJSON(t).uuid,mapping:this.mapping,channel:this.channel,repeat:[this.repeat.x,this.repeat.y],offset:[this.offset.x,this.offset.y],center:[this.center.x,this.center.y],rotation:this.rotation,wrap:[this.wrapS,this.wrapT],format:this.format,internalFormat:this.internalFormat,type:this.type,normalized:this.normalized,colorSpace:this.colorSpace,minFilter:this.minFilter,magFilter:this.magFilter,anisotropy:this.anisotropy,flipY:this.flipY,generateMipmaps:this.generateMipmaps,mipmapsAutoUpdate:this.mipmapsAutoUpdate,premultiplyAlpha:this.premultiplyAlpha,unpackAlignment:this.unpackAlignment};return Object.keys(this.userData).length>0&&(i.userData=this.userData),e||(t.textures[this.uuid]=i),i}dispose(){this.dispatchEvent({type:"dispose"})}transformUv(t){if(this.mapping!==_h)return t;if(t.applyMatrix3(this.matrix),t.x<0||t.x>1)switch(this.wrapS){case un:t.x=t.x-Math.floor(t.x);break;case Wi:t.x=t.x<0?0:1;break;case Sa:Math.abs(Math.floor(t.x)%2)===1?t.x=Math.ceil(t.x)-t.x:t.x=t.x-Math.floor(t.x);break}if(t.y<0||t.y>1)switch(this.wrapT){case un:t.y=t.y-Math.floor(t.y);break;case Wi:t.y=t.y<0?0:1;break;case Sa:Math.abs(Math.floor(t.y)%2)===1?t.y=Math.ceil(t.y)-t.y:t.y=t.y-Math.floor(t.y);break}return this.flipY&&(t.y=1-t.y),t}set needsUpdate(t){t===!0&&(this.version++,this.source.needsUpdate=!0)}set needsPMREMUpdate(t){t===!0&&this.pmremVersion++}};qe.DEFAULT_IMAGE=null;qe.DEFAULT_MAPPING=_h;qe.DEFAULT_ANISOTROPY=1;var oe=class{static{this.prototype.isVector4=!0}constructor(t=0,e=0,i=0,s=1){this.x=t,this.y=e,this.z=i,this.w=s}get width(){return this.z}set width(t){this.z=t}get height(){return this.w}set height(t){this.w=t}set(t,e,i,s){return this.x=t,this.y=e,this.z=i,this.w=s,this}setScalar(t){return this.x=t,this.y=t,this.z=t,this.w=t,this}setX(t){return this.x=t,this}setY(t){return this.y=t,this}setZ(t){return this.z=t,this}setW(t){return this.w=t,this}setComponent(t,e){switch(t){case 0:this.x=e;break;case 1:this.y=e;break;case 2:this.z=e;break;case 3:this.w=e;break;default:throw new Error("THREE.Vector4: index is out of range: "+t)}return this}getComponent(t){switch(t){case 0:return this.x;case 1:return this.y;case 2:return this.z;case 3:return this.w;default:throw new Error("THREE.Vector4: index is out of range: "+t)}}clone(){return new this.constructor(this.x,this.y,this.z,this.w)}copy(t){return this.x=t.x,this.y=t.y,this.z=t.z,this.w=t.w!==void 0?t.w:1,this}add(t){return this.x+=t.x,this.y+=t.y,this.z+=t.z,this.w+=t.w,this}addScalar(t){return this.x+=t,this.y+=t,this.z+=t,this.w+=t,this}addVectors(t,e){return this.x=t.x+e.x,this.y=t.y+e.y,this.z=t.z+e.z,this.w=t.w+e.w,this}addScaledVector(t,e){return this.x+=t.x*e,this.y+=t.y*e,this.z+=t.z*e,this.w+=t.w*e,this}sub(t){return this.x-=t.x,this.y-=t.y,this.z-=t.z,this.w-=t.w,this}subScalar(t){return this.x-=t,this.y-=t,this.z-=t,this.w-=t,this}subVectors(t,e){return this.x=t.x-e.x,this.y=t.y-e.y,this.z=t.z-e.z,this.w=t.w-e.w,this}multiply(t){return this.x*=t.x,this.y*=t.y,this.z*=t.z,this.w*=t.w,this}multiplyScalar(t){return this.x*=t,this.y*=t,this.z*=t,this.w*=t,this}applyMatrix4(t){let e=this.x,i=this.y,s=this.z,r=this.w,o=t.elements;return this.x=o[0]*e+o[4]*i+o[8]*s+o[12]*r,this.y=o[1]*e+o[5]*i+o[9]*s+o[13]*r,this.z=o[2]*e+o[6]*i+o[10]*s+o[14]*r,this.w=o[3]*e+o[7]*i+o[11]*s+o[15]*r,this}divide(t){return this.x/=t.x,this.y/=t.y,this.z/=t.z,this.w/=t.w,this}divideScalar(t){return this.multiplyScalar(1/t)}setAxisAngleFromQuaternion(t){this.w=2*Math.acos(t.w);let e=Math.sqrt(1-t.w*t.w);return e<1e-4?(this.x=1,this.y=0,this.z=0):(this.x=t.x/e,this.y=t.y/e,this.z=t.z/e),this}setAxisAngleFromRotationMatrix(t){let e,i,s,r,l=t.elements,c=l[0],h=l[4],u=l[8],d=l[1],f=l[5],m=l[9],x=l[2],p=l[6],g=l[10];if(Math.abs(h-d)<.01&&Math.abs(u-x)<.01&&Math.abs(m-p)<.01){if(Math.abs(h+d)<.1&&Math.abs(u+x)<.1&&Math.abs(m+p)<.1&&Math.abs(c+f+g-3)<.1)return this.set(1,0,0,0),this;e=Math.PI;let v=(c+1)/2,S=(f+1)/2,T=(g+1)/2,b=(h+d)/4,_=(u+x)/4,M=(m+p)/4;return v>S&&v>T?v<.01?(i=0,s=.707106781,r=.707106781):(i=Math.sqrt(v),s=b/i,r=_/i):S>T?S<.01?(i=.707106781,s=0,r=.707106781):(s=Math.sqrt(S),i=b/s,r=M/s):T<.01?(i=.707106781,s=.707106781,r=0):(r=Math.sqrt(T),i=_/r,s=M/r),this.set(i,s,r,e),this}let y=Math.sqrt((p-m)*(p-m)+(u-x)*(u-x)+(d-h)*(d-h));return Math.abs(y)<.001&&(y=1),this.x=(p-m)/y,this.y=(u-x)/y,this.z=(d-h)/y,this.w=Math.acos((c+f+g-1)/2),this}setFromMatrixPosition(t){let e=t.elements;return this.x=e[12],this.y=e[13],this.z=e[14],this.w=e[15],this}min(t){return this.x=Math.min(this.x,t.x),this.y=Math.min(this.y,t.y),this.z=Math.min(this.z,t.z),this.w=Math.min(this.w,t.w),this}max(t){return this.x=Math.max(this.x,t.x),this.y=Math.max(this.y,t.y),this.z=Math.max(this.z,t.z),this.w=Math.max(this.w,t.w),this}clamp(t,e){return this.x=Vt(this.x,t.x,e.x),this.y=Vt(this.y,t.y,e.y),this.z=Vt(this.z,t.z,e.z),this.w=Vt(this.w,t.w,e.w),this}clampScalar(t,e){return this.x=Vt(this.x,t,e),this.y=Vt(this.y,t,e),this.z=Vt(this.z,t,e),this.w=Vt(this.w,t,e),this}clampLength(t,e){let i=this.length();return this.divideScalar(i||1).multiplyScalar(Vt(i,t,e))}floor(){return this.x=Math.floor(this.x),this.y=Math.floor(this.y),this.z=Math.floor(this.z),this.w=Math.floor(this.w),this}ceil(){return this.x=Math.ceil(this.x),this.y=Math.ceil(this.y),this.z=Math.ceil(this.z),this.w=Math.ceil(this.w),this}round(){return this.x=Math.round(this.x),this.y=Math.round(this.y),this.z=Math.round(this.z),this.w=Math.round(this.w),this}roundToZero(){return this.x=Math.trunc(this.x),this.y=Math.trunc(this.y),this.z=Math.trunc(this.z),this.w=Math.trunc(this.w),this}negate(){return this.x=-this.x,this.y=-this.y,this.z=-this.z,this.w=-this.w,this}dot(t){return this.x*t.x+this.y*t.y+this.z*t.z+this.w*t.w}lengthSq(){return this.x*this.x+this.y*this.y+this.z*this.z+this.w*this.w}length(){return Math.sqrt(this.x*this.x+this.y*this.y+this.z*this.z+this.w*this.w)}manhattanLength(){return Math.abs(this.x)+Math.abs(this.y)+Math.abs(this.z)+Math.abs(this.w)}normalize(){return this.divideScalar(this.length()||1)}setLength(t){return this.normalize().multiplyScalar(t)}lerp(t,e){return this.x+=(t.x-this.x)*e,this.y+=(t.y-this.y)*e,this.z+=(t.z-this.z)*e,this.w+=(t.w-this.w)*e,this}lerpVectors(t,e,i){return this.x=t.x+(e.x-t.x)*i,this.y=t.y+(e.y-t.y)*i,this.z=t.z+(e.z-t.z)*i,this.w=t.w+(e.w-t.w)*i,this}equals(t){return t.x===this.x&&t.y===this.y&&t.z===this.z&&t.w===this.w}fromArray(t,e=0){return this.x=t[e],this.y=t[e+1],this.z=t[e+2],this.w=t[e+3],this}toArray(t=[],e=0){return t[e]=this.x,t[e+1]=this.y,t[e+2]=this.z,t[e+3]=this.w,t}fromBufferAttribute(t,e){return this.x=t.getX(e),this.y=t.getY(e),this.z=t.getZ(e),this.w=t.getW(e),this}random(){return this.x=Math.random(),this.y=Math.random(),this.z=Math.random(),this.w=Math.random(),this}*[Symbol.iterator](){yield this.x,yield this.y,yield this.z,yield this.w}},Ea=class extends Ni{constructor(t=1,e=1,i={}){super(),i=Object.assign({generateMipmaps:!1,internalFormat:null,minFilter:ce,depthBuffer:!0,stencilBuffer:!1,resolveColorBuffer:!0,resolveDepthBuffer:!0,resolveStencilBuffer:!0,storeMultisampledColorBuffer:!0,storeMultisampledDepthBuffer:!0,storeMultisampledStencilBuffer:!0,depthTexture:null,samples:0,count:1,depth:1,multiview:!1,useArrayDepthTexture:!1},i),this.isRenderTarget=!0,this.width=t,this.height=e,this.depth=i.depth,this.scissor=new oe(0,0,t,e),this.scissorTest=!1,this.viewport=new oe(0,0,t,e),this.textures=[];let s=i.count;for(let r=0;r<s;r++)this.textures[r]=new qe({width:t,height:e,depth:i.depth}),this.textures[r].isRenderTargetTexture=!0,this.textures[r].renderTarget=this;this._setTextureOptions(i),this.depthBuffer=i.depthBuffer,this.stencilBuffer=i.stencilBuffer,this.resolveColorBuffer=i.resolveColorBuffer,this.resolveDepthBuffer=i.resolveDepthBuffer,this.resolveStencilBuffer=i.resolveStencilBuffer,this.storeMultisampledColorBuffer=i.storeMultisampledColorBuffer,this.storeMultisampledDepthBuffer=i.storeMultisampledDepthBuffer,this.storeMultisampledStencilBuffer=i.storeMultisampledStencilBuffer,this._depthTexture=null,this.depthTexture=i.depthTexture,this.samples=i.samples,this.multiview=i.multiview,this.useArrayDepthTexture=i.useArrayDepthTexture}_setTextureOptions(t={}){let e={minFilter:ce,generateMipmaps:!1,flipY:!1,internalFormat:null};t.mapping!==void 0&&(e.mapping=t.mapping),t.wrapS!==void 0&&(e.wrapS=t.wrapS),t.wrapT!==void 0&&(e.wrapT=t.wrapT),t.wrapR!==void 0&&(e.wrapR=t.wrapR),t.magFilter!==void 0&&(e.magFilter=t.magFilter),t.minFilter!==void 0&&(e.minFilter=t.minFilter),t.format!==void 0&&(e.format=t.format),t.type!==void 0&&(e.type=t.type),t.anisotropy!==void 0&&(e.anisotropy=t.anisotropy),t.colorSpace!==void 0&&(e.colorSpace=t.colorSpace),t.flipY!==void 0&&(e.flipY=t.flipY),t.generateMipmaps!==void 0&&(e.generateMipmaps=t.generateMipmaps),t.mipmapsAutoUpdate!==void 0&&(e.mipmapsAutoUpdate=t.mipmapsAutoUpdate),t.internalFormat!==void 0&&(e.internalFormat=t.internalFormat);for(let i=0;i<this.textures.length;i++)this.textures[i].setValues(e)}get texture(){return this.textures[0]}set texture(t){this.textures[0]=t}set depthTexture(t){this._depthTexture!==null&&this._depthTexture.renderTarget===this&&(this._depthTexture.renderTarget=null),t!==null&&t.renderTarget===null&&(t.renderTarget=this),this._depthTexture=t}get depthTexture(){return this._depthTexture}setSize(t,e,i=1){if(this.width!==t||this.height!==e||this.depth!==i){this.width=t,this.height=e,this.depth=i;for(let s=0,r=this.textures.length;s<r;s++)this.textures[s].image.width=t,this.textures[s].image.height=e,this.textures[s].image.depth=i,this.textures[s].isData3DTexture!==!0&&(this.textures[s].isArrayTexture=this.textures[s].image.depth>1);this.dispose()}this.viewport.set(0,0,t,e),this.scissor.set(0,0,t,e)}clone(){return new this.constructor().copy(this)}copy(t){this.width=t.width,this.height=t.height,this.depth=t.depth,this.scissor.copy(t.scissor),this.scissorTest=t.scissorTest,this.viewport.copy(t.viewport),this.textures.length=0;for(let e=0,i=t.textures.length;e<i;e++){this.textures[e]=t.textures[e].clone(),this.textures[e].isRenderTargetTexture=!0,this.textures[e].renderTarget=this;let s=Object.assign({},t.textures[e].image);this.textures[e].source=new zs(s)}if(this.depthBuffer=t.depthBuffer,this.stencilBuffer=t.stencilBuffer,this.resolveColorBuffer=t.resolveColorBuffer,this.resolveDepthBuffer=t.resolveDepthBuffer,this.resolveStencilBuffer=t.resolveStencilBuffer,this.storeMultisampledColorBuffer=t.storeMultisampledColorBuffer,this.storeMultisampledDepthBuffer=t.storeMultisampledDepthBuffer,this.storeMultisampledStencilBuffer=t.storeMultisampledStencilBuffer,t.depthTexture!==null)if(t.depthTexture.renderTarget===t){let e=t.depthTexture.clone();e.renderTarget=null,this.depthTexture=e}else this.depthTexture=t.depthTexture;else this.depthTexture=null;return this.samples=t.samples,this.multiview=t.multiview,this.useArrayDepthTexture=t.useArrayDepthTexture,this}dispose(){this.dispatchEvent({type:"dispose"})}},me=class extends Ea{constructor(t=1,e=1,i={}){super(t,e,i),this.isWebGLRenderTarget=!0}},Lr=class extends qe{constructor(t=null,e=1,i=1,s=1){super(null),this.isDataArrayTexture=!0,this.image={data:t,width:e,height:i,depth:s},this.magFilter=De,this.minFilter=De,this.wrapR=Wi,this.generateMipmaps=!1,this.flipY=!1,this.unpackAlignment=1,this.layerUpdates=new Set}copy(t){return super.copy(t),this.wrapR=t.wrapR,this}addLayerUpdate(t){this.layerUpdates.add(t)}clearLayerUpdates(){this.layerUpdates.clear()}};var ks=class extends qe{constructor(t=null,e=1,i=1,s=1){super(null),this.isData3DTexture=!0,this.image={data:t,width:e,height:i,depth:s},this.magFilter=De,this.minFilter=De,this.wrapR=Wi,this.generateMipmaps=!1,this.flipY=!1,this.unpackAlignment=1}copy(t){return super.copy(t),this.wrapR=t.wrapR,this}};var Xt=class n{static{this.prototype.isMatrix4=!0}constructor(t,e,i,s,r,o,a,l,c,h,u,d,f,m,x,p){this.elements=[1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1],t!==void 0&&this.set(t,e,i,s,r,o,a,l,c,h,u,d,f,m,x,p)}set(t,e,i,s,r,o,a,l,c,h,u,d,f,m,x,p){let g=this.elements;return g[0]=t,g[4]=e,g[8]=i,g[12]=s,g[1]=r,g[5]=o,g[9]=a,g[13]=l,g[2]=c,g[6]=h,g[10]=u,g[14]=d,g[3]=f,g[7]=m,g[11]=x,g[15]=p,this}identity(){return this.set(1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1),this}clone(){return new n().fromArray(this.elements)}copy(t){let e=this.elements,i=t.elements;return e[0]=i[0],e[1]=i[1],e[2]=i[2],e[3]=i[3],e[4]=i[4],e[5]=i[5],e[6]=i[6],e[7]=i[7],e[8]=i[8],e[9]=i[9],e[10]=i[10],e[11]=i[11],e[12]=i[12],e[13]=i[13],e[14]=i[14],e[15]=i[15],this}copyPosition(t){let e=this.elements,i=t.elements;return e[12]=i[12],e[13]=i[13],e[14]=i[14],this}setFromMatrix3(t){let e=t.elements;return this.set(e[0],e[3],e[6],0,e[1],e[4],e[7],0,e[2],e[5],e[8],0,0,0,0,1),this}extractBasis(t,e,i){return this.determinantAffine()===0?(t.set(1,0,0),e.set(0,1,0),i.set(0,0,1),this):(t.setFromMatrixColumn(this,0),e.setFromMatrixColumn(this,1),i.setFromMatrixColumn(this,2),this)}makeBasis(t,e,i){return this.set(t.x,e.x,i.x,0,t.y,e.y,i.y,0,t.z,e.z,i.z,0,0,0,0,1),this}extractRotation(t){if(t.determinantAffine()===0)return this.identity();let e=this.elements,i=t.elements,s=1/Ms.setFromMatrixColumn(t,0).length(),r=1/Ms.setFromMatrixColumn(t,1).length(),o=1/Ms.setFromMatrixColumn(t,2).length();return e[0]=i[0]*s,e[1]=i[1]*s,e[2]=i[2]*s,e[3]=0,e[4]=i[4]*r,e[5]=i[5]*r,e[6]=i[6]*r,e[7]=0,e[8]=i[8]*o,e[9]=i[9]*o,e[10]=i[10]*o,e[11]=0,e[12]=0,e[13]=0,e[14]=0,e[15]=1,this}makeRotationFromEuler(t){let e=this.elements,i=t.x,s=t.y,r=t.z,o=Math.cos(i),a=Math.sin(i),l=Math.cos(s),c=Math.sin(s),h=Math.cos(r),u=Math.sin(r);if(t.order==="XYZ"){let d=o*h,f=o*u,m=a*h,x=a*u;e[0]=l*h,e[4]=-l*u,e[8]=c,e[1]=f+m*c,e[5]=d-x*c,e[9]=-a*l,e[2]=x-d*c,e[6]=m+f*c,e[10]=o*l}else if(t.order==="YXZ"){let d=l*h,f=l*u,m=c*h,x=c*u;e[0]=d+x*a,e[4]=m*a-f,e[8]=o*c,e[1]=o*u,e[5]=o*h,e[9]=-a,e[2]=f*a-m,e[6]=x+d*a,e[10]=o*l}else if(t.order==="ZXY"){let d=l*h,f=l*u,m=c*h,x=c*u;e[0]=d-x*a,e[4]=-o*u,e[8]=m+f*a,e[1]=f+m*a,e[5]=o*h,e[9]=x-d*a,e[2]=-o*c,e[6]=a,e[10]=o*l}else if(t.order==="ZYX"){let d=o*h,f=o*u,m=a*h,x=a*u;e[0]=l*h,e[4]=m*c-f,e[8]=d*c+x,e[1]=l*u,e[5]=x*c+d,e[9]=f*c-m,e[2]=-c,e[6]=a*l,e[10]=o*l}else if(t.order==="YZX"){let d=o*l,f=o*c,m=a*l,x=a*c;e[0]=l*h,e[4]=x-d*u,e[8]=m*u+f,e[1]=u,e[5]=o*h,e[9]=-a*h,e[2]=-c*h,e[6]=f*u+m,e[10]=d-x*u}else if(t.order==="XZY"){let d=o*l,f=o*c,m=a*l,x=a*c;e[0]=l*h,e[4]=-u,e[8]=c*h,e[1]=d*u+x,e[5]=o*h,e[9]=f*u-m,e[2]=m*u-f,e[6]=a*h,e[10]=x*u+d}return e[3]=0,e[7]=0,e[11]=0,e[12]=0,e[13]=0,e[14]=0,e[15]=1,this}makeRotationFromQuaternion(t){return this.compose(ym,t,vm)}lookAt(t,e,i){let s=this.elements;return _i.subVectors(t,e),_i.lengthSq()===0&&(_i.z=1),_i.normalize(),Tn.crossVectors(i,_i),Tn.lengthSq()===0&&(Math.abs(i.z)===1?_i.x+=1e-4:_i.z+=1e-4,_i.normalize(),Tn.crossVectors(i,_i)),Tn.normalize(),zo.crossVectors(_i,Tn),s[0]=Tn.x,s[4]=zo.x,s[8]=_i.x,s[1]=Tn.y,s[5]=zo.y,s[9]=_i.y,s[2]=Tn.z,s[6]=zo.z,s[10]=_i.z,this}multiply(t){return this.multiplyMatrices(this,t)}premultiply(t){return this.multiplyMatrices(t,this)}multiplyMatrices(t,e){let i=t.elements,s=e.elements,r=this.elements,o=i[0],a=i[4],l=i[8],c=i[12],h=i[1],u=i[5],d=i[9],f=i[13],m=i[2],x=i[6],p=i[10],g=i[14],y=i[3],v=i[7],S=i[11],T=i[15],b=s[0],_=s[4],M=s[8],D=s[12],P=s[1],A=s[5],R=s[9],U=s[13],L=s[2],B=s[6],H=s[10],z=s[14],Q=s[3],W=s[7],Z=s[11],$=s[15];return r[0]=o*b+a*P+l*L+c*Q,r[4]=o*_+a*A+l*B+c*W,r[8]=o*M+a*R+l*H+c*Z,r[12]=o*D+a*U+l*z+c*$,r[1]=h*b+u*P+d*L+f*Q,r[5]=h*_+u*A+d*B+f*W,r[9]=h*M+u*R+d*H+f*Z,r[13]=h*D+u*U+d*z+f*$,r[2]=m*b+x*P+p*L+g*Q,r[6]=m*_+x*A+p*B+g*W,r[10]=m*M+x*R+p*H+g*Z,r[14]=m*D+x*U+p*z+g*$,r[3]=y*b+v*P+S*L+T*Q,r[7]=y*_+v*A+S*B+T*W,r[11]=y*M+v*R+S*H+T*Z,r[15]=y*D+v*U+S*z+T*$,this}multiplyScalar(t){let e=this.elements;return e[0]*=t,e[4]*=t,e[8]*=t,e[12]*=t,e[1]*=t,e[5]*=t,e[9]*=t,e[13]*=t,e[2]*=t,e[6]*=t,e[10]*=t,e[14]*=t,e[3]*=t,e[7]*=t,e[11]*=t,e[15]*=t,this}determinant(){let t=this.elements,e=t[0],i=t[4],s=t[8],r=t[12],o=t[1],a=t[5],l=t[9],c=t[13],h=t[2],u=t[6],d=t[10],f=t[14],m=t[3],x=t[7],p=t[11],g=t[15],y=l*f-c*d,v=a*f-c*u,S=a*d-l*u,T=o*f-c*h,b=o*d-l*h,_=o*u-a*h;return e*(x*y-p*v+g*S)-i*(m*y-p*T+g*b)+s*(m*v-x*T+g*_)-r*(m*S-x*b+p*_)}determinantAffine(){let t=this.elements,e=t[0],i=t[4],s=t[8],r=t[1],o=t[5],a=t[9],l=t[2],c=t[6],h=t[10];return e*(o*h-a*c)-i*(r*h-a*l)+s*(r*c-o*l)}transpose(){let t=this.elements,e;return e=t[1],t[1]=t[4],t[4]=e,e=t[2],t[2]=t[8],t[8]=e,e=t[6],t[6]=t[9],t[9]=e,e=t[3],t[3]=t[12],t[12]=e,e=t[7],t[7]=t[13],t[13]=e,e=t[11],t[11]=t[14],t[14]=e,this}setPosition(t,e,i){let s=this.elements;return t.isVector3?(s[12]=t.x,s[13]=t.y,s[14]=t.z):(s[12]=t,s[13]=e,s[14]=i),this}invert(){let t=this.elements,e=t[0],i=t[1],s=t[2],r=t[3],o=t[4],a=t[5],l=t[6],c=t[7],h=t[8],u=t[9],d=t[10],f=t[11],m=t[12],x=t[13],p=t[14],g=t[15],y=e*a-i*o,v=e*l-s*o,S=e*c-r*o,T=i*l-s*a,b=i*c-r*a,_=s*c-r*l,M=h*x-u*m,D=h*p-d*m,P=h*g-f*m,A=u*p-d*x,R=u*g-f*x,U=d*g-f*p,L=y*U-v*R+S*A+T*P-b*D+_*M;if(L===0)return this.set(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0);let B=1/L;return t[0]=(a*U-l*R+c*A)*B,t[1]=(s*R-i*U-r*A)*B,t[2]=(x*_-p*b+g*T)*B,t[3]=(d*b-u*_-f*T)*B,t[4]=(l*P-o*U-c*D)*B,t[5]=(e*U-s*P+r*D)*B,t[6]=(p*S-m*_-g*v)*B,t[7]=(h*_-d*S+f*v)*B,t[8]=(o*R-a*P+c*M)*B,t[9]=(i*P-e*R-r*M)*B,t[10]=(m*b-x*S+g*y)*B,t[11]=(u*S-h*b-f*y)*B,t[12]=(a*D-o*A-l*M)*B,t[13]=(e*A-i*D+s*M)*B,t[14]=(x*v-m*T-p*y)*B,t[15]=(h*T-u*v+d*y)*B,this}scale(t){let e=this.elements,i=t.x,s=t.y,r=t.z;return e[0]*=i,e[4]*=s,e[8]*=r,e[1]*=i,e[5]*=s,e[9]*=r,e[2]*=i,e[6]*=s,e[10]*=r,e[3]*=i,e[7]*=s,e[11]*=r,this}getMaxScaleOnAxis(){let t=this.elements,e=t[0]*t[0]+t[1]*t[1]+t[2]*t[2],i=t[4]*t[4]+t[5]*t[5]+t[6]*t[6],s=t[8]*t[8]+t[9]*t[9]+t[10]*t[10];return Math.sqrt(Math.max(e,i,s))}makeTranslation(t,e,i){return t.isVector3?this.set(1,0,0,t.x,0,1,0,t.y,0,0,1,t.z,0,0,0,1):this.set(1,0,0,t,0,1,0,e,0,0,1,i,0,0,0,1),this}makeRotationX(t){let e=Math.cos(t),i=Math.sin(t);return this.set(1,0,0,0,0,e,-i,0,0,i,e,0,0,0,0,1),this}makeRotationY(t){let e=Math.cos(t),i=Math.sin(t);return this.set(e,0,i,0,0,1,0,0,-i,0,e,0,0,0,0,1),this}makeRotationZ(t){let e=Math.cos(t),i=Math.sin(t);return this.set(e,-i,0,0,i,e,0,0,0,0,1,0,0,0,0,1),this}makeRotationAxis(t,e){let i=Math.cos(e),s=Math.sin(e),r=1-i,o=t.x,a=t.y,l=t.z,c=r*o,h=r*a;return this.set(c*o+i,c*a-s*l,c*l+s*a,0,c*a+s*l,h*a+i,h*l-s*o,0,c*l-s*a,h*l+s*o,r*l*l+i,0,0,0,0,1),this}makeScale(t,e,i){return this.set(t,0,0,0,0,e,0,0,0,0,i,0,0,0,0,1),this}makeShear(t,e,i,s,r,o){return this.set(1,i,r,0,t,1,o,0,e,s,1,0,0,0,0,1),this}compose(t,e,i){let s=this.elements,r=e._x,o=e._y,a=e._z,l=e._w,c=r+r,h=o+o,u=a+a,d=r*c,f=r*h,m=r*u,x=o*h,p=o*u,g=a*u,y=l*c,v=l*h,S=l*u,T=i.x,b=i.y,_=i.z;return s[0]=(1-(x+g))*T,s[1]=(f+S)*T,s[2]=(m-v)*T,s[3]=0,s[4]=(f-S)*b,s[5]=(1-(d+g))*b,s[6]=(p+y)*b,s[7]=0,s[8]=(m+v)*_,s[9]=(p-y)*_,s[10]=(1-(d+x))*_,s[11]=0,s[12]=t.x,s[13]=t.y,s[14]=t.z,s[15]=1,this}decompose(t,e,i){let s=this.elements;t.x=s[12],t.y=s[13],t.z=s[14];let r=this.determinantAffine();if(r===0)return i.set(1,1,1),e.identity(),this;let o=Ms.set(s[0],s[1],s[2]).length(),a=Ms.set(s[4],s[5],s[6]).length(),l=Ms.set(s[8],s[9],s[10]).length();r<0&&(o=-o),Pi.copy(this);let c=1/o,h=1/a,u=1/l;return Pi.elements[0]*=c,Pi.elements[1]*=c,Pi.elements[2]*=c,Pi.elements[4]*=h,Pi.elements[5]*=h,Pi.elements[6]*=h,Pi.elements[8]*=u,Pi.elements[9]*=u,Pi.elements[10]*=u,e.setFromRotationMatrix(Pi),i.x=o,i.y=a,i.z=l,this}makePerspective(t,e,i,s,r,o,a=Ti,l=!1){let c=this.elements,h=2*r/(e-t),u=2*r/(i-s),d=(e+t)/(e-t),f=(i+s)/(i-s),m,x;if(l)m=r/(o-r),x=o*r/(o-r);else if(a===Ti)m=-(o+r)/(o-r),x=-2*o*r/(o-r);else if(a===Os)m=-o/(o-r),x=-o*r/(o-r);else throw new Error("THREE.Matrix4.makePerspective(): Invalid coordinate system: "+a);return c[0]=h,c[4]=0,c[8]=d,c[12]=0,c[1]=0,c[5]=u,c[9]=f,c[13]=0,c[2]=0,c[6]=0,c[10]=m,c[14]=x,c[3]=0,c[7]=0,c[11]=-1,c[15]=0,this}makeOrthographic(t,e,i,s,r,o,a=Ti,l=!1){let c=this.elements,h=2/(e-t),u=2/(i-s),d=-(e+t)/(e-t),f=-(i+s)/(i-s),m,x;if(l)m=1/(o-r),x=o/(o-r);else if(a===Ti)m=-2/(o-r),x=-(o+r)/(o-r);else if(a===Os)m=-1/(o-r),x=-r/(o-r);else throw new Error("THREE.Matrix4.makeOrthographic(): Invalid coordinate system: "+a);return c[0]=h,c[4]=0,c[8]=0,c[12]=d,c[1]=0,c[5]=u,c[9]=0,c[13]=f,c[2]=0,c[6]=0,c[10]=m,c[14]=x,c[3]=0,c[7]=0,c[11]=0,c[15]=1,this}equals(t){let e=this.elements,i=t.elements;for(let s=0;s<16;s++)if(e[s]!==i[s])return!1;return!0}fromArray(t,e=0){for(let i=0;i<16;i++)this.elements[i]=t[i+e];return this}toArray(t=[],e=0){let i=this.elements;return t[e]=i[0],t[e+1]=i[1],t[e+2]=i[2],t[e+3]=i[3],t[e+4]=i[4],t[e+5]=i[5],t[e+6]=i[6],t[e+7]=i[7],t[e+8]=i[8],t[e+9]=i[9],t[e+10]=i[10],t[e+11]=i[11],t[e+12]=i[12],t[e+13]=i[13],t[e+14]=i[14],t[e+15]=i[15],t}},Ms=new C,Pi=new Xt,ym=new C(0,0,0),vm=new C(1,1,1),Tn=new C,zo=new C,_i=new C,Wu=new Xt,Xu=new ti,Ai=class n{constructor(t=0,e=0,i=0,s=n.DEFAULT_ORDER){this.isEuler=!0,this._x=t,this._y=e,this._z=i,this._order=s}get x(){return this._x}set x(t){this._x=t,this._onChangeCallback()}get y(){return this._y}set y(t){this._y=t,this._onChangeCallback()}get z(){return this._z}set z(t){this._z=t,this._onChangeCallback()}get order(){return this._order}set order(t){this._order=t,this._onChangeCallback()}set(t,e,i,s=this._order){return this._x=t,this._y=e,this._z=i,this._order=s,this._onChangeCallback(),this}clone(){return new this.constructor(this._x,this._y,this._z,this._order)}copy(t){return this._x=t._x,this._y=t._y,this._z=t._z,this._order=t._order,this._onChangeCallback(),this}setFromRotationMatrix(t,e=this._order,i=!0){let s=t.elements,r=s[0],o=s[4],a=s[8],l=s[1],c=s[5],h=s[9],u=s[2],d=s[6],f=s[10];switch(e){case"XYZ":this._y=Math.asin(Vt(a,-1,1)),Math.abs(a)<.9999999?(this._x=Math.atan2(-h,f),this._z=Math.atan2(-o,r)):(this._x=Math.atan2(d,c),this._z=0);break;case"YXZ":this._x=Math.asin(-Vt(h,-1,1)),Math.abs(h)<.9999999?(this._y=Math.atan2(a,f),this._z=Math.atan2(l,c)):(this._y=Math.atan2(-u,r),this._z=0);break;case"ZXY":this._x=Math.asin(Vt(d,-1,1)),Math.abs(d)<.9999999?(this._y=Math.atan2(-u,f),this._z=Math.atan2(-o,c)):(this._y=0,this._z=Math.atan2(l,r));break;case"ZYX":this._y=Math.asin(-Vt(u,-1,1)),Math.abs(u)<.9999999?(this._x=Math.atan2(d,f),this._z=Math.atan2(l,r)):(this._x=0,this._z=Math.atan2(-o,c));break;case"YZX":this._z=Math.asin(Vt(l,-1,1)),Math.abs(l)<.9999999?(this._x=Math.atan2(-h,c),this._y=Math.atan2(-u,r)):(this._x=0,this._y=Math.atan2(a,f));break;case"XZY":this._z=Math.asin(-Vt(o,-1,1)),Math.abs(o)<.9999999?(this._x=Math.atan2(d,c),this._y=Math.atan2(a,r)):(this._x=Math.atan2(-h,f),this._y=0);break;default:Lt("Euler: .setFromRotationMatrix() encountered an unknown order: "+e)}return this._order=e,i===!0&&this._onChangeCallback(),this}setFromQuaternion(t,e,i){return Wu.makeRotationFromQuaternion(t),this.setFromRotationMatrix(Wu,e,i)}setFromVector3(t,e=this._order){return this.set(t.x,t.y,t.z,e)}reorder(t){return Xu.setFromEuler(this),this.setFromQuaternion(Xu,t)}equals(t){return t._x===this._x&&t._y===this._y&&t._z===this._z&&t._order===this._order}fromArray(t){return this._x=t[0],this._y=t[1],this._z=t[2],t[3]!==void 0&&(this._order=t[3]),this._onChangeCallback(),this}toArray(t=[],e=0){return t[e]=this._x,t[e+1]=this._y,t[e+2]=this._z,t[e+3]=this._order,t}_onChange(t){return this._onChangeCallback=t,this}_onChangeCallback(){}*[Symbol.iterator](){yield this._x,yield this._y,yield this._z,yield this._order}};Ai.DEFAULT_ORDER="XYZ";var Hs=class{constructor(){this.mask=1}set(t){this.mask=(1<<t|0)>>>0}enable(t){this.mask|=1<<t|0}enableAll(){this.mask=-1}toggle(t){this.mask^=1<<t|0}disable(t){this.mask&=~(1<<t|0)}disableAll(){this.mask=0}test(t){return(this.mask&t.mask)!==0}isEnabled(t){return(this.mask&(1<<t|0))!==0}},Mm=0,qu=new C,Ss=new ti,nn=new Xt,ko=new C,vr=new C,Sm=new C,bm=new ti,Yu=new C(1,0,0),Zu=new C(0,1,0),Ku=new C(0,0,1),Ju={type:"added"},wm={type:"removed"},bs={type:"childadded",child:null},Nc={type:"childremoved",child:null},Me=class n extends Ni{constructor(){super(),this.isObject3D=!0,Object.defineProperty(this,"id",{value:Mm++}),this.uuid=sr(),this.name="",this.type="Object3D",this.parent=null,this.children=[],this.up=n.DEFAULT_UP.clone();let t=new C,e=new Ai,i=new ti,s=new C(1,1,1);function r(){i.setFromEuler(e,!1)}function o(){e.setFromQuaternion(i,void 0,!1)}e._onChange(r),i._onChange(o),Object.defineProperties(this,{position:{configurable:!0,enumerable:!0,value:t},rotation:{configurable:!0,enumerable:!0,value:e},quaternion:{configurable:!0,enumerable:!0,value:i},scale:{configurable:!0,enumerable:!0,value:s},modelViewMatrix:{value:new Xt},normalMatrix:{value:new zt}}),this.matrix=new Xt,this.matrixWorld=new Xt,this.matrixAutoUpdate=n.DEFAULT_MATRIX_AUTO_UPDATE,this.matrixWorldAutoUpdate=n.DEFAULT_MATRIX_WORLD_AUTO_UPDATE,this.matrixWorldNeedsUpdate=!1,this.layers=new Hs,this.visible=!0,this.castShadow=!1,this.receiveShadow=!1,this.frustumCulled=!0,this.renderOrder=0,this.animations=[],this.customDepthMaterial=void 0,this.customDistanceMaterial=void 0,this.static=!1,this.userData={},this.pivot=null}onBeforeShadow(){}onAfterShadow(){}onBeforeRender(){}onAfterRender(){}applyMatrix4(t){this.matrixAutoUpdate&&this.updateMatrix(),this.matrix.premultiply(t),this.matrix.decompose(this.position,this.quaternion,this.scale),this.pivot!==null&&this.position.copy(this.pivot).applyMatrix4(this.matrix).sub(this.pivot)}applyQuaternion(t){return this.quaternion.premultiply(t),this}setRotationFromAxisAngle(t,e){this.quaternion.setFromAxisAngle(t,e)}setRotationFromEuler(t){this.quaternion.setFromEuler(t,!0)}setRotationFromMatrix(t){this.quaternion.setFromRotationMatrix(t)}setRotationFromQuaternion(t){this.quaternion.copy(t)}rotateOnAxis(t,e){return Ss.setFromAxisAngle(t,e),this.quaternion.multiply(Ss),this}rotateOnWorldAxis(t,e){return Ss.setFromAxisAngle(t,e),this.quaternion.premultiply(Ss),this}rotateX(t){return this.rotateOnAxis(Yu,t)}rotateY(t){return this.rotateOnAxis(Zu,t)}rotateZ(t){return this.rotateOnAxis(Ku,t)}translateOnAxis(t,e){return qu.copy(t).applyQuaternion(this.quaternion),this.position.add(qu.multiplyScalar(e)),this}translateX(t){return this.translateOnAxis(Yu,t)}translateY(t){return this.translateOnAxis(Zu,t)}translateZ(t){return this.translateOnAxis(Ku,t)}localToWorld(t){return this.updateWorldMatrix(!0,!1),t.applyMatrix4(this.matrixWorld)}worldToLocal(t){return this.updateWorldMatrix(!0,!1),t.applyMatrix4(nn.copy(this.matrixWorld).invert())}lookAt(t,e,i){t.isVector3?ko.copy(t):ko.set(t,e,i);let s=this.parent;this.updateWorldMatrix(!0,!1),vr.setFromMatrixPosition(this.matrixWorld),this.isCamera||this.isLight?nn.lookAt(vr,ko,this.up):nn.lookAt(ko,vr,this.up),this.quaternion.setFromRotationMatrix(nn),s&&(nn.extractRotation(s.matrixWorld),Ss.setFromRotationMatrix(nn),this.quaternion.premultiply(Ss.invert()))}add(t){if(arguments.length>1){for(let e=0;e<arguments.length;e++)this.add(arguments[e]);return this}return t===this?(Ot("Object3D.add: object can't be added as a child of itself.",t),this):(t&&t.isObject3D?(t.removeFromParent(),t.parent=this,this.children.push(t),t.dispatchEvent(Ju),bs.child=t,this.dispatchEvent(bs),bs.child=null):Ot("Object3D.add: object not an instance of THREE.Object3D.",t),this)}remove(t){if(arguments.length>1){for(let i=0;i<arguments.length;i++)this.remove(arguments[i]);return this}let e=this.children.indexOf(t);return e!==-1&&(t.parent=null,this.children.splice(e,1),t.dispatchEvent(wm),Nc.child=t,this.dispatchEvent(Nc),Nc.child=null),this}removeFromParent(){let t=this.parent;return t!==null&&t.remove(this),this}clear(){return this.remove(...this.children)}attach(t){return this.updateWorldMatrix(!0,!1),nn.copy(this.matrixWorld).invert(),t.parent!==null&&(t.parent.updateWorldMatrix(!0,!1),nn.multiply(t.parent.matrixWorld)),t.applyMatrix4(nn),t.removeFromParent(),t.parent=this,this.children.push(t),t.updateWorldMatrix(!1,!0),t.dispatchEvent(Ju),bs.child=t,this.dispatchEvent(bs),bs.child=null,this}getObjectById(t){return this.getObjectByProperty("id",t)}getObjectByName(t){return this.getObjectByProperty("name",t)}getObjectByProperty(t,e){if(this[t]===e)return this;for(let i=0,s=this.children.length;i<s;i++){let o=this.children[i].getObjectByProperty(t,e);if(o!==void 0)return o}}getObjectsByProperty(t,e,i=[]){this[t]===e&&i.push(this);let s=this.children;for(let r=0,o=s.length;r<o;r++)s[r].getObjectsByProperty(t,e,i);return i}getWorldPosition(t){return this.updateWorldMatrix(!0,!1),t.setFromMatrixPosition(this.matrixWorld)}getWorldQuaternion(t){return this.updateWorldMatrix(!0,!1),this.matrixWorld.decompose(vr,t,Sm),t}getWorldScale(t){return this.updateWorldMatrix(!0,!1),this.matrixWorld.decompose(vr,bm,t),t}getWorldDirection(t){this.updateWorldMatrix(!0,!1);let e=this.matrixWorld.elements;return t.set(e[8],e[9],e[10]).normalize()}raycast(){}intersectsFrustum(){return!0}traverse(t){t(this);let e=this.children;for(let i=0,s=e.length;i<s;i++)e[i].traverse(t)}traverseVisible(t){if(this.visible===!1)return;t(this);let e=this.children;for(let i=0,s=e.length;i<s;i++)e[i].traverseVisible(t)}traverseAncestors(t){let e=this.parent;e!==null&&(t(e),e.traverseAncestors(t))}updateMatrix(){this.matrix.compose(this.position,this.quaternion,this.scale);let t=this.pivot;if(t!==null){let e=t.x,i=t.y,s=t.z,r=this.matrix.elements;r[12]+=e-r[0]*e-r[4]*i-r[8]*s,r[13]+=i-r[1]*e-r[5]*i-r[9]*s,r[14]+=s-r[2]*e-r[6]*i-r[10]*s}this.matrixWorldNeedsUpdate=!0}updateMatrixWorld(t){this.matrixAutoUpdate&&this.updateMatrix(),(this.matrixWorldNeedsUpdate||t)&&(this.matrixWorldAutoUpdate===!0&&(this.parent===null?this.matrixWorld.copy(this.matrix):this.matrixWorld.multiplyMatrices(this.parent.matrixWorld,this.matrix)),this.matrixWorldNeedsUpdate=!1,t=!0);let e=this.children;for(let i=0,s=e.length;i<s;i++)e[i].updateMatrixWorld(t)}updateWorldMatrix(t,e,i=!1){let s=this.parent;if(t===!0&&s!==null&&s.updateWorldMatrix(!0,!1),this.matrixAutoUpdate&&this.updateMatrix(),(this.matrixWorldNeedsUpdate||i)&&(this.matrixWorldAutoUpdate===!0&&(this.parent===null?this.matrixWorld.copy(this.matrix):this.matrixWorld.multiplyMatrices(this.parent.matrixWorld,this.matrix)),this.matrixWorldNeedsUpdate=!1,i=!0),e===!0){let r=this.children;for(let o=0,a=r.length;o<a;o++)r[o].updateWorldMatrix(!1,!0,i)}}toJSON(t){let e=t===void 0||typeof t=="string",i={};e&&(t={geometries:{},materials:{},textures:{},images:{},shapes:{},skeletons:{},animations:{},nodes:{}},i.metadata={version:4.7,type:"Object",generator:"Object3D.toJSON"});let s={};s.uuid=this.uuid,s.type=this.type,s.name=this.name,s.castShadow=this.castShadow,s.receiveShadow=this.receiveShadow,s.visible=this.visible,s.frustumCulled=this.frustumCulled,s.renderOrder=this.renderOrder,s.static=this.static,s.matrixAutoUpdate=this.matrixAutoUpdate,Object.keys(this.userData).length>0&&(s.userData=this.userData),s.layers=this.layers.mask,s.matrix=this.matrix.toArray(),s.up=this.up.toArray(),this.pivot!==null&&(s.pivot=this.pivot.toArray()),this.morphTargetDictionary!==void 0&&(s.morphTargetDictionary=Object.assign({},this.morphTargetDictionary)),this.morphTargetInfluences!==void 0&&(s.morphTargetInfluences=this.morphTargetInfluences.slice()),this.isInstancedMesh&&(s.type="InstancedMesh",s.count=this.count,s.instanceMatrix=this.instanceMatrix.toJSON(),this.instanceColor!==null&&(s.instanceColor=this.instanceColor.toJSON())),this.isBatchedMesh&&(s.type="BatchedMesh",s.perObjectFrustumCulled=this.perObjectFrustumCulled,s.sortObjects=this.sortObjects,s.drawRanges=this._drawRanges,s.reservedRanges=this._reservedRanges,s.geometryInfo=this._geometryInfo.map(a=>({...a,boundingBox:a.boundingBox?a.boundingBox.toJSON():void 0,boundingSphere:a.boundingSphere?a.boundingSphere.toJSON():void 0})),s.instanceInfo=this._instanceInfo.map(a=>({...a})),s.availableInstanceIds=this._availableInstanceIds.slice(),s.availableGeometryIds=this._availableGeometryIds.slice(),s.nextIndexStart=this._nextIndexStart,s.nextVertexStart=this._nextVertexStart,s.geometryCount=this._geometryCount,s.maxInstanceCount=this._maxInstanceCount,s.maxVertexCount=this._maxVertexCount,s.maxIndexCount=this._maxIndexCount,s.geometryInitialized=this._geometryInitialized,s.matricesTexture=this._matricesTexture.toJSON(t),s.indirectTexture=this._indirectTexture.toJSON(t),this._colorsTexture!==null&&(s.colorsTexture=this._colorsTexture.toJSON(t)),this.boundingSphere!==null&&(s.boundingSphere=this.boundingSphere.toJSON()),this.boundingBox!==null&&(s.boundingBox=this.boundingBox.toJSON()));function r(a,l){return a[l.uuid]===void 0&&(a[l.uuid]=l.toJSON(t)),l.uuid}if(this.isScene)this.background&&(this.background.isColor?s.background=this.background.toJSON():this.background.isTexture&&(s.background=this.background.toJSON(t).uuid)),this.environment&&this.environment.isTexture&&this.environment.isRenderTargetTexture!==!0&&(s.environment=this.environment.toJSON(t).uuid);else if(this.isMesh||this.isLine||this.isPoints){s.geometry=r(t.geometries,this.geometry);let a=this.geometry.parameters;if(a!==void 0&&a.shapes!==void 0){let l=a.shapes;if(Array.isArray(l))for(let c=0,h=l.length;c<h;c++){let u=l[c];r(t.shapes,u)}else r(t.shapes,l)}}if(this.isSkinnedMesh&&(s.bindMode=this.bindMode,s.bindMatrix=this.bindMatrix.toArray(),this.skeleton!==void 0&&(r(t.skeletons,this.skeleton),s.skeleton=this.skeleton.uuid)),this.material!==void 0)if(Array.isArray(this.material)){let a=[];for(let l=0,c=this.material.length;l<c;l++)a.push(r(t.materials,this.material[l]));s.material=a}else s.material=r(t.materials,this.material);if(this.children.length>0){s.children=[];for(let a=0;a<this.children.length;a++)s.children.push(this.children[a].toJSON(t).object)}if(this.animations.length>0){s.animations=[];for(let a=0;a<this.animations.length;a++){let l=this.animations[a];s.animations.push(r(t.animations,l))}}if(e){let a=o(t.geometries),l=o(t.materials),c=o(t.textures),h=o(t.images),u=o(t.shapes),d=o(t.skeletons),f=o(t.animations),m=o(t.nodes);a.length>0&&(i.geometries=a),l.length>0&&(i.materials=l),c.length>0&&(i.textures=c),h.length>0&&(i.images=h),u.length>0&&(i.shapes=u),d.length>0&&(i.skeletons=d),f.length>0&&(i.animations=f),m.length>0&&(i.nodes=m)}return i.object=s,i;function o(a){let l=[];for(let c in a){let h=a[c];delete h.metadata,l.push(h)}return l}}clone(t){return new this.constructor().copy(this,t)}copy(t,e=!0){if(this.name=t.name,this.up.copy(t.up),this.position.copy(t.position),this.rotation.order=t.rotation.order,this.quaternion.copy(t.quaternion),this.scale.copy(t.scale),this.pivot=t.pivot!==null?t.pivot.clone():null,this.matrix.copy(t.matrix),this.matrixWorld.copy(t.matrixWorld),this.matrixAutoUpdate=t.matrixAutoUpdate,this.matrixWorldAutoUpdate=t.matrixWorldAutoUpdate,this.matrixWorldNeedsUpdate=t.matrixWorldNeedsUpdate,this.layers.mask=t.layers.mask,this.visible=t.visible,this.castShadow=t.castShadow,this.receiveShadow=t.receiveShadow,this.frustumCulled=t.frustumCulled,this.renderOrder=t.renderOrder,this.static=t.static,this.animations=t.animations.slice(),this.userData=JSON.parse(JSON.stringify(t.userData)),e===!0)for(let i=0;i<t.children.length;i++){let s=t.children[i];this.add(s.clone())}return this}dispose(){this.dispatchEvent({type:"dispose"})}};Me.DEFAULT_UP=new C(0,1,0);Me.DEFAULT_MATRIX_AUTO_UPDATE=!0;Me.DEFAULT_MATRIX_WORLD_AUTO_UPDATE=!0;var ve=class extends Me{constructor(){super(),this.isGroup=!0,this.type="Group"}},Em={type:"move"},Vs=class{constructor(){this._targetRay=null,this._grip=null,this._hand=null}getHandSpace(){return this._hand===null&&(this._hand=new ve,this._hand.matrixAutoUpdate=!1,this._hand.visible=!1,this._hand.joints={},this._hand.inputState={pinching:!1}),this._hand}getTargetRaySpace(){return this._targetRay===null&&(this._targetRay=new ve,this._targetRay.matrixAutoUpdate=!1,this._targetRay.visible=!1,this._targetRay.hasLinearVelocity=!1,this._targetRay.linearVelocity=new C,this._targetRay.hasAngularVelocity=!1,this._targetRay.angularVelocity=new C),this._targetRay}getGripSpace(){return this._grip===null&&(this._grip=new ve,this._grip.matrixAutoUpdate=!1,this._grip.visible=!1,this._grip.hasLinearVelocity=!1,this._grip.linearVelocity=new C,this._grip.hasAngularVelocity=!1,this._grip.angularVelocity=new C,this._grip.eventsEnabled=!1),this._grip}dispatchEvent(t){return this._targetRay!==null&&this._targetRay.dispatchEvent(t),this._grip!==null&&this._grip.dispatchEvent(t),this._hand!==null&&this._hand.dispatchEvent(t),this}connect(t){if(t&&t.hand){let e=this._hand;if(e)for(let i of t.hand.values())this._getHandJoint(e,i)}return this.dispatchEvent({type:"connected",data:t}),this}disconnect(t){return this.dispatchEvent({type:"disconnected",data:t}),this._targetRay!==null&&(this._targetRay.visible=!1),this._grip!==null&&(this._grip.visible=!1),this._hand!==null&&(this._hand.visible=!1),this}update(t,e,i){let s=null,r=null,o=null,a=this._targetRay,l=this._grip,c=this._hand;if(t&&e.session.visibilityState!=="visible-blurred"){if(c&&t.hand){o=!0;for(let x of t.hand.values()){let p=e.getJointPose(x,i),g=this._getHandJoint(c,x);p!==null&&(g.matrix.fromArray(p.transform.matrix),g.matrix.decompose(g.position,g.quaternion,g.scale),g.matrixWorldNeedsUpdate=!0,g.jointRadius=p.radius),g.visible=p!==null}let h=c.joints["index-finger-tip"],u=c.joints["thumb-tip"],d=h.position.distanceTo(u.position),f=.02,m=.005;c.inputState.pinching&&d>f+m?(c.inputState.pinching=!1,this.dispatchEvent({type:"pinchend",handedness:t.handedness,target:this})):!c.inputState.pinching&&d<=f-m&&(c.inputState.pinching=!0,this.dispatchEvent({type:"pinchstart",handedness:t.handedness,target:this}))}else l!==null&&t.gripSpace&&(r=e.getPose(t.gripSpace,i),r!==null&&(l.matrix.fromArray(r.transform.matrix),l.matrix.decompose(l.position,l.quaternion,l.scale),l.matrixWorldNeedsUpdate=!0,r.linearVelocity?(l.hasLinearVelocity=!0,l.linearVelocity.copy(r.linearVelocity)):l.hasLinearVelocity=!1,r.angularVelocity?(l.hasAngularVelocity=!0,l.angularVelocity.copy(r.angularVelocity)):l.hasAngularVelocity=!1,l.eventsEnabled&&l.dispatchEvent({type:"gripUpdated",data:t,target:this})));a!==null&&(s=e.getPose(t.targetRaySpace,i),s===null&&r!==null&&(s=r),s!==null&&(a.matrix.fromArray(s.transform.matrix),a.matrix.decompose(a.position,a.quaternion,a.scale),a.matrixWorldNeedsUpdate=!0,s.linearVelocity?(a.hasLinearVelocity=!0,a.linearVelocity.copy(s.linearVelocity)):a.hasLinearVelocity=!1,s.angularVelocity?(a.hasAngularVelocity=!0,a.angularVelocity.copy(s.angularVelocity)):a.hasAngularVelocity=!1,this.dispatchEvent(Em)))}return a!==null&&(a.visible=s!==null),l!==null&&(l.visible=r!==null),c!==null&&(c.visible=o!==null),this}_getHandJoint(t,e){if(t.joints[e.jointName]===void 0){let i=new ve;i.matrixAutoUpdate=!1,i.visible=!1,t.joints[e.jointName]=i,t.add(i)}return t.joints[e.jointName]}},rf={aliceblue:15792383,antiquewhite:16444375,aqua:65535,aquamarine:8388564,azure:15794175,beige:16119260,bisque:16770244,black:0,blanchedalmond:16772045,blue:255,blueviolet:9055202,brown:10824234,burlywood:14596231,cadetblue:6266528,chartreuse:8388352,chocolate:13789470,coral:16744272,cornflowerblue:6591981,cornsilk:16775388,crimson:14423100,cyan:65535,darkblue:139,darkcyan:35723,darkgoldenrod:12092939,darkgray:11119017,darkgreen:25600,darkgrey:11119017,darkkhaki:12433259,darkmagenta:9109643,darkolivegreen:5597999,darkorange:16747520,darkorchid:10040012,darkred:9109504,darksalmon:15308410,darkseagreen:9419919,darkslateblue:4734347,darkslategray:3100495,darkslategrey:3100495,darkturquoise:52945,darkviolet:9699539,deeppink:16716947,deepskyblue:49151,dimgray:6908265,dimgrey:6908265,dodgerblue:2003199,firebrick:11674146,floralwhite:16775920,forestgreen:2263842,fuchsia:16711935,gainsboro:14474460,ghostwhite:16316671,gold:16766720,goldenrod:14329120,gray:8421504,green:32768,greenyellow:11403055,grey:8421504,honeydew:15794160,hotpink:16738740,indianred:13458524,indigo:4915330,ivory:16777200,khaki:15787660,lavender:15132410,lavenderblush:16773365,lawngreen:8190976,lemonchiffon:16775885,lightblue:11393254,lightcoral:15761536,lightcyan:14745599,lightgoldenrodyellow:16448210,lightgray:13882323,lightgreen:9498256,lightgrey:13882323,lightpink:16758465,lightsalmon:16752762,lightseagreen:2142890,lightskyblue:8900346,lightslategray:7833753,lightslategrey:7833753,lightsteelblue:11584734,lightyellow:16777184,lime:65280,limegreen:3329330,linen:16445670,magenta:16711935,maroon:8388608,mediumaquamarine:6737322,mediumblue:205,mediumorchid:12211667,mediumpurple:9662683,mediumseagreen:3978097,mediumslateblue:8087790,mediumspringgreen:64154,mediumturquoise:4772300,mediumvioletred:13047173,midnightblue:1644912,mintcream:16121850,mistyrose:16770273,moccasin:16770229,navajowhite:16768685,navy:128,oldlace:16643558,olive:8421376,olivedrab:7048739,orange:16753920,orangered:16729344,orchid:14315734,palegoldenrod:15657130,palegreen:10025880,paleturquoise:11529966,palevioletred:14381203,papayawhip:16773077,peachpuff:16767673,peru:13468991,pink:16761035,plum:14524637,powderblue:11591910,purple:8388736,rebeccapurple:6697881,red:16711680,rosybrown:12357519,royalblue:4286945,saddlebrown:9127187,salmon:16416882,sandybrown:16032864,seagreen:3050327,seashell:16774638,sienna:10506797,silver:12632256,skyblue:8900331,slateblue:6970061,slategray:7372944,slategrey:7372944,snow:16775930,springgreen:65407,steelblue:4620980,tan:13808780,teal:32896,thistle:14204888,tomato:16737095,turquoise:4251856,violet:15631086,wheat:16113331,white:16777215,whitesmoke:16119285,yellow:16776960,yellowgreen:10145074},An={h:0,s:0,l:0},Ho={h:0,s:0,l:0};function Uc(n,t,e){return e<0&&(e+=1),e>1&&(e-=1),e<1/6?n+(t-n)*6*e:e<1/2?t:e<2/3?n+(t-n)*6*(2/3-e):n}var bt=class{constructor(t,e,i){return this.isColor=!0,this.r=1,this.g=1,this.b=1,this.set(t,e,i)}set(t,e,i){if(e===void 0&&i===void 0){let s=t;s&&s.isColor?this.copy(s):typeof s=="number"?this.setHex(s):typeof s=="string"&&this.setStyle(s)}else this.setRGB(t,e,i);return this}setScalar(t){return this.r=t,this.g=t,this.b=t,this}setHex(t,e=Ie){return t=Math.floor(t),this.r=(t>>16&255)/255,this.g=(t>>8&255)/255,this.b=(t&255)/255,Zt.colorSpaceToWorking(this,e),this}setRGB(t,e,i,s=Zt.workingColorSpace){return this.r=t,this.g=e,this.b=i,Zt.colorSpaceToWorking(this,s),this}setHSL(t,e,i,s=Zt.workingColorSpace){if(t=Rh(t,1),e=Vt(e,0,1),i=Vt(i,0,1),e===0)this.r=this.g=this.b=i;else{let r=i<=.5?i*(1+e):i+e-i*e,o=2*i-r;this.r=Uc(o,r,t+1/3),this.g=Uc(o,r,t),this.b=Uc(o,r,t-1/3)}return Zt.colorSpaceToWorking(this,s),this}setStyle(t,e=Ie){function i(r){r!==void 0&&parseFloat(r)<1&&Lt("Color: Alpha component of "+t+" will be ignored.")}let s;if(s=/^(\w+)\(([^\)]*)\)/.exec(t)){let r,o=s[1],a=s[2];switch(o){case"rgb":case"rgba":if(r=/^\s*(\d+)\s*,\s*(\d+)\s*,\s*(\d+)\s*(?:,\s*(\d+(?:\.\d+)?|\.\d+)\s*)?$/.exec(a))return i(r[4]),this.setRGB(Math.min(255,parseInt(r[1],10))/255,Math.min(255,parseInt(r[2],10))/255,Math.min(255,parseInt(r[3],10))/255,e);if(r=/^\s*(\d+(?:\.\d+)?|\.\d+)\%\s*,\s*(\d+(?:\.\d+)?|\.\d+)\%\s*,\s*(\d+(?:\.\d+)?|\.\d+)\%\s*(?:,\s*(\d+(?:\.\d+)?|\.\d+)\s*)?$/.exec(a))return i(r[4]),this.setRGB(Math.min(100,parseFloat(r[1]))/100,Math.min(100,parseFloat(r[2]))/100,Math.min(100,parseFloat(r[3]))/100,e);break;case"hsl":case"hsla":if(r=/^\s*([+-]?(?:\d+(?:\.\d+)?|\.\d+))\s*,\s*(\d+(?:\.\d+)?|\.\d+)\%\s*,\s*(\d+(?:\.\d+)?|\.\d+)\%\s*(?:,\s*(\d+(?:\.\d+)?|\.\d+)\s*)?$/.exec(a))return i(r[4]),this.setHSL(parseFloat(r[1])/360,parseFloat(r[2])/100,parseFloat(r[3])/100,e);break;default:Lt("Color: Unknown color model "+t)}}else if(s=/^\#([A-Fa-f\d]+)$/.exec(t)){let r=s[1],o=r.length;if(o===3)return this.setRGB(parseInt(r.charAt(0),16)/15,parseInt(r.charAt(1),16)/15,parseInt(r.charAt(2),16)/15,e);if(o===6)return this.setHex(parseInt(r,16),e);Lt("Color: Invalid hex color "+t)}else if(t&&t.length>0)return this.setColorName(t,e);return this}setColorName(t,e=Ie){let i=rf[t.toLowerCase()];return i!==void 0?this.setHex(i,e):Lt("Color: Unknown color "+t),this}clone(){return new this.constructor(this.r,this.g,this.b)}copy(t){return this.r=t.r,this.g=t.g,this.b=t.b,this}copySRGBToLinear(t){return this.r=hn(t.r),this.g=hn(t.g),this.b=hn(t.b),this}copyLinearToSRGB(t){return this.r=Us(t.r),this.g=Us(t.g),this.b=Us(t.b),this}convertSRGBToLinear(){return this.copySRGBToLinear(this),this}convertLinearToSRGB(){return this.copyLinearToSRGB(this),this}getHex(t=Ie){return Zt.workingToColorSpace(Qe.copy(this),t),Math.round(Vt(Qe.r*255,0,255))*65536+Math.round(Vt(Qe.g*255,0,255))*256+Math.round(Vt(Qe.b*255,0,255))}getHexString(t=Ie){return("000000"+this.getHex(t).toString(16)).slice(-6)}getHSL(t,e=Zt.workingColorSpace){Zt.workingToColorSpace(Qe.copy(this),e);let i=Qe.r,s=Qe.g,r=Qe.b,o=Math.max(i,s,r),a=Math.min(i,s,r),l,c,h=(a+o)/2;if(a===o)l=0,c=0;else{let u=o-a;switch(c=h<=.5?u/(o+a):u/(2-o-a),o){case i:l=(s-r)/u+(s<r?6:0);break;case s:l=(r-i)/u+2;break;case r:l=(i-s)/u+4;break}l/=6}return t.h=l,t.s=c,t.l=h,t}getRGB(t,e=Zt.workingColorSpace){return Zt.workingToColorSpace(Qe.copy(this),e),t.r=Qe.r,t.g=Qe.g,t.b=Qe.b,t}getStyle(t=Ie){Zt.workingToColorSpace(Qe.copy(this),t);let e=Qe.r,i=Qe.g,s=Qe.b;return t!==Ie?`color(${t} ${e.toFixed(3)} ${i.toFixed(3)} ${s.toFixed(3)})`:`rgb(${Math.round(e*255)},${Math.round(i*255)},${Math.round(s*255)})`}offsetHSL(t,e,i){return this.getHSL(An),this.setHSL(An.h+t,An.s+e,An.l+i)}add(t){return this.r+=t.r,this.g+=t.g,this.b+=t.b,this}addColors(t,e){return this.r=t.r+e.r,this.g=t.g+e.g,this.b=t.b+e.b,this}addScalar(t){return this.r+=t,this.g+=t,this.b+=t,this}sub(t){return this.r=Math.max(0,this.r-t.r),this.g=Math.max(0,this.g-t.g),this.b=Math.max(0,this.b-t.b),this}multiply(t){return this.r*=t.r,this.g*=t.g,this.b*=t.b,this}multiplyScalar(t){return this.r*=t,this.g*=t,this.b*=t,this}lerp(t,e){return this.r+=(t.r-this.r)*e,this.g+=(t.g-this.g)*e,this.b+=(t.b-this.b)*e,this}lerpColors(t,e,i){return this.r=t.r+(e.r-t.r)*i,this.g=t.g+(e.g-t.g)*i,this.b=t.b+(e.b-t.b)*i,this}lerpHSL(t,e){this.getHSL(An),t.getHSL(Ho);let i=Ar(An.h,Ho.h,e),s=Ar(An.s,Ho.s,e),r=Ar(An.l,Ho.l,e);return this.setHSL(i,s,r),this}setFromVector3(t){return this.r=t.x,this.g=t.y,this.b=t.z,this}applyMatrix3(t){let e=this.r,i=this.g,s=this.b,r=t.elements;return this.r=r[0]*e+r[3]*i+r[6]*s,this.g=r[1]*e+r[4]*i+r[7]*s,this.b=r[2]*e+r[5]*i+r[8]*s,this}equals(t){return t.r===this.r&&t.g===this.g&&t.b===this.b}fromArray(t,e=0){return this.r=t[e],this.g=t[e+1],this.b=t[e+2],this}toArray(t=[],e=0){return t[e]=this.r,t[e+1]=this.g,t[e+2]=this.b,t}fromBufferAttribute(t,e){return this.r=t.getX(e),this.g=t.getY(e),this.b=t.getZ(e),this}toJSON(){return this.getHex()}*[Symbol.iterator](){yield this.r,yield this.g,yield this.b}},Qe=new bt;bt.NAMES=rf;var Ye=class extends Me{constructor(){super(),this.isScene=!0,this.type="Scene",this.background=null,this.environment=null,this.fog=null,this.backgroundBlurriness=0,this.backgroundIntensity=1,this.backgroundRotation=new Ai,this.environmentIntensity=1,this.environmentRotation=new Ai,this.overrideMaterial=null,typeof __THREE_DEVTOOLS__<"u"&&__THREE_DEVTOOLS__.dispatchEvent(new CustomEvent("observe",{detail:this}))}copy(t,e){return super.copy(t,e),this.background=t.background!==null?t.background.clone():null,this.environment=t.environment!==null?t.environment.clone():null,this.fog=t.fog!==null?t.fog.clone():null,this.backgroundBlurriness=t.backgroundBlurriness,this.backgroundIntensity=t.backgroundIntensity,this.backgroundRotation.copy(t.backgroundRotation),this.environmentIntensity=t.environmentIntensity,this.environmentRotation.copy(t.environmentRotation),this.overrideMaterial=t.overrideMaterial!==null?t.overrideMaterial.clone():null,this.matrixAutoUpdate=t.matrixAutoUpdate,this}toJSON(t){let e=super.toJSON(t);return this.fog!==null&&(e.object.fog=this.fog.toJSON()),e.object.backgroundBlurriness=this.backgroundBlurriness,e.object.backgroundIntensity=this.backgroundIntensity,e.object.backgroundRotation=this.backgroundRotation.toArray(),e.object.environmentIntensity=this.environmentIntensity,e.object.environmentRotation=this.environmentRotation.toArray(),e}},Ii=new C,sn=new C,Fc=new C,rn=new C,ws=new C,Es=new C,$u=new C,Oc=new C,Bc=new C,zc=new C,kc=new oe,Hc=new oe,Vc=new oe,ln=class n{constructor(t=new C,e=new C,i=new C){this.a=t,this.b=e,this.c=i}static getNormal(t,e,i,s){s.subVectors(i,e),Ii.subVectors(t,e),s.cross(Ii);let r=s.lengthSq();return r>0?s.multiplyScalar(1/Math.sqrt(r)):s.set(0,0,0)}static getBarycoord(t,e,i,s,r){Ii.subVectors(s,e),sn.subVectors(i,e),Fc.subVectors(t,e);let o=Ii.dot(Ii),a=Ii.dot(sn),l=Ii.dot(Fc),c=sn.dot(sn),h=sn.dot(Fc),u=o*c-a*a;if(u===0)return r.set(0,0,0),null;let d=1/u,f=(c*l-a*h)*d,m=(o*h-a*l)*d;return r.set(1-f-m,m,f)}static containsPoint(t,e,i,s){return this.getBarycoord(t,e,i,s,rn)===null?!1:rn.x>=0&&rn.y>=0&&rn.x+rn.y<=1}static getInterpolation(t,e,i,s,r,o,a,l){return this.getBarycoord(t,e,i,s,rn)===null?(l.x=0,l.y=0,"z"in l&&(l.z=0),"w"in l&&(l.w=0),null):(l.setScalar(0),l.addScaledVector(r,rn.x),l.addScaledVector(o,rn.y),l.addScaledVector(a,rn.z),l)}static getInterpolatedAttribute(t,e,i,s,r,o){return kc.setScalar(0),Hc.setScalar(0),Vc.setScalar(0),kc.fromBufferAttribute(t,e),Hc.fromBufferAttribute(t,i),Vc.fromBufferAttribute(t,s),o.setScalar(0),o.addScaledVector(kc,r.x),o.addScaledVector(Hc,r.y),o.addScaledVector(Vc,r.z),o}static isFrontFacing(t,e,i,s){return Ii.subVectors(i,e),sn.subVectors(t,e),Ii.cross(sn).dot(s)<0}set(t,e,i){return this.a.copy(t),this.b.copy(e),this.c.copy(i),this}setFromPointsAndIndices(t,e,i,s){return this.a.copy(t[e]),this.b.copy(t[i]),this.c.copy(t[s]),this}setFromAttributeAndIndices(t,e,i,s){return this.a.fromBufferAttribute(t,e),this.b.fromBufferAttribute(t,i),this.c.fromBufferAttribute(t,s),this}clone(){return new this.constructor().copy(this)}copy(t){return this.a.copy(t.a),this.b.copy(t.b),this.c.copy(t.c),this}getArea(){return Ii.subVectors(this.c,this.b),sn.subVectors(this.a,this.b),Ii.cross(sn).length()*.5}getMidpoint(t){return t.addVectors(this.a,this.b).add(this.c).multiplyScalar(1/3)}getNormal(t){return n.getNormal(this.a,this.b,this.c,t)}getPlane(t){return t.setFromCoplanarPoints(this.a,this.b,this.c)}getBarycoord(t,e){return n.getBarycoord(t,this.a,this.b,this.c,e)}getInterpolation(t,e,i,s,r){return n.getInterpolation(t,this.a,this.b,this.c,e,i,s,r)}containsPoint(t){return n.containsPoint(t,this.a,this.b,this.c)}isFrontFacing(t){return n.isFrontFacing(this.a,this.b,this.c,t)}intersectsBox(t){return t.intersectsTriangle(this)}closestPointToPoint(t,e){let i=this.a,s=this.b,r=this.c,o,a;ws.subVectors(s,i),Es.subVectors(r,i),Oc.subVectors(t,i);let l=ws.dot(Oc),c=Es.dot(Oc);if(l<=0&&c<=0)return e.copy(i);Bc.subVectors(t,s);let h=ws.dot(Bc),u=Es.dot(Bc);if(h>=0&&u<=h)return e.copy(s);let d=l*u-h*c;if(d<=0&&l>=0&&h<=0&&l-h>0)return o=l/(l-h),e.copy(i).addScaledVector(ws,o);zc.subVectors(t,r);let f=ws.dot(zc),m=Es.dot(zc);if(m>=0&&f<=m)return e.copy(r);let x=f*c-l*m;if(x<=0&&c>=0&&m<=0)return a=c/(c-m),e.copy(i).addScaledVector(Es,a);let p=h*m-f*u;if(p<=0&&u-h>=0&&f-m>=0)return $u.subVectors(r,s),a=(u-h)/(u-h+(f-m)),e.copy(s).addScaledVector($u,a);let g=1/(p+x+d);return o=x*g,a=d*g,e.copy(i).addScaledVector(ws,o).addScaledVector(Es,a)}equals(t){return t.a.equals(this.a)&&t.b.equals(this.b)&&t.c.equals(this.c)}},Xi=class{constructor(t=new C(1/0,1/0,1/0),e=new C(-1/0,-1/0,-1/0)){this.isBox3=!0,this.min=t,this.max=e}set(t,e){return this.min.copy(t),this.max.copy(e),this}setFromArray(t){this.makeEmpty();for(let e=0,i=t.length;e<i;e+=3)this.expandByPoint(Di.fromArray(t,e));return this}setFromBufferAttribute(t){this.makeEmpty();for(let e=0,i=t.count;e<i;e++)this.expandByPoint(Di.fromBufferAttribute(t,e));return this}setFromPoints(t){this.makeEmpty();for(let e=0,i=t.length;e<i;e++)this.expandByPoint(t[e]);return this}setFromCenterAndSize(t,e){let i=Di.copy(e).multiplyScalar(.5);return this.min.copy(t).sub(i),this.max.copy(t).add(i),this}setFromObject(t,e=!1){return this.makeEmpty(),this.expandByObject(t,e)}clone(){return new this.constructor().copy(this)}copy(t){return this.min.copy(t.min),this.max.copy(t.max),this}makeEmpty(){return this.min.x=this.min.y=this.min.z=1/0,this.max.x=this.max.y=this.max.z=-1/0,this}isEmpty(){return this.max.x<this.min.x||this.max.y<this.min.y||this.max.z<this.min.z}getCenter(t){return this.isEmpty()?t.set(0,0,0):t.addVectors(this.min,this.max).multiplyScalar(.5)}getSize(t){return this.isEmpty()?t.set(0,0,0):t.subVectors(this.max,this.min)}expandByPoint(t){return this.min.min(t),this.max.max(t),this}expandByVector(t){return this.min.sub(t),this.max.add(t),this}expandByScalar(t){return this.min.addScalar(-t),this.max.addScalar(t),this}expandByObject(t,e=!1){t.updateWorldMatrix(!1,!1);let i=t.geometry;if(i!==void 0){let r=i.getAttribute("position");if(e===!0&&r!==void 0&&t.isInstancedMesh!==!0&&t.isBatchedMesh!==!0)for(let o=0,a=r.count;o<a;o++)t.isMesh===!0?t.getVertexPosition(o,Di):Di.fromBufferAttribute(r,o),Di.applyMatrix4(t.matrixWorld),this.expandByPoint(Di);else t.boundingBox!==void 0?(t.boundingBox===null&&t.computeBoundingBox(),Vo.copy(t.boundingBox)):(i.boundingBox===null&&i.computeBoundingBox(),Vo.copy(i.boundingBox)),Vo.applyMatrix4(t.matrixWorld),this.union(Vo)}let s=t.children;for(let r=0,o=s.length;r<o;r++)this.expandByObject(s[r],e);return this}containsPoint(t){return t.x>=this.min.x&&t.x<=this.max.x&&t.y>=this.min.y&&t.y<=this.max.y&&t.z>=this.min.z&&t.z<=this.max.z}containsBox(t){return this.min.x<=t.min.x&&t.max.x<=this.max.x&&this.min.y<=t.min.y&&t.max.y<=this.max.y&&this.min.z<=t.min.z&&t.max.z<=this.max.z}getParameter(t,e){return e.set((t.x-this.min.x)/(this.max.x-this.min.x),(t.y-this.min.y)/(this.max.y-this.min.y),(t.z-this.min.z)/(this.max.z-this.min.z))}intersectsBox(t){return t.max.x>=this.min.x&&t.min.x<=this.max.x&&t.max.y>=this.min.y&&t.min.y<=this.max.y&&t.max.z>=this.min.z&&t.min.z<=this.max.z}intersectsSphere(t){return this.clampPoint(t.center,Di),Di.distanceToSquared(t.center)<=t.radius*t.radius}intersectsPlane(t){let e,i;return t.normal.x>0?(e=t.normal.x*this.min.x,i=t.normal.x*this.max.x):(e=t.normal.x*this.max.x,i=t.normal.x*this.min.x),t.normal.y>0?(e+=t.normal.y*this.min.y,i+=t.normal.y*this.max.y):(e+=t.normal.y*this.max.y,i+=t.normal.y*this.min.y),t.normal.z>0?(e+=t.normal.z*this.min.z,i+=t.normal.z*this.max.z):(e+=t.normal.z*this.max.z,i+=t.normal.z*this.min.z),e<=-t.constant&&i>=-t.constant}intersectsTriangle(t){if(this.isEmpty())return!1;this.getCenter(Mr),Go.subVectors(this.max,Mr),Ts.subVectors(t.a,Mr),As.subVectors(t.b,Mr),Rs.subVectors(t.c,Mr),Rn.subVectors(As,Ts),Cn.subVectors(Rs,As),Yn.subVectors(Ts,Rs);let e=[0,-Rn.z,Rn.y,0,-Cn.z,Cn.y,0,-Yn.z,Yn.y,Rn.z,0,-Rn.x,Cn.z,0,-Cn.x,Yn.z,0,-Yn.x,-Rn.y,Rn.x,0,-Cn.y,Cn.x,0,-Yn.y,Yn.x,0];return!Gc(e,Ts,As,Rs,Go)||(e=[1,0,0,0,1,0,0,0,1],!Gc(e,Ts,As,Rs,Go))?!1:(Wo.crossVectors(Rn,Cn),e=[Wo.x,Wo.y,Wo.z],Gc(e,Ts,As,Rs,Go))}clampPoint(t,e){return e.copy(t).clamp(this.min,this.max)}distanceToPoint(t){return this.clampPoint(t,Di).distanceTo(t)}getBoundingSphere(t){return this.isEmpty()?t.makeEmpty():(this.getCenter(t.center),t.radius=this.getSize(Di).length()*.5),t}intersect(t){return this.min.max(t.min),this.max.min(t.max),this.isEmpty()&&this.makeEmpty(),this}union(t){return this.min.min(t.min),this.max.max(t.max),this}applyMatrix4(t){return this.isEmpty()?this:(on[0].set(this.min.x,this.min.y,this.min.z).applyMatrix4(t),on[1].set(this.min.x,this.min.y,this.max.z).applyMatrix4(t),on[2].set(this.min.x,this.max.y,this.min.z).applyMatrix4(t),on[3].set(this.min.x,this.max.y,this.max.z).applyMatrix4(t),on[4].set(this.max.x,this.min.y,this.min.z).applyMatrix4(t),on[5].set(this.max.x,this.min.y,this.max.z).applyMatrix4(t),on[6].set(this.max.x,this.max.y,this.min.z).applyMatrix4(t),on[7].set(this.max.x,this.max.y,this.max.z).applyMatrix4(t),this.setFromPoints(on),this)}translate(t){return this.min.add(t),this.max.add(t),this}equals(t){return t.min.equals(this.min)&&t.max.equals(this.max)}toJSON(){return{min:this.min.toArray(),max:this.max.toArray()}}fromJSON(t){return this.min.fromArray(t.min),this.max.fromArray(t.max),this}},on=[new C,new C,new C,new C,new C,new C,new C,new C],Di=new C,Vo=new Xi,Ts=new C,As=new C,Rs=new C,Rn=new C,Cn=new C,Yn=new C,Mr=new C,Go=new C,Wo=new C,Zn=new C;function Gc(n,t,e,i,s){for(let r=0,o=n.length-3;r<=o;r+=3){Zn.fromArray(n,r);let a=s.x*Math.abs(Zn.x)+s.y*Math.abs(Zn.y)+s.z*Math.abs(Zn.z),l=t.dot(Zn),c=e.dot(Zn),h=i.dot(Zn);if(Math.max(-Math.max(l,c,h),Math.min(l,c,h))>a)return!1}return!0}var cn=Tm();function Tm(){let n=new ArrayBuffer(4),t=new Float32Array(n),e=new Uint32Array(n),i=new Uint32Array(512),s=new Uint32Array(512);for(let l=0;l<256;++l){let c=l-127;c<-27?(i[l]=0,i[l|256]=32768,s[l]=24,s[l|256]=24):c<-14?(i[l]=1024>>-c-14,i[l|256]=1024>>-c-14|32768,s[l]=-c-1,s[l|256]=-c-1):c<=15?(i[l]=c+15<<10,i[l|256]=c+15<<10|32768,s[l]=13,s[l|256]=13):c<128?(i[l]=31744,i[l|256]=64512,s[l]=24,s[l|256]=24):(i[l]=31744,i[l|256]=64512,s[l]=13,s[l|256]=13)}let r=new Uint32Array(2048),o=new Uint32Array(64),a=new Uint32Array(64);for(let l=1;l<1024;++l){let c=l<<13,h=0;for(;(c&8388608)===0;)c<<=1,h-=8388608;c&=-8388609,h+=947912704,r[l]=c|h}for(let l=1024;l<2048;++l)r[l]=939524096+(l-1024<<13);for(let l=1;l<31;++l)o[l]=l<<23;o[31]=1199570944,o[32]=2147483648;for(let l=33;l<63;++l)o[l]=2147483648+(l-32<<23);o[63]=3347054592;for(let l=1;l<64;++l)l!==32&&(a[l]=1024);return{floatView:t,uint32View:e,baseTable:i,shiftTable:s,mantissaTable:r,exponentTable:o,offsetTable:a}}function Am(n){Math.abs(n)>65504&&Lt("DataUtils.toHalfFloat(): Value out of range."),n=Vt(n,-65504,65504),cn.floatView[0]=n;let t=cn.uint32View[0],e=t>>23&511;return cn.baseTable[e]+((t&8388607)>>cn.shiftTable[e])}function Rm(n){let t=n>>10;return cn.uint32View[0]=cn.mantissaTable[cn.offsetTable[t]+(n&1023)]+cn.exponentTable[t],cn.floatView[0]}var Gs=class{static toHalfFloat(t){return Am(t)}static fromHalfFloat(t){return Rm(t)}},Pe=new C,Xo=new et,Cm=0,le=class extends Ni{constructor(t,e,i=!1){if(super(),Array.isArray(t))throw new TypeError("THREE.BufferAttribute: array should be a Typed Array.");this.isBufferAttribute=!0,Object.defineProperty(this,"id",{value:Cm++}),this.name="",this.array=t,this.itemSize=e,this.count=t!==void 0?t.length/e:0,this.normalized=i,this.usage=$d,this.updateRanges=[],this.gpuType=gi,this.version=0,this._normalizationScale=t!==void 0?Ph(t):void 0}onUploadCallback(){}set needsUpdate(t){t===!0&&this.version++}setUsage(t){return this.usage=t,this}addUpdateRange(t,e){this.updateRanges.push({start:t,count:e})}clearUpdateRanges(){this.updateRanges.length=0}copy(t){return this.name=t.name,this.array=new t.array.constructor(t.array),this.itemSize=t.itemSize,this.count=t.count,this.normalized=t.normalized,this.usage=t.usage,this.gpuType=t.gpuType,this._normalizationScale=t._normalizationScale,this}copyAt(t,e,i){t*=this.itemSize,i*=e.itemSize;for(let s=0,r=this.itemSize;s<r;s++)this.array[t+s]=e.array[i+s];return this}copyArray(t){return this.array.set(t),this}applyMatrix3(t){if(this.itemSize===2)for(let e=0,i=this.count;e<i;e++)Xo.fromBufferAttribute(this,e),Xo.applyMatrix3(t),this.setXY(e,Xo.x,Xo.y);else if(this.itemSize===3)for(let e=0,i=this.count;e<i;e++)Pe.fromBufferAttribute(this,e),Pe.applyMatrix3(t),this.setXYZ(e,Pe.x,Pe.y,Pe.z);return this}applyMatrix4(t){for(let e=0,i=this.count;e<i;e++)Pe.fromBufferAttribute(this,e),Pe.applyMatrix4(t),this.setXYZ(e,Pe.x,Pe.y,Pe.z);return this}applyNormalMatrix(t){for(let e=0,i=this.count;e<i;e++)Pe.fromBufferAttribute(this,e),Pe.applyNormalMatrix(t),this.setXYZ(e,Pe.x,Pe.y,Pe.z);return this}transformDirection(t){for(let e=0,i=this.count;e<i;e++)Pe.fromBufferAttribute(this,e),Pe.transformDirection(t),this.setXYZ(e,Pe.x,Pe.y,Pe.z);return this}set(t,e=0){return this.array.set(t,e),this}getComponent(t,e){let i=this.array[t*this.itemSize+e];return this.normalized&&(i=this._denormalize(i)),i}setComponent(t,e,i){return this.normalized&&(i=this._normalize(i)),this.array[t*this.itemSize+e]=i,this}getX(t){let e=this.array[t*this.itemSize];return this.normalized&&(e=this._denormalize(e)),e}setX(t,e){return this.normalized&&(e=this._normalize(e)),this.array[t*this.itemSize]=e,this}getY(t){let e=this.array[t*this.itemSize+1];return this.normalized&&(e=this._denormalize(e)),e}setY(t,e){return this.normalized&&(e=this._normalize(e)),this.array[t*this.itemSize+1]=e,this}getZ(t){let e=this.array[t*this.itemSize+2];return this.normalized&&(e=this._denormalize(e)),e}setZ(t,e){return this.normalized&&(e=this._normalize(e)),this.array[t*this.itemSize+2]=e,this}getW(t){let e=this.array[t*this.itemSize+3];return this.normalized&&(e=this._denormalize(e)),e}setW(t,e){return this.normalized&&(e=this._normalize(e)),this.array[t*this.itemSize+3]=e,this}setXY(t,e,i){return t*=this.itemSize,this.normalized&&(e=this._normalize(e),i=this._normalize(i)),this.array[t+0]=e,this.array[t+1]=i,this}setXYZ(t,e,i,s){return t*=this.itemSize,this.normalized&&(e=this._normalize(e),i=this._normalize(i),s=this._normalize(s)),this.array[t+0]=e,this.array[t+1]=i,this.array[t+2]=s,this}setXYZW(t,e,i,s,r){return t*=this.itemSize,this.normalized&&(e=this._normalize(e),i=this._normalize(i),s=this._normalize(s),r=this._normalize(r)),this.array[t+0]=e,this.array[t+1]=i,this.array[t+2]=s,this.array[t+3]=r,this}onUpload(t){return this.onUploadCallback=t,this}clone(){return new this.constructor(this.array,this.itemSize).copy(this)}toJSON(){let t={itemSize:this.itemSize,type:this.array.constructor.name,array:Array.from(this.array),normalized:this.normalized};return t.name=this.name,t.usage=this.usage,t.gpuType=this.gpuType,t}dispose(){this.dispatchEvent({type:"dispose"})}_denormalize(t){let e=this._normalizationScale;return e!==void 0?Math.max(t/e,-1):nf(t,this.array)}_normalize(t){let e=this._normalizationScale;return e!==void 0?Math.round(t*e):sf(t,this.array)}};var Nr=class extends le{constructor(t,e,i){super(new Uint16Array(t),e,i)}};var Ur=class extends le{constructor(t,e,i){super(new Uint32Array(t),e,i)}};var ee=class extends le{constructor(t,e,i){super(new Float32Array(t),e,i)}},Pm=new Xi,Sr=new C,Wc=new C,ci=class{constructor(t=new C,e=-1){this.isSphere=!0,this.center=t,this.radius=e}set(t,e){return this.center.copy(t),this.radius=e,this}setFromPoints(t,e){let i=this.center;e!==void 0?i.copy(e):Pm.setFromPoints(t).getCenter(i);let s=0;for(let r=0,o=t.length;r<o;r++)s=Math.max(s,i.distanceToSquared(t[r]));return this.radius=Math.sqrt(s),this}copy(t){return this.center.copy(t.center),this.radius=t.radius,this}isEmpty(){return this.radius<0}makeEmpty(){return this.center.set(0,0,0),this.radius=-1,this}containsPoint(t){return t.distanceToSquared(this.center)<=this.radius*this.radius}distanceToPoint(t){return t.distanceTo(this.center)-this.radius}intersectsSphere(t){let e=this.radius+t.radius;return t.center.distanceToSquared(this.center)<=e*e}intersectsBox(t){return t.intersectsSphere(this)}intersectsPlane(t){return Math.abs(t.distanceToPoint(this.center))<=this.radius}clampPoint(t,e){let i=this.center.distanceToSquared(t);return e.copy(t),i>this.radius*this.radius&&(e.sub(this.center).normalize(),e.multiplyScalar(this.radius).add(this.center)),e}getBoundingBox(t){return this.isEmpty()?(t.makeEmpty(),t):(t.set(this.center,this.center),t.expandByScalar(this.radius),t)}applyMatrix4(t){return this.center.applyMatrix4(t),this.radius=this.radius*t.getMaxScaleOnAxis(),this}translate(t){return this.center.add(t),this}expandByPoint(t){if(this.isEmpty())return this.center.copy(t),this.radius=0,this;Sr.subVectors(t,this.center);let e=Sr.lengthSq();if(e>this.radius*this.radius){let i=Math.sqrt(e),s=(i-this.radius)*.5;this.center.addScaledVector(Sr,s/i),this.radius+=s}return this}union(t){return t.isEmpty()?this:this.isEmpty()?(this.copy(t),this):(this.center.equals(t.center)===!0?this.radius=Math.max(this.radius,t.radius):(Wc.subVectors(t.center,this.center).setLength(t.radius),this.expandByPoint(Sr.copy(t.center).add(Wc)),this.expandByPoint(Sr.copy(t.center).sub(Wc))),this)}equals(t){return t.center.equals(this.center)&&t.radius===this.radius}clone(){return new this.constructor().copy(this)}toJSON(){return{radius:this.radius,center:this.center.toArray()}}fromJSON(t){return this.radius=t.radius,this.center.fromArray(t.center),this}},Im=0,Ei=new Xt,Xc=new Me,Cs=new C,yi=new Xi,br=new Xi,Ge=new C,Jt=class n extends Ni{constructor(){super(),this.isBufferGeometry=!0,Object.defineProperty(this,"id",{value:Im++}),this.uuid=sr(),this.name="",this.type="BufferGeometry",this.index=null,this.indirect=null,this.indirectOffset=0,this.attributes={},this.morphAttributes={},this.morphTargetsRelative=!1,this.groups=[],this.boundingBox=null,this.boundingSphere=null,this.drawRange={start:0,count:1/0},this.userData={},this._transformed=!1}getIndex(){return this.index}setIndex(t){return Array.isArray(t)?this.index=new(Qp(t)?Ur:Nr)(t,1):this.index=t,this}setIndirect(t,e=0){return this.indirect=t,this.indirectOffset=e,this}getIndirect(){return this.indirect}getAttribute(t){return this.attributes[t]}setAttribute(t,e){return this.attributes[t]=e,this}deleteAttribute(t){return delete this.attributes[t],this}hasAttribute(t){return this.attributes[t]!==void 0}addGroup(t,e,i=0){this.groups.push({start:t,count:e,materialIndex:i})}clearGroups(){this.groups=[]}setDrawRange(t,e){this.drawRange.start=t,this.drawRange.count=e}applyMatrix4(t){let e=this.attributes.position;e!==void 0&&(e.applyMatrix4(t),e.needsUpdate=!0);let i=this.attributes.normal;if(i!==void 0){let r=new zt().getNormalMatrix(t);i.applyNormalMatrix(r),i.needsUpdate=!0}let s=this.attributes.tangent;return s!==void 0&&(s.transformDirection(t),s.needsUpdate=!0),this.boundingBox!==null&&this.computeBoundingBox(),this.boundingSphere!==null&&this.computeBoundingSphere(),this._transformed=!0,this}applyQuaternion(t){return Ei.makeRotationFromQuaternion(t),this.applyMatrix4(Ei),this}rotateX(t){return Ei.makeRotationX(t),this.applyMatrix4(Ei),this}rotateY(t){return Ei.makeRotationY(t),this.applyMatrix4(Ei),this}rotateZ(t){return Ei.makeRotationZ(t),this.applyMatrix4(Ei),this}translate(t,e,i){return Ei.makeTranslation(t,e,i),this.applyMatrix4(Ei),this}scale(t,e,i){return Ei.makeScale(t,e,i),this.applyMatrix4(Ei),this}lookAt(t){return Xc.lookAt(t),Xc.updateMatrix(),this.applyMatrix4(Xc.matrix),this}center(){return this.computeBoundingBox(),this.boundingBox.getCenter(Cs).negate(),this.translate(Cs.x,Cs.y,Cs.z),this}setFromPoints(t){let e=this.getAttribute("position");if(e===void 0){let i=[];for(let s=0,r=t.length;s<r;s++){let o=t[s];i.push(o.x,o.y,o.z||0)}this.setAttribute("position",new ee(i,3))}else{let i=Math.min(t.length,e.count);for(let s=0;s<i;s++){let r=t[s];e.setXYZ(s,r.x,r.y,r.z||0)}t.length>e.count&&Lt("BufferGeometry: Buffer size too small for points data. Use .dispose() and create a new geometry."),e.needsUpdate=!0}return this}computeBoundingBox(){this.boundingBox===null&&(this.boundingBox=new Xi);let t=this.attributes.position,e=this.morphAttributes.position;if(t&&t.isGLBufferAttribute){Ot("BufferGeometry.computeBoundingBox(): GLBufferAttribute requires a manual bounding box.",this),this.boundingBox.set(new C(-1/0,-1/0,-1/0),new C(1/0,1/0,1/0));return}if(t!==void 0){if(this.boundingBox.setFromBufferAttribute(t),e)for(let i=0,s=e.length;i<s;i++){let r=e[i];yi.setFromBufferAttribute(r),this.morphTargetsRelative?(Ge.addVectors(this.boundingBox.min,yi.min),this.boundingBox.expandByPoint(Ge),Ge.addVectors(this.boundingBox.max,yi.max),this.boundingBox.expandByPoint(Ge)):(this.boundingBox.expandByPoint(yi.min),this.boundingBox.expandByPoint(yi.max))}}else this.boundingBox.makeEmpty();(isNaN(this.boundingBox.min.x)||isNaN(this.boundingBox.min.y)||isNaN(this.boundingBox.min.z))&&Ot('BufferGeometry.computeBoundingBox(): Computed min/max have NaN values. The "position" attribute is likely to have NaN values.',this)}computeBoundingSphere(){this.boundingSphere===null&&(this.boundingSphere=new ci);let t=this.attributes.position,e=this.morphAttributes.position;if(t&&t.isGLBufferAttribute){Ot("BufferGeometry.computeBoundingSphere(): GLBufferAttribute requires a manual bounding sphere.",this),this.boundingSphere.set(new C,1/0);return}if(t){let i=this.boundingSphere.center;if(yi.setFromBufferAttribute(t),e)for(let r=0,o=e.length;r<o;r++){let a=e[r];br.setFromBufferAttribute(a),this.morphTargetsRelative?(Ge.addVectors(yi.min,br.min),yi.expandByPoint(Ge),Ge.addVectors(yi.max,br.max),yi.expandByPoint(Ge)):(yi.expandByPoint(br.min),yi.expandByPoint(br.max))}yi.getCenter(i);let s=0;for(let r=0,o=t.count;r<o;r++)Ge.fromBufferAttribute(t,r),s=Math.max(s,i.distanceToSquared(Ge));if(e)for(let r=0,o=e.length;r<o;r++){let a=e[r],l=this.morphTargetsRelative;for(let c=0,h=a.count;c<h;c++)Ge.fromBufferAttribute(a,c),l&&(Cs.fromBufferAttribute(t,c),Ge.add(Cs)),s=Math.max(s,i.distanceToSquared(Ge))}this.boundingSphere.radius=Math.sqrt(s),isNaN(this.boundingSphere.radius)&&Ot('BufferGeometry.computeBoundingSphere(): Computed radius is NaN. The "position" attribute is likely to have NaN values.',this)}}computeTangents(){let t=this.index,e=this.attributes;if(t===null||e.position===void 0||e.normal===void 0||e.uv===void 0){Ot("BufferGeometry: .computeTangents() failed. Missing required attributes (index, position, normal or uv)");return}let i=e.position,s=e.normal,r=e.uv,o=this.getAttribute("tangent");(o===void 0||o.count!==i.count)&&(o=new le(new Float32Array(4*i.count),4),this.setAttribute("tangent",o));let a=[],l=[],c=new Uint8Array(i.count);for(let D=0;D<i.count;D++)a[D]=new C,l[D]=new C;let h=new C,u=new C,d=new C,f=new et,m=new et,x=new et,p=new C,g=new C;function y(D,P,A){c[D]=c[P]=c[A]=1,h.fromBufferAttribute(i,D),u.fromBufferAttribute(i,P),d.fromBufferAttribute(i,A),f.fromBufferAttribute(r,D),m.fromBufferAttribute(r,P),x.fromBufferAttribute(r,A),u.sub(h),d.sub(h),m.sub(f),x.sub(f);let R=1/(m.x*x.y-x.x*m.y);isFinite(R)&&(p.copy(u).multiplyScalar(x.y).addScaledVector(d,-m.y).multiplyScalar(R),g.copy(d).multiplyScalar(m.x).addScaledVector(u,-x.x).multiplyScalar(R),a[D].add(p),a[P].add(p),a[A].add(p),l[D].add(g),l[P].add(g),l[A].add(g))}let v=this.groups;v.length===0&&(v=[{start:0,count:t.count}]);for(let D=0,P=v.length;D<P;++D){let A=v[D],R=A.start,U=A.count;for(let L=R,B=R+U;L<B;L+=3)y(t.getX(L+0),t.getX(L+1),t.getX(L+2))}let S=new C,T=new C,b=new C,_=new C;function M(D){b.fromBufferAttribute(s,D),_.copy(b);let P=a[D];S.copy(P),S.sub(b.multiplyScalar(b.dot(P))).normalize(),T.crossVectors(_,P);let R=T.dot(l[D])<0?-1:1;o.setXYZW(D,S.x,S.y,S.z,R)}for(let D=0,P=i.count;D<P;++D)c[D]&&M(D);this._transformed=!0}computeVertexNormals(){let t=this.index,e=this.getAttribute("position");if(e!==void 0){let i=this.getAttribute("normal");if(i===void 0||i.count!==e.count)i=new le(new Float32Array(e.count*3),3),this.setAttribute("normal",i);else for(let d=0,f=i.count;d<f;d++)i.setXYZ(d,0,0,0);let s=new C,r=new C,o=new C,a=new C,l=new C,c=new C,h=new C,u=new C;if(t)for(let d=0,f=t.count;d<f;d+=3){let m=t.getX(d+0),x=t.getX(d+1),p=t.getX(d+2);s.fromBufferAttribute(e,m),r.fromBufferAttribute(e,x),o.fromBufferAttribute(e,p),h.subVectors(o,r),u.subVectors(s,r),h.cross(u),a.fromBufferAttribute(i,m),l.fromBufferAttribute(i,x),c.fromBufferAttribute(i,p),a.add(h),l.add(h),c.add(h),i.setXYZ(m,a.x,a.y,a.z),i.setXYZ(x,l.x,l.y,l.z),i.setXYZ(p,c.x,c.y,c.z)}else for(let d=0,f=e.count;d<f;d+=3)s.fromBufferAttribute(e,d+0),r.fromBufferAttribute(e,d+1),o.fromBufferAttribute(e,d+2),h.subVectors(o,r),u.subVectors(s,r),h.cross(u),i.setXYZ(d+0,h.x,h.y,h.z),i.setXYZ(d+1,h.x,h.y,h.z),i.setXYZ(d+2,h.x,h.y,h.z);this.normalizeNormals(),i.needsUpdate=!0}}normalizeNormals(){let t=this.attributes.normal;for(let e=0,i=t.count;e<i;e++)Ge.fromBufferAttribute(t,e),Ge.normalize(),t.setXYZ(e,Ge.x,Ge.y,Ge.z)}toNonIndexed(){function t(a,l){let c=a.array,h=a.itemSize,u=a.normalized,d=new c.constructor(l.length*h),f=0,m=0;for(let x=0,p=l.length;x<p;x++){a.isInterleavedBufferAttribute?f=l[x]*a.data.stride+a.offset:f=l[x]*h;for(let g=0;g<h;g++)d[m++]=c[f++]}return new le(d,h,u)}if(this.index===null)return Lt("BufferGeometry.toNonIndexed(): BufferGeometry is already non-indexed."),this;let e=new n,i=this.index.array,s=this.attributes;for(let a in s){let l=s[a],c=t(l,i);e.setAttribute(a,c)}let r=this.morphAttributes;for(let a in r){let l=[],c=r[a];for(let h=0,u=c.length;h<u;h++){let d=c[h],f=t(d,i);l.push(f)}e.morphAttributes[a]=l}e.morphTargetsRelative=this.morphTargetsRelative;let o=this.groups;for(let a=0,l=o.length;a<l;a++){let c=o[a];e.addGroup(c.start,c.count,c.materialIndex)}return e}toJSON(){let t={metadata:{version:4.7,type:"BufferGeometry",generator:"BufferGeometry.toJSON"}};if(t.uuid=this.uuid,t.type=this.parameters!==void 0&&this._transformed===!0?"BufferGeometry":this.type,t.name=this.name,Object.keys(this.userData).length>0&&(t.userData=this.userData),this.parameters!==void 0&&this._transformed!==!0){let l=this.parameters;for(let c in l)l[c]!==void 0&&(t[c]=l[c]);return t}t.data={attributes:{}};let e=this.index;e!==null&&(t.data.index={type:e.array.constructor.name,array:Array.prototype.slice.call(e.array)});let i=this.attributes;for(let l in i){let c=i[l];t.data.attributes[l]=c.toJSON(t.data)}let s={},r=!1;for(let l in this.morphAttributes){let c=this.morphAttributes[l],h=[];for(let u=0,d=c.length;u<d;u++){let f=c[u];h.push(f.toJSON(t.data))}h.length>0&&(s[l]=h,r=!0)}r&&(t.data.morphAttributes=s,t.data.morphTargetsRelative=this.morphTargetsRelative);let o=this.groups;o.length>0&&(t.data.groups=JSON.parse(JSON.stringify(o)));let a=this.boundingSphere;return a!==null&&(t.data.boundingSphere=a.toJSON()),t}clone(){return new this.constructor().copy(this)}copy(t){this.index=null,this.attributes={},this.morphAttributes={},this.groups=[],this.boundingBox=null,this.boundingSphere=null;let e={};this.name=t.name;let i=t.index;i!==null&&this.setIndex(i.clone());let s=t.attributes;for(let c in s){let h=s[c];this.setAttribute(c,h.clone(e))}let r=t.morphAttributes;for(let c in r){let h=[],u=r[c];for(let d=0,f=u.length;d<f;d++)h.push(u[d].clone(e));this.morphAttributes[c]=h}this.morphTargetsRelative=t.morphTargetsRelative;let o=t.groups;for(let c=0,h=o.length;c<h;c++){let u=o[c];this.addGroup(u.start,u.count,u.materialIndex)}let a=t.boundingBox;a!==null&&(this.boundingBox=a.clone());let l=t.boundingSphere;return l!==null&&(this.boundingSphere=l.clone()),this.drawRange.start=t.drawRange.start,this.drawRange.count=t.drawRange.count,this.userData=t.userData,this._transformed=t._transformed,this}dispose(){this.dispatchEvent({type:"dispose"})}};var qc=new C,Dm=new C,Lm=new zt,li=class{constructor(t=new C(1,0,0),e=0){this.isPlane=!0,this.normal=t,this.constant=e}set(t,e){return this.normal.copy(t),this.constant=e,this}setComponents(t,e,i,s){return this.normal.set(t,e,i),this.constant=s,this}setFromNormalAndCoplanarPoint(t,e){return this.normal.copy(t),this.constant=-e.dot(this.normal),this}setFromCoplanarPoints(t,e,i){let s=qc.subVectors(i,e).cross(Dm.subVectors(t,e)).normalize();return this.setFromNormalAndCoplanarPoint(s,t),this}copy(t){return this.normal.copy(t.normal),this.constant=t.constant,this}normalize(){let t=1/this.normal.length();return this.normal.multiplyScalar(t),this.constant*=t,this}negate(){return this.constant*=-1,this.normal.negate(),this}distanceToPoint(t){return this.normal.dot(t)+this.constant}distanceToSphere(t){return this.distanceToPoint(t.center)-t.radius}projectPoint(t,e){return e.copy(t).addScaledVector(this.normal,-this.distanceToPoint(t))}intersectLine(t,e,i=!0){let s=t.delta(qc),r=this.normal.dot(s);if(r===0)return this.distanceToPoint(t.start)===0?e.copy(t.start):null;let o=-(t.start.dot(this.normal)+this.constant)/r;return i===!0&&(o<0||o>1)?null:e.copy(t.start).addScaledVector(s,o)}intersectsLine(t){let e=this.distanceToPoint(t.start),i=this.distanceToPoint(t.end);return e<0&&i>0||i<0&&e>0}intersectsBox(t){return t.intersectsPlane(this)}intersectsSphere(t){return t.intersectsPlane(this)}coplanarPoint(t){return t.copy(this.normal).multiplyScalar(-this.constant)}applyMatrix4(t,e){let i=e||Lm.getNormalMatrix(t),s=this.coplanarPoint(qc).applyMatrix4(t),r=this.normal.applyMatrix3(i).normalize();return this.constant=-s.dot(r),this}translate(t){return this.constant-=t.dot(this.normal),this}equals(t){return t.normal.equals(this.normal)&&t.constant===this.constant}clone(){return new this.constructor().copy(this)}toJSON(){return{normal:this.normal.toArray(),constant:this.constant}}fromJSON(t){return this.normal.fromArray(t.normal),this.constant=t.constant,this}},Nm=0,Ui=class extends Ni{constructor(){super(),this.isMaterial=!0,Object.defineProperty(this,"id",{value:Nm++}),this.uuid=sr(),this.name="",this.type="Material",this.blending=Qs,this.side=Bn,this.vertexColors=!1,this.opacity=1,this.transparent=!1,this.alphaHash=!1,this.blendSrc=gh,this.blendDst=xh,this.blendEquation=gn,this.blendSrcAlpha=null,this.blendDstAlpha=null,this.blendEquationAlpha=null,this.blendColor=new bt(0,0,0),this.blendAlpha=0,this.depthFunc=Fs,this.depthTest=!0,this.depthWrite=!0,this.stencilWriteMask=255,this.stencilFunc=Xd,this.stencilRef=0,this.stencilFuncMask=255,this.stencilFail=pa,this.stencilZFail=pa,this.stencilZPass=pa,this.stencilWrite=!1,this.clippingPlanes=null,this.clipIntersection=!1,this.clipShadows=!1,this.shadowSide=null,this.colorWrite=!0,this.precision=null,this.polygonOffset=!1,this.polygonOffsetFactor=0,this.polygonOffsetUnits=0,this.dithering=!1,this.alphaToCoverage=!1,this.premultipliedAlpha=!1,this.forceSinglePass=!1,this.allowOverride=!0,this.visible=!0,this.toneMapped=!0,this.userData={},this.version=0,this._alphaTest=0}get alphaTest(){return this._alphaTest}set alphaTest(t){this._alphaTest>0!=t>0&&this.version++,this._alphaTest=t}onBeforeRender(){}onBeforeCompile(){}customProgramCacheKey(){return this.onBeforeCompile.toString()}setValues(t){if(t!==void 0)for(let e in t){let i=t[e];if(i===void 0){Lt(`Material: parameter '${e}' has value of undefined.`);continue}let s=this[e];if(s===void 0){Lt(`Material: '${e}' is not a property of THREE.${this.type}.`);continue}s&&s.isColor?s.set(i):s&&s.isVector2&&i&&i.isVector2||s&&s.isEuler&&i&&i.isEuler||s&&s.isVector3&&i&&i.isVector3?s.copy(i):this[e]=i}}toJSON(t){let e=t===void 0||typeof t=="string";e&&(t={textures:{},images:{}});let i={metadata:{version:4.7,type:"Material",generator:"Material.toJSON"}};i.uuid=this.uuid,i.type=this.type,i.blending=this.blending,i.side=this.side,i.shadowSide=this.shadowSide,i.vertexColors=this.vertexColors,i.opacity=this.opacity,i.transparent=this.transparent,i.blendSrc=this.blendSrc,i.blendDst=this.blendDst,i.blendEquation=this.blendEquation,i.blendSrcAlpha=this.blendSrcAlpha,i.blendDstAlpha=this.blendDstAlpha,i.blendEquationAlpha=this.blendEquationAlpha,i.blendColor=this.blendColor.getHex(),i.blendAlpha=this.blendAlpha,i.depthFunc=this.depthFunc,i.depthTest=this.depthTest,i.depthWrite=this.depthWrite,i.colorWrite=this.colorWrite,i.clipIntersection=this.clipIntersection,i.clipShadows=this.clipShadows,i.stencilWriteMask=this.stencilWriteMask,i.stencilFunc=this.stencilFunc,i.stencilRef=this.stencilRef,i.stencilFuncMask=this.stencilFuncMask,i.stencilFail=this.stencilFail,i.stencilZFail=this.stencilZFail,i.stencilZPass=this.stencilZPass,i.stencilWrite=this.stencilWrite,i.polygonOffset=this.polygonOffset,i.polygonOffsetFactor=this.polygonOffsetFactor,i.polygonOffsetUnits=this.polygonOffsetUnits,i.dithering=this.dithering,i.alphaTest=this.alphaTest,i.alphaHash=this.alphaHash,i.alphaToCoverage=this.alphaToCoverage,i.premultipliedAlpha=this.premultipliedAlpha,i.forceSinglePass=this.forceSinglePass,i.allowOverride=this.allowOverride,i.visible=this.visible,i.toneMapped=this.toneMapped,i.name=this.name,this.color&&this.color.isColor&&(i.color=this.color.getHex()),this.roughness!==void 0&&(i.roughness=this.roughness),this.metalness!==void 0&&(i.metalness=this.metalness),this.sheen!==void 0&&(i.sheen=this.sheen),this.sheenColor&&this.sheenColor.isColor&&(i.sheenColor=this.sheenColor.getHex()),this.sheenRoughness!==void 0&&(i.sheenRoughness=this.sheenRoughness),this.emissive&&this.emissive.isColor&&(i.emissive=this.emissive.getHex()),this.emissiveIntensity!==void 0&&(i.emissiveIntensity=this.emissiveIntensity),this.specular&&this.specular.isColor&&(i.specular=this.specular.getHex()),this.specularIntensity!==void 0&&(i.specularIntensity=this.specularIntensity),this.specularColor&&this.specularColor.isColor&&(i.specularColor=this.specularColor.getHex()),this.shininess!==void 0&&(i.shininess=this.shininess),this.clearcoat!==void 0&&(i.clearcoat=this.clearcoat),this.clearcoatRoughness!==void 0&&(i.clearcoatRoughness=this.clearcoatRoughness),this.diffuseRoughness!==void 0&&(i.diffuseRoughness=this.diffuseRoughness),this.diffuseRoughnessMap&&this.diffuseRoughnessMap.isTexture&&(i.diffuseRoughnessMap=this.diffuseRoughnessMap.toJSON(t).uuid),this.clearcoatMap&&this.clearcoatMap.isTexture&&(i.clearcoatMap=this.clearcoatMap.toJSON(t).uuid),this.clearcoatRoughnessMap&&this.clearcoatRoughnessMap.isTexture&&(i.clearcoatRoughnessMap=this.clearcoatRoughnessMap.toJSON(t).uuid),this.clearcoatNormalMap&&this.clearcoatNormalMap.isTexture&&(i.clearcoatNormalMap=this.clearcoatNormalMap.toJSON(t).uuid,i.clearcoatNormalScale=this.clearcoatNormalScale.toArray()),this.sheenColorMap&&this.sheenColorMap.isTexture&&(i.sheenColorMap=this.sheenColorMap.toJSON(t).uuid),this.sheenRoughnessMap&&this.sheenRoughnessMap.isTexture&&(i.sheenRoughnessMap=this.sheenRoughnessMap.toJSON(t).uuid),this.dispersion!==void 0&&(i.dispersion=this.dispersion),this.retroreflectivity!==void 0&&(i.retroreflectivity=this.retroreflectivity),this.iridescence!==void 0&&(i.iridescence=this.iridescence),this.iridescenceIOR!==void 0&&(i.iridescenceIOR=this.iridescenceIOR),this.iridescenceThicknessMinimum!==void 0&&(i.iridescenceThicknessMinimum=this.iridescenceThicknessMinimum),this.iridescenceThicknessMaximum!==void 0&&(i.iridescenceThicknessMaximum=this.iridescenceThicknessMaximum),this.iridescenceMap&&this.iridescenceMap.isTexture&&(i.iridescenceMap=this.iridescenceMap.toJSON(t).uuid),this.iridescenceThicknessMap&&this.iridescenceThicknessMap.isTexture&&(i.iridescenceThicknessMap=this.iridescenceThicknessMap.toJSON(t).uuid),this.anisotropy!==void 0&&(i.anisotropy=this.anisotropy),this.anisotropyRotation!==void 0&&(i.anisotropyRotation=this.anisotropyRotation),this.anisotropyMap&&this.anisotropyMap.isTexture&&(i.anisotropyMap=this.anisotropyMap.toJSON(t).uuid),this.map&&this.map.isTexture&&(i.map=this.map.toJSON(t).uuid),this.matcap&&this.matcap.isTexture&&(i.matcap=this.matcap.toJSON(t).uuid),this.alphaMap&&this.alphaMap.isTexture&&(i.alphaMap=this.alphaMap.toJSON(t).uuid),this.lightMap&&this.lightMap.isTexture&&(i.lightMap=this.lightMap.toJSON(t).uuid,i.lightMapIntensity=this.lightMapIntensity),this.aoMap&&this.aoMap.isTexture&&(i.aoMap=this.aoMap.toJSON(t).uuid,i.aoMapIntensity=this.aoMapIntensity),this.bumpMap&&this.bumpMap.isTexture&&(i.bumpMap=this.bumpMap.toJSON(t).uuid,i.bumpScale=this.bumpScale),this.normalMap&&this.normalMap.isTexture&&(i.normalMap=this.normalMap.toJSON(t).uuid,i.normalMapType=this.normalMapType,i.normalScale=this.normalScale.toArray()),this.displacementMap&&this.displacementMap.isTexture&&(i.displacementMap=this.displacementMap.toJSON(t).uuid,i.displacementScale=this.displacementScale,i.displacementBias=this.displacementBias),this.roughnessMap&&this.roughnessMap.isTexture&&(i.roughnessMap=this.roughnessMap.toJSON(t).uuid),this.metalnessMap&&this.metalnessMap.isTexture&&(i.metalnessMap=this.metalnessMap.toJSON(t).uuid),this.emissiveMap&&this.emissiveMap.isTexture&&(i.emissiveMap=this.emissiveMap.toJSON(t).uuid),this.specularMap&&this.specularMap.isTexture&&(i.specularMap=this.specularMap.toJSON(t).uuid),this.specularIntensityMap&&this.specularIntensityMap.isTexture&&(i.specularIntensityMap=this.specularIntensityMap.toJSON(t).uuid),this.specularColorMap&&this.specularColorMap.isTexture&&(i.specularColorMap=this.specularColorMap.toJSON(t).uuid),this.envMap&&this.envMap.isTexture&&(i.envMap=this.envMap.toJSON(t).uuid,this.combine!==void 0&&(i.combine=this.combine)),this.envMapRotation!==void 0&&(i.envMapRotation=this.envMapRotation.toArray()),this.envMapIntensity!==void 0&&(i.envMapIntensity=this.envMapIntensity),this.reflectivity!==void 0&&(i.reflectivity=this.reflectivity),this.refractionRatio!==void 0&&(i.refractionRatio=this.refractionRatio),this.gradientMap&&this.gradientMap.isTexture&&(i.gradientMap=this.gradientMap.toJSON(t).uuid),this.transmission!==void 0&&(i.transmission=this.transmission),this.transmissionMap&&this.transmissionMap.isTexture&&(i.transmissionMap=this.transmissionMap.toJSON(t).uuid),this.thickness!==void 0&&(i.thickness=this.thickness),this.thicknessMap&&this.thicknessMap.isTexture&&(i.thicknessMap=this.thicknessMap.toJSON(t).uuid),this.attenuationDistance!==void 0&&(i.attenuationDistance=this.attenuationDistance),this.attenuationColor!==void 0&&(i.attenuationColor=this.attenuationColor.getHex()),this.size!==void 0&&(i.size=this.size),this.sizeAttenuation!==void 0&&(i.sizeAttenuation=this.sizeAttenuation),Array.isArray(this.clippingPlanes)&&this.clippingPlanes.length>0&&(i.clippingPlanes=this.clippingPlanes.map(r=>r.toJSON())),this.rotation!==void 0&&(i.rotation=this.rotation),this.depthPacking!==void 0&&(i.depthPacking=this.depthPacking),this.linewidth!==void 0&&(i.linewidth=this.linewidth),this.linecap!==void 0&&(i.linecap=this.linecap),this.linejoin!==void 0&&(i.linejoin=this.linejoin),this.dashSize!==void 0&&(i.dashSize=this.dashSize),this.gapSize!==void 0&&(i.gapSize=this.gapSize),this.scale!==void 0&&(i.scale=this.scale),this.wireframe!==void 0&&(i.wireframe=this.wireframe),this.wireframeLinewidth!==void 0&&(i.wireframeLinewidth=this.wireframeLinewidth),this.wireframeLinecap!==void 0&&(i.wireframeLinecap=this.wireframeLinecap),this.wireframeLinejoin!==void 0&&(i.wireframeLinejoin=this.wireframeLinejoin),this.flatShading!==void 0&&(i.flatShading=this.flatShading),this.fog!==void 0&&(i.fog=this.fog),Object.keys(this.userData).length>0&&(i.userData=this.userData);function s(r){let o=[];for(let a in r){let l=r[a];delete l.metadata,o.push(l)}return o}if(e){let r=s(t.textures),o=s(t.images);r.length>0&&(i.textures=r),o.length>0&&(i.images=o)}return i}fromJSON(t,e){if(t.uuid!==void 0&&(this.uuid=t.uuid),t.name!==void 0&&(this.name=t.name),t.color!==void 0&&this.color!==void 0&&this.color.setHex(t.color),t.roughness!==void 0&&(this.roughness=t.roughness),t.metalness!==void 0&&(this.metalness=t.metalness),t.sheen!==void 0&&(this.sheen=t.sheen),t.sheenColor!==void 0&&(this.sheenColor=new bt().setHex(t.sheenColor)),t.sheenRoughness!==void 0&&(this.sheenRoughness=t.sheenRoughness),t.emissive!==void 0&&this.emissive!==void 0&&this.emissive.setHex(t.emissive),t.specular!==void 0&&this.specular!==void 0&&this.specular.setHex(t.specular),t.specularIntensity!==void 0&&(this.specularIntensity=t.specularIntensity),t.specularColor!==void 0&&this.specularColor!==void 0&&this.specularColor.setHex(t.specularColor),t.shininess!==void 0&&(this.shininess=t.shininess),t.clearcoat!==void 0&&(this.clearcoat=t.clearcoat),t.clearcoatRoughness!==void 0&&(this.clearcoatRoughness=t.clearcoatRoughness),t.diffuseRoughness!==void 0&&(this.diffuseRoughness=t.diffuseRoughness),t.dispersion!==void 0&&(this.dispersion=t.dispersion),t.retroreflectivity!==void 0&&(this.retroreflectivity=t.retroreflectivity),t.iridescence!==void 0&&(this.iridescence=t.iridescence),t.iridescenceIOR!==void 0&&(this.iridescenceIOR=t.iridescenceIOR),t.iridescenceThicknessMinimum!==void 0&&(this.iridescenceThicknessMinimum=t.iridescenceThicknessMinimum),t.iridescenceThicknessMaximum!==void 0&&(this.iridescenceThicknessMaximum=t.iridescenceThicknessMaximum),t.iridescenceThicknessRange!==void 0&&(this.iridescenceThicknessMinimum=t.iridescenceThicknessRange[0],this.iridescenceThicknessMaximum=t.iridescenceThicknessRange[1]),t.transmission!==void 0&&(this.transmission=t.transmission),t.thickness!==void 0&&(this.thickness=t.thickness),t.attenuationDistance!==void 0&&(this.attenuationDistance=t.attenuationDistance),t.attenuationColor!==void 0&&this.attenuationColor!==void 0&&this.attenuationColor.setHex(t.attenuationColor),t.anisotropy!==void 0&&(this.anisotropy=t.anisotropy),t.anisotropyRotation!==void 0&&(this.anisotropyRotation=t.anisotropyRotation),t.fog!==void 0&&(this.fog=t.fog),t.flatShading!==void 0&&(this.flatShading=t.flatShading),t.blending!==void 0&&(this.blending=t.blending),t.combine!==void 0&&(this.combine=t.combine),t.side!==void 0&&(this.side=t.side),t.shadowSide!==void 0&&(this.shadowSide=t.shadowSide),t.opacity!==void 0&&(this.opacity=t.opacity),t.transparent!==void 0&&(this.transparent=t.transparent),t.alphaTest!==void 0&&(this.alphaTest=t.alphaTest),t.alphaHash!==void 0&&(this.alphaHash=t.alphaHash),t.depthFunc!==void 0&&(this.depthFunc=t.depthFunc),t.depthTest!==void 0&&(this.depthTest=t.depthTest),t.depthWrite!==void 0&&(this.depthWrite=t.depthWrite),t.colorWrite!==void 0&&(this.colorWrite=t.colorWrite),t.clippingPlanes!==void 0&&(this.clippingPlanes=t.clippingPlanes.map(i=>new li().fromJSON(i))),t.clipIntersection!==void 0&&(this.clipIntersection=t.clipIntersection),t.clipShadows!==void 0&&(this.clipShadows=t.clipShadows),t.depthPacking!==void 0&&(this.depthPacking=t.depthPacking),t.blendSrc!==void 0&&(this.blendSrc=t.blendSrc),t.blendDst!==void 0&&(this.blendDst=t.blendDst),t.blendEquation!==void 0&&(this.blendEquation=t.blendEquation),t.blendSrcAlpha!==void 0&&(this.blendSrcAlpha=t.blendSrcAlpha),t.blendDstAlpha!==void 0&&(this.blendDstAlpha=t.blendDstAlpha),t.blendEquationAlpha!==void 0&&(this.blendEquationAlpha=t.blendEquationAlpha),t.blendColor!==void 0&&this.blendColor!==void 0&&this.blendColor.setHex(t.blendColor),t.blendAlpha!==void 0&&(this.blendAlpha=t.blendAlpha),t.stencilWriteMask!==void 0&&(this.stencilWriteMask=t.stencilWriteMask),t.stencilFunc!==void 0&&(this.stencilFunc=t.stencilFunc),t.stencilRef!==void 0&&(this.stencilRef=t.stencilRef),t.stencilFuncMask!==void 0&&(this.stencilFuncMask=t.stencilFuncMask),t.stencilFail!==void 0&&(this.stencilFail=t.stencilFail),t.stencilZFail!==void 0&&(this.stencilZFail=t.stencilZFail),t.stencilZPass!==void 0&&(this.stencilZPass=t.stencilZPass),t.stencilWrite!==void 0&&(this.stencilWrite=t.stencilWrite),t.wireframe!==void 0&&(this.wireframe=t.wireframe),t.wireframeLinewidth!==void 0&&(this.wireframeLinewidth=t.wireframeLinewidth),t.wireframeLinecap!==void 0&&(this.wireframeLinecap=t.wireframeLinecap),t.wireframeLinejoin!==void 0&&(this.wireframeLinejoin=t.wireframeLinejoin),t.rotation!==void 0&&(this.rotation=t.rotation),t.linewidth!==void 0&&(this.linewidth=t.linewidth),t.linecap!==void 0&&(this.linecap=t.linecap),t.linejoin!==void 0&&(this.linejoin=t.linejoin),t.dashSize!==void 0&&(this.dashSize=t.dashSize),t.gapSize!==void 0&&(this.gapSize=t.gapSize),t.scale!==void 0&&(this.scale=t.scale),t.polygonOffset!==void 0&&(this.polygonOffset=t.polygonOffset),t.polygonOffsetFactor!==void 0&&(this.polygonOffsetFactor=t.polygonOffsetFactor),t.polygonOffsetUnits!==void 0&&(this.polygonOffsetUnits=t.polygonOffsetUnits),t.dithering!==void 0&&(this.dithering=t.dithering),t.alphaToCoverage!==void 0&&(this.alphaToCoverage=t.alphaToCoverage),t.premultipliedAlpha!==void 0&&(this.premultipliedAlpha=t.premultipliedAlpha),t.forceSinglePass!==void 0&&(this.forceSinglePass=t.forceSinglePass),t.allowOverride!==void 0&&(this.allowOverride=t.allowOverride),t.visible!==void 0&&(this.visible=t.visible),t.toneMapped!==void 0&&(this.toneMapped=t.toneMapped),t.userData!==void 0&&(this.userData=t.userData),t.vertexColors!==void 0&&(typeof t.vertexColors=="number"?this.vertexColors=t.vertexColors>0:this.vertexColors=t.vertexColors),t.size!==void 0&&(this.size=t.size),t.sizeAttenuation!==void 0&&(this.sizeAttenuation=t.sizeAttenuation),t.map!==void 0&&(this.map=e[t.map]||null),t.matcap!==void 0&&(this.matcap=e[t.matcap]||null),t.alphaMap!==void 0&&(this.alphaMap=e[t.alphaMap]||null),t.bumpMap!==void 0&&(this.bumpMap=e[t.bumpMap]||null),t.bumpScale!==void 0&&(this.bumpScale=t.bumpScale),t.normalMap!==void 0&&(this.normalMap=e[t.normalMap]||null),t.normalMapType!==void 0&&(this.normalMapType=t.normalMapType),t.normalScale!==void 0){let i=t.normalScale;Array.isArray(i)===!1&&(i=[i,i]),this.normalScale=new et().fromArray(i)}return t.displacementMap!==void 0&&(this.displacementMap=e[t.displacementMap]||null),t.displacementScale!==void 0&&(this.displacementScale=t.displacementScale),t.displacementBias!==void 0&&(this.displacementBias=t.displacementBias),t.roughnessMap!==void 0&&(this.roughnessMap=e[t.roughnessMap]||null),t.metalnessMap!==void 0&&(this.metalnessMap=e[t.metalnessMap]||null),t.emissiveMap!==void 0&&(this.emissiveMap=e[t.emissiveMap]||null),t.emissiveIntensity!==void 0&&(this.emissiveIntensity=t.emissiveIntensity),t.specularMap!==void 0&&(this.specularMap=e[t.specularMap]||null),t.specularIntensityMap!==void 0&&(this.specularIntensityMap=e[t.specularIntensityMap]||null),t.specularColorMap!==void 0&&(this.specularColorMap=e[t.specularColorMap]||null),t.envMap!==void 0&&(this.envMap=e[t.envMap]||null),t.envMapRotation!==void 0&&this.envMapRotation.fromArray(t.envMapRotation),t.envMapIntensity!==void 0&&(this.envMapIntensity=t.envMapIntensity),t.reflectivity!==void 0&&(this.reflectivity=t.reflectivity),t.refractionRatio!==void 0&&(this.refractionRatio=t.refractionRatio),t.lightMap!==void 0&&(this.lightMap=e[t.lightMap]||null),t.lightMapIntensity!==void 0&&(this.lightMapIntensity=t.lightMapIntensity),t.aoMap!==void 0&&(this.aoMap=e[t.aoMap]||null),t.aoMapIntensity!==void 0&&(this.aoMapIntensity=t.aoMapIntensity),t.gradientMap!==void 0&&(this.gradientMap=e[t.gradientMap]||null),t.clearcoatMap!==void 0&&(this.clearcoatMap=e[t.clearcoatMap]||null),t.clearcoatRoughnessMap!==void 0&&(this.clearcoatRoughnessMap=e[t.clearcoatRoughnessMap]||null),t.clearcoatNormalMap!==void 0&&(this.clearcoatNormalMap=e[t.clearcoatNormalMap]||null),t.clearcoatNormalScale!==void 0&&(this.clearcoatNormalScale=new et().fromArray(t.clearcoatNormalScale)),t.diffuseRoughnessMap!==void 0&&(this.diffuseRoughnessMap=e[t.diffuseRoughnessMap]||null),t.iridescenceMap!==void 0&&(this.iridescenceMap=e[t.iridescenceMap]||null),t.iridescenceThicknessMap!==void 0&&(this.iridescenceThicknessMap=e[t.iridescenceThicknessMap]||null),t.transmissionMap!==void 0&&(this.transmissionMap=e[t.transmissionMap]||null),t.thicknessMap!==void 0&&(this.thicknessMap=e[t.thicknessMap]||null),t.anisotropyMap!==void 0&&(this.anisotropyMap=e[t.anisotropyMap]||null),t.sheenColorMap!==void 0&&(this.sheenColorMap=e[t.sheenColorMap]||null),t.sheenRoughnessMap!==void 0&&(this.sheenRoughnessMap=e[t.sheenRoughnessMap]||null),this}clone(){return new this.constructor().copy(this)}copy(t){this.name=t.name,this.blending=t.blending,this.side=t.side,this.vertexColors=t.vertexColors,this.opacity=t.opacity,this.transparent=t.transparent,this.blendSrc=t.blendSrc,this.blendDst=t.blendDst,this.blendEquation=t.blendEquation,this.blendSrcAlpha=t.blendSrcAlpha,this.blendDstAlpha=t.blendDstAlpha,this.blendEquationAlpha=t.blendEquationAlpha,this.blendColor.copy(t.blendColor),this.blendAlpha=t.blendAlpha,this.depthFunc=t.depthFunc,this.depthTest=t.depthTest,this.depthWrite=t.depthWrite,this.stencilWriteMask=t.stencilWriteMask,this.stencilFunc=t.stencilFunc,this.stencilRef=t.stencilRef,this.stencilFuncMask=t.stencilFuncMask,this.stencilFail=t.stencilFail,this.stencilZFail=t.stencilZFail,this.stencilZPass=t.stencilZPass,this.stencilWrite=t.stencilWrite;let e=t.clippingPlanes,i=null;if(e!==null){let s=e.length;i=new Array(s);for(let r=0;r!==s;++r)i[r]=e[r].clone()}return this.clippingPlanes=i,this.clipIntersection=t.clipIntersection,this.clipShadows=t.clipShadows,this.shadowSide=t.shadowSide,this.colorWrite=t.colorWrite,this.precision=t.precision,this.polygonOffset=t.polygonOffset,this.polygonOffsetFactor=t.polygonOffsetFactor,this.polygonOffsetUnits=t.polygonOffsetUnits,this.dithering=t.dithering,this.alphaTest=t.alphaTest,this.alphaHash=t.alphaHash,this.alphaToCoverage=t.alphaToCoverage,this.premultipliedAlpha=t.premultipliedAlpha,this.forceSinglePass=t.forceSinglePass,this.allowOverride=t.allowOverride,this.visible=t.visible,this.toneMapped=t.toneMapped,this.userData=JSON.parse(JSON.stringify(t.userData)),this}dispose(){this.dispatchEvent({type:"dispose"})}set needsUpdate(t){t===!0&&this.version++}};var an=new C,Yc=new C,qo=new C,Yo=new C,dn=class{constructor(t=new C,e=new C(0,0,-1)){this.origin=t,this.direction=e}set(t,e){return this.origin.copy(t),this.direction.copy(e),this}copy(t){return this.origin.copy(t.origin),this.direction.copy(t.direction),this}at(t,e){return e.copy(this.origin).addScaledVector(this.direction,t)}lookAt(t){return this.direction.copy(t).sub(this.origin).normalize(),this}recast(t){return this.origin.copy(this.at(t,an)),this}closestPointToPoint(t,e){e.subVectors(t,this.origin);let i=e.dot(this.direction);return i<0?e.copy(this.origin):e.copy(this.origin).addScaledVector(this.direction,i)}distanceToPoint(t){return Math.sqrt(this.distanceSqToPoint(t))}distanceSqToPoint(t){let e=an.subVectors(t,this.origin).dot(this.direction);return e<0?this.origin.distanceToSquared(t):(an.copy(this.origin).addScaledVector(this.direction,e),an.distanceToSquared(t))}distanceSqToSegment(t,e,i,s){Yc.copy(t).add(e).multiplyScalar(.5),qo.copy(e).sub(t).normalize(),Yo.copy(this.origin).sub(Yc);let r=t.distanceTo(e)*.5,o=-this.direction.dot(qo),a=Yo.dot(this.direction),l=-Yo.dot(qo),c=Yo.lengthSq(),h=Math.abs(1-o*o),u,d,f,m;if(h>0)if(u=o*l-a,d=o*a-l,m=r*h,u>=0)if(d>=-m)if(d<=m){let x=1/h;u*=x,d*=x,f=u*(u+o*d+2*a)+d*(o*u+d+2*l)+c}else d=r,u=Math.max(0,-(o*d+a)),f=-u*u+d*(d+2*l)+c;else d=-r,u=Math.max(0,-(o*d+a)),f=-u*u+d*(d+2*l)+c;else d<=-m?(u=Math.max(0,-(-o*r+a)),d=u>0?-r:Math.min(Math.max(-r,-l),r),f=-u*u+d*(d+2*l)+c):d<=m?(u=0,d=Math.min(Math.max(-r,-l),r),f=d*(d+2*l)+c):(u=Math.max(0,-(o*r+a)),d=u>0?r:Math.min(Math.max(-r,-l),r),f=-u*u+d*(d+2*l)+c);else d=o>0?-r:r,u=Math.max(0,-(o*d+a)),f=-u*u+d*(d+2*l)+c;return i&&i.copy(this.origin).addScaledVector(this.direction,u),s&&s.copy(Yc).addScaledVector(qo,d),f}intersectSphere(t,e){if(t.radius<0)return null;an.subVectors(t.center,this.origin);let i=an.dot(this.direction),s=an.dot(an)-i*i,r=t.radius*t.radius;if(s>r)return null;let o=Math.sqrt(r-s),a=i-o,l=i+o;return l<0?null:a<0?this.at(l,e):this.at(a,e)}intersectsSphere(t){return t.radius<0?!1:this.distanceSqToPoint(t.center)<=t.radius*t.radius}distanceToPlane(t){let e=t.normal.dot(this.direction);if(e===0)return t.distanceToPoint(this.origin)===0?0:null;let i=-(this.origin.dot(t.normal)+t.constant)/e;return i>=0?i:null}intersectPlane(t,e){let i=this.distanceToPlane(t);return i===null?null:this.at(i,e)}intersectsPlane(t){let e=t.distanceToPoint(this.origin);return e===0||t.normal.dot(this.direction)*e<0}intersectBox(t,e){let i,s,r,o,a,l,c=1/this.direction.x,h=1/this.direction.y,u=1/this.direction.z,d=this.origin;return c>=0?(i=(t.min.x-d.x)*c,s=(t.max.x-d.x)*c):(i=(t.max.x-d.x)*c,s=(t.min.x-d.x)*c),h>=0?(r=(t.min.y-d.y)*h,o=(t.max.y-d.y)*h):(r=(t.max.y-d.y)*h,o=(t.min.y-d.y)*h),i>o||r>s||((r>i||isNaN(i))&&(i=r),(o<s||isNaN(s))&&(s=o),u>=0?(a=(t.min.z-d.z)*u,l=(t.max.z-d.z)*u):(a=(t.max.z-d.z)*u,l=(t.min.z-d.z)*u),i>l||a>s)||((a>i||i!==i)&&(i=a),(l<s||s!==s)&&(s=l),s<0)?null:this.at(i>=0?i:s,e)}intersectsBox(t){return this.intersectBox(t,an)!==null}intersectTriangle(t,e,i,s,r){let o=this.origin,a=this.direction,l=a.x,c=a.y,h=a.z,u=t.x-o.x,d=t.y-o.y,f=t.z-o.z,m=e.x-o.x,x=e.y-o.y,p=e.z-o.z,g=i.x-o.x,y=i.y-o.y,v=i.z-o.z,S=Math.abs(l),T=Math.abs(c),b=Math.abs(h),_,M,D,P,A,R,U,L,B,H,z,Q;if(S>=T&&S>=b?(D=l,R=u,B=m,Q=g,l>=0?(_=c,M=h,P=d,A=f,U=x,L=p,H=y,z=v):(_=h,M=c,P=f,A=d,U=p,L=x,H=v,z=y)):T>=b?(D=c,R=d,B=x,Q=y,c>=0?(_=h,M=l,P=f,A=u,U=p,L=m,H=v,z=g):(_=l,M=h,P=u,A=f,U=m,L=p,H=g,z=v)):(D=h,R=f,B=p,Q=v,h>=0?(_=l,M=c,P=u,A=d,U=m,L=x,H=g,z=y):(_=c,M=l,P=d,A=u,U=x,L=m,H=y,z=g)),D===0)return null;let W=_/D,Z=M/D,$=1/D,xt=P-W*R,Mt=A-Z*R,q=U-W*B,st=L-Z*B,_t=H-W*Q,wt=z-Z*Q,rt=_t*st-wt*q,Pt=xt*wt-Mt*_t,Qt=q*Mt-st*xt;if(s){if(rt<0||Pt<0||Qt<0)return null}else if((rt<0||Pt<0||Qt<0)&&(rt>0||Pt>0||Qt>0))return null;let ot=rt+Pt+Qt;if(ot===0)return null;let Ct=$*(rt*R+Pt*B+Qt*Q);return(ot>0?Ct<0:Ct>0)?null:this.at(Ct/ot,r)}applyMatrix4(t){return this.origin.applyMatrix4(t),this.direction.transformDirection(t),this}equals(t){return t.origin.equals(this.origin)&&t.direction.equals(this.direction)}clone(){return new this.constructor().copy(this)}},pi=class extends Ui{constructor(t){super(),this.isMeshBasicMaterial=!0,this.type="MeshBasicMaterial",this.color=new bt(16777215),this.map=null,this.lightMap=null,this.lightMapIntensity=1,this.aoMap=null,this.aoMapIntensity=1,this.specularMap=null,this.alphaMap=null,this.envMap=null,this.envMapRotation=new Ai,this.combine=Ja,this.reflectivity=1,this.refractionRatio=.98,this.wireframe=!1,this.wireframeLinewidth=1,this.wireframeLinecap="round",this.wireframeLinejoin="round",this.fog=!0,this.setValues(t)}copy(t){return super.copy(t),this.color.copy(t.color),this.map=t.map,this.lightMap=t.lightMap,this.lightMapIntensity=t.lightMapIntensity,this.aoMap=t.aoMap,this.aoMapIntensity=t.aoMapIntensity,this.specularMap=t.specularMap,this.alphaMap=t.alphaMap,this.envMap=t.envMap,this.envMapRotation.copy(t.envMapRotation),this.combine=t.combine,this.reflectivity=t.reflectivity,this.refractionRatio=t.refractionRatio,this.wireframe=t.wireframe,this.wireframeLinewidth=t.wireframeLinewidth,this.wireframeLinecap=t.wireframeLinecap,this.wireframeLinejoin=t.wireframeLinejoin,this.fog=t.fog,this}},ju=new Xt,Kn=new dn,Zo=new ci,Qu=new C,Ko=new C,Jo=new C,$o=new C,Zc=new C,jo=new C,td=new C,Qo=new C,Ut=class extends Me{constructor(t=new Jt,e=new pi){super(),this.isMesh=!0,this.type="Mesh",this.geometry=t,this.material=e,this.morphTargetDictionary=void 0,this.morphTargetInfluences=void 0,this.count=1,this.updateMorphTargets()}copy(t,e){return super.copy(t,e),t.morphTargetInfluences!==void 0?this.morphTargetInfluences=t.morphTargetInfluences.slice():this.morphTargetInfluences=void 0,t.morphTargetDictionary!==void 0?this.morphTargetDictionary=Object.assign({},t.morphTargetDictionary):this.morphTargetDictionary=void 0,this.material=Array.isArray(t.material)?t.material.slice():t.material,this.geometry=t.geometry,this}updateMorphTargets(){let e=this.geometry.morphAttributes,i=Object.keys(e);if(i.length>0){let s=e[i[0]];if(s!==void 0){this.morphTargetInfluences=[],this.morphTargetDictionary={};for(let r=0,o=s.length;r<o;r++){let a=s[r].name||String(r);this.morphTargetInfluences.push(0),this.morphTargetDictionary[a]=r}}}}getVertexPosition(t,e){let i=this.geometry,s=i.attributes.position,r=i.morphAttributes.position,o=i.morphTargetsRelative;e.fromBufferAttribute(s,t);let a=this.morphTargetInfluences;if(r&&a){jo.set(0,0,0);for(let l=0,c=r.length;l<c;l++){let h=a[l],u=r[l];h!==0&&(Zc.fromBufferAttribute(u,t),o?jo.addScaledVector(Zc,h):jo.addScaledVector(Zc.sub(e),h))}e.add(jo)}return e}intersectsFrustum(t){return t.intersectsObject(this)}raycast(t,e){let i=this.geometry,s=this.material,r=this.matrixWorld;s!==void 0&&(i.boundingSphere===null&&i.computeBoundingSphere(),Zo.copy(i.boundingSphere),Zo.applyMatrix4(r),Kn.copy(t.ray).recast(t.near),!(Zo.containsPoint(Kn.origin)===!1&&(Kn.intersectSphere(Zo,Qu)===null||Kn.origin.distanceToSquared(Qu)>(t.far-t.near)**2))&&(ju.copy(r).invert(),Kn.copy(t.ray).applyMatrix4(ju),!(i.boundingBox!==null&&Kn.intersectsBox(i.boundingBox)===!1)&&this._computeIntersections(t,e,Kn)))}_computeIntersections(t,e,i){let s,r=this.geometry,o=this.material,a=r.index,l=r.attributes.position,c=r.attributes.uv,h=r.attributes.uv1,u=r.attributes.normal,d=r.groups,f=r.drawRange;if(a!==null)if(Array.isArray(o))for(let m=0,x=d.length;m<x;m++){let p=d[m],g=o[p.materialIndex],y=Math.max(p.start,f.start),v=Math.min(a.count,Math.min(p.start+p.count,f.start+f.count));for(let S=y,T=v;S<T;S+=3){let b=a.getX(S),_=a.getX(S+1),M=a.getX(S+2);s=ta(this,g,t,i,c,h,u,b,_,M),s&&(s.faceIndex=Math.floor(S/3),s.face.materialIndex=p.materialIndex,e.push(s))}}else{let m=Math.max(0,f.start),x=Math.min(a.count,f.start+f.count);for(let p=m,g=x;p<g;p+=3){let y=a.getX(p),v=a.getX(p+1),S=a.getX(p+2);s=ta(this,o,t,i,c,h,u,y,v,S),s&&(s.faceIndex=Math.floor(p/3),e.push(s))}}else if(l!==void 0)if(Array.isArray(o))for(let m=0,x=d.length;m<x;m++){let p=d[m],g=o[p.materialIndex],y=Math.max(p.start,f.start),v=Math.min(l.count,Math.min(p.start+p.count,f.start+f.count));for(let S=y,T=v;S<T;S+=3){let b=S,_=S+1,M=S+2;s=ta(this,g,t,i,c,h,u,b,_,M),s&&(s.faceIndex=Math.floor(S/3),s.face.materialIndex=p.materialIndex,e.push(s))}}else{let m=Math.max(0,f.start),x=Math.min(l.count,f.start+f.count);for(let p=m,g=x;p<g;p+=3){let y=p,v=p+1,S=p+2;s=ta(this,o,t,i,c,h,u,y,v,S),s&&(s.faceIndex=Math.floor(p/3),e.push(s))}}}};function Um(n,t,e,i,s,r,o,a){let l;if(t.side===Ze?l=i.intersectTriangle(o,r,s,!0,a):l=i.intersectTriangle(s,r,o,t.side===Bn,a),l===null)return null;Qo.copy(a),Qo.applyMatrix4(n.matrixWorld);let c=e.ray.origin.distanceTo(Qo);return c<e.near||c>e.far?null:{distance:c,point:Qo.clone(),object:n}}function ta(n,t,e,i,s,r,o,a,l,c){n.getVertexPosition(a,Ko),n.getVertexPosition(l,Jo),n.getVertexPosition(c,$o);let h=Um(n,t,e,i,Ko,Jo,$o,td);if(h){let u=new C;ln.getBarycoord(td,Ko,Jo,$o,u),s&&(h.uv=ln.getInterpolatedAttribute(s,a,l,c,u,new et)),r&&(h.uv1=ln.getInterpolatedAttribute(r,a,l,c,u,new et)),o&&(h.normal=ln.getInterpolatedAttribute(o,a,l,c,u,new C),h.normal.dot(i.direction)>0&&h.normal.multiplyScalar(-1));let d={a,b:l,c,normal:new C,materialIndex:0};ln.getNormal(Ko,Jo,$o,d.normal),h.face=d,h.barycoord=u}return h}var Fi=class extends qe{constructor(t=null,e=1,i=1,s,r,o,a,l,c=De,h=De,u,d){super(null,o,a,l,c,h,s,r,u,d),this.isDataTexture=!0,this.image={data:t,width:e,height:i},this.generateMipmaps=!1,this.flipY=!1,this.unpackAlignment=1}};var Qn=class extends le{constructor(t,e,i,s=1){super(t,e,i),this.isInstancedBufferAttribute=!0,this.meshPerAttribute=s}copy(t){return super.copy(t),this.meshPerAttribute=t.meshPerAttribute,this}toJSON(){let t=super.toJSON();return t.meshPerAttribute=this.meshPerAttribute,t.isInstancedBufferAttribute=!0,t}},Ps=new Xt,ed=new Xt,ea=[],id=new Xi,Fm=new Xt,wr=new Ut,Er=new ci,ts=class extends Ut{constructor(t,e,i){super(t,e),this.isInstancedMesh=!0,this.instanceMatrix=new Qn(new Float32Array(i*16),16),this.instanceColor=null,this.morphTexture=null,this.count=i,this.boundingBox=null,this.boundingSphere=null;for(let s=0;s<i;s++)this.setMatrixAt(s,Fm)}computeBoundingBox(){let t=this.geometry,e=this.count;this.boundingBox===null&&(this.boundingBox=new Xi),t.boundingBox===null&&t.computeBoundingBox(),this.boundingBox.makeEmpty();for(let i=0;i<e;i++)this.getMatrixAt(i,Ps),id.copy(t.boundingBox).applyMatrix4(Ps),this.boundingBox.union(id)}computeBoundingSphere(){let t=this.geometry,e=this.count;this.boundingSphere===null&&(this.boundingSphere=new ci),t.boundingSphere===null&&t.computeBoundingSphere(),this.boundingSphere.makeEmpty();for(let i=0;i<e;i++)this.getMatrixAt(i,Ps),Er.copy(t.boundingSphere).applyMatrix4(Ps),this.boundingSphere.union(Er)}copy(t,e){return super.copy(t,e),this.instanceMatrix.copy(t.instanceMatrix),this.morphTexture=t.morphTexture!==null?t.morphTexture.clone():null,this.instanceColor=t.instanceColor!==null?t.instanceColor.clone():null,this.count=t.count,this.boundingBox=t.boundingBox!==null?t.boundingBox.clone():null,this.boundingSphere=t.boundingSphere!==null?t.boundingSphere.clone():null,this}getColorAt(t,e){return this.instanceColor===null?e.setRGB(1,1,1):e.fromArray(this.instanceColor.array,t*3)}getMatrixAt(t,e){return e.fromArray(this.instanceMatrix.array,t*16)}getMorphAt(t,e){let i=e.morphTargetInfluences,s=this.morphTexture.source.data.data,r=i.length+1,o=t*r+1;for(let a=0;a<i.length;a++)i[a]=s[o+a]}raycast(t,e){let i=this.matrixWorld,s=this.count;if(wr.geometry=this.geometry,wr.material=this.material,wr.material!==void 0&&(this.boundingSphere===null&&this.computeBoundingSphere(),Er.copy(this.boundingSphere),Er.applyMatrix4(i),t.ray.intersectsSphere(Er)!==!1))for(let r=0;r<s;r++){this.getMatrixAt(r,Ps),ed.multiplyMatrices(i,Ps),wr.matrixWorld=ed,wr.raycast(t,ea);for(let o=0,a=ea.length;o<a;o++){let l=ea[o];l.instanceId=r,l.object=this,e.push(l)}ea.length=0}}setColorAt(t,e){return this.instanceColor===null&&(this.instanceColor=new Qn(new Float32Array(this.instanceMatrix.count*3).fill(1),3)),e.toArray(this.instanceColor.array,t*3),this}setMatrixAt(t,e){return e.toArray(this.instanceMatrix.array,t*16),this}setMorphAt(t,e){let i=e.morphTargetInfluences,s=i.length+1;this.morphTexture===null&&(this.morphTexture=new Fi(new Float32Array(s*this.count),s,this.count,zn,gi));let r=this.morphTexture.source.data.data,o=0;for(let c=0;c<i.length;c++)o+=i[c];let a=this.geometry.morphTargetsRelative?1:1-o,l=s*t;return r[l]=a,r.set(i,l+1),this}updateMorphTargets(){}dispose(){super.dispose(),this.instanceMatrix.dispose(),this.instanceColor!==null&&this.instanceColor.dispose(),this.morphTexture!==null&&(this.morphTexture.dispose(),this.morphTexture=null)}},Jn=new ci,Om=new et(.5,.5),ia=new C,Dn=class{constructor(t=new li,e=new li,i=new li,s=new li,r=new li,o=new li){this.planes=[t,e,i,s,r,o]}set(t,e,i,s,r,o){let a=this.planes;return a[0].copy(t),a[1].copy(e),a[2].copy(i),a[3].copy(s),a[4].copy(r),a[5].copy(o),this}copy(t){let e=this.planes;for(let i=0;i<6;i++)e[i].copy(t.planes[i]);return this}setFromProjectionMatrix(t,e=Ti,i=!1){let s=this.planes,r=t.elements,o=r[0],a=r[1],l=r[2],c=r[3],h=r[4],u=r[5],d=r[6],f=r[7],m=r[8],x=r[9],p=r[10],g=r[11],y=r[12],v=r[13],S=r[14],T=r[15];if(s[0].setComponents(c-o,f-h,g-m,T-y).normalize(),s[1].setComponents(c+o,f+h,g+m,T+y).normalize(),s[2].setComponents(c+a,f+u,g+x,T+v).normalize(),s[3].setComponents(c-a,f-u,g-x,T-v).normalize(),i)s[4].setComponents(l,d,p,S).normalize(),s[5].setComponents(c-l,f-d,g-p,T-S).normalize();else if(s[4].setComponents(c-l,f-d,g-p,T-S).normalize(),e===Ti)s[5].setComponents(c+l,f+d,g+p,T+S).normalize();else if(e===Os)s[5].setComponents(l,d,p,S).normalize();else throw new Error("THREE.Frustum.setFromProjectionMatrix(): Invalid coordinate system: "+e);return this}intersectsObject(t){if(t.boundingSphere!==void 0)t.boundingSphere===null&&t.computeBoundingSphere(),Jn.copy(t.boundingSphere).applyMatrix4(t.matrixWorld);else{let e=t.geometry;e.boundingSphere===null&&e.computeBoundingSphere(),Jn.copy(e.boundingSphere).applyMatrix4(t.matrixWorld)}return this.intersectsSphere(Jn)}intersectsSprite(t){Jn.center.set(0,0,0);let e=Om.distanceTo(t.center);return Jn.radius=.7071067811865476+e,Jn.applyMatrix4(t.matrixWorld),this.intersectsSphere(Jn)}intersectsSphere(t){let e=this.planes,i=t.center,s=-t.radius;for(let r=0;r<6;r++)if(e[r].distanceToPoint(i)<s)return!1;return!0}intersectsBox(t){let e=this.planes;for(let i=0;i<6;i++){let s=e[i];if(ia.x=s.normal.x>0?t.max.x:t.min.x,ia.y=s.normal.y>0?t.max.y:t.min.y,ia.z=s.normal.z>0?t.max.z:t.min.z,s.distanceToPoint(ia)<0)return!1}return!0}containsPoint(t){let e=this.planes;for(let i=0;i<6;i++)if(e[i].distanceToPoint(t)<0)return!1;return!0}clone(){return new this.constructor().copy(this)}},nd=new Xt,Ta=class n{constructor(){this.coordinateSystem=Ti,this._frustums=[],this._count=0}setFromArrayCamera(t){let e=t.cameras,i=this._frustums;for(let s=0;s<e.length;s++){let r=e[s];nd.multiplyMatrices(r.projectionMatrix,r.matrixWorldInverse),i[s]===void 0&&(i[s]=new Dn),i[s].setFromProjectionMatrix(nd,r.coordinateSystem,r.reversedDepth)}return this._count=e.length,this}getFrustum(t){return this._frustums[t]}intersectsObject(t){let e=this._frustums;for(let i=0;i<this._count;i++)if(e[i].intersectsObject(t))return!0;return!1}intersectsSprite(t){let e=this._frustums;for(let i=0;i<this._count;i++)if(e[i].intersectsSprite(t))return!0;return!1}intersectsSphere(t){let e=this._frustums;for(let i=0;i<this._count;i++)if(e[i].intersectsSphere(t))return!0;return!1}intersectsBox(t){let e=this._frustums;for(let i=0;i<this._count;i++)if(e[i].intersectsBox(t))return!0;return!1}containsPoint(t){let e=this._frustums;for(let i=0;i<this._count;i++)if(e[i].containsPoint(t))return!0;return!1}copy(t){this.coordinateSystem=t.coordinateSystem;let e=this._frustums,i=t._frustums;for(let s=0;s<t._count;s++)e[s]===void 0&&(e[s]=new Dn),e[s].copy(i[s]);return this._count=t._count,this}clone(){return new n().copy(this)}};var vi=class extends Ui{constructor(t){super(),this.isLineBasicMaterial=!0,this.type="LineBasicMaterial",this.color=new bt(16777215),this.map=null,this.linewidth=1,this.linecap="round",this.linejoin="round",this.fog=!0,this.setValues(t)}copy(t){return super.copy(t),this.color.copy(t.color),this.map=t.map,this.linewidth=t.linewidth,this.linecap=t.linecap,this.linejoin=t.linejoin,this.fog=t.fog,this}},Aa=new C,Ra=new C,sd=new Xt,Tr=new dn,na=new ci,Kc=new C,rd=new C,es=class extends Me{constructor(t=new Jt,e=new vi){super(),this.isLine=!0,this.type="Line",this.geometry=t,this.material=e,this.morphTargetDictionary=void 0,this.morphTargetInfluences=void 0,this.updateMorphTargets()}copy(t,e){return super.copy(t,e),t.morphTargetInfluences!==void 0?this.morphTargetInfluences=t.morphTargetInfluences.slice():this.morphTargetInfluences=void 0,t.morphTargetDictionary!==void 0?this.morphTargetDictionary=Object.assign({},t.morphTargetDictionary):this.morphTargetDictionary=void 0,this.material=Array.isArray(t.material)?t.material.slice():t.material,this.geometry=t.geometry,this}computeLineDistances(){let t=this.geometry;if(t.index===null){let e=t.attributes.position,i=[0];for(let s=1,r=e.count;s<r;s++)Aa.fromBufferAttribute(e,s-1),Ra.fromBufferAttribute(e,s),i[s]=i[s-1],i[s]+=Aa.distanceTo(Ra);t.setAttribute("lineDistance",new ee(i,1))}else Lt("Line.computeLineDistances(): Computation only possible with non-indexed BufferGeometry.");return this}intersectsFrustum(t){return t.intersectsObject(this)}raycast(t,e){let i=this.geometry,s=this.matrixWorld,r=t.params.Line.threshold,o=i.drawRange;if(i.boundingSphere===null&&i.computeBoundingSphere(),na.copy(i.boundingSphere),na.applyMatrix4(s),na.radius+=r,t.ray.intersectsSphere(na)===!1)return;sd.copy(s).invert(),Tr.copy(t.ray).applyMatrix4(sd);let a=r/((this.scale.x+this.scale.y+this.scale.z)/3),l=a*a,c=this.isLineSegments?2:1,h=i.index,d=i.attributes.position;if(h!==null){let f=Math.max(0,o.start),m=Math.min(h.count,o.start+o.count);for(let x=f,p=m-1;x<p;x+=c){let g=h.getX(x),y=h.getX(x+1),v=sa(this,t,Tr,l,g,y,x);v&&e.push(v)}if(this.isLineLoop){let x=h.getX(m-1),p=h.getX(f),g=sa(this,t,Tr,l,x,p,m-1);g&&e.push(g)}}else{let f=Math.max(0,o.start),m=Math.min(d.count,o.start+o.count);for(let x=f,p=m-1;x<p;x+=c){let g=sa(this,t,Tr,l,x,x+1,x);g&&e.push(g)}if(this.isLineLoop){let x=sa(this,t,Tr,l,m-1,f,m-1);x&&e.push(x)}}}updateMorphTargets(){let e=this.geometry.morphAttributes,i=Object.keys(e);if(i.length>0){let s=e[i[0]];if(s!==void 0){this.morphTargetInfluences=[],this.morphTargetDictionary={};for(let r=0,o=s.length;r<o;r++){let a=s[r].name||String(r);this.morphTargetInfluences.push(0),this.morphTargetDictionary[a]=r}}}}};function sa(n,t,e,i,s,r,o){let a=n.geometry.attributes.position;if(Aa.fromBufferAttribute(a,s),Ra.fromBufferAttribute(a,r),e.distanceSqToSegment(Aa,Ra,Kc,rd)>i)return;Kc.applyMatrix4(n.matrixWorld);let c=t.ray.origin.distanceTo(Kc);if(!(c<t.near||c>t.far))return{distance:c,point:rd.clone().applyMatrix4(n.matrixWorld),index:o,face:null,faceIndex:null,barycoord:null,object:n}}var od=new C,ad=new C,fn=class extends es{constructor(t,e){super(t,e),this.isLineSegments=!0,this.type="LineSegments"}computeLineDistances(){let t=this.geometry;if(t.index===null){let e=t.attributes.position,i=[];for(let s=0,r=e.count;s<r;s+=2)od.fromBufferAttribute(e,s),ad.fromBufferAttribute(e,s+1),i[s]=s===0?0:i[s-1],i[s+1]=i[s]+od.distanceTo(ad);t.setAttribute("lineDistance",new ee(i,1))}else Lt("LineSegments.computeLineDistances(): Computation only possible with non-indexed BufferGeometry.");return this}},Fr=class extends es{constructor(t,e){super(t,e),this.isLineLoop=!0,this.type="LineLoop"}},Ws=class extends Ui{constructor(t){super(),this.isPointsMaterial=!0,this.type="PointsMaterial",this.color=new bt(16777215),this.map=null,this.alphaMap=null,this.size=1,this.sizeAttenuation=!0,this.fog=!0,this.setValues(t)}copy(t){return super.copy(t),this.color.copy(t.color),this.map=t.map,this.alphaMap=t.alphaMap,this.size=t.size,this.sizeAttenuation=t.sizeAttenuation,this.fog=t.fog,this}},ld=new Xt,rh=new dn,ra=new ci,oa=new C,is=class extends Me{constructor(t=new Jt,e=new Ws){super(),this.isPoints=!0,this.type="Points",this.geometry=t,this.material=e,this.morphTargetDictionary=void 0,this.morphTargetInfluences=void 0,this.updateMorphTargets()}copy(t,e){return super.copy(t,e),t.morphTargetInfluences!==void 0?this.morphTargetInfluences=t.morphTargetInfluences.slice():this.morphTargetInfluences=void 0,t.morphTargetDictionary!==void 0?this.morphTargetDictionary=Object.assign({},t.morphTargetDictionary):this.morphTargetDictionary=void 0,this.material=Array.isArray(t.material)?t.material.slice():t.material,this.geometry=t.geometry,this}intersectsFrustum(t){return t.intersectsObject(this)}raycast(t,e){let i=this.geometry,s=this.matrixWorld,r=t.params.Points.threshold,o=i.drawRange;if(i.boundingSphere===null&&i.computeBoundingSphere(),ra.copy(i.boundingSphere),ra.applyMatrix4(s),ra.radius+=r,t.ray.intersectsSphere(ra)===!1)return;ld.copy(s).invert(),rh.copy(t.ray).applyMatrix4(ld);let a=r/((this.scale.x+this.scale.y+this.scale.z)/3),l=a*a,c=i.index,u=i.attributes.position;if(c!==null){let d=Math.max(0,o.start),f=Math.min(c.count,o.start+o.count);for(let m=d,x=f;m<x;m++){let p=c.getX(m);oa.fromBufferAttribute(u,p),cd(oa,p,l,s,t,e,this)}}else{let d=Math.max(0,o.start),f=Math.min(u.count,o.start+o.count);for(let m=d,x=f;m<x;m++)oa.fromBufferAttribute(u,m),cd(oa,m,l,s,t,e,this)}}updateMorphTargets(){let e=this.geometry.morphAttributes,i=Object.keys(e);if(i.length>0){let s=e[i[0]];if(s!==void 0){this.morphTargetInfluences=[],this.morphTargetDictionary={};for(let r=0,o=s.length;r<o;r++){let a=s[r].name||String(r);this.morphTargetInfluences.push(0),this.morphTargetDictionary[a]=r}}}}};function cd(n,t,e,i,s,r,o){let a=rh.distanceSqToPoint(n);if(a<e){let l=new C;rh.closestPointToPoint(n,l),l.applyMatrix4(i);let c=s.ray.origin.distanceTo(l);if(c<s.near||c>s.far)return;r.push({distance:c,distanceToRay:Math.sqrt(a),point:l,index:t,face:null,faceIndex:null,barycoord:null,object:o})}}var Or=class extends qe{constructor(t=[],e=hs,i,s,r,o,a,l,c,h){super(t,e,i,s,r,o,a,l,c,h),this.isCubeTexture=!0,this.flipY=!1}get images(){return this.image}set images(t){this.image=t}},Xs=class extends qe{constructor(t,e,i,s,r,o,a,l,c){super(t,e,i,s,r,o,a,l,c),this.isCanvasTexture=!0,this.needsUpdate=!0}};var Oi=class extends qe{constructor(t,e,i=hi,s,r,o,a=De,l=De,c,h=Li,u=1){if(h!==Li&&h!==xn)throw new Error("THREE.DepthTexture: format must be either THREE.DepthFormat or THREE.DepthStencilFormat");let d={width:t,height:e,depth:u};super(d,s,r,o,a,l,h,i,c),this.isDepthTexture=!0,this.flipY=!1,this.generateMipmaps=!1,this.compareFunction=null}copy(t){return super.copy(t),this.source=new zs(Object.assign({},t.image)),this.compareFunction=t.compareFunction,this}toJSON(t){let e=super.toJSON(t);return e.compareFunction=this.compareFunction,e}},Br=class extends Oi{constructor(t,e=hi,i=hs,s,r,o=De,a=De,l,c=Li){let h={width:t,height:t,depth:1},u=[h,h,h,h,h,h];super(t,t,e,i,s,r,o,a,l,c),this.image=u,this.isCubeDepthTexture=!0,this.isCubeTexture=!0}get images(){return this.image}set images(t){this.image=t}},zr=class extends qe{constructor(t=null){super(),this.sourceTexture=t,this.isExternalTexture=!0}copy(t){return super.copy(t),this.sourceTexture=t.sourceTexture,this}},Te=class n extends Jt{constructor(t=1,e=1,i=1,s=1,r=1,o=1){super(),this.type="BoxGeometry",this.parameters={width:t,height:e,depth:i,widthSegments:s,heightSegments:r,depthSegments:o};let a=this;s=Math.floor(s),r=Math.floor(r),o=Math.floor(o);let l=[],c=[],h=[],u=[],d=0,f=0;m("z","y","x",-1,-1,i,e,t,o,r,0),m("z","y","x",1,-1,i,e,-t,o,r,1),m("x","z","y",1,1,t,i,e,s,o,2),m("x","z","y",1,-1,t,i,-e,s,o,3),m("x","y","z",1,-1,t,e,i,s,r,4),m("x","y","z",-1,-1,t,e,-i,s,r,5),this.setIndex(l),this.setAttribute("position",new ee(c,3)),this.setAttribute("normal",new ee(h,3)),this.setAttribute("uv",new ee(u,2));function m(x,p,g,y,v,S,T,b,_,M,D){let P=S/_,A=T/M,R=S/2,U=T/2,L=b/2,B=_+1,H=M+1,z=0,Q=0,W=new C;for(let Z=0;Z<H;Z++){let $=Z*A-U;for(let xt=0;xt<B;xt++){let Mt=xt*P-R;W[x]=Mt*y,W[p]=$*v,W[g]=L,c.push(W.x,W.y,W.z),W[x]=0,W[p]=0,W[g]=b>0?1:-1,h.push(W.x,W.y,W.z),u.push(xt/_),u.push(1-Z/M),z+=1}}for(let Z=0;Z<M;Z++)for(let $=0;$<_;$++){let xt=d+$+B*Z,Mt=d+$+B*(Z+1),q=d+($+1)+B*(Z+1),st=d+($+1)+B*Z;l.push(xt,Mt,st),l.push(Mt,q,st),Q+=6}a.addGroup(f,Q,D),f+=Q,d+=z}}copy(t){return super.copy(t),this.parameters=Object.assign({},t.parameters),this}static fromJSON(t){return new n(t.width,t.height,t.depth,t.widthSegments,t.heightSegments,t.depthSegments)}};var kr=class n extends Jt{constructor(t=1,e=32,i=0,s=Math.PI*2){super(),this.type="CircleGeometry",this.parameters={radius:t,segments:e,thetaStart:i,thetaLength:s},e=Math.max(3,e);let r=[],o=[],a=[],l=[],c=new C,h=new et;o.push(0,0,0),a.push(0,0,1),l.push(.5,.5);for(let u=0,d=3;u<=e;u++,d+=3){let f=i+u/e*s;c.x=t*Math.cos(f),c.y=t*Math.sin(f),o.push(c.x,c.y,c.z),a.push(0,0,1),h.x=(o[d]/t+1)/2,h.y=(o[d+1]/t+1)/2,l.push(h.x,h.y)}for(let u=1;u<=e;u++)r.push(u,u+1,0);this.setIndex(r),this.setAttribute("position",new ee(o,3)),this.setAttribute("normal",new ee(a,3)),this.setAttribute("uv",new ee(l,2))}copy(t){return super.copy(t),this.parameters=Object.assign({},t.parameters),this}static fromJSON(t){return new n(t.radius,t.segments,t.thetaStart,t.thetaLength)}},mi=class n extends Jt{constructor(t=1,e=1,i=1,s=32,r=1,o=!1,a=0,l=Math.PI*2){super(),this.type="CylinderGeometry",this.parameters={radiusTop:t,radiusBottom:e,height:i,radialSegments:s,heightSegments:r,openEnded:o,thetaStart:a,thetaLength:l};let c=this;s=Math.floor(s),r=Math.floor(r);let h=[],u=[],d=[],f=[],m=0,x=[],p=i/2,g=0;y(),o===!1&&(t>0&&v(!0),e>0&&v(!1)),this.setIndex(h),this.setAttribute("position",new ee(u,3)),this.setAttribute("normal",new ee(d,3)),this.setAttribute("uv",new ee(f,2));function y(){let S=new C,T=new C,b=0,_=(e-t)/i;for(let M=0;M<=r;M++){let D=[],P=M/r,A=P*(e-t)+t;for(let R=0;R<=s;R++){let U=R/s,L=U*l+a,B=Math.sin(L),H=Math.cos(L);T.x=A*B,T.y=-P*i+p,T.z=A*H,u.push(T.x,T.y,T.z),S.set(B,_,H).normalize(),d.push(S.x,S.y,S.z),f.push(U,1-P),D.push(m++)}x.push(D)}for(let M=0;M<s;M++)for(let D=0;D<r;D++){let P=x[D][M],A=x[D+1][M],R=x[D+1][M+1],U=x[D][M+1];(t>0||D!==0)&&(h.push(P,A,U),b+=3),(e>0||D!==r-1)&&(h.push(A,R,U),b+=3)}c.addGroup(g,b,0),g+=b}function v(S){let T=m,b=new et,_=new C,M=0,D=S===!0?t:e,P=S===!0?1:-1;for(let R=1;R<=s;R++)u.push(0,p*P,0),d.push(0,P,0),f.push(.5,.5),m++;let A=m;for(let R=0;R<=s;R++){let L=R/s*l+a,B=Math.cos(L),H=Math.sin(L);_.x=D*H,_.y=p*P,_.z=D*B,u.push(_.x,_.y,_.z),d.push(0,P,0),b.x=B*.5+.5,b.y=H*.5*P+.5,f.push(b.x,b.y),m++}for(let R=0;R<s;R++){let U=T+R,L=A+R;S===!0?h.push(L,L+1,U):h.push(L+1,L,U),M+=3}c.addGroup(g,M,S===!0?1:2),g+=M}}copy(t){return super.copy(t),this.parameters=Object.assign({},t.parameters),this}static fromJSON(t){return new n(t.radiusTop,t.radiusBottom,t.height,t.radialSegments,t.heightSegments,t.openEnded,t.thetaStart,t.thetaLength)}};var aa=new C,la=new C,Jc=new C,ca=new ln,ns=class extends Jt{constructor(t=null,e=1){if(super(),this.type="EdgesGeometry",this.parameters={geometry:t,thresholdAngle:e},t!==null){let s=Math.pow(10,4),r=Math.cos(Ns*e),o=t.getIndex(),a=t.getAttribute("position"),l=o?o.count:a.count,c=[0,0,0],h=["a","b","c"],u=new Array(3),d=new Map,f=[];for(let m=0;m<l;m+=3){o?(c[0]=o.getX(m),c[1]=o.getX(m+1),c[2]=o.getX(m+2)):(c[0]=m,c[1]=m+1,c[2]=m+2);let{a:x,b:p,c:g}=ca;if(x.fromBufferAttribute(a,c[0]),p.fromBufferAttribute(a,c[1]),g.fromBufferAttribute(a,c[2]),ca.getNormal(Jc),u[0]=`${Math.round(x.x*s)},${Math.round(x.y*s)},${Math.round(x.z*s)}`,u[1]=`${Math.round(p.x*s)},${Math.round(p.y*s)},${Math.round(p.z*s)}`,u[2]=`${Math.round(g.x*s)},${Math.round(g.y*s)},${Math.round(g.z*s)}`,!(u[0]===u[1]||u[1]===u[2]||u[2]===u[0]))for(let y=0;y<3;y++){let v=(y+1)%3,S=u[y],T=u[v],b=ca[h[y]],_=ca[h[v]],M=`${S}_${T}`,D=`${T}_${S}`,P=d.get(D);P?(Jc.dot(P.normal)<=r&&(f.push(b.x,b.y,b.z),f.push(_.x,_.y,_.z)),d.set(D,null)):d.has(M)||d.set(M,{index0:c[y],index1:c[v],normal:Jc.clone()})}}for(let m of d.values())if(m){let{index0:x,index1:p}=m;aa.fromBufferAttribute(a,x),la.fromBufferAttribute(a,p),f.push(aa.x,aa.y,aa.z),f.push(la.x,la.y,la.z)}this.setAttribute("position",new ee(f,3))}}copy(t){return super.copy(t),this.parameters=Object.assign({},t.parameters),this}},Mi=class{constructor(){this.type="Curve",this.arcLengthDivisions=200,this.needsUpdate=!1,this.cacheArcLengths=null}getPoint(){Lt("Curve: .getPoint() not implemented.")}getPointAt(t,e){let i=this.getUtoTmapping(t);return this.getPoint(i,e)}getPoints(t=5){let e=[];for(let i=0;i<=t;i++)e.push(this.getPoint(i/t));return e}getSpacedPoints(t=5){let e=[];for(let i=0;i<=t;i++)e.push(this.getPointAt(i/t));return e}getLength(){let t=this.getLengths();return t[t.length-1]}getLengths(t=this.arcLengthDivisions){if(this.cacheArcLengths&&this.cacheArcLengths.length===t+1&&!this.needsUpdate)return this.cacheArcLengths;this.needsUpdate=!1;let e=[],i,s=this.getPoint(0),r=0;e.push(0);for(let o=1;o<=t;o++)i=this.getPoint(o/t),r+=i.distanceTo(s),e.push(r),s=i;return this.cacheArcLengths=e,e}updateArcLengths(){this.needsUpdate=!0,this.getLengths()}getUtoTmapping(t,e=null){let i=this.getLengths(),s=0,r=i.length,o;e?o=e:o=t*i[r-1];let a=0,l=r-1,c;for(;a<=l;)if(s=Math.floor(a+(l-a)/2),c=i[s]-o,c<0)a=s+1;else if(c>0)l=s-1;else{l=s;break}if(s=l,i[s]===o)return s/(r-1);let h=i[s],d=i[s+1]-h,f=(o-h)/d;return(s+f)/(r-1)}getTangent(t,e){let s=t-1e-4,r=t+1e-4;s<0&&(s=0),r>1&&(r=1);let o=this.getPoint(s),a=this.getPoint(r),l=e||(o.isVector2?new et:new C);return l.copy(a).sub(o).normalize(),l}getTangentAt(t,e){let i=this.getUtoTmapping(t);return this.getTangent(i,e)}computeFrenetFrames(t,e=!1){let i=new C,s=[],r=[],o=[],a=new C,l=new Xt;for(let f=0;f<=t;f++){let m=f/t;s[f]=this.getTangentAt(m,new C)}r[0]=new C,o[0]=new C;let c=Number.MAX_VALUE,h=Math.abs(s[0].x),u=Math.abs(s[0].y),d=Math.abs(s[0].z);h<=c&&(c=h,i.set(1,0,0)),u<=c&&(c=u,i.set(0,1,0)),d<=c&&i.set(0,0,1),a.crossVectors(s[0],i).normalize(),r[0].crossVectors(s[0],a),o[0].crossVectors(s[0],r[0]);for(let f=1;f<=t;f++){if(r[f]=r[f-1].clone(),o[f]=o[f-1].clone(),a.crossVectors(s[f-1],s[f]),a.length()>Number.EPSILON){a.normalize();let m=Math.acos(Vt(s[f-1].dot(s[f]),-1,1));r[f].applyMatrix4(l.makeRotationAxis(a,m))}o[f].crossVectors(s[f],r[f])}if(e===!0){let f=Math.acos(Vt(r[0].dot(r[t]),-1,1));f/=t,s[0].dot(a.crossVectors(r[0],r[t]))>0&&(f=-f);for(let m=1;m<=t;m++)r[m].applyMatrix4(l.makeRotationAxis(s[m],f*m)),o[m].crossVectors(s[m],r[m])}return{tangents:s,normals:r,binormals:o}}clone(){return new this.constructor().copy(this)}copy(t){return this.arcLengthDivisions=t.arcLengthDivisions,this.cacheArcLengths=null,this}toJSON(){let t={metadata:{version:4.7,type:"Curve",generator:"Curve.toJSON"}};return t.arcLengthDivisions=this.arcLengthDivisions,t.type=this.type,t}fromJSON(t){return this.arcLengthDivisions=t.arcLengthDivisions,this.cacheArcLengths=null,this}},Hr=class extends Mi{constructor(t=0,e=0,i=1,s=1,r=0,o=Math.PI*2,a=!1,l=0){super(),this.isEllipseCurve=!0,this.type="EllipseCurve",this.aX=t,this.aY=e,this.xRadius=i,this.yRadius=s,this.aStartAngle=r,this.aEndAngle=o,this.aClockwise=a,this.aRotation=l}getPoint(t,e=new et){let i=e,s=Math.PI*2,r=this.aEndAngle-this.aStartAngle,o=Math.abs(r)<Number.EPSILON;for(;r<0;)r+=s;for(;r>s;)r-=s;r<Number.EPSILON&&(o?r=0:r=s),this.aClockwise===!0&&!o&&(r===s?r=-s:r=r-s);let a=this.aStartAngle+t*r,l=this.aX+this.xRadius*Math.cos(a),c=this.aY+this.yRadius*Math.sin(a);if(this.aRotation!==0){let h=Math.cos(this.aRotation),u=Math.sin(this.aRotation),d=l-this.aX,f=c-this.aY;l=d*h-f*u+this.aX,c=d*u+f*h+this.aY}return i.set(l,c)}copy(t){return super.copy(t),this.aX=t.aX,this.aY=t.aY,this.xRadius=t.xRadius,this.yRadius=t.yRadius,this.aStartAngle=t.aStartAngle,this.aEndAngle=t.aEndAngle,this.aClockwise=t.aClockwise,this.aRotation=t.aRotation,this}toJSON(){let t=super.toJSON();return t.aX=this.aX,t.aY=this.aY,t.xRadius=this.xRadius,t.yRadius=this.yRadius,t.aStartAngle=this.aStartAngle,t.aEndAngle=this.aEndAngle,t.aClockwise=this.aClockwise,t.aRotation=this.aRotation,t}fromJSON(t){return super.fromJSON(t),this.aX=t.aX,this.aY=t.aY,this.xRadius=t.xRadius,this.yRadius=t.yRadius,this.aStartAngle=t.aStartAngle,this.aEndAngle=t.aEndAngle,this.aClockwise=t.aClockwise,this.aRotation=t.aRotation,this}},Ca=class extends Hr{constructor(t,e,i,s,r,o){super(t,e,i,i,s,r,o),this.isArcCurve=!0,this.type="ArcCurve"}};function Ih(){let n=0,t=0,e=0,i=0;function s(r,o,a,l){n=r,t=a,e=-3*r+3*o-2*a-l,i=2*r-2*o+a+l}return{initCatmullRom:function(r,o,a,l,c){s(o,a,c*(a-r),c*(l-o))},initNonuniformCatmullRom:function(r,o,a,l,c,h,u){let d=(o-r)/c-(a-r)/(c+h)+(a-o)/h,f=(a-o)/h-(l-o)/(h+u)+(l-a)/u;d*=h,f*=h,s(o,a,d,f)},calc:function(r){let o=r*r,a=o*r;return n+t*r+e*o+i*a}}}var hd=new C,ud=new C,$c=new Ih,jc=new Ih,Qc=new Ih,qs=class extends Mi{constructor(t=[],e=!1,i="centripetal",s=.5){super(),this.isCatmullRomCurve3=!0,this.type="CatmullRomCurve3",this.points=t,this.closed=e,this.curveType=i,this.tension=s}getPoint(t,e=new C){let i=e,s=this.points,r=s.length,o=(r-(this.closed?0:1))*t,a=Math.floor(o),l=o-a;this.closed?a+=a>0?0:(Math.floor(Math.abs(a)/r)+1)*r:l===0&&a===r-1&&(a=r-2,l=1);let c,h;this.closed||a>0?c=s[(a-1)%r]:(ud.subVectors(s[0],s[1]).add(s[0]),c=ud);let u=s[a%r],d=s[(a+1)%r];if(this.closed||a+2<r?h=s[(a+2)%r]:(hd.subVectors(s[r-1],s[r-2]).add(s[r-1]),h=hd),this.curveType==="centripetal"||this.curveType==="chordal"){let f=this.curveType==="chordal"?.5:.25,m=Math.pow(c.distanceToSquared(u),f),x=Math.pow(u.distanceToSquared(d),f),p=Math.pow(d.distanceToSquared(h),f);x<1e-4&&(x=1),m<1e-4&&(m=x),p<1e-4&&(p=x),$c.initNonuniformCatmullRom(c.x,u.x,d.x,h.x,m,x,p),jc.initNonuniformCatmullRom(c.y,u.y,d.y,h.y,m,x,p),Qc.initNonuniformCatmullRom(c.z,u.z,d.z,h.z,m,x,p)}else this.curveType==="catmullrom"&&($c.initCatmullRom(c.x,u.x,d.x,h.x,this.tension),jc.initCatmullRom(c.y,u.y,d.y,h.y,this.tension),Qc.initCatmullRom(c.z,u.z,d.z,h.z,this.tension));return i.set($c.calc(l),jc.calc(l),Qc.calc(l)),i}copy(t){super.copy(t),this.points=[];for(let e=0,i=t.points.length;e<i;e++){let s=t.points[e];this.points.push(s.clone())}return this.closed=t.closed,this.curveType=t.curveType,this.tension=t.tension,this}toJSON(){let t=super.toJSON();t.points=[];for(let e=0,i=this.points.length;e<i;e++){let s=this.points[e];t.points.push(s.toArray())}return t.closed=this.closed,t.curveType=this.curveType,t.tension=this.tension,t}fromJSON(t){super.fromJSON(t),this.points=[];for(let e=0,i=t.points.length;e<i;e++){let s=t.points[e];this.points.push(new C().fromArray(s))}return this.closed=t.closed,this.curveType=t.curveType,this.tension=t.tension,this}};function dd(n,t,e,i,s){let r=(i-t)*.5,o=(s-e)*.5,a=n*n,l=n*a;return(2*e-2*i+r+o)*l+(-3*e+3*i-2*r-o)*a+r*n+e}function Bm(n,t){let e=1-n;return e*e*t}function zm(n,t){return 2*(1-n)*n*t}function km(n,t){return n*n*t}function Rr(n,t,e,i){return Bm(n,t)+zm(n,e)+km(n,i)}function Hm(n,t){let e=1-n;return e*e*e*t}function Vm(n,t){let e=1-n;return 3*e*e*n*t}function Gm(n,t){return 3*(1-n)*n*n*t}function Wm(n,t){return n*n*n*t}function Cr(n,t,e,i,s){return Hm(n,t)+Vm(n,e)+Gm(n,i)+Wm(n,s)}var Pa=class extends Mi{constructor(t=new et,e=new et,i=new et,s=new et){super(),this.isCubicBezierCurve=!0,this.type="CubicBezierCurve",this.v0=t,this.v1=e,this.v2=i,this.v3=s}getPoint(t,e=new et){let i=e,s=this.v0,r=this.v1,o=this.v2,a=this.v3;return i.set(Cr(t,s.x,r.x,o.x,a.x),Cr(t,s.y,r.y,o.y,a.y)),i}copy(t){return super.copy(t),this.v0.copy(t.v0),this.v1.copy(t.v1),this.v2.copy(t.v2),this.v3.copy(t.v3),this}toJSON(){let t=super.toJSON();return t.v0=this.v0.toArray(),t.v1=this.v1.toArray(),t.v2=this.v2.toArray(),t.v3=this.v3.toArray(),t}fromJSON(t){return super.fromJSON(t),this.v0.fromArray(t.v0),this.v1.fromArray(t.v1),this.v2.fromArray(t.v2),this.v3.fromArray(t.v3),this}},Ia=class extends Mi{constructor(t=new C,e=new C,i=new C,s=new C){super(),this.isCubicBezierCurve3=!0,this.type="CubicBezierCurve3",this.v0=t,this.v1=e,this.v2=i,this.v3=s}getPoint(t,e=new C){let i=e,s=this.v0,r=this.v1,o=this.v2,a=this.v3;return i.set(Cr(t,s.x,r.x,o.x,a.x),Cr(t,s.y,r.y,o.y,a.y),Cr(t,s.z,r.z,o.z,a.z)),i}copy(t){return super.copy(t),this.v0.copy(t.v0),this.v1.copy(t.v1),this.v2.copy(t.v2),this.v3.copy(t.v3),this}toJSON(){let t=super.toJSON();return t.v0=this.v0.toArray(),t.v1=this.v1.toArray(),t.v2=this.v2.toArray(),t.v3=this.v3.toArray(),t}fromJSON(t){return super.fromJSON(t),this.v0.fromArray(t.v0),this.v1.fromArray(t.v1),this.v2.fromArray(t.v2),this.v3.fromArray(t.v3),this}},Da=class extends Mi{constructor(t=new et,e=new et){super(),this.isLineCurve=!0,this.type="LineCurve",this.v1=t,this.v2=e}getPoint(t,e=new et){let i=e;return t===1?i.copy(this.v2):(i.copy(this.v2).sub(this.v1),i.multiplyScalar(t).add(this.v1)),i}getPointAt(t,e){return this.getPoint(t,e)}getTangent(t,e=new et){return e.subVectors(this.v2,this.v1).normalize()}getTangentAt(t,e){return this.getTangent(t,e)}copy(t){return super.copy(t),this.v1.copy(t.v1),this.v2.copy(t.v2),this}toJSON(){let t=super.toJSON();return t.v1=this.v1.toArray(),t.v2=this.v2.toArray(),t}fromJSON(t){return super.fromJSON(t),this.v1.fromArray(t.v1),this.v2.fromArray(t.v2),this}},Ys=class extends Mi{constructor(t=new C,e=new C){super(),this.isLineCurve3=!0,this.type="LineCurve3",this.v1=t,this.v2=e}getPoint(t,e=new C){let i=e;return t===1?i.copy(this.v2):(i.copy(this.v2).sub(this.v1),i.multiplyScalar(t).add(this.v1)),i}getPointAt(t,e){return this.getPoint(t,e)}getTangent(t,e=new C){return e.subVectors(this.v2,this.v1).normalize()}getTangentAt(t,e){return this.getTangent(t,e)}copy(t){return super.copy(t),this.v1.copy(t.v1),this.v2.copy(t.v2),this}toJSON(){let t=super.toJSON();return t.v1=this.v1.toArray(),t.v2=this.v2.toArray(),t}fromJSON(t){return super.fromJSON(t),this.v1.fromArray(t.v1),this.v2.fromArray(t.v2),this}},La=class extends Mi{constructor(t=new et,e=new et,i=new et){super(),this.isQuadraticBezierCurve=!0,this.type="QuadraticBezierCurve",this.v0=t,this.v1=e,this.v2=i}getPoint(t,e=new et){let i=e,s=this.v0,r=this.v1,o=this.v2;return i.set(Rr(t,s.x,r.x,o.x),Rr(t,s.y,r.y,o.y)),i}copy(t){return super.copy(t),this.v0.copy(t.v0),this.v1.copy(t.v1),this.v2.copy(t.v2),this}toJSON(){let t=super.toJSON();return t.v0=this.v0.toArray(),t.v1=this.v1.toArray(),t.v2=this.v2.toArray(),t}fromJSON(t){return super.fromJSON(t),this.v0.fromArray(t.v0),this.v1.fromArray(t.v1),this.v2.fromArray(t.v2),this}},Vr=class extends Mi{constructor(t=new C,e=new C,i=new C){super(),this.isQuadraticBezierCurve3=!0,this.type="QuadraticBezierCurve3",this.v0=t,this.v1=e,this.v2=i}getPoint(t,e=new C){let i=e,s=this.v0,r=this.v1,o=this.v2;return i.set(Rr(t,s.x,r.x,o.x),Rr(t,s.y,r.y,o.y),Rr(t,s.z,r.z,o.z)),i}copy(t){return super.copy(t),this.v0.copy(t.v0),this.v1.copy(t.v1),this.v2.copy(t.v2),this}toJSON(){let t=super.toJSON();return t.v0=this.v0.toArray(),t.v1=this.v1.toArray(),t.v2=this.v2.toArray(),t}fromJSON(t){return super.fromJSON(t),this.v0.fromArray(t.v0),this.v1.fromArray(t.v1),this.v2.fromArray(t.v2),this}},Na=class extends Mi{constructor(t=[]){super(),this.isSplineCurve=!0,this.type="SplineCurve",this.points=t}getPoint(t,e=new et){let i=e,s=this.points,r=(s.length-1)*t,o=Math.floor(r),a=r-o,l=s[o===0?o:o-1],c=s[o],h=s[o>s.length-2?s.length-1:o+1],u=s[o>s.length-3?s.length-1:o+2];return i.set(dd(a,l.x,c.x,h.x,u.x),dd(a,l.y,c.y,h.y,u.y)),i}copy(t){super.copy(t),this.points=[];for(let e=0,i=t.points.length;e<i;e++){let s=t.points[e];this.points.push(s.clone())}return this}toJSON(){let t=super.toJSON();t.points=[];for(let e=0,i=this.points.length;e<i;e++){let s=this.points[e];t.points.push(s.toArray())}return t}fromJSON(t){super.fromJSON(t),this.points=[];for(let e=0,i=t.points.length;e<i;e++){let s=t.points[e];this.points.push(new et().fromArray(s))}return this}},oh=Object.freeze({__proto__:null,ArcCurve:Ca,CatmullRomCurve3:qs,CubicBezierCurve:Pa,CubicBezierCurve3:Ia,EllipseCurve:Hr,LineCurve:Da,LineCurve3:Ys,QuadraticBezierCurve:La,QuadraticBezierCurve3:Vr,SplineCurve:Na}),Gr=class extends Mi{constructor(){super(),this.type="CurvePath",this.curves=[],this.autoClose=!1,this.cacheLengths=null}add(t){this.curves.push(t)}closePath(){let t=this.curves[0].getPoint(0),e=this.curves[this.curves.length-1].getPoint(1);if(!t.equals(e)){let i=t.isVector2===!0?"LineCurve":"LineCurve3";this.curves.push(new oh[i](e,t))}return this}getPoint(t,e){let i=t*this.getLength(),s=this.getCurveLengths(),r=0;for(;r<s.length;){if(s[r]>=i){let o=s[r]-i,a=this.curves[r],l=a.getLength(),c=l===0?0:1-o/l;return a.getPointAt(c,e)}r++}return null}getLength(){let t=this.getCurveLengths();return t[t.length-1]}updateArcLengths(){this.needsUpdate=!0,this.cacheLengths=null,this.getCurveLengths()}getCurveLengths(){if(this.cacheLengths&&this.cacheLengths.length===this.curves.length)return this.cacheLengths;let t=[],e=0;for(let i=0,s=this.curves.length;i<s;i++)e+=this.curves[i].getLength(),t.push(e);return this.cacheLengths=t,t}getSpacedPoints(t=40){let e=[];for(let i=0;i<=t;i++)e.push(this.getPoint(i/t));return this.autoClose&&e.push(e[0]),e}getPoints(t=12){let e=[],i;for(let s=0,r=this.curves;s<r.length;s++){let o=r[s],a=o.isEllipseCurve?t*2:o.isLineCurve||o.isLineCurve3?1:o.isSplineCurve?t*o.points.length:t,l=o.getPoints(a);for(let c=0;c<l.length;c++){let h=l[c];i&&i.equals(h)||(e.push(h),i=h)}}return this.autoClose&&e.length>1&&!e[e.length-1].equals(e[0])&&e.push(e[0]),e}copy(t){super.copy(t),this.curves=[];for(let e=0,i=t.curves.length;e<i;e++){let s=t.curves[e];this.curves.push(s.clone())}return this.autoClose=t.autoClose,this.cacheLengths=null,this}toJSON(){let t=super.toJSON();t.autoClose=this.autoClose,t.curves=[];for(let e=0,i=this.curves.length;e<i;e++){let s=this.curves[e];t.curves.push(s.toJSON())}return t}fromJSON(t){super.fromJSON(t),this.autoClose=t.autoClose,this.curves=[];for(let e=0,i=t.curves.length;e<i;e++){let s=t.curves[e];this.curves.push(new oh[s.type]().fromJSON(s))}return this.cacheLengths=null,this}};var Si=class n extends Jt{constructor(t=1,e=1,i=1,s=1){super(),this.type="PlaneGeometry",this.parameters={width:t,height:e,widthSegments:i,heightSegments:s};let r=t/2,o=e/2,a=Math.floor(i),l=Math.floor(s),c=a+1,h=l+1,u=t/a,d=e/l,f=[],m=[],x=[],p=[];for(let g=0;g<h;g++){let y=g*d-o;for(let v=0;v<c;v++){let S=v*u-r;m.push(S,-y,0),x.push(0,0,1),p.push(v/a),p.push(1-g/l)}}for(let g=0;g<l;g++)for(let y=0;y<a;y++){let v=y+c*g,S=y+c*(g+1),T=y+1+c*(g+1),b=y+1+c*g;f.push(v,S,b),f.push(S,T,b)}this.setIndex(f),this.setAttribute("position",new ee(m,3)),this.setAttribute("normal",new ee(x,3)),this.setAttribute("uv",new ee(p,2))}copy(t){return super.copy(t),this.parameters=Object.assign({},t.parameters),this}static fromJSON(t){return new n(t.width,t.height,t.widthSegments,t.heightSegments)}};var pn=class n extends Jt{constructor(t=1,e=32,i=16,s=0,r=Math.PI*2,o=0,a=Math.PI){super(),this.type="SphereGeometry",this.parameters={radius:t,widthSegments:e,heightSegments:i,phiStart:s,phiLength:r,thetaStart:o,thetaLength:a},e=Math.max(3,Math.floor(e)),i=Math.max(2,Math.floor(i));let l=Math.min(o+a,Math.PI),c=0,h=[],u=new C,d=new C,f=[],m=[],x=[],p=[];for(let g=0;g<=i;g++){let y=[],v=g/i,S=o+v*a,T=t*Math.cos(S),b=Math.sqrt(t*t-T*T),_=0;g===0&&o===0?_=.5/e:g===i&&l===Math.PI&&(_=-.5/e);for(let M=0;M<=e;M++){let D=M/e,P=s+D*r;u.x=-b*Math.cos(P),u.y=T,u.z=b*Math.sin(P),m.push(u.x,u.y,u.z),d.copy(u).normalize(),x.push(d.x,d.y,d.z),p.push(D+_,1-v),y.push(c++)}h.push(y)}for(let g=0;g<i;g++)for(let y=0;y<e;y++){let v=h[g][y+1],S=h[g][y],T=h[g+1][y],b=h[g+1][y+1];(g!==0||o>0)&&f.push(v,S,b),(g!==i-1||l<Math.PI)&&f.push(S,T,b)}this.setIndex(f),this.setAttribute("position",new ee(m,3)),this.setAttribute("normal",new ee(x,3)),this.setAttribute("uv",new ee(p,2))}copy(t){return super.copy(t),this.parameters=Object.assign({},t.parameters),this}static fromJSON(t){return new n(t.radius,t.widthSegments,t.heightSegments,t.phiStart,t.phiLength,t.thetaStart,t.thetaLength)}};var Wr=class n extends Jt{constructor(t=1,e=.4,i=12,s=48,r=Math.PI*2,o=0,a=Math.PI*2){super(),this.type="TorusGeometry",this.parameters={radius:t,tube:e,radialSegments:i,tubularSegments:s,arc:r,thetaStart:o,thetaLength:a},i=Math.floor(i),s=Math.floor(s);let l=[],c=[],h=[],u=[],d=new C,f=new C,m=new C;for(let x=0;x<=i;x++){let p=o+x/i*a;for(let g=0;g<=s;g++){let y=g/s*r;f.x=(t+e*Math.cos(p))*Math.cos(y),f.y=(t+e*Math.cos(p))*Math.sin(y),f.z=e*Math.sin(p),c.push(f.x,f.y,f.z),d.x=t*Math.cos(y),d.y=t*Math.sin(y),m.subVectors(f,d).normalize(),h.push(m.x,m.y,m.z),u.push(g/s),u.push(x/i)}}for(let x=1;x<=i;x++)for(let p=1;p<=s;p++){let g=(s+1)*x+p-1,y=(s+1)*(x-1)+p-1,v=(s+1)*(x-1)+p,S=(s+1)*x+p;l.push(g,y,S),l.push(y,v,S)}this.setIndex(l),this.setAttribute("position",new ee(c,3)),this.setAttribute("normal",new ee(h,3)),this.setAttribute("uv",new ee(u,2))}copy(t){return super.copy(t),this.parameters=Object.assign({},t.parameters),this}static fromJSON(t){return new n(t.radius,t.tube,t.radialSegments,t.tubularSegments,t.arc,t.thetaStart,t.thetaLength)}};var ss=class n extends Jt{constructor(t=new Vr(new C(-1,-1,0),new C(-1,1,0),new C(1,1,0)),e=64,i=1,s=8,r=!1){super(),this.type="TubeGeometry",this.parameters={path:t,tubularSegments:e,radius:i,radialSegments:s,closed:r};let o=t.computeFrenetFrames(e,r);this.tangents=o.tangents,this.normals=o.normals,this.binormals=o.binormals;let a=new C,l=new C,c=new et,h=new C,u=[],d=[],f=[],m=[];x(),this.setIndex(m),this.setAttribute("position",new ee(u,3)),this.setAttribute("normal",new ee(d,3)),this.setAttribute("uv",new ee(f,2));function x(){for(let v=0;v<e;v++)p(v);p(r===!1?e:0),y(),g()}function p(v){h=t.getPointAt(v/e,h);let S=o.normals[v],T=o.binormals[v];for(let b=0;b<=s;b++){let _=b/s*Math.PI*2,M=Math.sin(_),D=-Math.cos(_);l.x=D*S.x+M*T.x,l.y=D*S.y+M*T.y,l.z=D*S.z+M*T.z,l.normalize(),d.push(l.x,l.y,l.z),a.x=h.x+i*l.x,a.y=h.y+i*l.y,a.z=h.z+i*l.z,u.push(a.x,a.y,a.z)}}function g(){for(let v=1;v<=e;v++)for(let S=1;S<=s;S++){let T=(s+1)*(v-1)+(S-1),b=(s+1)*v+(S-1),_=(s+1)*v+S,M=(s+1)*(v-1)+S;m.push(T,b,M),m.push(b,_,M)}}function y(){for(let v=0;v<=e;v++)for(let S=0;S<=s;S++)c.x=v/e,c.y=S/s,f.push(c.x,c.y)}}copy(t){return super.copy(t),this.parameters=Object.assign({},t.parameters),this}toJSON(){let t=super.toJSON();return t.path=this.parameters.path.toJSON(),t}static fromJSON(t){return new n(new oh[t.path.type]().fromJSON(t.path),t.tubularSegments,t.radius,t.radialSegments,t.closed)}};function ds(n){let t={};for(let e in n){t[e]={};for(let i in n[e]){let s=n[e][i];if(fd(s))s.isRenderTargetTexture?(Lt("UniformsUtils: Textures of render targets cannot be cloned via cloneUniforms() or mergeUniforms()."),t[e][i]=null):t[e][i]=s.clone();else if(Array.isArray(s))if(fd(s[0])){let r=[];for(let o=0,a=s.length;o<a;o++)r[o]=s[o].clone();t[e][i]=r}else t[e][i]=s.slice();else t[e][i]=s}}return t}function ii(n){let t={};for(let e=0;e<n.length;e++){let i=ds(n[e]);for(let s in i)t[s]=i[s]}return t}function fd(n){return n&&(n.isColor||n.isMatrix3||n.isMatrix4||n.isVector2||n.isVector3||n.isVector4||n.isTexture||n.isQuaternion)}function Xm(n){let t=[];for(let e=0;e<n.length;e++)t.push(n[e].clone());return t}function Dh(n){let t=n.getRenderTarget();return t===null?n.outputColorSpace:t.isXRRenderTarget===!0?t.texture.colorSpace:Zt.workingColorSpace}var Hi={clone:ds,merge:ii},qm=`void main() {
	gl_Position = projectionMatrix * modelViewMatrix * vec4( position, 1.0 );
}`,Ym=`void main() {
	gl_FragColor = vec4( 1.0, 0.0, 0.0, 1.0 );
}`,fe=class extends Ui{constructor(t){super(),this.isShaderMaterial=!0,this.type="ShaderMaterial",this.defines={},this.uniforms={},this.uniformsGroups=[],this.vertexShader=qm,this.fragmentShader=Ym,this.linewidth=1,this.wireframe=!1,this.wireframeLinewidth=1,this.fog=!1,this.lights=!1,this.clipping=!1,this.forceSinglePass=!0,this.extensions={clipCullDistance:!1,multiDraw:!1},this.defaultAttributeValues={color:[1,1,1],uv:[0,0],uv1:[0,0]},this.index0AttributeName=void 0,this.uniformsNeedUpdate=!1,this.glslVersion=null,t!==void 0&&this.setValues(t)}copy(t){return super.copy(t),this.fragmentShader=t.fragmentShader,this.vertexShader=t.vertexShader,this.uniforms=ds(t.uniforms),this.uniformsGroups=Xm(t.uniformsGroups),this.defines=Object.assign({},t.defines),this.wireframe=t.wireframe,this.wireframeLinewidth=t.wireframeLinewidth,this.fog=t.fog,this.lights=t.lights,this.clipping=t.clipping,this.extensions=Object.assign({},t.extensions),this.glslVersion=t.glslVersion,this.defaultAttributeValues=Object.assign({},t.defaultAttributeValues),this.index0AttributeName=t.index0AttributeName,this.uniformsNeedUpdate=t.uniformsNeedUpdate,this}toJSON(t){let e=super.toJSON(t);e.glslVersion=this.glslVersion,e.uniforms={};for(let s in this.uniforms){let o=this.uniforms[s].value;o&&o.isTexture?e.uniforms[s]={type:"t",value:o.toJSON(t).uuid}:o&&o.isColor?e.uniforms[s]={type:"c",value:o.getHex()}:o&&o.isVector2?e.uniforms[s]={type:"v2",value:o.toArray()}:o&&o.isVector3?e.uniforms[s]={type:"v3",value:o.toArray()}:o&&o.isVector4?e.uniforms[s]={type:"v4",value:o.toArray()}:o&&o.isMatrix3?e.uniforms[s]={type:"m3",value:o.toArray()}:o&&o.isMatrix4?e.uniforms[s]={type:"m4",value:o.toArray()}:e.uniforms[s]={value:o}}Object.keys(this.defines).length>0&&(e.defines=this.defines),e.vertexShader=this.vertexShader,e.fragmentShader=this.fragmentShader,e.lights=this.lights,e.clipping=this.clipping;let i={};for(let s in this.extensions)this.extensions[s]===!0&&(i[s]=!0);return Object.keys(i).length>0&&(e.extensions=i),e}fromJSON(t,e){if(super.fromJSON(t,e),t.uniforms!==void 0)for(let i in t.uniforms){let s=t.uniforms[i];switch(this.uniforms[i]={},s.type){case"t":this.uniforms[i].value=e[s.value]||null;break;case"c":this.uniforms[i].value=new bt().setHex(s.value);break;case"v2":this.uniforms[i].value=new et().fromArray(s.value);break;case"v3":this.uniforms[i].value=new C().fromArray(s.value);break;case"v4":this.uniforms[i].value=new oe().fromArray(s.value);break;case"m3":this.uniforms[i].value=new zt().fromArray(s.value);break;case"m4":this.uniforms[i].value=new Xt().fromArray(s.value);break;default:this.uniforms[i].value=s.value}}if(t.defines!==void 0&&(this.defines=t.defines),t.vertexShader!==void 0&&(this.vertexShader=t.vertexShader),t.fragmentShader!==void 0&&(this.fragmentShader=t.fragmentShader),t.glslVersion!==void 0&&(this.glslVersion=t.glslVersion),t.extensions!==void 0)for(let i in t.extensions)this.extensions[i]=t.extensions[i];return t.lights!==void 0&&(this.lights=t.lights),t.clipping!==void 0&&(this.clipping=t.clipping),this}},Ln=class extends fe{constructor(t){super(t),this.isRawShaderMaterial=!0,this.type="RawShaderMaterial"}},Le=class extends Ui{constructor(t){super(),this.isMeshStandardMaterial=!0,this.type="MeshStandardMaterial",this.defines={STANDARD:""},this.color=new bt(16777215),this.roughness=1,this.metalness=0,this.map=null,this.lightMap=null,this.lightMapIntensity=1,this.aoMap=null,this.aoMapIntensity=1,this.emissive=new bt(0),this.emissiveIntensity=1,this.emissiveMap=null,this.bumpMap=null,this.bumpScale=1,this.normalMap=null,this.normalMapType=po,this.normalScale=new et(1,1),this.displacementMap=null,this.displacementScale=1,this.displacementBias=0,this.roughnessMap=null,this.metalnessMap=null,this.alphaMap=null,this.envMap=null,this.envMapRotation=new Ai,this.envMapIntensity=1,this.wireframe=!1,this.wireframeLinewidth=1,this.wireframeLinecap="round",this.wireframeLinejoin="round",this.flatShading=!1,this.fog=!0,this.setValues(t)}copy(t){return super.copy(t),this.defines={STANDARD:""},this.color.copy(t.color),this.roughness=t.roughness,this.metalness=t.metalness,this.map=t.map,this.lightMap=t.lightMap,this.lightMapIntensity=t.lightMapIntensity,this.aoMap=t.aoMap,this.aoMapIntensity=t.aoMapIntensity,this.emissive.copy(t.emissive),this.emissiveMap=t.emissiveMap,this.emissiveIntensity=t.emissiveIntensity,this.bumpMap=t.bumpMap,this.bumpScale=t.bumpScale,this.normalMap=t.normalMap,this.normalMapType=t.normalMapType,this.normalScale.copy(t.normalScale),this.displacementMap=t.displacementMap,this.displacementScale=t.displacementScale,this.displacementBias=t.displacementBias,this.roughnessMap=t.roughnessMap,this.metalnessMap=t.metalnessMap,this.alphaMap=t.alphaMap,this.envMap=t.envMap,this.envMapRotation.copy(t.envMapRotation),this.envMapIntensity=t.envMapIntensity,this.wireframe=t.wireframe,this.wireframeLinewidth=t.wireframeLinewidth,this.wireframeLinecap=t.wireframeLinecap,this.wireframeLinejoin=t.wireframeLinejoin,this.flatShading=t.flatShading,this.fog=t.fog,this}},Xr=class extends Le{constructor(t){super(),this.isMeshPhysicalMaterial=!0,this.defines={STANDARD:"",PHYSICAL:""},this.type="MeshPhysicalMaterial",this.anisotropyRotation=0,this.anisotropyMap=null,this.clearcoatMap=null,this.clearcoatRoughness=0,this.clearcoatRoughnessMap=null,this.clearcoatNormalScale=new et(1,1),this.clearcoatNormalMap=null,this.diffuseRoughnessMap=null,this.ior=1.5,Object.defineProperty(this,"reflectivity",{get:function(){return Vt(2.5*(this.ior-1)/(this.ior+1),0,1)},set:function(e){this.ior=(1+.4*e)/(1-.4*e)}}),this.iridescenceMap=null,this.iridescenceIOR=1.3,this.iridescenceThicknessMinimum=100,this.iridescenceThicknessMaximum=400,this.iridescenceThicknessMap=null,this.sheenColor=new bt(0),this.sheenColorMap=null,this.sheenRoughness=1,this.sheenRoughnessMap=null,this.transmissionMap=null,this.thickness=0,this.thicknessMap=null,this.attenuationDistance=1/0,this.attenuationColor=new bt(1,1,1),this.specularIntensity=1,this.specularIntensityMap=null,this.specularColor=new bt(1,1,1),this.specularColorMap=null,this._anisotropy=0,this._clearcoat=0,this._diffuseRoughness=0,this._dispersion=0,this._iridescence=0,this._retroreflectivity=0,this._sheen=0,this._transmission=0,this.setValues(t)}get anisotropy(){return this._anisotropy}set anisotropy(t){this._anisotropy>0!=t>0&&this.version++,this._anisotropy=t}get clearcoat(){return this._clearcoat}set clearcoat(t){this._clearcoat>0!=t>0&&this.version++,this._clearcoat=t}get diffuseRoughness(){return this._diffuseRoughness}set diffuseRoughness(t){this._diffuseRoughness>0!=t>0&&this.version++,this._diffuseRoughness=t}get iridescence(){return this._iridescence}set iridescence(t){this._iridescence>0!=t>0&&this.version++,this._iridescence=t}get dispersion(){return this._dispersion}set dispersion(t){this._dispersion>0!=t>0&&this.version++,this._dispersion=t}get retroreflectivity(){return this._retroreflectivity}set retroreflectivity(t){this._retroreflectivity>0!=t>0&&this.version++,this._retroreflectivity=t}get sheen(){return this._sheen}set sheen(t){this._sheen>0!=t>0&&this.version++,this._sheen=t}get transmission(){return this._transmission}set transmission(t){this._transmission>0!=t>0&&this.version++,this._transmission=t}get iridescenceThicknessRange(){return $n('MeshPhysicalMaterial: The "iridescenceThicknessRange" property has been replaced by "iridescenceThicknessMinimum" and "iridescenceThicknessMaximum".'),[this.iridescenceThicknessMinimum,this.iridescenceThicknessMaximum]}set iridescenceThicknessRange(t){$n('MeshPhysicalMaterial: The "iridescenceThicknessRange" property has been replaced by "iridescenceThicknessMinimum" and "iridescenceThicknessMaximum".'),this.iridescenceThicknessMinimum=t[0],this.iridescenceThicknessMaximum=t[1]}copy(t){return super.copy(t),this.defines={STANDARD:"",PHYSICAL:""},this.anisotropy=t.anisotropy,this.anisotropyRotation=t.anisotropyRotation,this.anisotropyMap=t.anisotropyMap,this.clearcoat=t.clearcoat,this.clearcoatMap=t.clearcoatMap,this.clearcoatRoughness=t.clearcoatRoughness,this.clearcoatRoughnessMap=t.clearcoatRoughnessMap,this.clearcoatNormalMap=t.clearcoatNormalMap,this.clearcoatNormalScale.copy(t.clearcoatNormalScale),this.diffuseRoughness=t.diffuseRoughness,this.diffuseRoughnessMap=t.diffuseRoughnessMap,this.dispersion=t.dispersion,this.ior=t.ior,this.iridescence=t.iridescence,this.iridescenceMap=t.iridescenceMap,this.iridescenceIOR=t.iridescenceIOR,this.iridescenceThicknessMinimum=t.iridescenceThicknessMinimum,this.iridescenceThicknessMaximum=t.iridescenceThicknessMaximum,this.iridescenceThicknessMap=t.iridescenceThicknessMap,this.retroreflectivity=t.retroreflectivity,this.sheen=t.sheen,this.sheenColor.copy(t.sheenColor),this.sheenColorMap=t.sheenColorMap,this.sheenRoughness=t.sheenRoughness,this.sheenRoughnessMap=t.sheenRoughnessMap,this.transmission=t.transmission,this.transmissionMap=t.transmissionMap,this.thickness=t.thickness,this.thicknessMap=t.thicknessMap,this.attenuationDistance=t.attenuationDistance,this.attenuationColor.copy(t.attenuationColor),this.specularIntensity=t.specularIntensity,this.specularIntensityMap=t.specularIntensityMap,this.specularColor.copy(t.specularColor),this.specularColorMap=t.specularColorMap,this}};var qr=class extends Ui{constructor(t){super(),this.isMeshLambertMaterial=!0,this.type="MeshLambertMaterial",this.color=new bt(16777215),this.map=null,this.lightMap=null,this.lightMapIntensity=1,this.aoMap=null,this.aoMapIntensity=1,this.emissive=new bt(0),this.emissiveIntensity=1,this.emissiveMap=null,this.bumpMap=null,this.bumpScale=1,this.normalMap=null,this.normalMapType=po,this.normalScale=new et(1,1),this.displacementMap=null,this.displacementScale=1,this.displacementBias=0,this.specularMap=null,this.alphaMap=null,this.envMap=null,this.envMapRotation=new Ai,this.combine=Ja,this.reflectivity=1,this.envMapIntensity=1,this.refractionRatio=.98,this.wireframe=!1,this.wireframeLinewidth=1,this.wireframeLinecap="round",this.wireframeLinejoin="round",this.flatShading=!1,this.fog=!0,this.setValues(t)}copy(t){return super.copy(t),this.color.copy(t.color),this.map=t.map,this.lightMap=t.lightMap,this.lightMapIntensity=t.lightMapIntensity,this.aoMap=t.aoMap,this.aoMapIntensity=t.aoMapIntensity,this.emissive.copy(t.emissive),this.emissiveMap=t.emissiveMap,this.emissiveIntensity=t.emissiveIntensity,this.bumpMap=t.bumpMap,this.bumpScale=t.bumpScale,this.normalMap=t.normalMap,this.normalMapType=t.normalMapType,this.normalScale.copy(t.normalScale),this.displacementMap=t.displacementMap,this.displacementScale=t.displacementScale,this.displacementBias=t.displacementBias,this.specularMap=t.specularMap,this.alphaMap=t.alphaMap,this.envMap=t.envMap,this.envMapRotation.copy(t.envMapRotation),this.combine=t.combine,this.reflectivity=t.reflectivity,this.envMapIntensity=t.envMapIntensity,this.refractionRatio=t.refractionRatio,this.wireframe=t.wireframe,this.wireframeLinewidth=t.wireframeLinewidth,this.wireframeLinecap=t.wireframeLinecap,this.wireframeLinejoin=t.wireframeLinejoin,this.flatShading=t.flatShading,this.fog=t.fog,this}},Ua=class extends Ui{constructor(t){super(),this.isMeshDepthMaterial=!0,this.type="MeshDepthMaterial",this.depthPacking=Gd,this.map=null,this.alphaMap=null,this.displacementMap=null,this.displacementScale=1,this.displacementBias=0,this.wireframe=!1,this.wireframeLinewidth=1,this.setValues(t)}copy(t){return super.copy(t),this.depthPacking=t.depthPacking,this.map=t.map,this.alphaMap=t.alphaMap,this.displacementMap=t.displacementMap,this.displacementScale=t.displacementScale,this.displacementBias=t.displacementBias,this.wireframe=t.wireframe,this.wireframeLinewidth=t.wireframeLinewidth,this}},Fa=class extends Ui{constructor(t){super(),this.isMeshDistanceMaterial=!0,this.type="MeshDistanceMaterial",this.map=null,this.alphaMap=null,this.displacementMap=null,this.displacementScale=1,this.displacementBias=0,this.setValues(t)}copy(t){return super.copy(t),this.map=t.map,this.alphaMap=t.alphaMap,this.displacementMap=t.displacementMap,this.displacementScale=t.displacementScale,this.displacementBias=t.displacementBias,this}};function Is(n,t){return!n||n.constructor===t?n:typeof t.BYTES_PER_ELEMENT=="number"?new t(n):Array.prototype.slice.call(n)}function ha(n){return n!==void 0&&n.inTangents!==void 0&&n.outTangents!==void 0}var Nn=class{constructor(t,e,i,s){this.parameterPositions=t,this._cachedIndex=0,this.resultBuffer=s!==void 0?s:new e.constructor(i),this.sampleValues=e,this.valueSize=i,this.settings=null,this.DefaultSettings_={}}evaluate(t){let e=this.parameterPositions,i=this._cachedIndex,s=e[i],r=e[i-1];t:{e:{let o;i:{n:if(!(t<s)){for(let a=i+2;;){if(s===void 0){if(t<r)break n;return i=e.length,this._cachedIndex=i,this.copySampleValue_(i-1)}if(i===a)break;if(r=s,s=e[++i],t<s)break e}o=e.length;break i}if(!(t>=r)){let a=e[1];t<a&&(i=2,r=a);for(let l=i-2;;){if(r===void 0)return this._cachedIndex=0,this.copySampleValue_(0);if(i===l)break;if(s=r,r=e[--i-1],t>=r)break e}o=i,i=0;break i}break t}for(;i<o;){let a=i+o>>>1;t<e[a]?o=a:i=a+1}if(s=e[i],r=e[i-1],r===void 0)return this._cachedIndex=0,this.copySampleValue_(0);if(s===void 0)return i=e.length,this._cachedIndex=i,this.copySampleValue_(i-1)}this._cachedIndex=i,this.intervalChanged_(i,r,s)}return this.interpolate_(i,r,t,s)}getSettings_(){return this.settings||this.DefaultSettings_}copySampleValue_(t){let e=this.resultBuffer,i=this.sampleValues,s=this.valueSize,r=t*s;for(let o=0;o!==s;++o)e[o]=i[r+o];return e}interpolate_(){throw new Error("THREE.Interpolant: Call to abstract method.")}intervalChanged_(){}},Oa=class extends Nn{constructor(t,e,i,s){super(t,e,i,s),this._weightPrev=-0,this._offsetPrev=-0,this._weightNext=-0,this._offsetNext=-0,this.DefaultSettings_={endingStart:ih,endingEnd:ih}}intervalChanged_(t,e,i){let s=this.parameterPositions,r=t-2,o=t+1,a=s[r],l=s[o];if(a===void 0)switch(this.getSettings_().endingStart){case nh:r=t,a=2*e-i;break;case sh:r=s.length-2,a=e+s[r]-s[r+1];break;default:r=t,a=i}if(l===void 0)switch(this.getSettings_().endingEnd){case nh:o=t,l=2*i-e;break;case sh:o=1,l=i+s[1]-s[0];break;default:o=t-1,l=e}let c=(i-e)*.5,h=this.valueSize;this._weightPrev=c/(e-a),this._weightNext=c/(l-i),this._offsetPrev=r*h,this._offsetNext=o*h}interpolate_(t,e,i,s){let r=this.resultBuffer,o=this.sampleValues,a=this.valueSize,l=t*a,c=l-a,h=this._offsetPrev,u=this._offsetNext,d=this._weightPrev,f=this._weightNext,m=(i-e)/(s-e),x=m*m,p=x*m,g=-d*p+2*d*x-d*m,y=(1+d)*p+(-1.5-2*d)*x+(-.5+d)*m+1,v=(-1-f)*p+(1.5+f)*x+.5*m,S=f*p-f*x;for(let T=0;T!==a;++T)r[T]=g*o[h+T]+y*o[c+T]+v*o[l+T]+S*o[u+T];return r}},Ba=class extends Nn{constructor(t,e,i,s){super(t,e,i,s)}interpolate_(t,e,i,s){let r=this.resultBuffer,o=this.sampleValues,a=this.valueSize,l=t*a,c=l-a,h=(i-e)/(s-e),u=1-h;for(let d=0;d!==a;++d)r[d]=o[c+d]*u+o[l+d]*h;return r}},za=class extends Nn{constructor(t,e,i,s){super(t,e,i,s)}interpolate_(t){return this.copySampleValue_(t-1)}},ka=class extends Nn{interpolate_(t,e,i,s){let r=this.resultBuffer,o=this.sampleValues,a=this.valueSize,l=t*a,c=l-a,h=this.inTangents,u=this.outTangents;if(!h||!u){let m=(i-e)/(s-e),x=1-m;for(let p=0;p!==a;++p)r[p]=o[c+p]*x+o[l+p]*m;return r}let d=a*2,f=t-1;for(let m=0;m!==a;++m){let x=o[c+m],p=o[l+m],g=f*d+m*2,y=u[g],v=u[g+1],S=t*d+m*2,T=h[S],b=h[S+1],_=Km(i,e,y,T,s);r[m]=of(_,x,v,b,p)}return r}};function of(n,t,e,i,s){let r=1-n;return r*r*r*t+3*r*r*n*e+3*r*n*n*i+n*n*n*s}function Zm(n,t,e,i,s){let r=1-n;return 3*r*r*(e-t)+6*r*n*(i-e)+3*n*n*(s-i)}function Km(n,t,e,i,s){let r=(n-t)/(s-t);for(let o=0;o<8;o++){let a=of(r,t,e,i,s)-n;if(Math.abs(a)<1e-10)break;let l=Zm(r,t,e,i,s);if(Math.abs(l)<1e-10)break;r=Math.max(0,Math.min(1,r-a/l))}return r}var bi=class{constructor(t,e,i,s){if(t===void 0)throw new Error("THREE.KeyframeTrack: track name is undefined");if(e===void 0||e.length===0)throw new Error("THREE.KeyframeTrack: no keyframes in track named "+t);this.name=t,this.times=Is(e,this.TimeBufferType),this.values=Is(i,this.ValueBufferType),this.setInterpolation(s||this.DefaultInterpolation)}static toJSON(t){let e=t.constructor,i;if(e.toJSON!==this.toJSON)i=e.toJSON(t);else{i={name:t.name,times:Is(t.times,Array),values:Is(t.values,Array)};let s=t.getInterpolation();s!==t.DefaultInterpolation&&(i.interpolation=s),ha(t.settings)&&(i.settings={inTangents:Is(t.settings.inTangents,Array),outTangents:Is(t.settings.outTangents,Array)})}return i.type=t.ValueTypeName,i}InterpolantFactoryMethodDiscrete(t){return new za(this.times,this.values,this.getValueSize(),t)}InterpolantFactoryMethodLinear(t){return new Ba(this.times,this.values,this.getValueSize(),t)}InterpolantFactoryMethodSmooth(t){return new Oa(this.times,this.values,this.getValueSize(),t)}InterpolantFactoryMethodBezier(t){let e=new ka(this.times,this.values,this.getValueSize(),t);return this.settings&&(e.inTangents=this.settings.inTangents,e.outTangents=this.settings.outTangents),e}setInterpolation(t){let e;switch(t){case Pr:e=this.InterpolantFactoryMethodDiscrete;break;case ba:e=this.InterpolantFactoryMethodLinear;break;case fa:e=this.InterpolantFactoryMethodSmooth;break;case eh:e=this.InterpolantFactoryMethodBezier;break}if(e===void 0){let i="unsupported interpolation for "+this.ValueTypeName+" keyframe track named "+this.name;if(this.createInterpolant===void 0)if(t!==this.DefaultInterpolation)this.setInterpolation(this.DefaultInterpolation);else throw new Error(i);return Lt("KeyframeTrack:",i),this}return this.createInterpolant=e,this}getInterpolation(){switch(this.createInterpolant){case this.InterpolantFactoryMethodDiscrete:return Pr;case this.InterpolantFactoryMethodLinear:return ba;case this.InterpolantFactoryMethodSmooth:return fa;case this.InterpolantFactoryMethodBezier:return eh}}getValueSize(){return this.values.length/this.times.length}shift(t){if(t!==0){let e=this.times;for(let i=0,s=e.length;i!==s;++i)e[i]+=t;ha(this.settings)&&(pd(this.settings.inTangents,t),pd(this.settings.outTangents,t))}return this}scale(t){if(t!==1){let e=this.times;for(let i=0,s=e.length;i!==s;++i)e[i]*=t;ha(this.settings)&&(md(this.settings.inTangents,t),md(this.settings.outTangents,t))}return this}trim(t,e){let i=this.times,s=i.length,r=0,o=s-1;for(;r!==s&&i[r]<t;)++r;for(;o!==-1&&i[o]>e;)--o;if(++o,r!==0||o!==s){r>=o&&(o=Math.max(o,1),r=o-1);let a=this.getValueSize();this.times=i.slice(r,o),this.values=this.values.slice(r*a,o*a)}return this}validate(){let t=!0,e=this.getValueSize();e-Math.floor(e)!==0&&(Ot("KeyframeTrack: Invalid value size in track.",this),t=!1);let i=this.times,s=this.values,r=i.length;r===0&&(Ot("KeyframeTrack: Track is empty.",this),t=!1);let o=null;for(let a=0;a!==r;a++){let l=i[a];if(typeof l=="number"&&isNaN(l)){Ot("KeyframeTrack: Time is not a valid number.",this,a,l),t=!1;break}if(o!==null&&o>l){Ot("KeyframeTrack: Out of order keys.",this,a,l,o),t=!1;break}o=l}if(s!==void 0&&tm(s))for(let a=0,l=s.length;a!==l;++a){let c=s[a];if(isNaN(c)){Ot("KeyframeTrack: Value is not a valid number.",this,a,c),t=!1;break}}return t}optimize(){let t=this.times.slice(),e=this.values.slice(),i=this.getValueSize(),s=this.getInterpolation()===fa,r=t.length-1,o=1;for(let a=1;a<r;++a){let l=!1,c=t[a],h=t[a+1];if(c!==h&&(a!==1||c!==t[0]))if(s)l=!0;else{let u=a*i,d=u-i,f=u+i;for(let m=0;m!==i;++m){let x=e[u+m];if(x!==e[d+m]||x!==e[f+m]){l=!0;break}}}if(l){if(a!==o){t[o]=t[a];let u=a*i,d=o*i;for(let f=0;f!==i;++f)e[d+f]=e[u+f]}++o}}if(r>0){t[o]=t[r];for(let a=r*i,l=o*i,c=0;c!==i;++c)e[l+c]=e[a+c];++o}return o!==t.length?(this.times=t.slice(0,o),this.values=e.slice(0,o*i)):(this.times=t,this.values=e),this}clone(){let t=this.times.slice(),e=this.values.slice(),i=this.constructor,s=new i(this.name,t,e);return s.createInterpolant=this.createInterpolant,ha(this.settings)&&(s.settings={inTangents:this.settings.inTangents.slice(),outTangents:this.settings.outTangents.slice()}),s}};function pd(n,t){for(let e=0,i=n.length;e!==i;e+=2)n[e]+=t}function md(n,t){for(let e=0,i=n.length;e!==i;e+=2)n[e]*=t}bi.prototype.ValueTypeName="";bi.prototype.TimeBufferType=Float32Array;bi.prototype.ValueBufferType=Float32Array;bi.prototype.DefaultInterpolation=ba;var Un=class extends bi{constructor(t,e,i){super(t,e,i)}};Un.prototype.ValueTypeName="bool";Un.prototype.ValueBufferType=Array;Un.prototype.DefaultInterpolation=Pr;Un.prototype.InterpolantFactoryMethodLinear=void 0;Un.prototype.InterpolantFactoryMethodSmooth=void 0;var Ha=class extends bi{constructor(t,e,i,s){super(t,e,i,s)}};Ha.prototype.ValueTypeName="color";var Va=class extends bi{constructor(t,e,i,s){super(t,e,i,s)}};Va.prototype.ValueTypeName="number";var Ga=class extends Nn{constructor(t,e,i,s){super(t,e,i,s)}interpolate_(t,e,i,s){let r=this.resultBuffer,o=this.sampleValues,a=this.valueSize,l=(i-e)/(s-e),c=t*a;for(let h=c+a;c!==h;c+=4)ti.slerpFlat(r,0,o,c-a,o,c,l);return r}},Yr=class extends bi{constructor(t,e,i,s){super(t,e,i,s)}InterpolantFactoryMethodLinear(t){return new Ga(this.times,this.values,this.getValueSize(),t)}};Yr.prototype.ValueTypeName="quaternion";Yr.prototype.InterpolantFactoryMethodSmooth=void 0;var Fn=class extends bi{constructor(t,e,i){super(t,e,i)}};Fn.prototype.ValueTypeName="string";Fn.prototype.ValueBufferType=Array;Fn.prototype.DefaultInterpolation=Pr;Fn.prototype.InterpolantFactoryMethodLinear=void 0;Fn.prototype.InterpolantFactoryMethodSmooth=void 0;var Wa=class extends bi{constructor(t,e,i,s){super(t,e,i,s)}};Wa.prototype.ValueTypeName="vector";var Xa=class{constructor(t,e,i){let s=this,r=!1,o=0,a=0,l,c=[];this.onStart=void 0,this.onLoad=t,this.onProgress=e,this.onError=i,this._abortController=null,this.itemStart=function(h){a++,r===!1&&s.onStart!==void 0&&s.onStart(h,o,a),r=!0},this.itemEnd=function(h){o++,s.onProgress!==void 0&&s.onProgress(h,o,a),o===a&&(r=!1,s.onLoad!==void 0&&s.onLoad())},this.itemError=function(h){s.onError!==void 0&&s.onError(h)},this.resolveURL=function(h){return h=h.normalize("NFC"),l?l(h):h},this.setURLModifier=function(h){return l=h,this},this.addHandler=function(h,u){return c.push(h,u),this},this.removeHandler=function(h){let u=c.indexOf(h);return u!==-1&&c.splice(u,2),this},this.getHandler=function(h){for(let u=0,d=c.length;u<d;u+=2){let f=c[u],m=c[u+1];if(f.global&&(f.lastIndex=0),f.test(h))return m}return null},this.abort=function(){return this.abortController.abort(),this._abortController=null,this}}get abortController(){return this._abortController||(this._abortController=new AbortController),this._abortController}},af=new Xa,qa=class{constructor(t){this.manager=t!==void 0?t:af,this.crossOrigin="anonymous",this.withCredentials=!1,this.path="",this.resourcePath="",this.requestHeader={},this.priority="auto",this.cache="default",typeof __THREE_DEVTOOLS__<"u"&&__THREE_DEVTOOLS__.dispatchEvent(new CustomEvent("observe",{detail:this}))}load(){}loadAsync(t,e){let i=this;return new Promise(function(s,r){i.load(t,s,e,r)})}parse(){}setCrossOrigin(t){return this.crossOrigin=t,this}setWithCredentials(t){return this.withCredentials=t,this}setPath(t){return this.path=t,this}setResourcePath(t){return this.resourcePath=t,this}setRequestHeader(t){return this.requestHeader=t,this}setPriority(t){return this.priority=t,this}setCache(t){return this.cache=t,this}abort(){return this}};qa.DEFAULT_MATERIAL_NAME="__DEFAULT";var rs=class extends Me{static registerNode(t){this.prototype._lightNode=t}constructor(t,e=1){super(),this.isLight=!0,this.type="Light",this.color=new bt(t),this.intensity=e}copy(t,e){return super.copy(t,e),this.color.copy(t.color),this.intensity=t.intensity,this}toJSON(t){let e=super.toJSON(t);return e.object.color=this.color.getHex(),e.object.intensity=this.intensity,e}},Zs=class extends rs{constructor(t,e,i){super(t,i),this.isHemisphereLight=!0,this.type="HemisphereLight",this.position.copy(Me.DEFAULT_UP),this.updateMatrix(),this.groundColor=new bt(e)}copy(t,e){return super.copy(t,e),this.groundColor.copy(t.groundColor),this}toJSON(t){let e=super.toJSON(t);return e.object.groundColor=this.groundColor.getHex(),e}},th=new Xt,gd=new C,xd=new C,Ks=class{constructor(t){this.camera=t,this.intensity=1,this.bias=0,this.biasNode=null,this.normalBias=0,this.radius=1,this.blurSamples=8,this.mapSize=new et(512,512),this.mapType=ei,this.map=null,this.mapPass=null,this.matrix=new Xt,this.autoUpdate=!0,this.needsUpdate=!1,this._frustum=new Dn,this._frameExtents=new et(1,1),this._viewportCount=1,this._viewports=[new oe(0,0,1,1)]}getViewportCount(){return this._viewportCount}getCamera(){return this.camera}getFrustum(){return this._frustum}updateMatrices(t){let e=this.camera;gd.setFromMatrixPosition(t.matrixWorld),e.position.copy(gd),xd.setFromMatrixPosition(t.target.matrixWorld),e.lookAt(xd),e.updateMatrixWorld(),this._updateMatrix(e,this.matrix,this._frustum)}_updateMatrix(t,e,i,s){th.multiplyMatrices(t.projectionMatrix,t.matrixWorldInverse),i.setFromProjectionMatrix(th,t.coordinateSystem,t.reversedDepth);let r=this._frameExtents,o=s?s.z/r.x:1,a=s?s.w/r.y:1,l=s?s.x/r.x:0,c=s?s.y/r.y:0;t.coordinateSystem===Os||t.reversedDepth?e.set(.5*o,0,0,.5*o+l,0,.5*a,0,.5*a+c,0,0,1,0,0,0,0,1):e.set(.5*o,0,0,.5*o+l,0,.5*a,0,.5*a+c,0,0,.5,.5,0,0,0,1),e.multiply(th)}getViewport(t){return this._viewports[t]}getFrameExtents(){return this._frameExtents}dispose(){this.map&&this.map.dispose(),this.mapPass&&this.mapPass.dispose()}copy(t){return this.camera=t.camera.clone(),this.intensity=t.intensity,this.bias=t.bias,this.radius=t.radius,this.autoUpdate=t.autoUpdate,this.needsUpdate=t.needsUpdate,this.normalBias=t.normalBias,this.blurSamples=t.blurSamples,this.mapSize.copy(t.mapSize),this.mapType=t.mapType,this.biasNode=t.biasNode,this}clone(){return new this.constructor().copy(this)}toJSON(){let t={};return t.intensity=this.intensity,t.bias=this.bias,t.normalBias=this.normalBias,t.radius=this.radius,t.blurSamples=this.blurSamples,t.mapSize=this.mapSize.toArray(),t.mapType=this.mapType,t.camera=this.camera.toJSON(!1).object,delete t.camera.matrix,t}},ua=new C,da=new ti,Gi=new C,mn=class extends Me{constructor(){super(),this.isCamera=!0,this.type="Camera",this.matrixWorldInverse=new Xt,this.projectionMatrix=new Xt,this.projectionMatrixInverse=new Xt,this.coordinateSystem=Ti,this._reversedDepth=!1}get reversedDepth(){return this._reversedDepth}copy(t,e){return super.copy(t,e),this.matrixWorldInverse.copy(t.matrixWorldInverse),this.projectionMatrix.copy(t.projectionMatrix),this.projectionMatrixInverse.copy(t.projectionMatrixInverse),this.coordinateSystem=t.coordinateSystem,this}getWorldDirection(t){return super.getWorldDirection(t).negate()}updateMatrixWorld(t){super.updateMatrixWorld(t),this.matrixWorld.decompose(ua,da,Gi),Gi.x===1&&Gi.y===1&&Gi.z===1?this.matrixWorldInverse.copy(this.matrixWorld).invert():this.matrixWorldInverse.compose(ua,da,Gi.set(1,1,1)).invert()}updateWorldMatrix(t,e,i=!1){super.updateWorldMatrix(t,e,i),this.matrixWorld.decompose(ua,da,Gi),Gi.x===1&&Gi.y===1&&Gi.z===1?this.matrixWorldInverse.copy(this.matrixWorld).invert():this.matrixWorldInverse.compose(ua,da,Gi.set(1,1,1)).invert()}clone(){return new this.constructor().copy(this)}},Pn=new C,_d=new et,yd=new et,Xe=class extends mn{constructor(t=50,e=1,i=.1,s=2e3){super(),this.isPerspectiveCamera=!0,this.type="PerspectiveCamera",this.fov=t,this.zoom=1,this.near=i,this.far=s,this.focus=10,this.aspect=e,this.view=null,this.filmGauge=35,this.filmOffset=0,this.updateProjectionMatrix()}copy(t,e){return super.copy(t,e),this.fov=t.fov,this.zoom=t.zoom,this.near=t.near,this.far=t.far,this.focus=t.focus,this.aspect=t.aspect,this.view=t.view===null?null:Object.assign({},t.view),this.filmGauge=t.filmGauge,this.filmOffset=t.filmOffset,this}setFocalLength(t){let e=.5*this.getFilmHeight()/t;this.fov=jn*2*Math.atan(e),this.updateProjectionMatrix()}getFocalLength(){let t=Math.tan(Ns*.5*this.fov);return .5*this.getFilmHeight()/t}getEffectiveFOV(){return jn*2*Math.atan(Math.tan(Ns*.5*this.fov)/this.zoom)}getFilmWidth(){return this.filmGauge*Math.min(this.aspect,1)}getFilmHeight(){return this.filmGauge/Math.max(this.aspect,1)}getViewBounds(t,e,i){Pn.set(-1,-1,.5).applyMatrix4(this.projectionMatrixInverse),e.set(Pn.x,Pn.y).multiplyScalar(-t/Pn.z),Pn.set(1,1,.5).applyMatrix4(this.projectionMatrixInverse),i.set(Pn.x,Pn.y).multiplyScalar(-t/Pn.z)}getViewSize(t,e){return this.getViewBounds(t,_d,yd),e.subVectors(yd,_d)}setViewOffset(t,e,i,s,r,o){this.aspect=t/e,this.view===null&&(this.view={enabled:!0,fullWidth:1,fullHeight:1,offsetX:0,offsetY:0,width:1,height:1}),this.view.enabled=!0,this.view.fullWidth=t,this.view.fullHeight=e,this.view.offsetX=i,this.view.offsetY=s,this.view.width=r,this.view.height=o,this.updateProjectionMatrix()}clearViewOffset(){this.view!==null&&(this.view.enabled=!1),this.updateProjectionMatrix()}updateProjectionMatrix(){let t=this.near,e=t*Math.tan(Ns*.5*this.fov)/this.zoom,i=2*e,s=this.aspect*i,r=-.5*s,o=this.view;if(this.view!==null&&this.view.enabled){let l=o.fullWidth,c=o.fullHeight;r+=o.offsetX*s/l,e-=o.offsetY*i/c,s*=o.width/l,i*=o.height/c}let a=this.filmOffset;a!==0&&(r+=t*a/this.getFilmWidth()),this.projectionMatrix.makePerspective(r,r+s,e,e-i,t,this.far,this.coordinateSystem,this.reversedDepth),this.projectionMatrixInverse.copy(this.projectionMatrix).invert()}toJSON(t){let e=super.toJSON(t);return e.object.fov=this.fov,e.object.zoom=this.zoom,e.object.near=this.near,e.object.far=this.far,e.object.focus=this.focus,e.object.aspect=this.aspect,this.view!==null&&(e.object.view=Object.assign({},this.view)),e.object.filmGauge=this.filmGauge,e.object.filmOffset=this.filmOffset,e}},ah=class extends Ks{constructor(){super(new Xe(50,1,.5,500)),this.isSpotLightShadow=!0,this.focus=1,this.aspect=1}updateMatrices(t){let e=this.camera,i=jn*2*t.angle*this.focus,s=this.mapSize.width/this.mapSize.height*this.aspect,r=t.distance||e.far;(i!==e.fov||s!==e.aspect||r!==e.far)&&(e.fov=i,e.aspect=s,e.far=r,e.updateProjectionMatrix()),super.updateMatrices(t)}copy(t){return super.copy(t),this.focus=t.focus,this.aspect=t.aspect,this}toJSON(){let t=super.toJSON();return t.focus=this.focus,t.aspect=this.aspect,t}},os=class extends rs{constructor(t,e,i=0,s=Math.PI/3,r=0,o=2){super(t,e),this.isSpotLight=!0,this.type="SpotLight",this.position.copy(Me.DEFAULT_UP),this.updateMatrix(),this.target=new Me,this.distance=i,this.angle=s,this.penumbra=r,this.decay=o,this.map=null,this.shadow=new ah}get power(){return this.intensity*Math.PI}set power(t){this.intensity=t/Math.PI}dispose(){super.dispose(),this.shadow.dispose()}copy(t,e){return super.copy(t,e),this.distance=t.distance,this.angle=t.angle,this.penumbra=t.penumbra,this.decay=t.decay,this.target=t.target.clone(),this.map=t.map,this.shadow=t.shadow.clone(),this}toJSON(t){let e=super.toJSON(t);return e.object.distance=this.distance,e.object.angle=this.angle,e.object.decay=this.decay,e.object.penumbra=this.penumbra,e.object.target=this.target.uuid,this.map&&this.map.isTexture&&(e.object.map=this.map.toJSON(t).uuid),e.object.shadow=this.shadow.toJSON(),e}},lh=class extends Ks{constructor(){super(new Xe(90,1,.5,500)),this.isPointLightShadow=!0}},Zr=class extends rs{constructor(t,e,i=0,s=2){super(t,e),this.isPointLight=!0,this.type="PointLight",this.distance=i,this.decay=s,this.shadow=new lh}get power(){return this.intensity*4*Math.PI}set power(t){this.intensity=t/(4*Math.PI)}dispose(){super.dispose(),this.shadow.dispose()}copy(t,e){return super.copy(t,e),this.distance=t.distance,this.decay=t.decay,this.shadow=t.shadow.clone(),this}toJSON(t){let e=super.toJSON(t);return e.object.distance=this.distance,e.object.decay=this.decay,e.object.shadow=this.shadow.toJSON(),e}},as=class extends mn{constructor(t=-1,e=1,i=1,s=-1,r=.1,o=2e3){super(),this.isOrthographicCamera=!0,this.type="OrthographicCamera",this.zoom=1,this.view=null,this.left=t,this.right=e,this.top=i,this.bottom=s,this.near=r,this.far=o,this.updateProjectionMatrix()}copy(t,e){return super.copy(t,e),this.left=t.left,this.right=t.right,this.top=t.top,this.bottom=t.bottom,this.near=t.near,this.far=t.far,this.zoom=t.zoom,this.view=t.view===null?null:Object.assign({},t.view),this}setViewOffset(t,e,i,s,r,o){this.view===null&&(this.view={enabled:!0,fullWidth:1,fullHeight:1,offsetX:0,offsetY:0,width:1,height:1}),this.view.enabled=!0,this.view.fullWidth=t,this.view.fullHeight=e,this.view.offsetX=i,this.view.offsetY=s,this.view.width=r,this.view.height=o,this.updateProjectionMatrix()}clearViewOffset(){this.view!==null&&(this.view.enabled=!1),this.updateProjectionMatrix()}updateProjectionMatrix(){let t=(this.right-this.left)/(2*this.zoom),e=(this.top-this.bottom)/(2*this.zoom),i=(this.right+this.left)/2,s=(this.top+this.bottom)/2,r=i-t,o=i+t,a=s+e,l=s-e;if(this.view!==null&&this.view.enabled){let c=(this.right-this.left)/this.view.fullWidth/this.zoom,h=(this.top-this.bottom)/this.view.fullHeight/this.zoom;r+=c*this.view.offsetX,o=r+c*this.view.width,a-=h*this.view.offsetY,l=a-h*this.view.height}this.projectionMatrix.makeOrthographic(r,o,a,l,this.near,this.far,this.coordinateSystem,this.reversedDepth),this.projectionMatrixInverse.copy(this.projectionMatrix).invert()}toJSON(t){let e=super.toJSON(t);return e.object.zoom=this.zoom,e.object.left=this.left,e.object.right=this.right,e.object.top=this.top,e.object.bottom=this.bottom,e.object.near=this.near,e.object.far=this.far,this.view!==null&&(e.object.view=Object.assign({},this.view)),e}},ch=class extends Ks{constructor(){super(new as(-5,5,5,-5,.5,500)),this.isDirectionalLightShadow=!0}},Kr=class extends rs{constructor(t,e){super(t,e),this.isDirectionalLight=!0,this.type="DirectionalLight",this.position.copy(Me.DEFAULT_UP),this.updateMatrix(),this.target=new Me,this.shadow=new ch}dispose(){super.dispose(),this.shadow.dispose()}copy(t){return super.copy(t),this.target=t.target.clone(),this.shadow=t.shadow.clone(),this}toJSON(t){let e=super.toJSON(t);return e.object.shadow=this.shadow.toJSON(),e.object.target=this.target.uuid,e}};var Jr=class extends Jt{constructor(){super(),this.isInstancedBufferGeometry=!0,this.type="InstancedBufferGeometry",this.instanceCount=1/0}copy(t){return super.copy(t),this.instanceCount=t.instanceCount,this}toJSON(){let t=super.toJSON();return t.instanceCount=this.instanceCount,t.isInstancedBufferGeometry=!0,t}};var Ds=-90,Ls=1,Js=class extends Me{constructor(t,e,i){super(),this.type="CubeCamera",this.renderTarget=i,this.coordinateSystem=null,this.activeMipmapLevel=0;let s=new Xe(Ds,Ls,t,e);s.layers=this.layers,this.add(s);let r=new Xe(Ds,Ls,t,e);r.layers=this.layers,this.add(r);let o=new Xe(Ds,Ls,t,e);o.layers=this.layers,this.add(o);let a=new Xe(Ds,Ls,t,e);a.layers=this.layers,this.add(a);let l=new Xe(Ds,Ls,t,e);l.layers=this.layers,this.add(l);let c=new Xe(Ds,Ls,t,e);c.layers=this.layers,this.add(c)}updateCoordinateSystem(){let t=this.coordinateSystem,e=this.children.concat(),[i,s,r,o,a,l]=e;for(let c of e)this.remove(c);if(t===Ti)i.up.set(0,1,0),i.lookAt(1,0,0),s.up.set(0,1,0),s.lookAt(-1,0,0),r.up.set(0,0,-1),r.lookAt(0,1,0),o.up.set(0,0,1),o.lookAt(0,-1,0),a.up.set(0,1,0),a.lookAt(0,0,1),l.up.set(0,1,0),l.lookAt(0,0,-1);else if(t===Os)i.up.set(0,-1,0),i.lookAt(-1,0,0),s.up.set(0,-1,0),s.lookAt(1,0,0),r.up.set(0,0,1),r.lookAt(0,1,0),o.up.set(0,0,-1),o.lookAt(0,-1,0),a.up.set(0,-1,0),a.lookAt(0,0,1),l.up.set(0,-1,0),l.lookAt(0,0,-1);else throw new Error("THREE.CubeCamera.updateCoordinateSystem(): Invalid coordinate system: "+t);for(let c of e)this.add(c),c.updateMatrixWorld()}update(t,e){this.parent===null&&this.updateMatrixWorld();let{renderTarget:i,activeMipmapLevel:s}=this;this.coordinateSystem!==t.coordinateSystem&&(this.coordinateSystem=t.coordinateSystem,this.updateCoordinateSystem());let[r,o,a,l,c,h]=this.children,u=t.getRenderTarget(),d=t.getActiveCubeFace(),f=t.getActiveMipmapLevel(),m=t.xr.enabled;t.xr.enabled=!1;let x=i.texture.mipmapsAutoUpdate;i.texture.mipmapsAutoUpdate=!1;let p=!1;t.isWebGLRenderer===!0?p=t.state.buffers.depth.getReversed():p=t.reversedDepthBuffer,t.setRenderTarget(i,0,s),p&&t.autoClear===!1&&t.clearDepth(),t.render(e,r),t.setRenderTarget(i,1,s),p&&t.autoClear===!1&&t.clearDepth(),t.render(e,o),t.setRenderTarget(i,2,s),p&&t.autoClear===!1&&t.clearDepth(),t.render(e,a),t.setRenderTarget(i,3,s),p&&t.autoClear===!1&&t.clearDepth(),t.render(e,l),t.setRenderTarget(i,4,s),p&&t.autoClear===!1&&t.clearDepth(),t.render(e,c),i.texture.mipmapsAutoUpdate=x,t.setRenderTarget(i,5,s),p&&t.autoClear===!1&&t.clearDepth(),t.render(e,h),t.setRenderTarget(u,d,f),t.xr.enabled=m,i.texture.needsPMREMUpdate=!0}},Ya=class extends Xe{constructor(t=[]){super(),this.isArrayCamera=!0,this.isMultiViewCamera=!1,this.cameras=t,this.perCameraFrustumCulled=!1}},$r=class{constructor(){this._previousTime=0,this._currentTime=0,this._startTime=performance.now(),this._delta=0,this._elapsed=0,this._timescale=1,this._document=null,this._pageVisibilityHandler=null}connect(t){this._document=t,t.hidden!==void 0&&(this._pageVisibilityHandler=Jm.bind(this),t.addEventListener("visibilitychange",this._pageVisibilityHandler,!1))}disconnect(){this._pageVisibilityHandler!==null&&(this._document.removeEventListener("visibilitychange",this._pageVisibilityHandler),this._pageVisibilityHandler=null),this._document=null}getDelta(){return this._delta/1e3}getElapsed(){return this._elapsed/1e3}getTimescale(){return this._timescale}setTimescale(t){return this._timescale=t,this}reset(){return this._currentTime=performance.now()-this._startTime,this}dispose(){this.disconnect()}update(t){return this._pageVisibilityHandler!==null&&this._document.hidden===!0?this._delta=0:(this._previousTime=this._currentTime,this._currentTime=(t!==void 0?t:performance.now())-this._startTime,this._delta=(this._currentTime-this._previousTime)*this._timescale,this._elapsed+=this._delta),this}};function Jm(){this._document.hidden===!1&&this.reset()}var Lh="\\[\\]\\.:\\/",$m=new RegExp("["+Lh+"]","g"),Nh="[^"+Lh+"]",jm="[^"+Lh.replace("\\.","")+"]",Qm=/((?:WC+[\/:])*)/.source.replace("WC",Nh),tg=/(WCOD+)?/.source.replace("WCOD",jm),eg=/(?:\.(WC+)(?:\[(.+)\])?)?/.source.replace("WC",Nh),ig=/\.(WC+)(?:\[(.+)\])?/.source.replace("WC",Nh),ng=new RegExp("^"+Qm+tg+eg+ig+"$"),sg=["material","materials","bones","map"],hh=class{constructor(t,e,i){let s=i||ye.parseTrackName(e);this._targetGroup=t,this._bindings=t.subscribe_(e,s)}getValue(t,e){this.bind();let i=this._targetGroup.nCachedObjects_,s=this._bindings[i];s!==void 0&&s.getValue(t,e)}setValue(t,e){let i=this._bindings;for(let s=this._targetGroup.nCachedObjects_,r=i.length;s!==r;++s)i[s].setValue(t,e)}bind(){let t=this._bindings;for(let e=this._targetGroup.nCachedObjects_,i=t.length;e!==i;++e)t[e].bind()}unbind(){let t=this._bindings;for(let e=this._targetGroup.nCachedObjects_,i=t.length;e!==i;++e)t[e].unbind()}},ye=class n{constructor(t,e,i){this.path=e,this.parsedPath=i||n.parseTrackName(e),this.node=n.findNode(t,this.parsedPath.nodeName),this.rootNode=t,this.getValue=this._getValue_unbound,this.setValue=this._setValue_unbound}static create(t,e,i){return t&&t.isAnimationObjectGroup?new n.Composite(t,e,i):new n(t,e,i)}static sanitizeNodeName(t){return t.replace(/\s/g,"_").replace($m,"")}static parseTrackName(t){let e=ng.exec(t);if(e===null)throw new Error("THREE.PropertyBinding: Cannot parse trackName: "+t);let i={nodeName:e[2],objectName:e[3],objectIndex:e[4],propertyName:e[5],propertyIndex:e[6]},s=i.nodeName&&i.nodeName.lastIndexOf(".");if(s!==void 0&&s!==-1){let r=i.nodeName.substring(s+1);sg.indexOf(r)!==-1&&(i.nodeName=i.nodeName.substring(0,s),i.objectName=r)}if(i.propertyName===null||i.propertyName.length===0)throw new Error("THREE.PropertyBinding: can not parse propertyName from trackName: "+t);return i}static findNode(t,e){if(e===void 0||e===""||e==="."||e===-1||e===t.name||e===t.uuid)return t;if(t.skeleton){let i=t.skeleton.getBoneByName(e);if(i!==void 0)return i}if(t.children){let i=function(r){for(let o=0;o<r.length;o++){let a=r[o];if(a.name===e||a.uuid===e)return a;let l=i(a.children);if(l)return l}return null},s=i(t.children);if(s)return s}return null}_getValue_unavailable(){}_setValue_unavailable(){}_getValue_direct(t,e){t[e]=this.targetObject[this.propertyName]}_getValue_array(t,e){let i=this.resolvedProperty;for(let s=0,r=i.length;s!==r;++s)t[e++]=i[s]}_getValue_arrayElement(t,e){t[e]=this.resolvedProperty[this.propertyIndex]}_getValue_toArray(t,e){this.resolvedProperty.toArray(t,e)}_setValue_direct(t,e){this.targetObject[this.propertyName]=t[e]}_setValue_direct_setNeedsUpdate(t,e){this.targetObject[this.propertyName]=t[e],this.targetObject.needsUpdate=!0}_setValue_direct_setMatrixWorldNeedsUpdate(t,e){this.targetObject[this.propertyName]=t[e],this.targetObject.matrixWorldNeedsUpdate=!0}_setValue_array(t,e){let i=this.resolvedProperty;for(let s=0,r=i.length;s!==r;++s)i[s]=t[e++]}_setValue_array_setNeedsUpdate(t,e){let i=this.resolvedProperty;for(let s=0,r=i.length;s!==r;++s)i[s]=t[e++];this.targetObject.needsUpdate=!0}_setValue_array_setMatrixWorldNeedsUpdate(t,e){let i=this.resolvedProperty;for(let s=0,r=i.length;s!==r;++s)i[s]=t[e++];this.targetObject.matrixWorldNeedsUpdate=!0}_setValue_arrayElement(t,e){this.resolvedProperty[this.propertyIndex]=t[e]}_setValue_arrayElement_setNeedsUpdate(t,e){this.resolvedProperty[this.propertyIndex]=t[e],this.targetObject.needsUpdate=!0}_setValue_arrayElement_setMatrixWorldNeedsUpdate(t,e){this.resolvedProperty[this.propertyIndex]=t[e],this.targetObject.matrixWorldNeedsUpdate=!0}_setValue_fromArray(t,e){this.resolvedProperty.fromArray(t,e)}_setValue_fromArray_setNeedsUpdate(t,e){this.resolvedProperty.fromArray(t,e),this.targetObject.needsUpdate=!0}_setValue_fromArray_setMatrixWorldNeedsUpdate(t,e){this.resolvedProperty.fromArray(t,e),this.targetObject.matrixWorldNeedsUpdate=!0}_getValue_unbound(t,e){this.bind(),this.getValue(t,e)}_setValue_unbound(t,e){this.bind(),this.setValue(t,e)}bind(){let t=this.node,e=this.parsedPath,i=e.objectName,s=e.propertyName,r=e.propertyIndex;if(t||(t=n.findNode(this.rootNode,e.nodeName),this.node=t),this.getValue=this._getValue_unavailable,this.setValue=this._setValue_unavailable,!t){Lt("PropertyBinding: No target node found for track: "+this.path+".");return}if(i){let c=e.objectIndex;switch(i){case"materials":if(!t.material){Ot("PropertyBinding: Can not bind to material as node does not have a material.",this);return}if(!t.material.materials){Ot("PropertyBinding: Can not bind to material.materials as node.material does not have a materials array.",this);return}t=t.material.materials;break;case"bones":if(!t.skeleton){Ot("PropertyBinding: Can not bind to bones as node does not have a skeleton.",this);return}t=t.skeleton.bones;for(let h=0;h<t.length;h++)if(t[h].name===c){c=h;break}break;case"map":if("map"in t){t=t.map;break}if(!t.material){Ot("PropertyBinding: Can not bind to material as node does not have a material.",this);return}if(!t.material.map){Ot("PropertyBinding: Can not bind to material.map as node.material does not have a map.",this);return}t=t.material.map;break;default:if(t[i]===void 0){Ot("PropertyBinding: Can not bind to objectName of node undefined.",this);return}t=t[i]}if(c!==void 0){if(t[c]===void 0){Ot("PropertyBinding: Trying to bind to objectIndex of objectName, but is undefined.",this,t);return}t=t[c]}}let o=t[s];if(o===void 0){let c=e.nodeName;Ot("PropertyBinding: Trying to update property for track: "+c+"."+s+" but it wasn't found.",t);return}let a=this.Versioning.None;this.targetObject=t,t.isMaterial===!0?a=this.Versioning.NeedsUpdate:t.isObject3D===!0&&(a=this.Versioning.MatrixWorldNeedsUpdate);let l=this.BindingType.Direct;if(r!==void 0){if(s==="morphTargetInfluences"){if(!t.geometry){Ot("PropertyBinding: Can not bind to morphTargetInfluences because node does not have a geometry.",this);return}if(!t.geometry.morphAttributes){Ot("PropertyBinding: Can not bind to morphTargetInfluences because node does not have a geometry.morphAttributes.",this);return}t.morphTargetDictionary[r]!==void 0&&(r=t.morphTargetDictionary[r])}l=this.BindingType.ArrayElement,this.resolvedProperty=o,this.propertyIndex=r}else o.fromArray!==void 0&&o.toArray!==void 0?(l=this.BindingType.HasFromToArray,this.resolvedProperty=o):Array.isArray(o)?(l=this.BindingType.EntireArray,this.resolvedProperty=o):this.propertyName=s;this.getValue=this.GetterByBindingType[l],this.setValue=this.SetterByBindingTypeAndVersioning[l][a]}unbind(){this.node=null,this.getValue=this._getValue_unbound,this.setValue=this._setValue_unbound}};ye.Composite=hh;ye.prototype.BindingType={Direct:0,EntireArray:1,ArrayElement:2,HasFromToArray:3};ye.prototype.Versioning={None:0,NeedsUpdate:1,MatrixWorldNeedsUpdate:2};ye.prototype.GetterByBindingType=[ye.prototype._getValue_direct,ye.prototype._getValue_array,ye.prototype._getValue_arrayElement,ye.prototype._getValue_toArray];ye.prototype.SetterByBindingTypeAndVersioning=[[ye.prototype._setValue_direct,ye.prototype._setValue_direct_setNeedsUpdate,ye.prototype._setValue_direct_setMatrixWorldNeedsUpdate],[ye.prototype._setValue_array,ye.prototype._setValue_array_setNeedsUpdate,ye.prototype._setValue_array_setMatrixWorldNeedsUpdate],[ye.prototype._setValue_arrayElement,ye.prototype._setValue_arrayElement_setNeedsUpdate,ye.prototype._setValue_arrayElement_setMatrixWorldNeedsUpdate],[ye.prototype._setValue_fromArray,ye.prototype._setValue_fromArray_setNeedsUpdate,ye.prototype._setValue_fromArray_setMatrixWorldNeedsUpdate]];var Mv=new Float32Array(1);var vd=new Xt,qi=class{constructor(t,e,i=0,s=1/0){this.ray=new dn(t,e),this.near=i,this.far=s,this.camera=null,this.layers=new Hs,this.params={Mesh:{},Line:{threshold:1},LOD:{},Points:{threshold:1},Sprite:{}}}set(t,e){this.ray.set(t,e)}setFromCamera(t,e){e.isPerspectiveCamera?(this.ray.origin.setFromMatrixPosition(e.matrixWorld),this.ray.direction.set(t.x,t.y,.5).unproject(e).sub(this.ray.origin).normalize(),this.camera=e):e.isOrthographicCamera?(this.ray.origin.set(t.x,t.y,e.projectionMatrix.elements[14]).unproject(e),this.ray.direction.set(0,0,-1).transformDirection(e.matrixWorld),this.camera=e):Ot("Raycaster: Unsupported camera type: "+e.type)}setFromXRController(t){return vd.identity().extractRotation(t.matrixWorld),this.ray.origin.setFromMatrixPosition(t.matrixWorld),this.ray.direction.set(0,0,-1).applyMatrix4(vd),this}intersectObject(t,e=!0,i=[]){return uh(t,this,i,e),i.sort(Md),i}intersectObjects(t,e=!0,i=[]){for(let s=0,r=t.length;s<r;s++)uh(t[s],this,i,e);return i.sort(Md),i}};function Md(n,t){return n.distance-t.distance}function uh(n,t,e,i){let s=!0;if(n.layers.test(t.layers)&&n.raycast(t,e)===!1&&(s=!1),s===!0&&i===!0){let r=n.children;for(let o=0,a=r.length;o<a;o++)uh(r[o],t,e,!0)}}var $s=class{constructor(t=1,e=0,i=0){this.radius=t,this.phi=e,this.theta=i}set(t,e,i){return this.radius=t,this.phi=e,this.theta=i,this}copy(t){return this.radius=t.radius,this.phi=t.phi,this.theta=t.theta,this}makeSafe(){return this.phi=Vt(this.phi,1e-6,Math.PI-1e-6),this}setFromVector3(t){return this.setFromCartesianCoords(t.x,t.y,t.z)}setFromCartesianCoords(t,e,i){return this.radius=Math.sqrt(t*t+e*e+i*i),this.radius===0?(this.theta=0,this.phi=0):(this.theta=Math.atan2(t,i),this.phi=Math.acos(Vt(e/this.radius,-1,1))),this}clone(){return new this.constructor().copy(this)}};var dh=class{static{this.prototype.isMatrix2=!0}constructor(t,e,i,s){this.elements=[1,0,0,1],t!==void 0&&this.set(t,e,i,s)}identity(){return this.set(1,0,0,1),this}fromArray(t,e=0){for(let i=0;i<4;i++)this.elements[i]=t[i+e];return this}set(t,e,i,s){let r=this.elements;return r[0]=t,r[2]=e,r[1]=i,r[3]=s,this}};var jr=class extends Ni{constructor(t,e=null){super(),this.object=t,this.domElement=e,this.enabled=!0,this.state=-1,this.keys={},this.mouseButtons={LEFT:null,MIDDLE:null,RIGHT:null},this.touches={ONE:null,TWO:null}}connect(t){this.domElement!==null&&this.disconnect(),this.domElement=t}disconnect(){}dispose(){}update(){}};function Uh(n,t,e,i){let s=rg(i);switch(e){case bh:return n*t;case zn:return n*t/s.components*s.byteLength;case nl:return n*t/s.components*s.byteLength;case kn:return n*t*2/s.components*s.byteLength;case sl:return n*t*2/s.components*s.byteLength;case wh:return n*t*3/s.components*s.byteLength;case ui:return n*t*4/s.components*s.byteLength;case rl:return n*t*4/s.components*s.byteLength;case ao:case lo:return Math.floor((n+3)/4)*Math.floor((t+3)/4)*8;case co:case ho:return Math.floor((n+3)/4)*Math.floor((t+3)/4)*16;case al:case cl:return Math.max(n,16)*Math.max(t,8)/4;case ol:case ll:return Math.max(n,8)*Math.max(t,8)/2;case hl:case ul:case fl:case pl:return Math.floor((n+3)/4)*Math.floor((t+3)/4)*8;case dl:case uo:case ml:return Math.floor((n+3)/4)*Math.floor((t+3)/4)*16;case gl:return Math.floor((n+3)/4)*Math.floor((t+3)/4)*16;case xl:return Math.floor((n+4)/5)*Math.floor((t+3)/4)*16;case _l:return Math.floor((n+4)/5)*Math.floor((t+4)/5)*16;case yl:return Math.floor((n+5)/6)*Math.floor((t+4)/5)*16;case vl:return Math.floor((n+5)/6)*Math.floor((t+5)/6)*16;case Ml:return Math.floor((n+7)/8)*Math.floor((t+4)/5)*16;case Sl:return Math.floor((n+7)/8)*Math.floor((t+5)/6)*16;case bl:return Math.floor((n+7)/8)*Math.floor((t+7)/8)*16;case wl:return Math.floor((n+9)/10)*Math.floor((t+4)/5)*16;case El:return Math.floor((n+9)/10)*Math.floor((t+5)/6)*16;case Tl:return Math.floor((n+9)/10)*Math.floor((t+7)/8)*16;case Al:return Math.floor((n+9)/10)*Math.floor((t+9)/10)*16;case Rl:return Math.floor((n+11)/12)*Math.floor((t+9)/10)*16;case Cl:return Math.floor((n+11)/12)*Math.floor((t+11)/12)*16;case Pl:case Il:case Dl:return Math.ceil(n/4)*Math.ceil(t/4)*16;case Ll:case Nl:return Math.ceil(n/4)*Math.ceil(t/4)*8;case fo:case Ul:return Math.ceil(n/4)*Math.ceil(t/4)*16}throw new Error(`Unable to determine texture byte length for ${e} format.`)}function rg(n){switch(n){case ei:case yh:return{byteLength:1,components:1};case ir:case vh:case xe:return{byteLength:2,components:1};case el:case il:return{byteLength:2,components:4};case hi:case tl:case gi:return{byteLength:4,components:1};case Mh:case Sh:return{byteLength:4,components:3}}throw new Error(`THREE.TextureUtils: Unknown texture type ${n}.`)}typeof __THREE_DEVTOOLS__<"u"&&__THREE_DEVTOOLS__.dispatchEvent(new CustomEvent("register",{detail:{revision:"187"}}));typeof window<"u"&&(window.__THREE__?Lt("WARNING: Multiple instances of Three.js being imported."):window.__THREE__="187");function Cf(){let n=null,t=!1,e=null,i=null;function s(r,o){i=n.requestAnimationFrame(s),e(r,o)}return{start:function(){t!==!0&&e!==null&&n!==null&&(i=n.requestAnimationFrame(s),t=!0)},stop:function(){n!==null&&n.cancelAnimationFrame(i),t=!1},setAnimationLoop:function(r){e=r},setContext:function(r){n=r}}}function og(n){let t=new WeakMap;function e(a,l){let c=a.array,h=a.usage,u=c.byteLength,d=n.createBuffer();n.bindBuffer(l,d),n.bufferData(l,c,h),a.onUploadCallback();let f;if(c instanceof Float32Array)f=n.FLOAT;else if(typeof Float16Array<"u"&&c instanceof Float16Array)f=n.HALF_FLOAT;else if(c instanceof Uint16Array)a.isFloat16BufferAttribute?f=n.HALF_FLOAT:f=n.UNSIGNED_SHORT;else if(c instanceof Int16Array)f=n.SHORT;else if(c instanceof Uint32Array)f=n.UNSIGNED_INT;else if(c instanceof Int32Array)f=n.INT;else if(c instanceof Int8Array)f=n.BYTE;else if(c instanceof Uint8Array)f=n.UNSIGNED_BYTE;else if(c instanceof Uint8ClampedArray)f=n.UNSIGNED_BYTE;else throw new Error("THREE.WebGLAttributes: Unsupported buffer data format: "+c);return{buffer:d,type:f,bytesPerElement:c.BYTES_PER_ELEMENT,version:a.version,size:u}}function i(a,l,c){let h=l.array,u=l.updateRanges;if(n.bindBuffer(c,a),u.length===0)n.bufferSubData(c,0,h);else{u.sort((f,m)=>f.start-m.start);let d=0;for(let f=1;f<u.length;f++){let m=u[d],x=u[f];x.start<=m.start+m.count+1?m.count=Math.max(m.count,x.start+x.count-m.start):(++d,u[d]=x)}u.length=d+1;for(let f=0,m=u.length;f<m;f++){let x=u[f];n.bufferSubData(c,x.start*h.BYTES_PER_ELEMENT,h,x.start,x.count)}l.clearUpdateRanges()}l.onUploadCallback()}function s(a){return a.isInterleavedBufferAttribute&&(a=a.data),t.get(a)}function r(a){a.isInterleavedBufferAttribute&&(a=a.data);let l=t.get(a);l&&(n.deleteBuffer(l.buffer),t.delete(a))}function o(a,l){if(a.isInterleavedBufferAttribute&&(a=a.data),a.isGLBufferAttribute){let h=t.get(a);(!h||h.version<a.version)&&t.set(a,{buffer:a.buffer,type:a.type,bytesPerElement:a.elementSize,version:a.version});return}let c=t.get(a);if(c===void 0)t.set(a,e(a,l));else if(c.version<a.version){if(c.size!==a.array.byteLength)throw new Error("THREE.WebGLAttributes: The size of the buffer attribute's array buffer does not match the original size. Resizing buffer attributes is not supported.");i(c.buffer,a,l),c.version=a.version}}return{get:s,remove:r,update:o}}var ag=`#ifdef USE_ALPHAHASH
	if ( diffuseColor.a < getAlphaHashThreshold( vPosition ) ) discard;
#endif`,lg=`#ifdef USE_ALPHAHASH
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
#endif`,cg=`#ifdef USE_ALPHAMAP
	diffuseColor.a *= texture2D( alphaMap, vAlphaMapUv ).g;
#endif`,hg=`#ifdef USE_ALPHAMAP
	uniform sampler2D alphaMap;
#endif`,ug=`#ifdef USE_ALPHATEST
	#ifdef ALPHA_TO_COVERAGE
	diffuseColor.a = smoothstep( alphaTest, alphaTest + fwidth( diffuseColor.a ), diffuseColor.a );
	if ( diffuseColor.a == 0.0 ) discard;
	#else
	if ( diffuseColor.a < alphaTest ) discard;
	#endif
#endif`,dg=`#ifdef USE_ALPHATEST
	uniform float alphaTest;
#endif`,fg=`#ifdef USE_AOMAP
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
#endif`,pg=`#ifdef USE_AOMAP
	uniform sampler2D aoMap;
	uniform float aoMapIntensity;
#endif`,mg=`#ifdef USE_BATCHING
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
#endif`,gg=`#ifdef USE_BATCHING
	mat4 batchingMatrix = getBatchingMatrix( getIndirectIndex( gl_DrawID ) );
#endif`,xg=`vec3 transformed = vec3( position );
#ifdef USE_ALPHAHASH
	vPosition = vec3( position );
#endif`,_g=`vec3 objectNormal = vec3( normal );
#ifdef USE_TANGENT
	vec3 objectTangent = vec3( tangent.xyz );
#endif`,yg=`float G_BlinnPhong_Implicit( ) {
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
} // validated`,vg=`#ifdef USE_IRIDESCENCE
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
#endif`,Mg=`#ifdef USE_BUMPMAP
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
#endif`,Sg=`#if NUM_CLIPPING_PLANES > 0
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
#endif`,bg=`#if NUM_CLIPPING_PLANES > 0
	varying vec3 vClipPosition;
	uniform vec4 clippingPlanes[ NUM_CLIPPING_PLANES ];
#endif`,wg=`#if NUM_CLIPPING_PLANES > 0
	varying vec3 vClipPosition;
#endif`,Eg=`#if NUM_CLIPPING_PLANES > 0
	vClipPosition = - mvPosition.xyz;
#endif`,Tg=`#if defined( USE_COLOR ) || defined( USE_COLOR_ALPHA )
	diffuseColor *= vColor;
#endif`,Ag=`#if defined( USE_COLOR ) || defined( USE_COLOR_ALPHA )
	varying vec4 vColor;
#endif`,Rg=`#if defined( USE_COLOR ) || defined( USE_COLOR_ALPHA ) || defined( USE_INSTANCING_COLOR ) || defined( USE_BATCHING_COLOR )
	varying vec4 vColor;
#endif`,Cg=`#if defined( USE_COLOR ) || defined( USE_COLOR_ALPHA ) || defined( USE_INSTANCING_COLOR ) || defined( USE_BATCHING_COLOR )
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
#endif`,Pg=`#define PI 3.141592653589793
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
} // validated`,Ig=`vec3 transformedNormal = objectNormal;
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
#endif`,Dg=`#ifdef USE_DISPLACEMENTMAP
	uniform sampler2D displacementMap;
	uniform float displacementScale;
	uniform float displacementBias;
#endif`,Lg=`#ifdef USE_DISPLACEMENTMAP
	transformed += normalize( objectNormal ) * ( texture2D( displacementMap, vDisplacementMapUv ).x * displacementScale + displacementBias );
#endif`,Ng=`#ifdef USE_EMISSIVEMAP
	vec4 emissiveColor = texture2D( emissiveMap, vEmissiveMapUv );
	#ifdef DECODE_VIDEO_TEXTURE_EMISSIVE
		emissiveColor = sRGBTransferEOTF( emissiveColor );
	#endif
	totalEmissiveRadiance *= emissiveColor.rgb;
#endif`,Ug=`#ifdef USE_EMISSIVEMAP
	uniform sampler2D emissiveMap;
#endif`,Fg="gl_FragColor = linearToOutputTexel( gl_FragColor );",Og=`vec4 LinearTransferOETF( in vec4 value ) {
	return value;
}
vec4 sRGBTransferEOTF( in vec4 value ) {
	return vec4( mix( pow( value.rgb * 0.9478672986 + vec3( 0.0521327014 ), vec3( 2.4 ) ), value.rgb * 0.0773993808, vec3( lessThanEqual( value.rgb, vec3( 0.04045 ) ) ) ), value.a );
}
vec4 sRGBTransferOETF( in vec4 value ) {
	return vec4( mix( pow( value.rgb, vec3( 0.41666 ) ) * 1.055 - vec3( 0.055 ), value.rgb * 12.92, vec3( lessThanEqual( value.rgb, vec3( 0.0031308 ) ) ) ), value.a );
}`,Bg=`#ifdef USE_ENVMAP
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
#endif`,zg=`#ifdef USE_ENVMAP
	uniform float envMapIntensity;
	uniform mat3 envMapRotation;
	uniform samplerCube envMap;
	#ifdef ENVMAP_TYPE_PMREM
		float roughnessToMip( const in float roughness ) {
			float r = clamp( roughness, 0.0, 1.0 );
			return ENVMAP_MAX_LOD * r * ( 2.0 - r );
		}
	#endif
#endif`,kg=`#ifdef USE_ENVMAP
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
#endif`,Hg=`#ifdef USE_ENVMAP
	#if defined( USE_BUMPMAP ) || defined( USE_NORMALMAP ) || defined( PHONG ) || defined( LAMBERT )
		#define ENV_WORLDPOS
	#endif
	#ifdef ENV_WORLDPOS

		varying vec3 vWorldPosition;
	#else
		varying vec3 vReflect;
		uniform float refractionRatio;
	#endif
#endif`,Vg=`#ifdef USE_ENVMAP
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
#endif`,Gg=`#ifdef USE_FOG
	vFogDepth = - mvPosition.z;
#endif`,Wg=`#ifdef USE_FOG
	varying float vFogDepth;
#endif`,Xg=`#ifdef USE_FOG
	#ifdef FOG_EXP2
		float fogFactor = 1.0 - exp( - fogDensity * fogDensity * vFogDepth * vFogDepth );
	#else
		float fogFactor = smoothstep( fogNear, fogFar, vFogDepth );
	#endif
	gl_FragColor.rgb = mix( gl_FragColor.rgb, fogColor, fogFactor );
#endif`,qg=`#ifdef USE_FOG
	uniform vec3 fogColor;
	varying float vFogDepth;
	#ifdef FOG_EXP2
		uniform float fogDensity;
	#else
		uniform float fogNear;
		uniform float fogFar;
	#endif
#endif`,Yg=`#ifdef USE_GRADIENTMAP
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
}`,Zg=`#ifdef USE_LIGHTMAP
	uniform sampler2D lightMap;
	uniform float lightMapIntensity;
#endif`,Kg=`LambertMaterial material;
material.diffuseColor = diffuseColor.rgb;
material.specularStrength = specularStrength;`,Jg=`varying vec3 vViewPosition;
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
#define RE_IndirectDiffuse		RE_IndirectDiffuse_Lambert`,$g=`uniform bool receiveShadow;
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
#include <lightprobes_pars_fragment>`,jg=`#ifdef USE_ENVMAP
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
#endif`,Qg=`ToonMaterial material;
material.diffuseColor = diffuseColor.rgb;`,t0=`varying vec3 vViewPosition;
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
#define RE_IndirectDiffuse		RE_IndirectDiffuse_Toon`,e0=`BlinnPhongMaterial material;
material.diffuseColor = diffuseColor.rgb;
material.specularColor = specular;
material.specularShininess = shininess;
material.specularStrength = specularStrength;`,i0=`varying vec3 vViewPosition;
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
#define RE_IndirectDiffuse		RE_IndirectDiffuse_BlinnPhong`,n0=`PhysicalMaterial material;
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
#endif`,s0=`uniform sampler2D dfgLUT;
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
}`,r0=`
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
#endif`,o0=`#if defined( RE_IndirectDiffuse )
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
#endif`,a0=`#if defined( RE_IndirectDiffuse )
	#if defined( LAMBERT ) || defined( PHONG )
		irradiance += iblIrradiance;
	#endif
	RE_IndirectDiffuse( irradiance, geometryPosition, geometryNormal, geometryViewDir, geometryClearcoatNormal, material, reflectedLight );
#endif
#if defined( RE_IndirectSpecular )
	RE_IndirectSpecular( radiance, iblIrradiance, clearcoatRadiance, geometryPosition, geometryNormal, geometryViewDir, geometryClearcoatNormal, material, reflectedLight );
#endif`,l0=`#if NUM_LIGHT_PROBE_GRIDS > 0
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
#endif`,c0=`#if defined( USE_LOGARITHMIC_DEPTH_BUFFER )
	gl_FragDepth = vIsPerspective == 0.0 ? gl_FragCoord.z : log2( vFragDepth ) * logDepthBufFC * 0.5;
#endif`,h0=`#if defined( USE_LOGARITHMIC_DEPTH_BUFFER )
	uniform float logDepthBufFC;
	varying float vFragDepth;
	varying float vIsPerspective;
#endif`,u0=`#ifdef USE_LOGARITHMIC_DEPTH_BUFFER
	varying float vFragDepth;
	varying float vIsPerspective;
#endif`,d0=`#ifdef USE_LOGARITHMIC_DEPTH_BUFFER
	vFragDepth = 1.0 + gl_Position.w;
	vIsPerspective = float( isPerspectiveMatrix( projectionMatrix ) );
#endif`,f0=`#ifdef USE_MAP
	vec4 sampledDiffuseColor = texture2D( map, vMapUv );
	#ifdef DECODE_VIDEO_TEXTURE
		sampledDiffuseColor = sRGBTransferEOTF( sampledDiffuseColor );
	#endif
	diffuseColor *= sampledDiffuseColor;
#endif`,p0=`#ifdef USE_MAP
	uniform sampler2D map;
#endif`,m0=`#if defined( USE_MAP ) || defined( USE_ALPHAMAP )
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
#endif`,g0=`#if defined( USE_POINTS_UV )
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
#endif`,x0=`float metalnessFactor = metalness;
#ifdef USE_METALNESSMAP
	vec4 texelMetalness = texture2D( metalnessMap, vMetalnessMapUv );
	metalnessFactor *= texelMetalness.b;
#endif`,_0=`#ifdef USE_METALNESSMAP
	uniform sampler2D metalnessMap;
#endif`,y0=`#ifdef USE_INSTANCING_MORPH
	float morphTargetInfluences[ MORPHTARGETS_COUNT ];
	float morphTargetBaseInfluence = texelFetch( morphTexture, ivec2( 0, gl_InstanceID ), 0 ).r;
	for ( int i = 0; i < MORPHTARGETS_COUNT; i ++ ) {
		morphTargetInfluences[i] =  texelFetch( morphTexture, ivec2( i + 1, gl_InstanceID ), 0 ).r;
	}
#endif`,v0=`#if defined( USE_MORPHCOLORS )
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
#endif`,M0=`#ifdef USE_MORPHNORMALS
	objectNormal *= morphTargetBaseInfluence;
	for ( int i = 0; i < MORPHTARGETS_COUNT; i ++ ) {
		if ( morphTargetInfluences[ i ] != 0.0 ) objectNormal += getMorph( gl_VertexID, i, 1 ).xyz * morphTargetInfluences[ i ];
	}
#endif`,S0=`#ifdef USE_MORPHTARGETS
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
#endif`,b0=`#ifdef USE_MORPHTARGETS
	transformed *= morphTargetBaseInfluence;
	for ( int i = 0; i < MORPHTARGETS_COUNT; i ++ ) {
		if ( morphTargetInfluences[ i ] != 0.0 ) transformed += getMorph( gl_VertexID, i, 0 ).xyz * morphTargetInfluences[ i ];
	}
#endif`,w0=`float faceDirection = gl_FrontFacing ? 1.0 : - 1.0;
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
vec3 nonPerturbedNormal = normal;`,E0=`#ifdef USE_NORMALMAP_OBJECTSPACE
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
#endif`,T0=`#ifndef FLAT_SHADED
	varying vec3 vNormal;
	#ifdef USE_TANGENT
		varying vec3 vTangent;
		varying vec3 vBitangent;
	#endif
#endif`,A0=`#ifndef FLAT_SHADED
	varying vec3 vNormal;
	#ifdef USE_TANGENT
		varying vec3 vTangent;
		varying vec3 vBitangent;
	#endif
#endif`,R0=`#ifndef FLAT_SHADED
	vNormal = normalize( transformedNormal );
	#ifdef USE_TANGENT
		vTangent = normalize( transformedTangent );
		vBitangent = normalize( cross( vNormal, vTangent ) * tangent.w );
		#ifdef FLIP_SIDED
			vBitangent = - vBitangent;
		#endif
	#endif
#endif`,C0=`#ifdef USE_NORMALMAP
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
#endif`,P0=`#ifdef USE_CLEARCOAT
	vec3 clearcoatNormal = nonPerturbedNormal;
#endif`,I0=`#ifdef USE_CLEARCOAT_NORMALMAP
	vec3 clearcoatMapN = texture2D( clearcoatNormalMap, vClearcoatNormalMapUv ).xyz * 2.0 - 1.0;
	clearcoatMapN.xy *= clearcoatNormalScale;
	clearcoatNormal = normalize( tbn2 * clearcoatMapN );
#endif`,D0=`#ifdef USE_CLEARCOATMAP
	uniform sampler2D clearcoatMap;
#endif
#ifdef USE_CLEARCOAT_NORMALMAP
	uniform sampler2D clearcoatNormalMap;
	uniform vec2 clearcoatNormalScale;
#endif
#ifdef USE_CLEARCOAT_ROUGHNESSMAP
	uniform sampler2D clearcoatRoughnessMap;
#endif`,L0=`#ifdef USE_IRIDESCENCEMAP
	uniform sampler2D iridescenceMap;
#endif
#ifdef USE_IRIDESCENCE_THICKNESSMAP
	uniform sampler2D iridescenceThicknessMap;
#endif`,N0=`#ifdef OPAQUE
diffuseColor.a = 1.0;
#endif
#ifdef USE_TRANSMISSION
diffuseColor.a *= material.transmissionAlpha;
#endif
gl_FragColor = vec4( outgoingLight, diffuseColor.a );`,U0=`vec3 packNormalToRGB( const in vec3 normal ) {
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
}`,F0=`#ifdef PREMULTIPLIED_ALPHA
	gl_FragColor.rgb *= gl_FragColor.a;
#endif`,O0=`vec4 mvPosition = vec4( transformed, 1.0 );
#ifdef USE_BATCHING
	mvPosition = batchingMatrix * mvPosition;
#endif
#ifdef USE_INSTANCING
	mvPosition = instanceMatrix * mvPosition;
#endif
mvPosition = modelViewMatrix * mvPosition;
gl_Position = projectionMatrix * mvPosition;`,B0=`#ifdef DITHERING
	gl_FragColor.rgb = dithering( gl_FragColor.rgb );
#endif`,z0=`#ifdef DITHERING
	vec3 dithering( vec3 color ) {
		float grid_position = rand( gl_FragCoord.xy );
		vec3 dither_shift_RGB = vec3( 0.25 / 255.0, -0.25 / 255.0, 0.25 / 255.0 );
		dither_shift_RGB = mix( 2.0 * dither_shift_RGB, -2.0 * dither_shift_RGB, grid_position );
		return color + dither_shift_RGB;
	}
#endif`,k0=`float roughnessFactor = roughness;
#ifdef USE_ROUGHNESSMAP
	vec4 texelRoughness = texture2D( roughnessMap, vRoughnessMapUv );
	roughnessFactor *= texelRoughness.g;
#endif`,H0=`#ifdef USE_ROUGHNESSMAP
	uniform sampler2D roughnessMap;
#endif`,V0=`#if NUM_SPOT_LIGHT_COORDS > 0
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
#endif`,G0=`#if NUM_SPOT_LIGHT_COORDS > 0
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
#endif`,W0=`#if ( defined( USE_SHADOWMAP ) && ( NUM_DIR_LIGHT_SHADOWS > 0 || NUM_SUN_LIGHT_SHADOWS > 0 || NUM_POINT_LIGHT_SHADOWS > 0 ) ) || ( NUM_SPOT_LIGHT_COORDS > 0 )
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
#endif`,X0=`float getShadowMask() {
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
}`,q0=`#ifdef USE_SKINNING
	mat4 boneMatX = getBoneMatrix( skinIndex.x );
	mat4 boneMatY = getBoneMatrix( skinIndex.y );
	mat4 boneMatZ = getBoneMatrix( skinIndex.z );
	mat4 boneMatW = getBoneMatrix( skinIndex.w );
#endif`,Y0=`#ifdef USE_SKINNING
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
#endif`,Z0=`#ifdef USE_SKINNING
	vec4 skinVertex = bindMatrix * vec4( transformed, 1.0 );
	vec4 skinned = vec4( 0.0 );
	skinned += boneMatX * skinVertex * skinWeight.x;
	skinned += boneMatY * skinVertex * skinWeight.y;
	skinned += boneMatZ * skinVertex * skinWeight.z;
	skinned += boneMatW * skinVertex * skinWeight.w;
	transformed = ( bindMatrixInverse * skinned ).xyz;
#endif`,K0=`#ifdef USE_SKINNING
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
#endif`,J0=`float specularStrength;
#ifdef USE_SPECULARMAP
	vec4 texelSpecular = texture2D( specularMap, vSpecularMapUv );
	specularStrength = texelSpecular.r;
#else
	specularStrength = 1.0;
#endif`,$0=`#ifdef USE_SPECULARMAP
	uniform sampler2D specularMap;
#endif`,j0=`#if defined( TONE_MAPPING )
	gl_FragColor.rgb = toneMapping( gl_FragColor.rgb );
#endif`,Q0=`#ifndef saturate
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
vec3 CustomToneMapping( vec3 color ) { return color; }`,tx=`#ifdef USE_TRANSMISSION
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
#endif`,ex=`#ifdef USE_TRANSMISSION
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
#endif`,ix=`#if defined( USE_UV ) || defined( USE_ANISOTROPY )
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
#endif`,nx=`#if defined( USE_UV ) || defined( USE_ANISOTROPY )
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
#endif`,sx=`#if defined( USE_UV ) || defined( USE_ANISOTROPY )
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
#endif`,rx=`#if defined( USE_ENVMAP ) || defined( DISTANCE ) || defined ( USE_SHADOWMAP ) || defined ( USE_TRANSMISSION ) || NUM_SPOT_LIGHT_COORDS > 0
	vec4 worldPosition = vec4( transformed, 1.0 );
	#ifdef USE_BATCHING
		worldPosition = batchingMatrix * worldPosition;
	#endif
	#ifdef USE_INSTANCING
		worldPosition = instanceMatrix * worldPosition;
	#endif
	worldPosition = modelMatrix * worldPosition;
#endif`,ox=`varying vec2 vUv;
uniform mat3 uvTransform;
void main() {
	vUv = ( uvTransform * vec3( uv, 1 ) ).xy;
	gl_Position = vec4( position.xy, 1.0, 1.0 );
}`,ax=`uniform sampler2D t2D;
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
}`,lx=`varying vec3 vWorldDirection;
#include <common>
void main() {
	vWorldDirection = transformDirection( position, modelMatrix );
	#include <begin_vertex>
	#include <project_vertex>
	gl_Position.z = gl_Position.w;
}`,cx=`uniform float backgroundBlurriness;
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
}`,hx=`varying vec3 vWorldDirection;
#include <common>
void main() {
	vWorldDirection = transformDirection( position, modelMatrix );
	#include <begin_vertex>
	#include <project_vertex>
	gl_Position.z = gl_Position.w;
}`,ux=`uniform samplerCube tCube;
uniform float tFlip;
uniform float opacity;
varying vec3 vWorldDirection;
void main() {
	vec4 texColor = textureCube( tCube, vec3( tFlip * vWorldDirection.x, vWorldDirection.yz ) );
	gl_FragColor = texColor;
	gl_FragColor.a *= opacity;
	#include <tonemapping_fragment>
	#include <colorspace_fragment>
}`,dx=`#include <common>
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
}`,fx=`#if DEPTH_PACKING == 3200
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
}`,px=`#define DISTANCE
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
}`,mx=`#define DISTANCE
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
}`,gx=`varying vec3 vWorldDirection;
#include <common>
void main() {
	vWorldDirection = transformDirection( position, modelMatrix );
	#include <begin_vertex>
	#include <project_vertex>
}`,xx=`uniform sampler2D tEquirect;
varying vec3 vWorldDirection;
#include <common>
void main() {
	vec3 direction = normalize( vWorldDirection );
	vec2 sampleUV = equirectUv( direction );
	gl_FragColor = texture2D( tEquirect, sampleUV );
	#include <tonemapping_fragment>
	#include <colorspace_fragment>
}`,_x=`uniform float scale;
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
}`,yx=`uniform vec3 diffuse;
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
}`,vx=`#include <common>
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
}`,Mx=`uniform vec3 diffuse;
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
}`,Sx=`#define LAMBERT
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
}`,bx=`#define LAMBERT
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
}`,wx=`#define MATCAP
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
}`,Ex=`#define MATCAP
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
}`,Tx=`#define NORMAL
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
}`,Ax=`#define NORMAL
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
}`,Rx=`#define PHONG
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
}`,Cx=`#define PHONG
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
}`,Px=`#define STANDARD
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
}`,Ix=`#define STANDARD
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
}`,Dx=`#define TOON
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
}`,Lx=`#define TOON
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
}`,Nx=`uniform float size;
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
}`,Ux=`uniform vec3 diffuse;
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
}`,Fx=`#include <common>
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
}`,Ox=`uniform vec3 color;
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
}`,Bx=`uniform float rotation;
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
}`,zx=`uniform vec3 diffuse;
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
}`,Gt={alphahash_fragment:ag,alphahash_pars_fragment:lg,alphamap_fragment:cg,alphamap_pars_fragment:hg,alphatest_fragment:ug,alphatest_pars_fragment:dg,aomap_fragment:fg,aomap_pars_fragment:pg,batching_pars_vertex:mg,batching_vertex:gg,begin_vertex:xg,beginnormal_vertex:_g,bsdfs:yg,iridescence_fragment:vg,bumpmap_pars_fragment:Mg,clipping_planes_fragment:Sg,clipping_planes_pars_fragment:bg,clipping_planes_pars_vertex:wg,clipping_planes_vertex:Eg,color_fragment:Tg,color_pars_fragment:Ag,color_pars_vertex:Rg,color_vertex:Cg,common:Pg,defaultnormal_vertex:Ig,displacementmap_pars_vertex:Dg,displacementmap_vertex:Lg,emissivemap_fragment:Ng,emissivemap_pars_fragment:Ug,colorspace_fragment:Fg,colorspace_pars_fragment:Og,envmap_fragment:Bg,envmap_common_pars_fragment:zg,envmap_pars_fragment:kg,envmap_pars_vertex:Hg,envmap_physical_pars_fragment:jg,envmap_vertex:Vg,fog_vertex:Gg,fog_pars_vertex:Wg,fog_fragment:Xg,fog_pars_fragment:qg,gradientmap_pars_fragment:Yg,lightmap_pars_fragment:Zg,lights_lambert_fragment:Kg,lights_lambert_pars_fragment:Jg,lights_pars_begin:$g,lights_toon_fragment:Qg,lights_toon_pars_fragment:t0,lights_phong_fragment:e0,lights_phong_pars_fragment:i0,lights_physical_fragment:n0,lights_physical_pars_fragment:s0,lights_fragment_begin:r0,lights_fragment_maps:o0,lights_fragment_end:a0,lightprobes_pars_fragment:l0,logdepthbuf_fragment:c0,logdepthbuf_pars_fragment:h0,logdepthbuf_pars_vertex:u0,logdepthbuf_vertex:d0,map_fragment:f0,map_pars_fragment:p0,map_particle_fragment:m0,map_particle_pars_fragment:g0,metalnessmap_fragment:x0,metalnessmap_pars_fragment:_0,morphinstance_vertex:y0,morphcolor_vertex:v0,morphnormal_vertex:M0,morphtarget_pars_vertex:S0,morphtarget_vertex:b0,normal_fragment_begin:w0,normal_fragment_maps:E0,normal_pars_fragment:T0,normal_pars_vertex:A0,normal_vertex:R0,normalmap_pars_fragment:C0,clearcoat_normal_fragment_begin:P0,clearcoat_normal_fragment_maps:I0,clearcoat_pars_fragment:D0,iridescence_pars_fragment:L0,opaque_fragment:N0,packing:U0,premultiplied_alpha_fragment:F0,project_vertex:O0,dithering_fragment:B0,dithering_pars_fragment:z0,roughnessmap_fragment:k0,roughnessmap_pars_fragment:H0,shadowmap_pars_fragment:V0,shadowmap_pars_vertex:G0,shadowmap_vertex:W0,shadowmask_pars_fragment:X0,skinbase_vertex:q0,skinning_pars_vertex:Y0,skinning_vertex:Z0,skinnormal_vertex:K0,specularmap_fragment:J0,specularmap_pars_fragment:$0,tonemapping_fragment:j0,tonemapping_pars_fragment:Q0,transmission_fragment:tx,transmission_pars_fragment:ex,uv_pars_fragment:ix,uv_pars_vertex:nx,uv_vertex:sx,worldpos_vertex:rx,background_vert:ox,background_frag:ax,backgroundCube_vert:lx,backgroundCube_frag:cx,cube_vert:hx,cube_frag:ux,depth_vert:dx,depth_frag:fx,distance_vert:px,distance_frag:mx,equirect_vert:gx,equirect_frag:xx,linedashed_vert:_x,linedashed_frag:yx,meshbasic_vert:vx,meshbasic_frag:Mx,meshlambert_vert:Sx,meshlambert_frag:bx,meshmatcap_vert:wx,meshmatcap_frag:Ex,meshnormal_vert:Tx,meshnormal_frag:Ax,meshphong_vert:Rx,meshphong_frag:Cx,meshphysical_vert:Px,meshphysical_frag:Ix,meshtoon_vert:Dx,meshtoon_frag:Lx,points_vert:Nx,points_frag:Ux,shadow_vert:Fx,shadow_frag:Ox,sprite_vert:Bx,sprite_frag:zx},vt={common:{diffuse:{value:new bt(16777215)},opacity:{value:1},map:{value:null},mapTransform:{value:new zt},alphaMap:{value:null},alphaMapTransform:{value:new zt},alphaTest:{value:0}},specularmap:{specularMap:{value:null},specularMapTransform:{value:new zt}},envmap:{envMap:{value:null},envMapRotation:{value:new zt},reflectivity:{value:1},ior:{value:1.5},refractionRatio:{value:.98},dfgLUT:{value:null}},aomap:{aoMap:{value:null},aoMapIntensity:{value:1},aoMapTransform:{value:new zt}},lightmap:{lightMap:{value:null},lightMapIntensity:{value:1},lightMapTransform:{value:new zt}},bumpmap:{bumpMap:{value:null},bumpMapTransform:{value:new zt},bumpScale:{value:1}},normalmap:{normalMap:{value:null},normalMapTransform:{value:new zt},normalScale:{value:new et(1,1)}},displacementmap:{displacementMap:{value:null},displacementMapTransform:{value:new zt},displacementScale:{value:1},displacementBias:{value:0}},emissivemap:{emissiveMap:{value:null},emissiveMapTransform:{value:new zt}},metalnessmap:{metalnessMap:{value:null},metalnessMapTransform:{value:new zt}},roughnessmap:{roughnessMap:{value:null},roughnessMapTransform:{value:new zt}},gradientmap:{gradientMap:{value:null}},fog:{fogDensity:{value:25e-5},fogNear:{value:1},fogFar:{value:2e3},fogColor:{value:new bt(16777215)}},lights:{ambientLightColor:{value:[]},lightProbe:{value:[]},sunLights:{value:[],properties:{direction:{},color:{}}},sunLightShadows:{value:[],properties:{shadowIntensity:1,shadowBias:{},shadowNormalBias:{},shadowRadius:{},shadowMapSize:{}}},sunShadowMatrix:{value:[]},sunShadowCascade:{value:[]},directionalLights:{value:[],properties:{direction:{},color:{}}},directionalLightShadows:{value:[],properties:{shadowIntensity:1,shadowBias:{},shadowNormalBias:{},shadowRadius:{},shadowMapSize:{}}},directionalShadowMatrix:{value:[]},spotLights:{value:[],properties:{color:{},position:{},direction:{},distance:{},coneCos:{},penumbraCos:{},decay:{}}},spotLightShadows:{value:[],properties:{shadowIntensity:1,shadowBias:{},shadowNormalBias:{},shadowRadius:{},shadowMapSize:{}}},spotLightMap:{value:[]},spotLightMatrix:{value:[]},pointLights:{value:[],properties:{color:{},position:{},decay:{},distance:{}}},pointLightShadows:{value:[],properties:{shadowIntensity:1,shadowBias:{},shadowNormalBias:{},shadowRadius:{},shadowMapSize:{},shadowCameraNear:{},shadowCameraFar:{}}},pointShadowMatrix:{value:[]},hemisphereLights:{value:[],properties:{direction:{},skyColor:{},groundColor:{}}},rectAreaLights:{value:[],properties:{color:{},position:{},width:{},height:{}}},ltc_1:{value:null},ltc_2:{value:null},probesSH:{value:[]},probesMin:{value:[]},probesMax:{value:[]},probesResolution:{value:[]}},points:{diffuse:{value:new bt(16777215)},opacity:{value:1},size:{value:1},scale:{value:1},map:{value:null},alphaMap:{value:null},alphaMapTransform:{value:new zt},alphaTest:{value:0},uvTransform:{value:new zt}},sprite:{diffuse:{value:new bt(16777215)},opacity:{value:1},center:{value:new et(.5,.5)},rotation:{value:0},map:{value:null},mapTransform:{value:new zt},alphaMap:{value:null},alphaMapTransform:{value:new zt},alphaTest:{value:0}}},Ji={basic:{uniforms:ii([vt.common,vt.specularmap,vt.envmap,vt.aomap,vt.lightmap,vt.fog]),vertexShader:Gt.meshbasic_vert,fragmentShader:Gt.meshbasic_frag},lambert:{uniforms:ii([vt.common,vt.specularmap,vt.envmap,vt.aomap,vt.lightmap,vt.emissivemap,vt.bumpmap,vt.normalmap,vt.displacementmap,vt.fog,vt.lights,{emissive:{value:new bt(0)},envMapIntensity:{value:1}}]),vertexShader:Gt.meshlambert_vert,fragmentShader:Gt.meshlambert_frag},phong:{uniforms:ii([vt.common,vt.specularmap,vt.envmap,vt.aomap,vt.lightmap,vt.emissivemap,vt.bumpmap,vt.normalmap,vt.displacementmap,vt.fog,vt.lights,{emissive:{value:new bt(0)},specular:{value:new bt(1118481)},shininess:{value:30},envMapIntensity:{value:1}}]),vertexShader:Gt.meshphong_vert,fragmentShader:Gt.meshphong_frag},standard:{uniforms:ii([vt.common,vt.envmap,vt.aomap,vt.lightmap,vt.emissivemap,vt.bumpmap,vt.normalmap,vt.displacementmap,vt.roughnessmap,vt.metalnessmap,vt.fog,vt.lights,{emissive:{value:new bt(0)},roughness:{value:1},metalness:{value:0},envMapIntensity:{value:1}}]),vertexShader:Gt.meshphysical_vert,fragmentShader:Gt.meshphysical_frag},toon:{uniforms:ii([vt.common,vt.aomap,vt.lightmap,vt.emissivemap,vt.bumpmap,vt.normalmap,vt.displacementmap,vt.gradientmap,vt.fog,vt.lights,{emissive:{value:new bt(0)}}]),vertexShader:Gt.meshtoon_vert,fragmentShader:Gt.meshtoon_frag},matcap:{uniforms:ii([vt.common,vt.bumpmap,vt.normalmap,vt.displacementmap,vt.fog,{matcap:{value:null}}]),vertexShader:Gt.meshmatcap_vert,fragmentShader:Gt.meshmatcap_frag},points:{uniforms:ii([vt.points,vt.fog]),vertexShader:Gt.points_vert,fragmentShader:Gt.points_frag},dashed:{uniforms:ii([vt.common,vt.fog,{scale:{value:1},dashSize:{value:1},totalSize:{value:2}}]),vertexShader:Gt.linedashed_vert,fragmentShader:Gt.linedashed_frag},depth:{uniforms:ii([vt.common,vt.displacementmap]),vertexShader:Gt.depth_vert,fragmentShader:Gt.depth_frag},normal:{uniforms:ii([vt.common,vt.bumpmap,vt.normalmap,vt.displacementmap,{opacity:{value:1}}]),vertexShader:Gt.meshnormal_vert,fragmentShader:Gt.meshnormal_frag},sprite:{uniforms:ii([vt.sprite,vt.fog]),vertexShader:Gt.sprite_vert,fragmentShader:Gt.sprite_frag},background:{uniforms:{uvTransform:{value:new zt},t2D:{value:null},backgroundIntensity:{value:1}},vertexShader:Gt.background_vert,fragmentShader:Gt.background_frag},backgroundCube:{uniforms:{envMap:{value:null},backgroundBlurriness:{value:0},backgroundIntensity:{value:1},backgroundRotation:{value:new zt}},vertexShader:Gt.backgroundCube_vert,fragmentShader:Gt.backgroundCube_frag},cube:{uniforms:{tCube:{value:null},tFlip:{value:-1},opacity:{value:1}},vertexShader:Gt.cube_vert,fragmentShader:Gt.cube_frag},equirect:{uniforms:{tEquirect:{value:null}},vertexShader:Gt.equirect_vert,fragmentShader:Gt.equirect_frag},distance:{uniforms:ii([vt.common,vt.displacementmap,{referencePosition:{value:new C},nearDistance:{value:1},farDistance:{value:1e3}}]),vertexShader:Gt.distance_vert,fragmentShader:Gt.distance_frag},shadow:{uniforms:ii([vt.lights,vt.fog,{color:{value:new bt(0)},opacity:{value:1}}]),vertexShader:Gt.shadow_vert,fragmentShader:Gt.shadow_frag}};Ji.physical={uniforms:ii([Ji.standard.uniforms,{clearcoat:{value:0},clearcoatMap:{value:null},clearcoatMapTransform:{value:new zt},clearcoatNormalMap:{value:null},clearcoatNormalMapTransform:{value:new zt},clearcoatNormalScale:{value:new et(1,1)},clearcoatRoughness:{value:0},clearcoatRoughnessMap:{value:null},clearcoatRoughnessMapTransform:{value:new zt},diffuseRoughness:{value:0},diffuseRoughnessMap:{value:null},diffuseRoughnessMapTransform:{value:new zt},dispersion:{value:0},retroreflectivity:{value:0},iridescence:{value:0},iridescenceMap:{value:null},iridescenceMapTransform:{value:new zt},iridescenceIOR:{value:1.3},iridescenceThicknessMinimum:{value:100},iridescenceThicknessMaximum:{value:400},iridescenceThicknessMap:{value:null},iridescenceThicknessMapTransform:{value:new zt},sheen:{value:0},sheenColor:{value:new bt(0)},sheenColorMap:{value:null},sheenColorMapTransform:{value:new zt},sheenRoughness:{value:1},sheenRoughnessMap:{value:null},sheenRoughnessMapTransform:{value:new zt},transmission:{value:0},transmissionMap:{value:null},transmissionMapTransform:{value:new zt},transmissionSamplerSize:{value:new et},transmissionSamplerMap:{value:null},thickness:{value:0},thicknessMap:{value:null},thicknessMapTransform:{value:new zt},attenuationDistance:{value:0},attenuationColor:{value:new bt(0)},specularColor:{value:new bt(1,1,1)},specularColorMap:{value:null},specularColorMapTransform:{value:new zt},specularIntensity:{value:1},specularIntensityMap:{value:null},specularIntensityMapTransform:{value:new zt},anisotropyVector:{value:new et},anisotropyMap:{value:null},anisotropyMapTransform:{value:new zt}}]),vertexShader:Gt.meshphysical_vert,fragmentShader:Gt.meshphysical_frag};var Fl={r:0,b:0,g:0},kx=new Xt,Pf=new zt;Pf.set(-1,0,0,0,1,0,0,0,1);function Hx(n,t,e,i,s,r){let o=new bt(0),a=s===!0?0:1,l,c,h=null,u=0,d=null;function f(y){let v=y.isScene===!0?y.background:null;if(v&&v.isTexture){let S=y.backgroundBlurriness>0;v=t.get(v,S)}return v}function m(y){let v=!1,S=f(y);S===null?p(o,a):S&&S.isColor&&(p(S,1),v=!0);let T=n.xr.getEnvironmentBlendMode();T==="additive"?e.buffers.color.setClear(0,0,0,1,r):T==="alpha-blend"&&e.buffers.color.setClear(0,0,0,0,r),(n.autoClear||v)&&(e.buffers.depth.setTest(!0),e.buffers.depth.setMask(!0),e.buffers.color.setMask(!0),n.clear(n.autoClearColor,n.autoClearDepth,n.autoClearStencil))}function x(y,v){let S=f(v);S&&S.isCubeTexture?(c===void 0&&(c=new Ut(new Te(1,1,1),new fe({name:"BackgroundCubeMaterial",uniforms:ds(Ji.backgroundCube.uniforms),vertexShader:Ji.backgroundCube.vertexShader,fragmentShader:Ji.backgroundCube.fragmentShader,side:Ze,depthTest:!1,depthWrite:!1,fog:!1,allowOverride:!1})),c.geometry.deleteAttribute("normal"),c.geometry.deleteAttribute("uv"),c.onBeforeRender=function(T,b,_){this.matrixWorld.copyPosition(_.matrixWorld)},Object.defineProperty(c.material,"envMap",{get:function(){return this.uniforms.envMap.value}}),i.update(c)),c.material.uniforms.envMap.value=S,c.material.uniforms.backgroundBlurriness.value=v.backgroundBlurriness,c.material.uniforms.backgroundIntensity.value=v.backgroundIntensity,c.material.uniforms.backgroundRotation.value.setFromMatrix4(kx.makeRotationFromEuler(v.backgroundRotation)).transpose(),S.isRenderTargetTexture===!1&&c.material.uniforms.backgroundRotation.value.premultiply(Pf),c.material.toneMapped=Zt.getTransfer(S.colorSpace)!==ne,(h!==S||u!==S.version||d!==n.toneMapping)&&(c.material.needsUpdate=!0,h=S,u=S.version,d=n.toneMapping),c.layers.enableAll(),y.unshift(c,c.geometry,c.material,0,0,null)):S&&S.isTexture&&(l===void 0&&(l=new Ut(new Si(2,2),new fe({name:"BackgroundMaterial",uniforms:ds(Ji.background.uniforms),vertexShader:Ji.background.vertexShader,fragmentShader:Ji.background.fragmentShader,side:Bn,depthTest:!1,depthWrite:!1,fog:!1,allowOverride:!1})),l.geometry.deleteAttribute("normal"),Object.defineProperty(l.material,"map",{get:function(){return this.uniforms.t2D.value}}),i.update(l)),l.material.uniforms.t2D.value=S,l.material.uniforms.backgroundIntensity.value=v.backgroundIntensity,l.material.toneMapped=Zt.getTransfer(S.colorSpace)!==ne,S.matrixAutoUpdate===!0&&S.updateMatrix(),l.material.uniforms.uvTransform.value.copy(S.matrix),(h!==S||u!==S.version||d!==n.toneMapping)&&(l.material.needsUpdate=!0,h=S,u=S.version,d=n.toneMapping),l.layers.enableAll(),y.unshift(l,l.geometry,l.material,0,0,null))}function p(y,v){y.getRGB(Fl,Dh(n)),e.buffers.color.setClear(Fl.r,Fl.g,Fl.b,v,r)}function g(){c!==void 0&&(c.geometry.dispose(),c.material.dispose(),c=void 0),l!==void 0&&(l.geometry.dispose(),l.material.dispose(),l=void 0)}return{getClearColor:function(){return o},setClearColor:function(y,v=1){o.set(y),a=v,p(o,a)},getClearAlpha:function(){return a},setClearAlpha:function(y){a=y,p(o,a)},render:m,addToRenderList:x,dispose:g}}function Vx(n,t){let e=n.getParameter(n.MAX_VERTEX_ATTRIBS),i={},s=d(null),r=s,o=!1;function a(A,R,U,L,B){let H=!1,z=u(A,L,U,R);r!==z&&(r=z,c(r.object)),H=f(A,L,U,B),H&&m(A,L,U,B),B!==null&&t.update(B,n.ELEMENT_ARRAY_BUFFER),(H||o)&&(o=!1,S(A,R,U,L),B!==null&&n.bindBuffer(n.ELEMENT_ARRAY_BUFFER,t.get(B).buffer))}function l(){return n.createVertexArray()}function c(A){return n.bindVertexArray(A)}function h(A){return n.deleteVertexArray(A)}function u(A,R,U,L){let B=L.wireframe===!0,H=i[R.id];H===void 0&&(H={},i[R.id]=H);let z=A.isInstancedMesh===!0?A.id:0,Q=H[z];Q===void 0&&(Q={},H[z]=Q);let W=Q[U.id];W===void 0&&(W={},Q[U.id]=W);let Z=W[B];return Z===void 0&&(Z=d(l()),W[B]=Z),Z}function d(A){let R=[],U=[],L=[];for(let B=0;B<e;B++)R[B]=0,U[B]=0,L[B]=0;return{geometry:null,program:null,wireframe:!1,newAttributes:R,enabledAttributes:U,attributeDivisors:L,object:A,attributes:{},index:null}}function f(A,R,U,L){let B=r.attributes,H=R.attributes,z=0,Q=U.getAttributes();for(let W in Q)if(Q[W].location>=0){let $=B[W],xt=H[W];if(xt===void 0&&(W==="instanceMatrix"&&A.instanceMatrix&&(xt=A.instanceMatrix),W==="instanceColor"&&A.instanceColor&&(xt=A.instanceColor)),$===void 0||$.attribute!==xt||xt&&$.data!==xt.data)return!0;z++}return r.attributesNum!==z||r.index!==L}function m(A,R,U,L){let B={},H=R.attributes,z=0,Q=U.getAttributes();for(let W in Q)if(Q[W].location>=0){let $=H[W];$===void 0&&(W==="instanceMatrix"&&A.instanceMatrix&&($=A.instanceMatrix),W==="instanceColor"&&A.instanceColor&&($=A.instanceColor));let xt={};xt.attribute=$,$&&$.data&&(xt.data=$.data),B[W]=xt,z++}r.attributes=B,r.attributesNum=z,r.index=L}function x(){let A=r.newAttributes;for(let R=0,U=A.length;R<U;R++)A[R]=0}function p(A){g(A,0)}function g(A,R){let U=r.newAttributes,L=r.enabledAttributes,B=r.attributeDivisors;U[A]=1,L[A]===0&&(n.enableVertexAttribArray(A),L[A]=1),B[A]!==R&&(n.vertexAttribDivisor(A,R),B[A]=R)}function y(){let A=r.newAttributes,R=r.enabledAttributes;for(let U=0,L=R.length;U<L;U++)R[U]!==A[U]&&(n.disableVertexAttribArray(U),R[U]=0)}function v(A,R,U,L,B,H,z){z===!0?n.vertexAttribIPointer(A,R,U,B,H):n.vertexAttribPointer(A,R,U,L,B,H)}function S(A,R,U,L){x();let B=L.attributes,H=U.getAttributes(),z=R.defaultAttributeValues;for(let Q in H){let W=H[Q];if(W.location>=0){let Z=B[Q];if(Z===void 0&&(Q==="instanceMatrix"&&A.instanceMatrix&&(Z=A.instanceMatrix),Q==="instanceColor"&&A.instanceColor&&(Z=A.instanceColor)),Z!==void 0){let $=Z.normalized,xt=Z.itemSize,Mt=t.get(Z);if(Mt===void 0)continue;let q=Mt.buffer,st=Mt.type,_t=Mt.bytesPerElement,wt=st===n.INT||st===n.UNSIGNED_INT||Z.gpuType===tl;if(Z.isInterleavedBufferAttribute){let rt=Z.data,Pt=rt.isInterleavedBuffer===!0?rt.stride:rt.itemSize,Qt=Z.offset;if(rt.isInstancedInterleavedBuffer||rt.isInstancedBufferAttribute){for(let ot=0;ot<W.locationSize;ot++)g(W.location+ot,rt.meshPerAttribute);A.isInstancedMesh!==!0&&L._maxInstanceCount===void 0&&(L._maxInstanceCount=rt.meshPerAttribute*rt.count)}else for(let ot=0;ot<W.locationSize;ot++)p(W.location+ot);n.bindBuffer(n.ARRAY_BUFFER,q);for(let ot=0;ot<W.locationSize;ot++)v(W.location+ot,xt/W.locationSize,st,$,Pt*_t,(Qt+xt/W.locationSize*ot)*_t,wt)}else{if(Z.isInstancedBufferAttribute){for(let rt=0;rt<W.locationSize;rt++)g(W.location+rt,Z.meshPerAttribute);A.isInstancedMesh!==!0&&L._maxInstanceCount===void 0&&(L._maxInstanceCount=Z.meshPerAttribute*Z.count)}else for(let rt=0;rt<W.locationSize;rt++)p(W.location+rt);n.bindBuffer(n.ARRAY_BUFFER,q);for(let rt=0;rt<W.locationSize;rt++)v(W.location+rt,xt/W.locationSize,st,$,xt*_t,xt/W.locationSize*rt*_t,wt)}}else if(z!==void 0){let $=z[Q];if($!==void 0)switch($.length){case 2:n.vertexAttrib2fv(W.location,$);break;case 3:n.vertexAttrib3fv(W.location,$);break;case 4:n.vertexAttrib4fv(W.location,$);break;default:n.vertexAttrib1fv(W.location,$)}}}}y()}function T(){D();for(let A in i){let R=i[A];for(let U in R){let L=R[U];for(let B in L){let H=L[B];for(let z in H)h(H[z].object),delete H[z];delete L[B]}}delete i[A]}}function b(A){if(i[A.id]===void 0)return;let R=i[A.id];for(let U in R){let L=R[U];for(let B in L){let H=L[B];for(let z in H)h(H[z].object),delete H[z];delete L[B]}}delete i[A.id]}function _(A){for(let R in i){let U=i[R];for(let L in U){let B=U[L];if(B[A.id]===void 0)continue;let H=B[A.id];for(let z in H)h(H[z].object),delete H[z];delete B[A.id]}}}function M(A){for(let R in i){let U=i[R],L=A.isInstancedMesh===!0?A.id:0,B=U[L];if(B!==void 0){for(let H in B){let z=B[H];for(let Q in z)h(z[Q].object),delete z[Q];delete B[H]}delete U[L],Object.keys(U).length===0&&delete i[R]}}}function D(){P(),o=!0,r!==s&&(r=s,c(r.object))}function P(){s.geometry=null,s.program=null,s.wireframe=!1}return{setup:a,reset:D,resetDefaultState:P,dispose:T,releaseStatesOfGeometry:b,releaseStatesOfObject:M,releaseStatesOfProgram:_,initAttributes:x,enableAttribute:p,disableUnusedAttributes:y}}function Gx(n,t,e){let i;function s(l){i=l}function r(l,c){n.drawArrays(i,l,c),e.update(c,i,1)}function o(l,c,h){h!==0&&(n.drawArraysInstanced(i,l,c,h),e.update(c,i,h))}function a(l,c,h){if(h===0)return;t.get("WEBGL_multi_draw").multiDrawArraysWEBGL(i,l,0,c,0,h);let d=0;for(let f=0;f<h;f++)d+=c[f];e.update(d,i,1)}this.setMode=s,this.render=r,this.renderInstances=o,this.renderMultiDraw=a}function Wx(n,t,e,i){let s;function r(){if(s!==void 0)return s;if(t.has("EXT_texture_filter_anisotropic")===!0){let _=t.get("EXT_texture_filter_anisotropic");s=n.getParameter(_.MAX_TEXTURE_MAX_ANISOTROPY_EXT)}else s=0;return s}function o(_){return!(_!==ui&&i.convert(_)!==n.getParameter(n.IMPLEMENTATION_COLOR_READ_FORMAT))}function a(_){let M=_===xe&&(t.has("EXT_color_buffer_half_float")||t.has("EXT_color_buffer_float"));return!(_!==ei&&_!==gi&&!M&&i.convert(_)!==n.getParameter(n.IMPLEMENTATION_COLOR_READ_TYPE))}function l(_){if(_==="highp"){if(n.getShaderPrecisionFormat(n.VERTEX_SHADER,n.HIGH_FLOAT).precision>0&&n.getShaderPrecisionFormat(n.FRAGMENT_SHADER,n.HIGH_FLOAT).precision>0)return"highp";_="mediump"}return _==="mediump"&&n.getShaderPrecisionFormat(n.VERTEX_SHADER,n.MEDIUM_FLOAT).precision>0&&n.getShaderPrecisionFormat(n.FRAGMENT_SHADER,n.MEDIUM_FLOAT).precision>0?"mediump":"lowp"}let c=e.precision!==void 0?e.precision:"highp",h=l(c);h!==c&&(Lt("WebGLRenderer:",c,"not supported, using",h,"instead."),c=h);let u=e.reversedDepthBuffer===!0&&t.has("EXT_clip_control"),d=e.logarithmicDepthBuffer===!0&&u===!1;e.reversedDepthBuffer===!0&&u===!1&&Lt("WebGLRenderer: Unable to use reversed depth buffer due to missing EXT_clip_control extension. Fallback to default depth buffer."),e.logarithmicDepthBuffer===!0&&u===!0&&Lt("WebGLRenderer: Logarithmic depth buffer is ignored when reversed depth buffer is enabled.");let f=n.getParameter(n.MAX_TEXTURE_IMAGE_UNITS),m=n.getParameter(n.MAX_VERTEX_TEXTURE_IMAGE_UNITS),x=n.getParameter(n.MAX_TEXTURE_SIZE),p=n.getParameter(n.MAX_CUBE_MAP_TEXTURE_SIZE),g=n.getParameter(n.MAX_VERTEX_ATTRIBS),y=n.getParameter(n.MAX_VERTEX_UNIFORM_VECTORS),v=n.getParameter(n.MAX_VARYING_VECTORS),S=n.getParameter(n.MAX_FRAGMENT_UNIFORM_VECTORS),T=n.getParameter(n.MAX_SAMPLES),b=n.getParameter(n.SAMPLES);return{isWebGL2:!0,getMaxAnisotropy:r,getMaxPrecision:l,textureFormatReadable:o,textureTypeReadable:a,precision:c,logarithmicDepthBuffer:d,reversedDepthBuffer:u,maxTextures:f,maxVertexTextures:m,maxTextureSize:x,maxCubemapSize:p,maxAttributes:g,maxVertexUniforms:y,maxVaryings:v,maxFragmentUniforms:S,maxSamples:T,samples:b}}function Xx(n){let t=this,e=null,i=0,s=!1,r=!1,o=new li,a=new zt,l={value:null,needsUpdate:!1};this.uniform=l,this.numPlanes=0,this.numIntersection=0,this.init=function(u,d){let f=u.length!==0||d||i!==0||s;return s=d,i=u.length,f},this.beginShadows=function(){r=!0,h(null)},this.endShadows=function(){r=!1},this.setGlobalState=function(u,d){e=h(u,d,0)},this.setState=function(u,d,f){let m=u.clippingPlanes,x=u.clipIntersection,p=u.clipShadows,g=n.get(u);if(!s||m===null||m.length===0||r&&!p)r?h(null):c();else{let y=r?0:i,v=y*4,S=g.clippingState||null;l.value=S,S=h(m,d,v,f);for(let T=0;T!==v;++T)S[T]=e[T];g.clippingState=S,this.numIntersection=x?this.numPlanes:0,this.numPlanes+=y}};function c(){l.value!==e&&(l.value=e,l.needsUpdate=i>0),t.numPlanes=i,t.numIntersection=0}function h(u,d,f,m){let x=u!==null?u.length:0,p=null;if(x!==0){if(p=l.value,m!==!0||p===null){let g=f+x*4,y=d.matrixWorldInverse;a.getNormalMatrix(y),(p===null||p.length<g)&&(p=new Float32Array(g));for(let v=0,S=f;v!==x;++v,S+=4)o.copy(u[v]).applyMatrix4(y,a),o.normal.toArray(p,S),p[S+3]=o.constant}l.value=p,l.needsUpdate=!0}return t.numPlanes=x,t.numIntersection=0,p}}var xo=class extends me{constructor(t=1,e={}){super(t,t,e),this.isWebGLCubeRenderTarget=!0;let i={width:t,height:t,depth:1},s=[i,i,i,i,i,i];this.texture=new Or(s),this._setTextureOptions(e),this.texture.isRenderTargetTexture=!0}fromEquirectangularTexture(t,e){this.texture.type=e.type,this.texture.colorSpace=e.colorSpace,this.texture.generateMipmaps=e.generateMipmaps,this.texture.minFilter=e.minFilter,this.texture.magFilter=e.magFilter;let i={uniforms:{tEquirect:{value:null}},vertexShader:`

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
			`},s=new Te(5,5,5),r=new fe({name:"CubemapFromEquirect",uniforms:ds(i.uniforms),vertexShader:i.vertexShader,fragmentShader:i.fragmentShader,side:Ze,blending:wi});r.uniforms.tEquirect.value=e;let o=new Ut(s,r),a=e.minFilter;return e.minFilter===zi&&(e.minFilter=ce),new Js(1,10,this).update(t,o),e.minFilter=a,o.geometry.dispose(),o.material.dispose(),this}clear(t,e=!0,i=!0,s=!0){let r=t.getRenderTarget();for(let o=0;o<6;o++)t.setRenderTarget(this,o),t.clear(e,i,s);t.setRenderTarget(r)}},lf=256,qx=3,Yx=20,If=256,Df=16,Zx=3,Kx=new C,Jx=new bt,ar=class n{constructor(t){this._renderer=t,this._cubeSize=0,this._sourceTarget=null,this._cubeCamera=new Js(1,10,null),this._boxMesh=new Ut(new Te(5,5,5),null),this._cubemapMaterial=null,this._equirectMaterial=null,this._blurMaterial=null,this._ggxMaterial=null,this._integrationMaterial=null}fromScene(t,e=0,i=.1,s=100,r={}){let{size:o=256,position:a=Kx}=r,l=this._renderer;this._setSize(o);let c=this._allocateTarget(),h=this._getSourceTarget(!0);e>0&&(h.texture.mipmapsAutoUpdate=!1);let u=l.autoClear,d=l.autoClearColor,f=l.autoClearDepth,m=l.autoClearStencil,x=t.background;l.autoClear=!0,l.autoClearColor=!0,l.autoClearDepth=!0,l.autoClearStencil=!0,x===null&&(t.background=l.getClearColor(Jx));let p=new Js(i,s,h);return p.position.copy(a),p.update(l,t),l.autoClear=u,l.autoClearColor=d,l.autoClearDepth=f,l.autoClearStencil=m,t.background=x,e>0&&(h.texture.mipmapsAutoUpdate=!0,this._blur(c,e)),this._applyPMREM(c),c}fromEquirectangular(t,e=null){return this._fromTexture(t,e)}fromCubemap(t,e=null){return this._fromTexture(t,e)}compileCubemapShader(){this._cubemapMaterial===null&&(this._cubemapMaterial=uf(),this._compileMaterial(this._cubemapMaterial))}compileEquirectangularShader(){this._equirectMaterial===null&&(this._equirectMaterial=hf(),this._compileMaterial(this._equirectMaterial))}dispose(){this._sourceTarget!==null&&this._sourceTarget.dispose(),this._cubemapMaterial!==null&&this._cubemapMaterial.dispose(),this._equirectMaterial!==null&&this._equirectMaterial.dispose(),this._blurMaterial!==null&&this._blurMaterial.dispose(),this._ggxMaterial!==null&&this._ggxMaterial.dispose(),this._integrationMaterial!==null&&this._integrationMaterial.dispose(),this._boxMesh.geometry.dispose()}static lodToRoughness(t,e){return e>0?1-Math.sqrt(1-t/e):0}_setSize(t){this._cubeSize=Math.max(lf,Ch(t))}_fromTexture(t,e){t.mapping===hs||t.mapping===er?this._setSize(t.image.length===0?lf:t.image[0].width||t.image[0].image.width):this._setSize(t.image.width/4);let i=e||this._allocateTarget();return this._textureToCubemap(t),this._applyPMREM(i),i}_allocateTarget(){let t=this._cubeSize,e=cf(t,!1,!1),i=Math.log2(t)-qx;for(let s=0;s<=i;s++)e.texture.mipmaps.push({width:t>>s,height:t>>s});return e.texture.name="PMREM",e.texture.isPMREMTexture=!0,e}_getSourceTarget(t=!1){let e=this._cubeSize,i=this._sourceTarget;return(i===null||i.width!==e||t&&i.depthBuffer===!1)&&(i!==null&&i.dispose(),this._sourceTarget=cf(e,!0,t)),this._sourceTarget}_compileMaterial(t){this._boxMesh.material=t,this._renderer.compile(this._boxMesh,this._cubeCamera.children[0])}_renderCube(t,e,i){let s=this._boxMesh,r=this._cubeCamera;s.material=i,r.renderTarget=t,r.activeMipmapLevel=e;let o=t.width>>e;t.viewport.set(0,0,o,o),r.update(this._renderer,s),t.viewport.set(0,0,t.width,t.height)}_textureToCubemap(t){let e;t.mapping===hs||t.mapping===er?(this._cubemapMaterial===null&&(this._cubemapMaterial=uf()),e=this._cubemapMaterial,e.uniforms.flipEnvMap.value=t.isRenderTargetTexture===!1?-1:1):(this._equirectMaterial===null&&(this._equirectMaterial=hf()),e=this._equirectMaterial),e.uniforms.envMap.value=t,this._renderCube(this._getSourceTarget(),0,e)}_applyPMREM(t){this._ggxMaterial===null&&(this._ggxMaterial=$x(),this._integrationMaterial=jx());let e=this._cubeSize,i=t.texture.mipmaps.length-1,s=this._ggxMaterial.uniforms;s.envMap.value=this._sourceTarget.texture;let r=this._integrationMaterial.uniforms;r.envMap.value=this._sourceTarget.texture,r.sourceLod.value=Math.log2(e/Df);for(let o=0;o<=i;o++){let a=n.lodToRoughness(o,i);if(o>i-Zx)r.roughness.value=a,this._renderCube(t,o,this._integrationMaterial);else{let l=a>0?Math.log2(e)+.5*Math.log2(6/(If*Math.pow(a,4)))+.5:0;s.roughness.value=a,s.lodBias.value=l,this._renderCube(t,o,this._ggxMaterial)}}}_blur(t,e){this._blurMaterial===null&&(this._blurMaterial=Qx());let i=this._blurMaterial,s=i.uniforms,r=this._sourceTarget;s.sigma.value=Math.min(e,Math.PI)/Math.SQRT2,s.envMap.value=r.texture,this._renderCube(t,0,i),s.envMap.value=t.texture,this._renderCube(r,0,i)}};function cf(n,t,e){return new xo(n,{minFilter:zi,generateMipmaps:t,type:xe,colorSpace:In,depthBuffer:e})}function _o(n,t,e){return new fe({name:n,uniforms:t,vertexShader:Gt.cube_vert,fragmentShader:e,side:Ze,blending:wi,depthTest:!1,depthWrite:!1})}function $x(){return _o("PMREMGGXConvolution",{envMap:{value:null},roughness:{value:0},lodBias:{value:0}},`

		#define GGX_SAMPLES ${If}u

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
	`)}function jx(){return _o("PMREMGGXIntegration",{envMap:{value:null},roughness:{value:0},sourceLod:{value:0},sourceSize:{value:Df}},`

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
	`)}function Qx(){return _o("PMREMSphericalGaussianBlur",{envMap:{value:null},sigma:{value:0}},`

		#define SAMPLES ${Yx}
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
	`)}function hf(){return _o("PMREMEquirectangularToCubemap",{envMap:{value:null}},`

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
	`)}function uf(){return _o("PMREMCubemapToCubemap",{envMap:{value:null},flipEnvMap:{value:-1}},`

		varying vec3 vWorldDirection;

		uniform samplerCube envMap;
		uniform float flipEnvMap;

		void main() {

			gl_FragColor = vec4( textureCube( envMap, vec3( flipEnvMap * vWorldDirection.x, vWorldDirection.yz ) ).rgb, 1.0 );

		}
	`)}function t1(n){let t=new WeakMap,e=new WeakMap,i=null;function s(d,f=!1){return d==null?null:f?o(d):r(d)}function r(d){if(d&&d.isTexture){let f=d.mapping;if(f===$a||f===ja)if(t.has(d)){let m=t.get(d).texture;return a(m,d.mapping)}else{let m=d.image;if(m&&m.height>0){let x=new xo(m.height);return x.fromEquirectangularTexture(n,d),t.set(d,x),d.addEventListener("dispose",c),a(x.texture,d.mapping)}else return null}}return d}function o(d){if(d&&d.isTexture&&d.isPMREMTexture!==!0){let f=d.mapping,m=f===$a||f===ja,x=f===hs||f===er;if(m||x){let p=e.get(d),g=p!==void 0?p.texture.pmremVersion:0;if(d.isRenderTargetTexture&&d.pmremVersion!==g)return i===null&&(i=new ar(n)),p=m?i.fromEquirectangular(d,p):i.fromCubemap(d,p),p.texture.pmremVersion=d.pmremVersion,e.set(d,p),p.texture;if(p!==void 0)return p.texture;{let y=d.image;return m&&y&&y.height>0||x&&y&&l(y)?(i===null&&(i=new ar(n)),p=m?i.fromEquirectangular(d):i.fromCubemap(d),p.texture.pmremVersion=d.pmremVersion,e.set(d,p),d.addEventListener("dispose",h),p.texture):null}}}return d}function a(d,f){return f===$a?d.mapping=hs:f===ja&&(d.mapping=er),d}function l(d){let f=0,m=6;for(let x=0;x<m;x++)d[x]!==void 0&&f++;return f===m}function c(d){let f=d.target;f.removeEventListener("dispose",c);let m=t.get(f);m!==void 0&&(t.delete(f),m.dispose())}function h(d){let f=d.target;f.removeEventListener("dispose",h);let m=e.get(f);m!==void 0&&(e.delete(f),m.dispose())}function u(){t=new WeakMap,e=new WeakMap,i!==null&&(i.dispose(),i=null)}return{get:s,dispose:u}}function e1(n){let t={};function e(i){if(t[i]!==void 0)return t[i];let s=n.getExtension(i);return t[i]=s,s}return{has:function(i){return e(i)!==null},init:function(){e("EXT_color_buffer_float"),e("WEBGL_clip_cull_distance"),e("OES_texture_float_linear"),e("EXT_color_buffer_half_float"),e("WEBGL_multisampled_render_to_texture"),e("WEBGL_render_shared_exponent")},get:function(i){let s=e(i);return s===null&&$n("WebGLRenderer: "+i+" extension not supported."),s}}}function i1(n,t,e,i){let s={},r=new WeakMap,o=new FinalizationRegistry(m=>delete s[m]);function a(m){l(m.target)}function l(m){m.index!==null&&t.remove(m.index);for(let p in m.attributes)t.remove(m.attributes[p]);m.removeEventListener("dispose",a),delete s[m.id],o.unregister(m);let x=r.get(m);x&&(t.remove(x),r.delete(m)),i.releaseStatesOfGeometry(m),m.isInstancedBufferGeometry===!0&&delete m._maxInstanceCount,e.memory.geometries--}function c(m,x){return s[x.id]!==void 0||(x.addEventListener("dispose",a),s[x.id]=new WeakRef(x),o.register(x,x.id,x),e.memory.geometries++),x}function h(m){let x=m.attributes;for(let p in x)t.update(x[p],n.ARRAY_BUFFER)}function u(m){let x=m.index,p=m.attributes.position,g=0;if(p===void 0)return;let y=x!==null?x.array.length:p.array.length/3-1,v=p.count>=65535?Ur:Nr,S=new v(Math.ceil(y/3)*6,1),T=S.array;if(x!==null){let _=x.array;g=x.version;for(let M=0,D=0;M<y;M+=3){let P=_[M+0],A=_[M+1],R=_[M+2];T[D++]=P,T[D++]=A,T[D++]=A,T[D++]=R,T[D++]=R,T[D++]=P}}else{g=p.version;for(let _=0,M=0;_<y;_+=3){let D=_+0,P=_+1,A=_+2;T[M++]=D,T[M++]=P,T[M++]=P,T[M++]=A,T[M++]=A,T[M++]=D}}S.version=g;let b=r.get(m);b&&t.remove(b),r.set(m,S)}function d(m){let x=r.get(m);if(x){let p=m.index;p!==null&&x.version<p.version&&u(m)}else u(m);return r.get(m)}function f(){for(let m in s){let x=s[m].deref();x!==void 0&&l(x)}}return{get:c,update:h,getWireframeAttribute:d,dispose:f}}function n1(n,t,e){let i;function s(u){i=u}let r,o;function a(u){r=u.type,o=u.bytesPerElement}function l(u,d){n.drawElements(i,d,r,u*o),e.update(d,i,1)}function c(u,d,f){f!==0&&(n.drawElementsInstanced(i,d,r,u*o,f),e.update(d,i,f))}function h(u,d,f){if(f===0)return;t.get("WEBGL_multi_draw").multiDrawElementsWEBGL(i,d,0,r,u,0,f);let x=0;for(let p=0;p<f;p++)x+=d[p];e.update(x,i,1)}this.setMode=s,this.setIndex=a,this.render=l,this.renderInstances=c,this.renderMultiDraw=h}function s1(n){let t={geometries:0,textures:0},e={frame:0,calls:0,triangles:0,points:0,lines:0};function i(r,o,a){switch(e.calls++,o){case n.TRIANGLES:e.triangles+=a*(r/3);break;case n.LINES:e.lines+=a*(r/2);break;case n.LINE_STRIP:e.lines+=a*(r-1);break;case n.LINE_LOOP:e.lines+=a*r;break;case n.POINTS:e.points+=a*r;break;default:Ot("WebGLInfo: Unknown draw mode:",o);break}}function s(){e.calls=0,e.triangles=0,e.points=0,e.lines=0}return{memory:t,render:e,programs:null,autoReset:!0,reset:s,update:i}}function r1(n,t,e){let i=new WeakMap,s=new oe;function r(o,a,l){let c=o.morphTargetInfluences,h=a.morphAttributes.position||a.morphAttributes.normal||a.morphAttributes.color,u=h!==void 0?h.length:0,d=i.get(a);if(d===void 0||d.count!==u){let D=function(){_.dispose(),i.delete(a),a.removeEventListener("dispose",D)};d!==void 0&&d.texture.dispose();let f=a.morphAttributes.position!==void 0,m=a.morphAttributes.normal!==void 0,x=a.morphAttributes.color!==void 0,p=a.morphAttributes.position||[],g=a.morphAttributes.normal||[],y=a.morphAttributes.color||[],v=0;f===!0&&(v=1),m===!0&&(v=2),x===!0&&(v=3);let S=a.attributes.position.count*v,T=1;S>t.maxTextureSize&&(T=Math.ceil(S/t.maxTextureSize),S=t.maxTextureSize);let b=new Float32Array(S*T*4*u),_=new Lr(b,S,T,u);_.type=gi,_.needsUpdate=!0;let M=v*4;for(let P=0;P<u;P++){let A=p[P],R=g[P],U=y[P],L=S*T*4*P;for(let B=0;B<A.count;B++){let H=B*M;f===!0&&(s.fromBufferAttribute(A,B),b[L+H+0]=s.x,b[L+H+1]=s.y,b[L+H+2]=s.z,b[L+H+3]=0),m===!0&&(s.fromBufferAttribute(R,B),b[L+H+4]=s.x,b[L+H+5]=s.y,b[L+H+6]=s.z,b[L+H+7]=0),x===!0&&(s.fromBufferAttribute(U,B),b[L+H+8]=s.x,b[L+H+9]=s.y,b[L+H+10]=s.z,b[L+H+11]=U.itemSize===4?s.w:1)}}d={count:u,texture:_,size:new et(S,T)},i.set(a,d),a.addEventListener("dispose",D)}if(o.isInstancedMesh===!0&&o.morphTexture!==null)l.getUniforms().setValue(n,"morphTexture",o.morphTexture,e);else{let f=0;for(let x=0;x<c.length;x++)f+=c[x];let m=a.morphTargetsRelative?1:1-f;l.getUniforms().setValue(n,"morphTargetBaseInfluence",m),l.getUniforms().setValue(n,"morphTargetInfluences",c)}l.getUniforms().setValue(n,"morphTargetsTexture",d.texture,e),l.getUniforms().setValue(n,"morphTargetsTextureSize",d.size)}return{update:r}}function o1(n,t,e,i,s){let r=new WeakMap;function o(c){let h=s.render.frame,u=c.geometry,d=t.get(c,u);if(r.get(d)!==h&&(t.update(d),r.set(d,h)),c.isInstancedMesh&&(c.hasEventListener("dispose",l)===!1&&c.addEventListener("dispose",l),r.get(c)!==h&&(e.update(c.instanceMatrix,n.ARRAY_BUFFER),c.instanceColor!==null&&e.update(c.instanceColor,n.ARRAY_BUFFER),r.set(c,h))),c.isSkinnedMesh){let f=c.skeleton;r.get(f)!==h&&(f.update(),r.set(f,h))}return d}function a(){r=new WeakMap}function l(c){let h=c.target;h.removeEventListener("dispose",l),i.releaseStatesOfObject(h),e.remove(h.instanceMatrix),h.instanceColor!==null&&e.remove(h.instanceColor)}return{update:o,dispose:a}}var a1={[to]:"LINEAR_TONE_MAPPING",[eo]:"REINHARD_TONE_MAPPING",[io]:"CINEON_TONE_MAPPING",[cs]:"ACES_FILMIC_TONE_MAPPING",[so]:"AGX_TONE_MAPPING",[ro]:"NEUTRAL_TONE_MAPPING",[no]:"CUSTOM_TONE_MAPPING"};function l1(n,t,e,i,s,r){let o=new me(t,e,{type:n,depthBuffer:s,stencilBuffer:r,samples:i?4:0,storeMultisampledDepthBuffer:!1,storeMultisampledStencilBuffer:!1,resolveDepthBuffer:!1,resolveStencilBuffer:!1}),a=null,l=null,c=null,h=new Jt;h.setAttribute("position",new ee([-1,3,0,-1,-1,0,3,-1,0],3)),h.setAttribute("uv",new ee([0,2,0,0,2,0],2));let u=new Ln({uniforms:{tDiffuse:{value:null}},vertexShader:`
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
			}`,depthTest:!1,depthWrite:!1}),d=new Ut(h,u),f=new as(-1,1,1,-1,0,1),m=null,x=null,p=!1,g,y=null,v=[],S=!1;this.setSize=function(T,b){o.setSize(T,b),a!==null&&a.setSize(T,b),l!==null&&l.setSize(T,b);for(let _=0;_<v.length;_++){let M=v[_];M.setSize&&M.setSize(T,b)}},this.setEffects=function(T){v=T,S=v.length>0&&v[0].isRenderPass===!0;let b=o.width,_=o.height;v.length>0&&a===null&&(a=new me(b,_,{type:xe,depthBuffer:!1,stencilBuffer:!1}),l=new me(b,_,{type:xe,depthBuffer:!1,stencilBuffer:!1}));let M=s===!0&&v.some(D=>D.needsDepthTexture===!0);M!==(o.depthTexture!==null)&&(M===!0&&c===null&&(c=new Oi(b,_),r===!0&&(c.format=xn,c.type=us)),o.dispose(),o.depthTexture=M?c:null,o.resolveDepthBuffer=M,o.storeMultisampledDepthBuffer=M);for(let D=0;D<v.length;D++){let P=v[D];P.setSize&&P.setSize(b,_),M&&P.needsDepthTexture===!0&&P.setDepthTexture&&P.setDepthTexture(c)}},this.begin=function(T,b){if(p||T.toneMapping===Yi&&v.length===0)return!1;if(y=b,b!==null){let _=b.width,M=b.height;(o.width!==_||o.height!==M)&&this.setSize(_,M)}return S===!1&&T.setRenderTarget(o),g=T.toneMapping,T.toneMapping=Yi,!0},this.hasRenderPass=function(){return S},this.end=function(T,b){T.toneMapping=g,p=!0;let _=o,M=a;for(let D=0;D<v.length;D++){let P=v[D];P.enabled!==!1&&(P.render(T,M,_,b),P.needsSwap!==!1&&(_=M,M=M===a?l:a))}if(m!==T.outputColorSpace||x!==T.toneMapping){m=T.outputColorSpace,x=T.toneMapping,u.defines={},Zt.getTransfer(m)===ne&&(u.defines.SRGB_TRANSFER="");let D=a1[x];D&&(u.defines[D]=""),u.needsUpdate=!0}u.uniforms.tDiffuse.value=_.texture,T.setRenderTarget(y),T.render(d,f),y=null,p=!1},this.isCompositing=function(){return p},this.dispose=function(){o.dispose(),a!==null&&a.dispose(),l!==null&&l.dispose(),h.dispose(),u.dispose()}}var Lf=new qe,Nf=new Oi(1,1),Uf=new Br(1),Ff=new Lr,Of=new ks,Bf=new Or,df=[],ff=[],pf=new Float32Array(16),mf=new Float32Array(9),gf=new Float32Array(4);function lr(n,t,e){let i=n[0];if(i<=0||i>0)return n;let s=t*e,r=df[s];if(r===void 0&&(r=new Float32Array(s),df[s]=r),t!==0){i.toArray(r,0);for(let o=1,a=0;o!==t;++o)a+=e,n[o].toArray(r,a)}return r}function Fe(n,t){if(n.length!==t.length)return!1;for(let e=0,i=n.length;e<i;e++)if(n[e]!==t[e])return!1;return!0}function Oe(n,t){for(let e=0,i=t.length;e<i;e++)n[e]=t[e]}function zl(n,t){let e=ff[t];e===void 0&&(e=new Int32Array(t),ff[t]=e);for(let i=0;i!==t;++i)e[i]=n.allocateTextureUnit();return e}function c1(n,t){let e=this.cache;e[0]!==t&&(n.uniform1f(this.addr,t),e[0]=t)}function h1(n,t){let e=this.cache;if(t.x!==void 0)(e[0]!==t.x||e[1]!==t.y)&&(n.uniform2f(this.addr,t.x,t.y),e[0]=t.x,e[1]=t.y);else{if(Fe(e,t))return;n.uniform2fv(this.addr,t),Oe(e,t)}}function u1(n,t){let e=this.cache;if(t.x!==void 0)(e[0]!==t.x||e[1]!==t.y||e[2]!==t.z)&&(n.uniform3f(this.addr,t.x,t.y,t.z),e[0]=t.x,e[1]=t.y,e[2]=t.z);else if(t.r!==void 0)(e[0]!==t.r||e[1]!==t.g||e[2]!==t.b)&&(n.uniform3f(this.addr,t.r,t.g,t.b),e[0]=t.r,e[1]=t.g,e[2]=t.b);else{if(Fe(e,t))return;n.uniform3fv(this.addr,t),Oe(e,t)}}function d1(n,t){let e=this.cache;if(t.x!==void 0)(e[0]!==t.x||e[1]!==t.y||e[2]!==t.z||e[3]!==t.w)&&(n.uniform4f(this.addr,t.x,t.y,t.z,t.w),e[0]=t.x,e[1]=t.y,e[2]=t.z,e[3]=t.w);else{if(Fe(e,t))return;n.uniform4fv(this.addr,t),Oe(e,t)}}function f1(n,t){let e=this.cache,i=t.elements;if(i===void 0){if(Fe(e,t))return;n.uniformMatrix2fv(this.addr,!1,t),Oe(e,t)}else{if(Fe(e,i))return;gf.set(i),n.uniformMatrix2fv(this.addr,!1,gf),Oe(e,i)}}function p1(n,t){let e=this.cache,i=t.elements;if(i===void 0){if(Fe(e,t))return;n.uniformMatrix3fv(this.addr,!1,t),Oe(e,t)}else{if(Fe(e,i))return;mf.set(i),n.uniformMatrix3fv(this.addr,!1,mf),Oe(e,i)}}function m1(n,t){let e=this.cache,i=t.elements;if(i===void 0){if(Fe(e,t))return;n.uniformMatrix4fv(this.addr,!1,t),Oe(e,t)}else{if(Fe(e,i))return;pf.set(i),n.uniformMatrix4fv(this.addr,!1,pf),Oe(e,i)}}function g1(n,t){let e=this.cache;e[0]!==t&&(n.uniform1i(this.addr,t),e[0]=t)}function x1(n,t){let e=this.cache;if(t.x!==void 0)(e[0]!==t.x||e[1]!==t.y)&&(n.uniform2i(this.addr,t.x,t.y),e[0]=t.x,e[1]=t.y);else{if(Fe(e,t))return;n.uniform2iv(this.addr,t),Oe(e,t)}}function _1(n,t){let e=this.cache;if(t.x!==void 0)(e[0]!==t.x||e[1]!==t.y||e[2]!==t.z)&&(n.uniform3i(this.addr,t.x,t.y,t.z),e[0]=t.x,e[1]=t.y,e[2]=t.z);else{if(Fe(e,t))return;n.uniform3iv(this.addr,t),Oe(e,t)}}function y1(n,t){let e=this.cache;if(t.x!==void 0)(e[0]!==t.x||e[1]!==t.y||e[2]!==t.z||e[3]!==t.w)&&(n.uniform4i(this.addr,t.x,t.y,t.z,t.w),e[0]=t.x,e[1]=t.y,e[2]=t.z,e[3]=t.w);else{if(Fe(e,t))return;n.uniform4iv(this.addr,t),Oe(e,t)}}function v1(n,t){let e=this.cache;e[0]!==t&&(n.uniform1ui(this.addr,t),e[0]=t)}function M1(n,t){let e=this.cache;if(t.x!==void 0)(e[0]!==t.x||e[1]!==t.y)&&(n.uniform2ui(this.addr,t.x,t.y),e[0]=t.x,e[1]=t.y);else{if(Fe(e,t))return;n.uniform2uiv(this.addr,t),Oe(e,t)}}function S1(n,t){let e=this.cache;if(t.x!==void 0)(e[0]!==t.x||e[1]!==t.y||e[2]!==t.z)&&(n.uniform3ui(this.addr,t.x,t.y,t.z),e[0]=t.x,e[1]=t.y,e[2]=t.z);else{if(Fe(e,t))return;n.uniform3uiv(this.addr,t),Oe(e,t)}}function b1(n,t){let e=this.cache;if(t.x!==void 0)(e[0]!==t.x||e[1]!==t.y||e[2]!==t.z||e[3]!==t.w)&&(n.uniform4ui(this.addr,t.x,t.y,t.z,t.w),e[0]=t.x,e[1]=t.y,e[2]=t.z,e[3]=t.w);else{if(Fe(e,t))return;n.uniform4uiv(this.addr,t),Oe(e,t)}}function kl(n){return n.version===0&&(n.compareFunction=Th,n.needsUpdate=!0),n}function w1(n,t,e){let i=this.cache,s=e.allocateTextureUnit();i[0]!==s&&(n.uniform1i(this.addr,s),i[0]=s);let r;this.type===n.SAMPLER_2D_SHADOW?r=kl(Nf):r=Lf,e.setTexture2D(t||r,s)}function E1(n,t,e){let i=this.cache,s=e.allocateTextureUnit();i[0]!==s&&(n.uniform1i(this.addr,s),i[0]=s),e.setTexture3D(t||Of,s)}function T1(n,t,e){let i=this.cache,s=e.allocateTextureUnit();i[0]!==s&&(n.uniform1i(this.addr,s),i[0]=s);let r;this.type===n.SAMPLER_CUBE_SHADOW?r=kl(Uf):r=Bf,e.setTextureCube(t||r,s)}function A1(n,t,e){let i=this.cache,s=e.allocateTextureUnit();i[0]!==s&&(n.uniform1i(this.addr,s),i[0]=s),e.setTexture2DArray(t||Ff,s)}function R1(n){switch(n){case 5126:return c1;case 35664:return h1;case 35665:return u1;case 35666:return d1;case 35674:return f1;case 35675:return p1;case 35676:return m1;case 5124:case 35670:return g1;case 35667:case 35671:return x1;case 35668:case 35672:return _1;case 35669:case 35673:return y1;case 5125:return v1;case 36294:return M1;case 36295:return S1;case 36296:return b1;case 35678:case 36198:case 36298:case 36306:case 35682:return w1;case 35679:case 36299:case 36307:return E1;case 35680:case 36300:case 36308:case 36293:return T1;case 36289:case 36303:case 36311:case 36292:return A1}}function C1(n,t){n.uniform1fv(this.addr,t)}function P1(n,t){let e=lr(t,this.size,2);n.uniform2fv(this.addr,e)}function I1(n,t){let e=lr(t,this.size,3);n.uniform3fv(this.addr,e)}function D1(n,t){let e=lr(t,this.size,4);n.uniform4fv(this.addr,e)}function L1(n,t){let e=lr(t,this.size,4);n.uniformMatrix2fv(this.addr,!1,e)}function N1(n,t){let e=lr(t,this.size,9);n.uniformMatrix3fv(this.addr,!1,e)}function U1(n,t){let e=lr(t,this.size,16);n.uniformMatrix4fv(this.addr,!1,e)}function F1(n,t){n.uniform1iv(this.addr,t)}function O1(n,t){n.uniform2iv(this.addr,t)}function B1(n,t){n.uniform3iv(this.addr,t)}function z1(n,t){n.uniform4iv(this.addr,t)}function k1(n,t){n.uniform1uiv(this.addr,t)}function H1(n,t){n.uniform2uiv(this.addr,t)}function V1(n,t){n.uniform3uiv(this.addr,t)}function G1(n,t){n.uniform4uiv(this.addr,t)}function W1(n,t,e){let i=this.cache,s=t.length,r=zl(e,s);Fe(i,r)||(n.uniform1iv(this.addr,r),Oe(i,r));let o;this.type===n.SAMPLER_2D_SHADOW?o=kl(Nf):o=Lf;for(let a=0;a!==s;++a)e.setTexture2D(t[a]||o,r[a])}function X1(n,t,e){let i=this.cache,s=t.length,r=zl(e,s);Fe(i,r)||(n.uniform1iv(this.addr,r),Oe(i,r));for(let o=0;o!==s;++o)e.setTexture3D(t[o]||Of,r[o])}function q1(n,t,e){let i=this.cache,s=t.length,r=zl(e,s);Fe(i,r)||(n.uniform1iv(this.addr,r),Oe(i,r));let o;this.type===n.SAMPLER_CUBE_SHADOW?o=kl(Uf):o=Bf;for(let a=0;a!==s;++a)e.setTextureCube(t[a]||o,r[a])}function Y1(n,t,e){let i=this.cache,s=t.length,r=zl(e,s);Fe(i,r)||(n.uniform1iv(this.addr,r),Oe(i,r));for(let o=0;o!==s;++o)e.setTexture2DArray(t[o]||Ff,r[o])}function Z1(n){switch(n){case 5126:return C1;case 35664:return P1;case 35665:return I1;case 35666:return D1;case 35674:return L1;case 35675:return N1;case 35676:return U1;case 5124:case 35670:return F1;case 35667:case 35671:return O1;case 35668:case 35672:return B1;case 35669:case 35673:return z1;case 5125:return k1;case 36294:return H1;case 36295:return V1;case 36296:return G1;case 35678:case 36198:case 36298:case 36306:case 35682:return W1;case 35679:case 36299:case 36307:return X1;case 35680:case 36300:case 36308:case 36293:return q1;case 36289:case 36303:case 36311:case 36292:return Y1}}var Bh=class{constructor(t,e,i){this.id=t,this.addr=i,this.cache=[],this.type=e.type,this.setValue=R1(e.type)}},zh=class{constructor(t,e,i){this.id=t,this.addr=i,this.cache=[],this.type=e.type,this.size=e.size,this.setValue=Z1(e.type)}},kh=class{constructor(t){this.id=t,this.seq=[],this.map={}}setValue(t,e,i){let s=this.seq;for(let r=0,o=s.length;r!==o;++r){let a=s[r];a.setValue(t,e[a.id],i)}}},Fh=/(\w+)(\])?(\[|\.)?/g;function xf(n,t){n.seq.push(t),n.map[t.id]=t}function K1(n,t,e){let i=n.name,s=i.length;for(Fh.lastIndex=0;;){let r=Fh.exec(i),o=Fh.lastIndex,a=r[1],l=r[2]==="]",c=r[3];if(l&&(a=a|0),c===void 0||c==="["&&o+2===s){xf(e,c===void 0?new Bh(a,n,t):new zh(a,n,t));break}else{let u=e.map[a];u===void 0&&(u=new kh(a),xf(e,u)),e=u}}}var or=class{constructor(t,e){this.seq=[],this.map={};let i=t.getProgramParameter(e,t.ACTIVE_UNIFORMS);for(let o=0;o<i;++o){let a=t.getActiveUniform(e,o),l=t.getUniformLocation(e,a.name);K1(a,l,this)}let s=[],r=[];for(let o of this.seq)o.type===t.SAMPLER_2D_SHADOW||o.type===t.SAMPLER_CUBE_SHADOW||o.type===t.SAMPLER_2D_ARRAY_SHADOW?s.push(o):r.push(o);s.length>0&&(this.seq=s.concat(r))}setValue(t,e,i,s){let r=this.map[e];r!==void 0&&r.setValue(t,i,s)}setOptional(t,e,i){let s=e[i];s!==void 0&&this.setValue(t,i,s)}static upload(t,e,i,s){for(let r=0,o=e.length;r!==o;++r){let a=e[r],l=i[a.id];l.needsUpdate!==!1&&a.setValue(t,l.value,s)}}static seqWithValue(t,e){let i=[];for(let s=0,r=t.length;s!==r;++s){let o=t[s];o.id in e&&i.push(o)}return i}};function _f(n,t,e){let i=n.createShader(t);return n.shaderSource(i,e),n.compileShader(i),i}var J1=37297,$1=0;function j1(n,t){let e=n.split(`
`),i=[],s=Math.max(t-6,0),r=Math.min(t+6,e.length);for(let o=s;o<r;o++){let a=o+1;i.push(`${a===t?">":" "} ${a}: ${e[o]}`)}return i.join(`
`)}var yf=new zt;function Q1(n){Zt._getMatrix(yf,Zt.workingColorSpace,n);let t=`mat3( ${yf.elements.map(e=>e.toFixed(4))} )`;switch(Zt.getTransfer(n)){case Ir:return[t,"LinearTransferOETF"];case ne:return[t,"sRGBTransferOETF"];default:return Lt("WebGLProgram: Unsupported color space: ",n),[t,"LinearTransferOETF"]}}function vf(n,t,e){let i=n.getShaderParameter(t,n.COMPILE_STATUS),r=(n.getShaderInfoLog(t)||"").trim();if(i&&r==="")return"";let o=/ERROR: 0:(\d+)/.exec(r);if(o){let a=parseInt(o[1]);return e.toUpperCase()+`

`+r+`

`+j1(n.getShaderSource(t),a)}else return r}function t_(n,t){let e=Q1(t);return[`vec4 ${n}( vec4 value ) {`,`	return ${e[1]}( vec4( ${e[0]} * value.rgb, value.a ) );`,"}"].join(`
`)}var e_={[to]:"Linear",[eo]:"Reinhard",[io]:"Cineon",[cs]:"ACESFilmic",[so]:"AgX",[ro]:"Neutral",[no]:"Custom"};function i_(n,t){let e=e_[t];return e===void 0?(Lt("WebGLProgram: Unsupported toneMapping:",t),"vec3 "+n+"( vec3 color ) { return LinearToneMapping( color ); }"):"vec3 "+n+"( vec3 color ) { return "+e+"ToneMapping( color ); }"}var Ol=new C;function n_(){Zt.getLuminanceCoefficients(Ol);let n=Ol.x.toFixed(4),t=Ol.y.toFixed(4),e=Ol.z.toFixed(4);return["float luminance( const in vec3 rgb ) {",`	const vec3 weights = vec3( ${n}, ${t}, ${e} );`,"	return dot( weights, rgb );","}"].join(`
`)}function s_(n){return[n.extensionClipCullDistance?"#extension GL_ANGLE_clip_cull_distance : require":"",n.extensionMultiDraw?"#extension GL_ANGLE_multi_draw : require":""].filter(go).join(`
`)}function r_(n){let t=[];for(let e in n){let i=n[e];i!==!1&&t.push("#define "+e+" "+i)}return t.join(`
`)}function o_(n,t){let e={},i=n.getProgramParameter(t,n.ACTIVE_ATTRIBUTES);for(let s=0;s<i;s++){let r=n.getActiveAttrib(t,s),o=r.name,a=1;r.type===n.FLOAT_MAT2&&(a=2),r.type===n.FLOAT_MAT3&&(a=3),r.type===n.FLOAT_MAT4&&(a=4),e[o]={type:r.type,location:n.getAttribLocation(t,o),locationSize:a}}return e}function go(n){return n!==""}function Mf(n,t){let e=t.numSpotLightShadows+t.numSpotLightMaps-t.numSpotLightShadowsWithMaps;return n.replace(/NUM_SUN_LIGHTS/g,t.numSunLights).replace(/NUM_DIR_LIGHTS/g,t.numDirLights).replace(/NUM_SPOT_LIGHTS/g,t.numSpotLights).replace(/NUM_SPOT_LIGHT_MAPS/g,t.numSpotLightMaps).replace(/NUM_SPOT_LIGHT_COORDS/g,e).replace(/NUM_RECT_AREA_LIGHTS/g,t.numRectAreaLights).replace(/NUM_POINT_LIGHTS/g,t.numPointLights).replace(/NUM_HEMI_LIGHTS/g,t.numHemiLights).replace(/NUM_SUN_LIGHT_SHADOWS/g,t.numSunLightShadows).replace(/NUM_DIR_LIGHT_SHADOWS/g,t.numDirLightShadows).replace(/NUM_SPOT_LIGHT_SHADOWS_WITH_MAPS/g,t.numSpotLightShadowsWithMaps).replace(/NUM_SPOT_LIGHT_SHADOWS/g,t.numSpotLightShadows).replace(/NUM_POINT_LIGHT_SHADOWS/g,t.numPointLightShadows).replace(/NUM_LIGHT_PROBE_GRIDS/g,t.numLightProbeGrids)}function Sf(n,t){return n.replace(/NUM_CLIPPING_PLANES/g,t.numClippingPlanes).replace(/UNION_CLIPPING_PLANES/g,t.numClippingPlanes-t.numClipIntersection)}var a_=/^[ \t]*#include +<([\w\d./]+)>/gm;function Hh(n){return n.replace(a_,l_)}function l_(n,t){let e=Gt[t];if(e===void 0)throw new Error("THREE.WebGLProgram: Can not resolve #include <"+t+">");return Hh(e)}var c_=/#pragma unroll_loop_start\s+for\s*\(\s*int\s+i\s*=\s*(\d+)\s*;\s*i\s*<\s*(\d+)\s*;\s*i\s*\+\+\s*\)\s*{([\s\S]+?)}\s+#pragma unroll_loop_end/g;function bf(n){return n.replace(c_,h_)}function h_(n,t,e,i){let s="";for(let r=parseInt(t);r<parseInt(e);r++)s+=i.replace(/\[\s*i\s*\]/g,"[ "+r+" ]").replace(/UNROLLED_LOOP_INDEX/g,r);return s}function wf(n){let t=`precision ${n.precision} float;
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
#define LOW_PRECISION`),t}var u_={[ls]:"SHADOWMAP_TYPE_PCF",[js]:"SHADOWMAP_TYPE_VSM"};function d_(n){return u_[n.shadowMapType]||"SHADOWMAP_TYPE_BASIC"}var f_={[er]:"ENVMAP_MODE_REFRACTION"};function p_(n){return n.envMap===!1?"ENVMAP_MODE_REFLECTION":f_[n.envMapMode]||"ENVMAP_MODE_REFLECTION"}var m_={[Ja]:"ENVMAP_BLENDING_MULTIPLY",[kd]:"ENVMAP_BLENDING_MIX",[Hd]:"ENVMAP_BLENDING_ADD"};function g_(n){return n.envMap===!1?"ENVMAP_BLENDING_NONE":m_[n.combine]||"ENVMAP_BLENDING_NONE"}function x_(n,t,e,i){let s=n.getContext(),r=e.defines,o=e.vertexShader,a=e.fragmentShader,l=d_(e),c=e.envMapPMREM===!0?"ENVMAP_TYPE_PMREM":"ENVMAP_TYPE_CUBE",h=p_(e),u=g_(e),d=s_(e),f=r_(r),m=s.createProgram(),x,p,g=e.glslVersion?"#version "+e.glslVersion+`
`:"";e.isRawShaderMaterial?(x=["#define SHADER_TYPE "+e.shaderType,"#define SHADER_NAME "+e.shaderName,f].filter(go).join(`
`),x.length>0&&(x+=`
`),p=["#define SHADER_TYPE "+e.shaderType,"#define SHADER_NAME "+e.shaderName,f].filter(go).join(`
`),p.length>0&&(p+=`
`)):(x=[wf(e),"#define SHADER_TYPE "+e.shaderType,"#define SHADER_NAME "+e.shaderName,f,e.extensionClipCullDistance?"#define USE_CLIP_DISTANCE":"",e.batching?"#define USE_BATCHING":"",e.batchingColor?"#define USE_BATCHING_COLOR":"",e.instancing?"#define USE_INSTANCING":"",e.instancingColor?"#define USE_INSTANCING_COLOR":"",e.instancingMorph?"#define USE_INSTANCING_MORPH":"",e.useFog&&e.fog?"#define USE_FOG":"",e.useFog&&e.fogExp2?"#define FOG_EXP2":"",e.map?"#define USE_MAP":"",e.envMap?"#define USE_ENVMAP":"",e.envMap?"#define "+h:"",e.lightMap?"#define USE_LIGHTMAP":"",e.aoMap?"#define USE_AOMAP":"",e.bumpMap?"#define USE_BUMPMAP":"",e.normalMap?"#define USE_NORMALMAP":"",e.normalMapObjectSpace?"#define USE_NORMALMAP_OBJECTSPACE":"",e.normalMapTangentSpace?"#define USE_NORMALMAP_TANGENTSPACE":"",e.displacementMap?"#define USE_DISPLACEMENTMAP":"",e.emissiveMap?"#define USE_EMISSIVEMAP":"",e.anisotropy?"#define USE_ANISOTROPY":"",e.anisotropyMap?"#define USE_ANISOTROPYMAP":"",e.clearcoatMap?"#define USE_CLEARCOATMAP":"",e.clearcoatRoughnessMap?"#define USE_CLEARCOAT_ROUGHNESSMAP":"",e.clearcoatNormalMap?"#define USE_CLEARCOAT_NORMALMAP":"",e.diffuseRoughnessMap?"#define USE_DIFFUSE_ROUGHNESSMAP":"",e.iridescenceMap?"#define USE_IRIDESCENCEMAP":"",e.iridescenceThicknessMap?"#define USE_IRIDESCENCE_THICKNESSMAP":"",e.specularMap?"#define USE_SPECULARMAP":"",e.specularColorMap?"#define USE_SPECULAR_COLORMAP":"",e.specularIntensityMap?"#define USE_SPECULAR_INTENSITYMAP":"",e.roughnessMap?"#define USE_ROUGHNESSMAP":"",e.metalnessMap?"#define USE_METALNESSMAP":"",e.alphaMap?"#define USE_ALPHAMAP":"",e.alphaHash?"#define USE_ALPHAHASH":"",e.transmission?"#define USE_TRANSMISSION":"",e.transmissionMap?"#define USE_TRANSMISSIONMAP":"",e.thicknessMap?"#define USE_THICKNESSMAP":"",e.sheenColorMap?"#define USE_SHEEN_COLORMAP":"",e.sheenRoughnessMap?"#define USE_SHEEN_ROUGHNESSMAP":"",e.mapUv?"#define MAP_UV "+e.mapUv:"",e.alphaMapUv?"#define ALPHAMAP_UV "+e.alphaMapUv:"",e.lightMapUv?"#define LIGHTMAP_UV "+e.lightMapUv:"",e.aoMapUv?"#define AOMAP_UV "+e.aoMapUv:"",e.emissiveMapUv?"#define EMISSIVEMAP_UV "+e.emissiveMapUv:"",e.bumpMapUv?"#define BUMPMAP_UV "+e.bumpMapUv:"",e.normalMapUv?"#define NORMALMAP_UV "+e.normalMapUv:"",e.displacementMapUv?"#define DISPLACEMENTMAP_UV "+e.displacementMapUv:"",e.metalnessMapUv?"#define METALNESSMAP_UV "+e.metalnessMapUv:"",e.roughnessMapUv?"#define ROUGHNESSMAP_UV "+e.roughnessMapUv:"",e.anisotropyMapUv?"#define ANISOTROPYMAP_UV "+e.anisotropyMapUv:"",e.clearcoatMapUv?"#define CLEARCOATMAP_UV "+e.clearcoatMapUv:"",e.clearcoatNormalMapUv?"#define CLEARCOAT_NORMALMAP_UV "+e.clearcoatNormalMapUv:"",e.clearcoatRoughnessMapUv?"#define CLEARCOAT_ROUGHNESSMAP_UV "+e.clearcoatRoughnessMapUv:"",e.diffuseRoughnessMapUv?"#define DIFFUSE_ROUGHNESSMAP_UV "+e.diffuseRoughnessMapUv:"",e.iridescenceMapUv?"#define IRIDESCENCEMAP_UV "+e.iridescenceMapUv:"",e.iridescenceThicknessMapUv?"#define IRIDESCENCE_THICKNESSMAP_UV "+e.iridescenceThicknessMapUv:"",e.sheenColorMapUv?"#define SHEEN_COLORMAP_UV "+e.sheenColorMapUv:"",e.sheenRoughnessMapUv?"#define SHEEN_ROUGHNESSMAP_UV "+e.sheenRoughnessMapUv:"",e.specularMapUv?"#define SPECULARMAP_UV "+e.specularMapUv:"",e.specularColorMapUv?"#define SPECULAR_COLORMAP_UV "+e.specularColorMapUv:"",e.specularIntensityMapUv?"#define SPECULAR_INTENSITYMAP_UV "+e.specularIntensityMapUv:"",e.transmissionMapUv?"#define TRANSMISSIONMAP_UV "+e.transmissionMapUv:"",e.thicknessMapUv?"#define THICKNESSMAP_UV "+e.thicknessMapUv:"",e.vertexTangents&&e.flatShading===!1?"#define USE_TANGENT":"",e.vertexNormals?"#define HAS_NORMAL":"",e.vertexColors?"#define USE_COLOR":"",e.vertexAlphas?"#define USE_COLOR_ALPHA":"",e.vertexUv1s?"#define USE_UV1":"",e.vertexUv2s?"#define USE_UV2":"",e.vertexUv3s?"#define USE_UV3":"",e.pointsUvs?"#define USE_POINTS_UV":"",e.flatShading?"#define FLAT_SHADED":"",e.skinning?"#define USE_SKINNING":"",e.morphTargets?"#define USE_MORPHTARGETS":"",e.morphNormals&&e.flatShading===!1?"#define USE_MORPHNORMALS":"",e.morphColors?"#define USE_MORPHCOLORS":"",e.morphTargetsCount>0?"#define MORPHTARGETS_TEXTURE_STRIDE "+e.morphTextureStride:"",e.morphTargetsCount>0?"#define MORPHTARGETS_COUNT "+e.morphTargetsCount:"",e.doubleSided?"#define DOUBLE_SIDED":"",e.flipSided?"#define FLIP_SIDED":"",e.shadowMapEnabled?"#define USE_SHADOWMAP":"",e.shadowMapEnabled?"#define "+l:"",e.sizeAttenuation?"#define USE_SIZEATTENUATION":"",e.numLightProbes>0?"#define USE_LIGHT_PROBES":"",e.logarithmicDepthBuffer?"#define USE_LOGARITHMIC_DEPTH_BUFFER":"",e.reversedDepthBuffer?"#define USE_REVERSED_DEPTH_BUFFER":"","uniform mat4 modelMatrix;","uniform mat4 modelViewMatrix;","uniform mat4 projectionMatrix;","uniform mat4 viewMatrix;","uniform mat3 normalMatrix;","uniform vec3 cameraPosition;","uniform bool isOrthographic;","#ifdef USE_INSTANCING","	attribute mat4 instanceMatrix;","#endif","#ifdef USE_INSTANCING_COLOR","	attribute vec3 instanceColor;","#endif","#ifdef USE_INSTANCING_MORPH","	uniform sampler2D morphTexture;","#endif","attribute vec3 position;","attribute vec3 normal;","attribute vec2 uv;","#ifdef USE_UV1","	attribute vec2 uv1;","#endif","#ifdef USE_UV2","	attribute vec2 uv2;","#endif","#ifdef USE_UV3","	attribute vec2 uv3;","#endif","#ifdef USE_TANGENT","	attribute vec4 tangent;","#endif","#if defined( USE_COLOR_ALPHA )","	attribute vec4 color;","#elif defined( USE_COLOR )","	attribute vec3 color;","#endif","#ifdef USE_SKINNING","	attribute vec4 skinIndex;","	attribute vec4 skinWeight;","#endif",`
`].filter(go).join(`
`),p=[wf(e),"#define SHADER_TYPE "+e.shaderType,"#define SHADER_NAME "+e.shaderName,f,e.useFog&&e.fog?"#define USE_FOG":"",e.useFog&&e.fogExp2?"#define FOG_EXP2":"",e.alphaToCoverage?"#define ALPHA_TO_COVERAGE":"",e.map?"#define USE_MAP":"",e.matcap?"#define USE_MATCAP":"",e.envMap?"#define USE_ENVMAP":"",e.envMap?"#define "+c:"",e.envMap?"#define "+h:"",e.envMap?"#define "+u:"",e.envMapPMREM?"#define ENVMAP_MAX_LOD "+e.envMapMaxLod+".0":"",e.envMapPMREM?"#define ENVMAP_SIZE "+e.envMapSize+".0":"",e.lightMap?"#define USE_LIGHTMAP":"",e.aoMap?"#define USE_AOMAP":"",e.bumpMap?"#define USE_BUMPMAP":"",e.normalMap?"#define USE_NORMALMAP":"",e.normalMapObjectSpace?"#define USE_NORMALMAP_OBJECTSPACE":"",e.normalMapTangentSpace?"#define USE_NORMALMAP_TANGENTSPACE":"",e.packedNormalMap?"#define USE_PACKED_NORMALMAP":"",e.emissiveMap?"#define USE_EMISSIVEMAP":"",e.anisotropy?"#define USE_ANISOTROPY":"",e.anisotropyMap?"#define USE_ANISOTROPYMAP":"",e.clearcoat?"#define USE_CLEARCOAT":"",e.clearcoatMap?"#define USE_CLEARCOATMAP":"",e.clearcoatRoughnessMap?"#define USE_CLEARCOAT_ROUGHNESSMAP":"",e.clearcoatNormalMap?"#define USE_CLEARCOAT_NORMALMAP":"",e.diffuseRoughness?"#define USE_DIFFUSE_ROUGHNESS":"",e.diffuseRoughnessMap?"#define USE_DIFFUSE_ROUGHNESSMAP":"",e.dispersion?"#define USE_DISPERSION":"",e.retroreflection?"#define USE_RETROREFLECTION":"",e.iridescence?"#define USE_IRIDESCENCE":"",e.iridescenceMap?"#define USE_IRIDESCENCEMAP":"",e.iridescenceThicknessMap?"#define USE_IRIDESCENCE_THICKNESSMAP":"",e.specularMap?"#define USE_SPECULARMAP":"",e.specularColorMap?"#define USE_SPECULAR_COLORMAP":"",e.specularIntensityMap?"#define USE_SPECULAR_INTENSITYMAP":"",e.roughnessMap?"#define USE_ROUGHNESSMAP":"",e.metalnessMap?"#define USE_METALNESSMAP":"",e.alphaMap?"#define USE_ALPHAMAP":"",e.alphaTest?"#define USE_ALPHATEST":"",e.alphaHash?"#define USE_ALPHAHASH":"",e.sheen?"#define USE_SHEEN":"",e.sheenColorMap?"#define USE_SHEEN_COLORMAP":"",e.sheenRoughnessMap?"#define USE_SHEEN_ROUGHNESSMAP":"",e.transmission?"#define USE_TRANSMISSION":"",e.transmissionMap?"#define USE_TRANSMISSIONMAP":"",e.thicknessMap?"#define USE_THICKNESSMAP":"",e.vertexTangents&&e.flatShading===!1?"#define USE_TANGENT":"",e.vertexColors||e.instancingColor?"#define USE_COLOR":"",e.vertexAlphas||e.batchingColor?"#define USE_COLOR_ALPHA":"",e.vertexUv1s?"#define USE_UV1":"",e.vertexUv2s?"#define USE_UV2":"",e.vertexUv3s?"#define USE_UV3":"",e.pointsUvs?"#define USE_POINTS_UV":"",e.gradientMap?"#define USE_GRADIENTMAP":"",e.flatShading?"#define FLAT_SHADED":"",e.doubleSided?"#define DOUBLE_SIDED":"",e.flipSided?"#define FLIP_SIDED":"",e.shadowMapEnabled?"#define USE_SHADOWMAP":"",e.shadowMapEnabled?"#define "+l:"",e.premultipliedAlpha?"#define PREMULTIPLIED_ALPHA":"",e.numLightProbes>0?"#define USE_LIGHT_PROBES":"",e.decodeVideoTexture?"#define DECODE_VIDEO_TEXTURE":"",e.decodeVideoTextureEmissive?"#define DECODE_VIDEO_TEXTURE_EMISSIVE":"",e.logarithmicDepthBuffer?"#define USE_LOGARITHMIC_DEPTH_BUFFER":"",e.reversedDepthBuffer?"#define USE_REVERSED_DEPTH_BUFFER":"","uniform mat4 viewMatrix;","uniform vec3 cameraPosition;","uniform bool isOrthographic;",e.toneMapping!==Yi?"#define TONE_MAPPING":"",e.toneMapping!==Yi?Gt.tonemapping_pars_fragment:"",e.toneMapping!==Yi?i_("toneMapping",e.toneMapping):"",e.dithering?"#define DITHERING":"",e.opaque?"#define OPAQUE":"",Gt.colorspace_pars_fragment,t_("linearToOutputTexel",e.outputColorSpace),n_(),e.useDepthPacking?"#define DEPTH_PACKING "+e.depthPacking:"",`
`].filter(go).join(`
`)),o=Hh(o),o=Mf(o,e),o=Sf(o,e),a=Hh(a),a=Mf(a,e),a=Sf(a,e),o=bf(o),a=bf(a),e.isRawShaderMaterial!==!0&&(g=`#version 300 es
`,x=[d,"#define attribute in","#define varying out","#define texture2D texture"].join(`
`)+`
`+x,p=["#define varying in",e.glslVersion===_n?"":"layout(location = 0) out highp vec4 pc_fragColor;",e.glslVersion===_n?"":"#define gl_FragColor pc_fragColor","#define gl_FragDepthEXT gl_FragDepth","#define texture2D texture","#define textureCube texture","#define texture2DProj textureProj","#define texture2DLodEXT textureLod","#define texture2DProjLodEXT textureProjLod","#define textureCubeLodEXT textureLod","#define texture2DGradEXT textureGrad","#define texture2DProjGradEXT textureProjGrad","#define textureCubeGradEXT textureGrad"].join(`
`)+`
`+p);let y=g+x+o,v=g+p+a,S=_f(s,s.VERTEX_SHADER,y),T=_f(s,s.FRAGMENT_SHADER,v);s.attachShader(m,S),s.attachShader(m,T),e.index0AttributeName!==void 0?s.bindAttribLocation(m,0,e.index0AttributeName):e.hasPositionAttribute===!0&&s.bindAttribLocation(m,0,"position"),s.linkProgram(m);function b(P){if(n.debug.checkShaderErrors){let A=s.getProgramInfoLog(m)||"",R=s.getShaderInfoLog(S)||"",U=s.getShaderInfoLog(T)||"",L=A.trim(),B=R.trim(),H=U.trim(),z=!0,Q=!0;if(s.getProgramParameter(m,s.LINK_STATUS)===!1)if(z=!1,typeof n.debug.onShaderError=="function")n.debug.onShaderError(s,m,S,T);else{let W=vf(s,S,"vertex"),Z=vf(s,T,"fragment");Ot("WebGLProgram: Shader Error "+s.getError()+" - VALIDATE_STATUS "+s.getProgramParameter(m,s.VALIDATE_STATUS)+`

Material Name: `+P.name+`
Material Type: `+P.type+`

Program Info Log: `+L+`
`+W+`
`+Z)}else L!==""?Lt("WebGLProgram: Program Info Log:",L):(B===""||H==="")&&(Q=!1);Q&&(P.diagnostics={runnable:z,programLog:L,vertexShader:{log:B,prefix:x},fragmentShader:{log:H,prefix:p}})}s.deleteShader(S),s.deleteShader(T),_=new or(s,m),M=o_(s,m)}let _;this.getUniforms=function(){return _===void 0&&b(this),_};let M;this.getAttributes=function(){return M===void 0&&b(this),M};let D=e.rendererExtensionParallelShaderCompile===!1;return this.isReady=function(){return D===!1&&(D=s.getProgramParameter(m,J1)),D},this.destroy=function(){i.releaseStatesOfProgram(this),s.deleteProgram(m),this.program=void 0},this.type=e.shaderType,this.name=e.shaderName,this.id=$1++,this.cacheKey=t,this.usedTimes=1,this.program=m,this.vertexShader=S,this.fragmentShader=T,this}var __=0,Vh=class{constructor(){this.shaderCache=new Map,this.materialCache=new Map}update(t,e,i){let s=this._getShaderCacheForMaterial(t);return s.has(e)===!1&&(s.add(e),e.usedTimes++),s.has(i)===!1&&(s.add(i),i.usedTimes++),this}remove(t){let e=this.materialCache.get(t);for(let i of e)i.usedTimes--,i.usedTimes===0&&this.shaderCache.delete(i.code);return this.materialCache.delete(t),this}getVertexShaderStage(t){return this._getShaderStage(t.vertexShader)}getFragmentShaderStage(t){return this._getShaderStage(t.fragmentShader)}dispose(){this.shaderCache.clear(),this.materialCache.clear()}_getShaderCacheForMaterial(t){let e=this.materialCache,i=e.get(t);return i===void 0&&(i=new Set,e.set(t,i)),i}_getShaderStage(t){let e=this.shaderCache,i=e.get(t);return i===void 0&&(i=new Gh(t),e.set(t,i)),i}},Gh=class{constructor(t){this.id=__++,this.code=t,this.usedTimes=0}};function y_(n){return n===kn||n===uo||n===fo}function v_(n,t,e,i,s,r){let o=new Hs,a=new Vh,l=new Set,c=[],h=new Map,u=i.logarithmicDepthBuffer,d={MeshDepthMaterial:"depth",MeshDistanceMaterial:"distance",MeshNormalMaterial:"normal",MeshBasicMaterial:"basic",MeshLambertMaterial:"lambert",MeshPhongMaterial:"phong",MeshToonMaterial:"toon",MeshStandardMaterial:"physical",MeshPhysicalMaterial:"physical",MeshMatcapMaterial:"matcap",LineBasicMaterial:"basic",LineDashedMaterial:"dashed",PointsMaterial:"points",ShadowMaterial:"shadow",SpriteMaterial:"sprite"};function f(_){return l.add(_),_===0?"uv":`uv${_}`}function m(_,M,D,P,A,R){let U=P.fog,L=A.geometry,B=_.isMeshStandardMaterial||_.isMeshLambertMaterial||_.isMeshPhongMaterial?P.environment:null,H=_.isMeshStandardMaterial||_.isMeshLambertMaterial&&!_.envMap||_.isMeshPhongMaterial&&!_.envMap,z=t.get(_.envMap||B,H),Q=d[_.type],W=i.precision;_.precision!==null&&(W=i.getMaxPrecision(_.precision),W!==_.precision&&Lt("WebGLProgram.getParameters:",_.precision,"not supported, using",W,"instead."));let Z=L.morphAttributes.position||L.morphAttributes.normal||L.morphAttributes.color,$=Z!==void 0?Z.length:0,xt=0;L.morphAttributes.position!==void 0&&(xt=1),L.morphAttributes.normal!==void 0&&(xt=2),L.morphAttributes.color!==void 0&&(xt=3);let Mt,q,st,_t;if(Q){let yt=Ji[Q];Mt=yt.vertexShader,q=yt.fragmentShader}else{Mt=_.vertexShader,q=_.fragmentShader;let yt=a.getVertexShaderStage(_),ae=a.getFragmentShaderStage(_);a.update(_,yt,ae),st=yt.id,_t=ae.id}let wt=n.getRenderTarget(),rt=n.state.buffers.depth.getReversed(),Pt=A.isInstancedMesh===!0,Qt=A.isBatchedMesh===!0,ot=!!_.map,Ct=!!_.matcap,Bt=!!z,Nt=Bt&&z.isPMREMTexture===!0,Kt=!!_.aoMap,Yt=!!_.lightMap,Se=!!_.bumpMap&&_.wireframe===!1,ie=!!_.normalMap,Re=!!_.displacementMap,_e=!!_.emissiveMap,Ee=!!_.metalnessMap,fi=!!_.roughnessMap,ri=_.anisotropy>0,O=_.clearcoat>0,oi=_.diffuseRoughness>0,ue=_.dispersion>0,de=_.retroreflectivity>0,dt=_.iridescence>0,Ne=_.sheen>0,Ft=_.transmission>0,kt=ri&&!!_.anisotropyMap,I=O&&!!_.clearcoatMap,w=O&&!!_.clearcoatNormalMap,V=O&&!!_.clearcoatRoughnessMap,it=oi&&!!_.diffuseRoughnessMap,tt=dt&&!!_.iridescenceMap,lt=dt&&!!_.iridescenceThicknessMap,mt=Ne&&!!_.sheenColorMap,K=Ne&&!!_.sheenRoughnessMap,J=!!_.specularMap,ht=!!_.specularColorMap,N=!!_.specularIntensityMap,j=Ft&&!!_.transmissionMap,Y=Ft&&!!_.thicknessMap,ut=!!_.gradientMap,at=!!_.alphaMap,ct=_.alphaTest>0,It=!!_.alphaHash,qt=!!_.extensions,nt=Yi;_.toneMapped&&(wt===null||wt.isXRRenderTarget===!0)&&(nt=n.toneMapping);let Dt=Nt?z.isCompressedCubeTexture?z.image[0].mipmaps:z.mipmaps:null,Wt={shaderID:Q,shaderType:_.type,shaderName:_.name,vertexShader:Mt,fragmentShader:q,defines:_.defines,customVertexShaderID:st,customFragmentShaderID:_t,isRawShaderMaterial:_.isRawShaderMaterial===!0,glslVersion:_.glslVersion,precision:W,batching:Qt,batchingColor:Qt&&A._colorsTexture!==null,instancing:Pt,instancingColor:Pt&&A.instanceColor!==null,instancingMorph:Pt&&A.morphTexture!==null,outputColorSpace:wt===null?n.outputColorSpace:wt.isXRRenderTarget===!0?wt.texture.colorSpace:Zt.workingColorSpace,alphaToCoverage:!!_.alphaToCoverage,map:ot,matcap:Ct,envMap:Bt,envMapMode:Bt&&z.mapping,envMapPMREM:Nt,envMapMaxLod:Nt?Dt.length-1:null,envMapSize:Nt?Dt[0].width:null,aoMap:Kt,lightMap:Yt,bumpMap:Se,normalMap:ie,displacementMap:Re,emissiveMap:_e,normalMapObjectSpace:ie&&_.normalMapType===Wd,normalMapTangentSpace:ie&&_.normalMapType===po,packedNormalMap:ie&&_.normalMapType===po&&y_(_.normalMap.format),metalnessMap:Ee,roughnessMap:fi,anisotropy:ri,anisotropyMap:kt,clearcoat:O,clearcoatMap:I,clearcoatNormalMap:w,clearcoatRoughnessMap:V,diffuseRoughness:oi,diffuseRoughnessMap:it,dispersion:ue,retroreflection:de,iridescence:dt,iridescenceMap:tt,iridescenceThicknessMap:lt,sheen:Ne,sheenColorMap:mt,sheenRoughnessMap:K,specularMap:J,specularColorMap:ht,specularIntensityMap:N,transmission:Ft,transmissionMap:j,thicknessMap:Y,gradientMap:ut,opaque:_.transparent===!1&&_.blending===Qs&&_.alphaToCoverage===!1,alphaMap:at,alphaTest:ct,alphaHash:It,combine:_.combine,mapUv:ot&&f(_.map.channel),aoMapUv:Kt&&f(_.aoMap.channel),lightMapUv:Yt&&f(_.lightMap.channel),bumpMapUv:Se&&f(_.bumpMap.channel),normalMapUv:ie&&f(_.normalMap.channel),displacementMapUv:Re&&f(_.displacementMap.channel),emissiveMapUv:_e&&f(_.emissiveMap.channel),metalnessMapUv:Ee&&f(_.metalnessMap.channel),roughnessMapUv:fi&&f(_.roughnessMap.channel),anisotropyMapUv:kt&&f(_.anisotropyMap.channel),clearcoatMapUv:I&&f(_.clearcoatMap.channel),clearcoatNormalMapUv:w&&f(_.clearcoatNormalMap.channel),clearcoatRoughnessMapUv:V&&f(_.clearcoatRoughnessMap.channel),diffuseRoughnessMapUv:it&&f(_.diffuseRoughnessMap.channel),iridescenceMapUv:tt&&f(_.iridescenceMap.channel),iridescenceThicknessMapUv:lt&&f(_.iridescenceThicknessMap.channel),sheenColorMapUv:mt&&f(_.sheenColorMap.channel),sheenRoughnessMapUv:K&&f(_.sheenRoughnessMap.channel),specularMapUv:J&&f(_.specularMap.channel),specularColorMapUv:ht&&f(_.specularColorMap.channel),specularIntensityMapUv:N&&f(_.specularIntensityMap.channel),transmissionMapUv:j&&f(_.transmissionMap.channel),thicknessMapUv:Y&&f(_.thicknessMap.channel),alphaMapUv:at&&f(_.alphaMap.channel),vertexTangents:!!L.attributes.tangent&&(ie||ri),vertexNormals:!!L.attributes.normal,vertexColors:_.vertexColors,vertexAlphas:_.vertexColors===!0&&!!L.attributes.color&&L.attributes.color.itemSize===4,pointsUvs:A.isPoints===!0&&!!L.attributes.uv&&(ot||at),fog:!!U,useFog:_.fog===!0,fogExp2:!!U&&U.isFogExp2,flatShading:_.wireframe===!1&&(_.flatShading===!0||L.attributes.normal===void 0&&ie===!1&&(_.isMeshLambertMaterial||_.isMeshPhongMaterial||_.isMeshStandardMaterial||_.isMeshPhysicalMaterial)),sizeAttenuation:_.sizeAttenuation===!0,logarithmicDepthBuffer:u,reversedDepthBuffer:rt,skinning:A.isSkinnedMesh===!0,hasPositionAttribute:L.attributes.position!==void 0,morphTargets:L.morphAttributes.position!==void 0,morphNormals:L.morphAttributes.normal!==void 0,morphColors:L.morphAttributes.color!==void 0,morphTargetsCount:$,morphTextureStride:xt,numSunLights:M.sun.length,numDirLights:M.directional.length,numPointLights:M.point.length,numSpotLights:M.spot.length,numSpotLightMaps:M.spotLightMap.length,numRectAreaLights:M.rectArea.length,numHemiLights:M.hemi.length,numSunLightShadows:M.sunShadowMap.length,numDirLightShadows:M.directionalShadowMap.length,numPointLightShadows:M.pointShadowMap.length,numSpotLightShadows:M.spotShadowMap.length,numSpotLightShadowsWithMaps:M.numSpotLightShadowsWithMaps,numLightProbes:M.numLightProbes,numLightProbeGrids:R.filter(yt=>yt.texture!==null).length,numClippingPlanes:r.numPlanes,numClipIntersection:r.numIntersection,dithering:_.dithering,shadowMapEnabled:n.shadowMap.enabled&&D.length>0,shadowMapType:n.shadowMap.type,toneMapping:nt,decodeVideoTexture:ot&&_.map.isVideoTexture===!0&&Zt.getTransfer(_.map.colorSpace)===ne,decodeVideoTextureEmissive:_e&&_.emissiveMap.isVideoTexture===!0&&Zt.getTransfer(_.emissiveMap.colorSpace)===ne,premultipliedAlpha:_.premultipliedAlpha,doubleSided:_.side===Ue,flipSided:_.side===Ze,useDepthPacking:_.depthPacking>=0,depthPacking:_.depthPacking||0,index0AttributeName:_.index0AttributeName,extensionClipCullDistance:qt&&_.extensions.clipCullDistance===!0&&e.has("WEBGL_clip_cull_distance"),extensionMultiDraw:(qt&&_.extensions.multiDraw===!0||Qt)&&e.has("WEBGL_multi_draw"),rendererExtensionParallelShaderCompile:e.has("KHR_parallel_shader_compile"),customProgramCacheKey:_.customProgramCacheKey()};return Wt.vertexUv1s=l.has(1),Wt.vertexUv2s=l.has(2),Wt.vertexUv3s=l.has(3),l.clear(),Wt}function x(_){let M=[];if(_.shaderID?M.push(_.shaderID):(M.push(_.customVertexShaderID),M.push(_.customFragmentShaderID)),_.defines!==void 0)for(let D in _.defines)M.push(D),M.push(_.defines[D]);return _.isRawShaderMaterial===!1&&(p(M,_),g(M,_),M.push(n.outputColorSpace)),M.push(_.customProgramCacheKey),M.join()}function p(_,M){_.push(M.precision),_.push(M.outputColorSpace),_.push(M.envMapMode),_.push(M.envMapPMREM),_.push(M.envMapMaxLod),_.push(M.envMapSize),_.push(M.mapUv),_.push(M.alphaMapUv),_.push(M.lightMapUv),_.push(M.aoMapUv),_.push(M.bumpMapUv),_.push(M.normalMapUv),_.push(M.displacementMapUv),_.push(M.emissiveMapUv),_.push(M.metalnessMapUv),_.push(M.roughnessMapUv),_.push(M.anisotropyMapUv),_.push(M.clearcoatMapUv),_.push(M.clearcoatNormalMapUv),_.push(M.clearcoatRoughnessMapUv),_.push(M.diffuseRoughnessMapUv),_.push(M.iridescenceMapUv),_.push(M.iridescenceThicknessMapUv),_.push(M.sheenColorMapUv),_.push(M.sheenRoughnessMapUv),_.push(M.specularMapUv),_.push(M.specularColorMapUv),_.push(M.specularIntensityMapUv),_.push(M.transmissionMapUv),_.push(M.thicknessMapUv),_.push(M.combine),_.push(M.fogExp2),_.push(M.sizeAttenuation),_.push(M.morphTargetsCount),_.push(M.numSunLights),_.push(M.numDirLights),_.push(M.numPointLights),_.push(M.numSpotLights),_.push(M.numSpotLightMaps),_.push(M.numHemiLights),_.push(M.numRectAreaLights),_.push(M.numSunLightShadows),_.push(M.numDirLightShadows),_.push(M.numPointLightShadows),_.push(M.numSpotLightShadows),_.push(M.numSpotLightShadowsWithMaps),_.push(M.numLightProbes),_.push(M.numLightProbeGrids),_.push(M.shadowMapType),_.push(M.toneMapping),_.push(M.numClippingPlanes),_.push(M.numClipIntersection),_.push(M.depthPacking)}function g(_,M){o.disableAll(),M.instancing&&o.enable(0),M.instancingColor&&o.enable(1),M.instancingMorph&&o.enable(2),M.matcap&&o.enable(3),M.envMap&&o.enable(4),M.normalMapObjectSpace&&o.enable(5),M.normalMapTangentSpace&&o.enable(6),M.clearcoat&&o.enable(7),M.iridescence&&o.enable(8),M.alphaTest&&o.enable(9),M.vertexColors&&o.enable(10),M.vertexAlphas&&o.enable(11),M.vertexUv1s&&o.enable(12),M.vertexUv2s&&o.enable(13),M.vertexUv3s&&o.enable(14),M.vertexTangents&&o.enable(15),M.anisotropy&&o.enable(16),M.alphaHash&&o.enable(17),M.batching&&o.enable(18),M.dispersion&&o.enable(19),M.retroreflection&&o.enable(24),M.diffuseRoughness&&o.enable(25),M.batchingColor&&o.enable(20),M.gradientMap&&o.enable(21),M.packedNormalMap&&o.enable(22),M.vertexNormals&&o.enable(23),_.push(o.mask),o.disableAll(),M.fog&&o.enable(0),M.useFog&&o.enable(1),M.flatShading&&o.enable(2),M.logarithmicDepthBuffer&&o.enable(3),M.reversedDepthBuffer&&o.enable(4),M.skinning&&o.enable(5),M.morphTargets&&o.enable(6),M.morphNormals&&o.enable(7),M.morphColors&&o.enable(8),M.premultipliedAlpha&&o.enable(9),M.shadowMapEnabled&&o.enable(10),M.doubleSided&&o.enable(11),M.flipSided&&o.enable(12),M.useDepthPacking&&o.enable(13),M.dithering&&o.enable(14),M.transmission&&o.enable(15),M.sheen&&o.enable(16),M.opaque&&o.enable(17),M.pointsUvs&&o.enable(18),M.decodeVideoTexture&&o.enable(19),M.decodeVideoTextureEmissive&&o.enable(20),M.alphaToCoverage&&o.enable(21),M.numLightProbeGrids>0&&o.enable(22),M.hasPositionAttribute&&o.enable(23),_.push(o.mask)}function y(_){let M=d[_.type],D;if(M){let P=Ji[M];D=Hi.clone(P.uniforms)}else D=_.uniforms;return D}function v(_,M){let D=h.get(M);return D!==void 0?++D.usedTimes:(D=new x_(n,M,_,s),c.push(D),h.set(M,D)),D}function S(_){if(--_.usedTimes===0){let M=c.indexOf(_);c[M]=c[c.length-1],c.pop(),h.delete(_.cacheKey),_.destroy()}}function T(_){a.remove(_)}function b(){a.dispose()}return{getParameters:m,getProgramCacheKey:x,getUniforms:y,acquireProgram:v,releaseProgram:S,releaseShaderCache:T,programs:c,dispose:b}}function M_(){let n=new WeakMap;function t(o){return n.has(o)}function e(o){let a=n.get(o);return a===void 0&&(a={},n.set(o,a)),a}function i(o){n.delete(o)}function s(o,a,l){n.get(o)[a]=l}function r(){n=new WeakMap}return{has:t,get:e,remove:i,update:s,dispose:r}}function S_(n,t){return n.groupOrder!==t.groupOrder?n.groupOrder-t.groupOrder:n.renderOrder!==t.renderOrder?n.renderOrder-t.renderOrder:n.material.id!==t.material.id?n.material.id-t.material.id:n.materialVariant!==t.materialVariant?n.materialVariant-t.materialVariant:n.z!==t.z?n.z-t.z:n.id-t.id}function Ef(n,t){return n.groupOrder!==t.groupOrder?n.groupOrder-t.groupOrder:n.renderOrder!==t.renderOrder?n.renderOrder-t.renderOrder:n.z!==t.z?t.z-n.z:n.id-t.id}function Tf(){let n=[],t=0,e=[],i=[],s=[];function r(){t=0,e.length=0,i.length=0,s.length=0}function o(d){let f=0;return d.isInstancedMesh&&(f+=2),d.isSkinnedMesh&&(f+=1),f}function a(d,f,m,x,p,g){let y=n[t];return y===void 0?(y={id:d.id,object:d,geometry:f,material:m,materialVariant:o(d),groupOrder:x,renderOrder:d.renderOrder,z:p,group:g},n[t]=y):(y.id=d.id,y.object=d,y.geometry=f,y.material=m,y.materialVariant=o(d),y.groupOrder=x,y.renderOrder=d.renderOrder,y.z=p,y.group=g),t++,y}function l(d,f,m,x,p,g,y){y.reversedDepth===!0&&(p=-p);let v=a(d,f,m,x,p,g);m.transmission>0?i.push(v):m.transparent===!0?s.push(v):e.push(v)}function c(d,f,m,x,p,g){let y=a(d,f,m,x,p,g);m.transmission>0?i.unshift(y):m.transparent===!0?s.unshift(y):e.unshift(y)}function h(d,f){e.length>1&&e.sort(d||S_),i.length>1&&i.sort(f||Ef),s.length>1&&s.sort(f||Ef)}function u(){for(let d=t,f=n.length;d<f;d++){let m=n[d];if(m.id===null)break;m.id=null,m.object=null,m.geometry=null,m.material=null,m.group=null}}return{opaque:e,transmissive:i,transparent:s,init:r,push:l,unshift:c,finish:u,sort:h}}function b_(){let n=new WeakMap;function t(i,s){let r=n.get(i),o;return r===void 0?(o=new Tf,n.set(i,[o])):s>=r.length?(o=new Tf,r.push(o)):o=r[s],o}function e(){n=new WeakMap}return{get:t,dispose:e}}function w_(){let n={};return{get:function(t){if(n[t.id]!==void 0)return n[t.id];let e;switch(t.type){case"SunLight":case"DirectionalLight":e={direction:new C,color:new bt};break;case"SpotLight":e={position:new C,direction:new C,color:new bt,distance:0,coneCos:0,penumbraCos:0,decay:0};break;case"PointLight":e={position:new C,color:new bt,distance:0,decay:0};break;case"HemisphereLight":e={direction:new C,skyColor:new bt,groundColor:new bt};break;case"RectAreaLight":e={color:new bt,position:new C,halfWidth:new C,halfHeight:new C};break}return n[t.id]=e,e}}}function E_(){let n={};return{get:function(t){if(n[t.id]!==void 0)return n[t.id];let e;switch(t.type){case"SunLight":case"DirectionalLight":e={shadowIntensity:1,shadowBias:0,shadowNormalBias:0,shadowRadius:1,shadowMapSize:new et};break;case"SpotLight":e={shadowIntensity:1,shadowBias:0,shadowNormalBias:0,shadowRadius:1,shadowMapSize:new et};break;case"PointLight":e={shadowIntensity:1,shadowBias:0,shadowNormalBias:0,shadowRadius:1,shadowMapSize:new et,shadowCameraNear:1,shadowCameraFar:1e3};break}return n[t.id]=e,e}}}var T_=0;function A_(n,t){return(t.castShadow?2:0)-(n.castShadow?2:0)+(t.map?1:0)-(n.map?1:0)}function R_(n){let t=new w_,e=E_(),i={version:0,hash:{sunLength:-1,directionalLength:-1,pointLength:-1,spotLength:-1,rectAreaLength:-1,hemiLength:-1,numSunShadows:-1,numDirectionalShadows:-1,numPointShadows:-1,numSpotShadows:-1,numSpotMaps:-1,numLightProbes:-1},ambient:[0,0,0],probe:[],sun:[],sunShadow:[],sunShadowMap:[],sunShadowMatrix:[],sunShadowCascade:[],directional:[],directionalShadow:[],directionalShadowMap:[],directionalShadowMatrix:[],spot:[],spotLightMap:[],spotShadow:[],spotShadowMap:[],spotLightMatrix:[],rectArea:[],rectAreaLTC1:null,rectAreaLTC2:null,point:[],pointShadow:[],pointShadowMap:[],pointShadowMatrix:[],hemi:[],numSpotLightShadowsWithMaps:0,numLightProbes:0};for(let c=0;c<9;c++)i.probe.push(new C);let s=new C,r=new Xt,o=new Xt;function a(c){let h=0,u=0,d=0;for(let R=0;R<9;R++)i.probe[R].set(0,0,0);let f=0,m=0,x=0,p=0,g=0,y=0,v=0,S=0,T=0,b=0,_=0,M=0,D=0,P=0;c.sort(A_);for(let R=0,U=c.length;R<U;R++){let L=c[R],B=L.color,H=L.intensity,z=L.distance,Q=null;if(L.shadow&&L.shadow.map&&(L.shadow.map.texture.format===kn?Q=L.shadow.map.texture:Q=L.shadow.map.depthTexture||L.shadow.map.texture),L.isAmbientLight)h+=B.r*H,u+=B.g*H,d+=B.b*H;else if(L.isLightProbe){for(let W=0;W<9;W++)i.probe[W].addScaledVector(L.sh.coefficients[W],H);P++}else if(L.isSunLight){let W=t.get(L);if(W.color.copy(L.color).multiplyScalar(L.intensity),L.castShadow){let Z=L.shadow,$=e.get(L);$.shadowIntensity=Z.intensity,$.shadowBias=Z.bias,$.shadowNormalBias=Z.normalBias,$.shadowRadius=Z.radius,$.shadowMapSize.copy(Z.mapSize).multiply(Z.getFrameExtents()),i.sunShadow[m]=$,i.sunShadowMap[m]=Q;let xt=Z.getViewportCount();for(let Mt=0;Mt<xt;Mt++)i.sunShadowMatrix[x+Mt]=Z.getMatrix(Mt),i.sunShadowCascade[x+Mt]=Z._cascadeData[Mt];x+=xt,m++}i.sun[f]=W,f++}else if(L.isDirectionalLight){let W=t.get(L);if(W.color.copy(L.color).multiplyScalar(L.intensity),L.castShadow){let Z=L.shadow,$=e.get(L);$.shadowIntensity=Z.intensity,$.shadowBias=Z.bias,$.shadowNormalBias=Z.normalBias,$.shadowRadius=Z.radius,$.shadowMapSize=Z.mapSize,i.directionalShadow[p]=$,i.directionalShadowMap[p]=Q,i.directionalShadowMatrix[p]=L.shadow.matrix,T++}i.directional[p]=W,p++}else if(L.isSpotLight){let W=t.get(L);W.position.setFromMatrixPosition(L.matrixWorld),W.color.copy(B).multiplyScalar(H),W.distance=z,W.coneCos=Math.cos(L.angle),W.penumbraCos=Math.cos(L.angle*(1-L.penumbra)),W.decay=L.decay,i.spot[y]=W;let Z=L.shadow;if(L.map&&(i.spotLightMap[M]=L.map,M++,Z.updateMatrices(L),L.castShadow&&D++),i.spotLightMatrix[y]=Z.matrix,L.castShadow){let $=e.get(L);$.shadowIntensity=Z.intensity,$.shadowBias=Z.bias,$.shadowNormalBias=Z.normalBias,$.shadowRadius=Z.radius,$.shadowMapSize=Z.mapSize,i.spotShadow[y]=$,i.spotShadowMap[y]=Q,_++}y++}else if(L.isRectAreaLight){let W=t.get(L);W.color.copy(B).multiplyScalar(H),W.halfWidth.set(L.width*.5,0,0),W.halfHeight.set(0,L.height*.5,0),i.rectArea[v]=W,v++}else if(L.isPointLight){let W=t.get(L);if(W.color.copy(L.color).multiplyScalar(L.intensity),W.distance=L.distance,W.decay=L.decay,L.castShadow){let Z=L.shadow,$=e.get(L);$.shadowIntensity=Z.intensity,$.shadowBias=Z.bias,$.shadowNormalBias=Z.normalBias,$.shadowRadius=Z.radius,$.shadowMapSize=Z.mapSize,$.shadowCameraNear=Z.camera.near,$.shadowCameraFar=Z.camera.far,i.pointShadow[g]=$,i.pointShadowMap[g]=Q,i.pointShadowMatrix[g]=L.shadow.matrix,b++}i.point[g]=W,g++}else if(L.isHemisphereLight){let W=t.get(L);W.skyColor.copy(L.color).multiplyScalar(H),W.groundColor.copy(L.groundColor).multiplyScalar(H),i.hemi[S]=W,S++}}v>0&&(n.has("OES_texture_float_linear")===!0?(i.rectAreaLTC1=vt.LTC_FLOAT_1,i.rectAreaLTC2=vt.LTC_FLOAT_2):(i.rectAreaLTC1=vt.LTC_HALF_1,i.rectAreaLTC2=vt.LTC_HALF_2)),i.ambient[0]=h,i.ambient[1]=u,i.ambient[2]=d;let A=i.hash;(A.sunLength!==f||A.directionalLength!==p||A.pointLength!==g||A.spotLength!==y||A.rectAreaLength!==v||A.hemiLength!==S||A.numSunShadows!==m||A.numDirectionalShadows!==T||A.numPointShadows!==b||A.numSpotShadows!==_||A.numSpotMaps!==M||A.numLightProbes!==P)&&(i.sun.length=f,i.directional.length=p,i.spot.length=y,i.rectArea.length=v,i.point.length=g,i.hemi.length=S,i.sunShadow.length=m,i.sunShadowMap.length=m,i.sunShadowMatrix.length=x,i.sunShadowCascade.length=x,i.directionalShadow.length=T,i.directionalShadowMap.length=T,i.directionalShadowMatrix.length=T,i.pointShadow.length=b,i.pointShadowMap.length=b,i.pointShadowMatrix.length=b,i.spotShadow.length=_,i.spotShadowMap.length=_,i.spotLightMatrix.length=_+M-D,i.spotLightMap.length=M,i.numSpotLightShadowsWithMaps=D,i.numLightProbes=P,A.sunLength=f,A.directionalLength=p,A.pointLength=g,A.spotLength=y,A.rectAreaLength=v,A.hemiLength=S,A.numSunShadows=m,A.numDirectionalShadows=T,A.numPointShadows=b,A.numSpotShadows=_,A.numSpotMaps=M,A.numLightProbes=P,i.version=T_++)}function l(c,h){let u=0,d=0,f=0,m=0,x=0,p=0,g=h.matrixWorldInverse;for(let y=0,v=c.length;y<v;y++){let S=c[y];if(S.isSunLight){let T=i.sun[u];T.direction.setFromMatrixPosition(S.matrixWorld),T.direction.transformDirection(g),u++}else if(S.isDirectionalLight){let T=i.directional[d];T.direction.setFromMatrixPosition(S.matrixWorld),s.setFromMatrixPosition(S.target.matrixWorld),T.direction.sub(s),T.direction.transformDirection(g),d++}else if(S.isSpotLight){let T=i.spot[m];T.position.setFromMatrixPosition(S.matrixWorld),T.position.applyMatrix4(g),T.direction.setFromMatrixPosition(S.matrixWorld),s.setFromMatrixPosition(S.target.matrixWorld),T.direction.sub(s),T.direction.transformDirection(g),m++}else if(S.isRectAreaLight){let T=i.rectArea[x];T.position.setFromMatrixPosition(S.matrixWorld),T.position.applyMatrix4(g),o.identity(),r.copy(S.matrixWorld),r.premultiply(g),o.extractRotation(r),T.halfWidth.set(S.width*.5,0,0),T.halfHeight.set(0,S.height*.5,0),T.halfWidth.applyMatrix4(o),T.halfHeight.applyMatrix4(o),x++}else if(S.isPointLight){let T=i.point[f];T.position.setFromMatrixPosition(S.matrixWorld),T.position.applyMatrix4(g),f++}else if(S.isHemisphereLight){let T=i.hemi[p];T.direction.setFromMatrixPosition(S.matrixWorld),T.direction.transformDirection(g),p++}}}return{setup:a,setupView:l,state:i}}function Af(n){let t=new R_(n),e=[],i=[],s=[];function r(u){h.camera=u,e.length=0,i.length=0,s.length=0}function o(u){u.isLightProbeGrid?s.push(u):e.push(u)}function a(u){i.push(u)}function l(){t.setup(e),s.sort((u,d)=>u.priority-d.priority||u.id-d.id)}function c(u){t.setupView(e,u)}let h={lightsArray:e,shadowsArray:i,lightProbeGridArray:s,camera:null,lights:t,transmissionRenderTarget:{},textureUnits:0};return{init:r,state:h,setupLights:l,setupLightsView:c,pushLight:o,pushShadow:a}}function C_(n){let t=new WeakMap;function e(s,r=0){let o=t.get(s),a;return o===void 0?(a=new Af(n),t.set(s,[a])):r>=o.length?(a=new Af(n),o.push(a)):a=o[r],a}function i(){t=new WeakMap}return{get:e,dispose:i}}var P_=`void main() {
	gl_Position = vec4( position, 1.0 );
}`,I_=`uniform sampler2D shadow_pass;
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
}`,D_=[new C(1,0,0),new C(-1,0,0),new C(0,1,0),new C(0,-1,0),new C(0,0,1),new C(0,0,-1)],L_=[new C(0,-1,0),new C(0,-1,0),new C(0,0,1),new C(0,0,-1),new C(0,-1,0),new C(0,-1,0)],Rf=new Xt,mo=new C,Oh=new C;function N_(n,t,e){let i=new Dn,s=new et,r=new et,o=new oe,a=new Ua,l=new Fa,c={},h=e.maxTextureSize,u={[Bn]:Ze,[Ze]:Bn,[Ue]:Ue},d=new fe({defines:{VSM_SAMPLES:8},uniforms:{shadow_pass:{value:null},resolution:{value:new et},radius:{value:4}},vertexShader:P_,fragmentShader:I_}),f=d.clone();f.defines.HORIZONTAL_PASS=1;let m=new Jt;m.setAttribute("position",new le(new Float32Array([-1,-1,.5,3,-1,.5,-1,3,.5]),3));let x=new Ut(m,d),p=this;this.enabled=!1,this.autoUpdate=!0,this.needsUpdate=!1,this.type=ls;let g=this.type;this.render=function(b,_,M){if(p.enabled===!1||p.autoUpdate===!1&&p.needsUpdate===!1||b.length===0)return;this.type===wd&&(Lt("WebGLShadowMap: PCFSoftShadowMap has been removed. Using PCFShadowMap instead."),this.type=ls);let D=n.getRenderTarget(),P=n.getActiveCubeFace(),A=n.getActiveMipmapLevel(),R=n.state;R.setBlending(wi),R.buffers.depth.getReversed()===!0?R.buffers.color.setClear(0,0,0,0):R.buffers.color.setClear(1,1,1,1),R.buffers.depth.setTest(!0),R.setScissorTest(!1);let U=g!==this.type;U&&_.traverse(function(L){L.material&&(Array.isArray(L.material)?L.material.forEach(B=>B.needsUpdate=!0):L.material.needsUpdate=!0)});for(let L=0,B=b.length;L<B;L++){let H=b[L],z=H.shadow;if(z===void 0){Lt("WebGLShadowMap:",H,"has no shadow.");continue}if(z.autoUpdate===!1&&z.needsUpdate===!1)continue;s.copy(z.mapSize);let Q=z.getFrameExtents();s.multiply(Q),r.copy(z.mapSize),(s.x>h||s.y>h)&&(s.x>h&&(r.x=Math.floor(h/Q.x),s.x=r.x*Q.x,z.mapSize.x=r.x),s.y>h&&(r.y=Math.floor(h/Q.y),s.y=r.y*Q.y,z.mapSize.y=r.y));let W=n.state.buffers.depth.getReversed();if(z.camera._reversedDepth=W,z.map===null||U===!0){if(z.map!==null&&(z.map.depthTexture!==null&&(z.map.depthTexture.dispose(),z.map.depthTexture=null),z.map.dispose()),this.type===js){if(H.isPointLight){Lt("WebGLShadowMap: VSM shadow maps are not supported for PointLights. Use PCF or BasicShadowMap instead.");continue}z.map=new me(s.x,s.y,{format:kn,type:xe,minFilter:ce,magFilter:ce,generateMipmaps:!1}),z.map.texture.name=H.name+".shadowMap",z.map.depthTexture=new Oi(s.x,s.y,gi),z.map.depthTexture.name=H.name+".shadowMapDepth",z.map.depthTexture.format=Li,z.map.depthTexture.compareFunction=null,z.map.depthTexture.minFilter=De,z.map.depthTexture.magFilter=De}else H.isPointLight?(z.map=new xo(s.x),z.map.depthTexture=new Br(s.x,hi)):(z.map=new me(s.x,s.y),z.map.depthTexture=new Oi(s.x,s.y,hi)),z.map.depthTexture.name=H.name+".shadowMap",z.map.depthTexture.format=Li,this.type===ls?(z.map.depthTexture.compareFunction=W?Eh:nr,z.map.depthTexture.minFilter=ce,z.map.depthTexture.magFilter=ce):(z.map.depthTexture.compareFunction=null,z.map.depthTexture.minFilter=De,z.map.depthTexture.magFilter=De);z.camera.updateProjectionMatrix()}z.map.isWebGLCubeRenderTarget!==!0&&(z.map.width!==s.x||z.map.height!==s.y)&&z.map.setSize(s.x,s.y);let Z=z.map.isWebGLCubeRenderTarget?6:z.getViewportCount();H.isPointLight!==!0&&z.updateMatrices(H,M);let $=(z.camera.layers.mask&4294967294)!==0?z.camera.layers:M.layers;for(let xt=0;xt<Z;xt++){let Mt=z.getCamera(xt);if(H.isPointLight){let q=z.camera,st=z.matrix,_t=H.distance||q.far;_t!==q.far&&(q.far=_t,q.updateProjectionMatrix()),mo.setFromMatrixPosition(H.matrixWorld),q.position.copy(mo),Oh.copy(q.position),Oh.add(D_[xt]),q.up.copy(L_[xt]),q.lookAt(Oh),q.updateMatrixWorld(),st.makeTranslation(-mo.x,-mo.y,-mo.z),Rf.multiplyMatrices(q.projectionMatrix,q.matrixWorldInverse),z._frustum.setFromProjectionMatrix(Rf,q.coordinateSystem,q.reversedDepth)}if(z.map.isWebGLCubeRenderTarget)n.setRenderTarget(z.map,xt),n.clear();else{xt===0&&(n.setRenderTarget(z.map),n.clear());let q=z.getViewport(xt);o.set(r.x*q.x,r.y*q.y,r.x*q.z,r.y*q.w),R.viewport(o)}i=z.getFrustum(xt),S(_,M,Mt,H,$,this.type)}z.isPointLightShadow!==!0&&this.type===js&&y(z,M),z.needsUpdate=!1}g=this.type,p.needsUpdate=!1,n.setRenderTarget(D,P,A)};function y(b,_){let M=t.update(x);d.defines.VSM_SAMPLES!==b.blurSamples&&(d.defines.VSM_SAMPLES=b.blurSamples,f.defines.VSM_SAMPLES=b.blurSamples,d.needsUpdate=!0,f.needsUpdate=!0),b.mapPass===null?b.mapPass=new me(s.x,s.y,{format:kn,type:xe}):(b.mapPass.width!==b.map.width||b.mapPass.height!==b.map.height)&&b.mapPass.setSize(b.map.width,b.map.height),d.uniforms.shadow_pass.value=b.map.depthTexture,d.uniforms.resolution.value.set(b.map.width,b.map.height),d.uniforms.radius.value=b.radius,n.setRenderTarget(b.mapPass),n.clear(),n.renderBufferDirect(_,null,M,d,x,null),f.uniforms.shadow_pass.value=b.mapPass.texture,f.uniforms.resolution.value.set(b.map.width,b.map.height),f.uniforms.radius.value=b.radius,n.setRenderTarget(b.map),n.clear(),n.renderBufferDirect(_,null,M,f,x,null)}function v(b,_,M,D){let P=null,A=M.isPointLight===!0?b.customDistanceMaterial:b.customDepthMaterial;if(A!==void 0)P=A;else if(P=M.isPointLight===!0?l:a,n.localClippingEnabled&&_.clipShadows===!0&&Array.isArray(_.clippingPlanes)&&_.clippingPlanes.length!==0||_.displacementMap&&_.displacementScale!==0||_.alphaMap&&_.alphaTest>0||_.map&&_.alphaTest>0||_.alphaToCoverage===!0){let R=P.uuid,U=_.uuid,L=c[R];L===void 0&&(L={},c[R]=L);let B=L[U];B===void 0&&(B=P.clone(),L[U]=B,_.addEventListener("dispose",T)),P=B}if(P.visible=_.visible,P.wireframe=_.wireframe,D===js?P.side=_.shadowSide!==null?_.shadowSide:_.side:P.side=_.shadowSide!==null?_.shadowSide:u[_.side],P.alphaMap=_.alphaMap,P.alphaTest=_.alphaToCoverage===!0?.5:_.alphaTest,P.map=_.map,P.clipShadows=_.clipShadows,P.clippingPlanes=_.clippingPlanes,P.clipIntersection=_.clipIntersection,P.displacementMap=_.displacementMap,P.displacementScale=_.displacementScale,P.displacementBias=_.displacementBias,P.wireframeLinewidth=_.wireframeLinewidth,P.linewidth=_.linewidth,M.isPointLight===!0&&P.isMeshDistanceMaterial===!0){let R=n.properties.get(P);R.light=M}return P}function S(b,_,M,D,P,A){if(b.visible===!1)return;if(b.layers.test(P)&&(b.isMesh||b.isLine||b.isPoints)&&(b.castShadow||b.receiveShadow&&A===js)&&(!b.frustumCulled||b.intersectsFrustum(i))){b.modelViewMatrix.multiplyMatrices(M.matrixWorldInverse,b.matrixWorld);let L=t.update(b),B=b.material;if(Array.isArray(B)){let H=L.groups;for(let z=0,Q=H.length;z<Q;z++){let W=H[z],Z=B[W.materialIndex];if(Z&&Z.visible){let $=v(b,Z,D,A);b.onBeforeShadow(n,b,_,M,L,$,W),n.renderBufferDirect(M,null,L,$,b,W),b.onAfterShadow(n,b,_,M,L,$,W)}}}else if(B.visible){let H=v(b,B,D,A);b.onBeforeShadow(n,b,_,M,L,H,null),n.renderBufferDirect(M,null,L,H,b,null),b.onAfterShadow(n,b,_,M,L,H,null)}}let U=b.children;for(let L=0,B=U.length;L<B;L++)S(U[L],_,M,D,P,A)}function T(b){b.target.removeEventListener("dispose",T);for(let M in c){let D=c[M],P=b.target.uuid;P in D&&(D[P].dispose(),delete D[P])}}}function U_(n,t){function e(){let N=!1,j=new oe,Y=null,ut=new oe(0,0,0,0);return{setMask:function(at){Y!==at&&!N&&(n.colorMask(at,at,at,at),Y=at)},setLocked:function(at){N=at},setClear:function(at,ct,It,qt,nt){nt===!0&&(at*=qt,ct*=qt,It*=qt),j.set(at,ct,It,qt),ut.equals(j)===!1&&(n.clearColor(at,ct,It,qt),ut.copy(j))},reset:function(){N=!1,Y=null,ut.set(-1,0,0,0)}}}function i(){let N=!1,j=!1,Y=null,ut=null,at=null;return{setReversed:function(ct){if(j!==ct){let It=t.get("EXT_clip_control");ct?It.clipControlEXT(It.LOWER_LEFT_EXT,It.ZERO_TO_ONE_EXT):It.clipControlEXT(It.LOWER_LEFT_EXT,It.NEGATIVE_ONE_TO_ONE_EXT),j=ct;let qt=at;at=null,this.setClear(qt)}},getReversed:function(){return j},setTest:function(ct){ct?rt(n.DEPTH_TEST):Pt(n.DEPTH_TEST)},setMask:function(ct){Y!==ct&&!N&&(n.depthMask(ct),Y=ct)},setFunc:function(ct){if(j&&(ct=ef[ct]),ut!==ct){switch(ct){case ma:n.depthFunc(n.NEVER);break;case ga:n.depthFunc(n.ALWAYS);break;case xa:n.depthFunc(n.LESS);break;case Fs:n.depthFunc(n.LEQUAL);break;case _a:n.depthFunc(n.EQUAL);break;case ya:n.depthFunc(n.GEQUAL);break;case va:n.depthFunc(n.GREATER);break;case Ma:n.depthFunc(n.NOTEQUAL);break;default:n.depthFunc(n.LEQUAL)}ut=ct}},setLocked:function(ct){N=ct},setClear:function(ct){at!==ct&&(at=ct,j&&(ct=1-ct),n.clearDepth(ct))},reset:function(){N=!1,Y=null,ut=null,at=null,j=!1}}}function s(){let N=!1,j=null,Y=null,ut=null,at=null,ct=null,It=null,qt=null,nt=null;return{setTest:function(Dt){N||(Dt?rt(n.STENCIL_TEST):Pt(n.STENCIL_TEST))},setMask:function(Dt){j!==Dt&&!N&&(n.stencilMask(Dt),j=Dt)},setFunc:function(Dt,Wt,yt){(Y!==Dt||ut!==Wt||at!==yt)&&(n.stencilFunc(Dt,Wt,yt),Y=Dt,ut=Wt,at=yt)},setOp:function(Dt,Wt,yt){(ct!==Dt||It!==Wt||qt!==yt)&&(n.stencilOp(Dt,Wt,yt),ct=Dt,It=Wt,qt=yt)},setLocked:function(Dt){N=Dt},setClear:function(Dt){nt!==Dt&&(n.clearStencil(Dt),nt=Dt)},reset:function(){N=!1,j=null,Y=null,ut=null,at=null,ct=null,It=null,qt=null,nt=null}}}let r=new e,o=new i,a=new s,l={},c=new WeakMap,h={},u={},d={},f=new WeakMap,m=[],x=null,p=!1,g=null,y=null,v=null,S=null,T=null,b=null,_=null,M=new bt(0,0,0),D=0,P=!1,A=null,R=null,U=null,L=null,B=null,H=n.getParameter(n.MAX_COMBINED_TEXTURE_IMAGE_UNITS),z=!1,Q=0,W=n.getParameter(n.VERSION);W.indexOf("WebGL")!==-1?(Q=parseFloat(/^WebGL (\d)/.exec(W)[1]),z=Q>=1):W.indexOf("OpenGL ES")!==-1&&(Q=parseFloat(/^OpenGL ES (\d)/.exec(W)[1]),z=Q>=2);let Z=null,$={},xt=n.getParameter(n.SCISSOR_BOX),Mt=n.getParameter(n.VIEWPORT),q=new oe().fromArray(xt),st=new oe().fromArray(Mt);function _t(N,j,Y,ut){let at=new Uint8Array(4),ct=n.createTexture();n.bindTexture(N,ct),n.texParameteri(N,n.TEXTURE_MIN_FILTER,n.NEAREST),n.texParameteri(N,n.TEXTURE_MAG_FILTER,n.NEAREST);for(let It=0;It<Y;It++)N===n.TEXTURE_3D||N===n.TEXTURE_2D_ARRAY?n.texImage3D(j,0,n.RGBA,1,1,ut,0,n.RGBA,n.UNSIGNED_BYTE,at):n.texImage2D(j+It,0,n.RGBA,1,1,0,n.RGBA,n.UNSIGNED_BYTE,at);return ct}let wt={};wt[n.TEXTURE_2D]=_t(n.TEXTURE_2D,n.TEXTURE_2D,1),wt[n.TEXTURE_CUBE_MAP]=_t(n.TEXTURE_CUBE_MAP,n.TEXTURE_CUBE_MAP_POSITIVE_X,6),wt[n.TEXTURE_2D_ARRAY]=_t(n.TEXTURE_2D_ARRAY,n.TEXTURE_2D_ARRAY,1,1),wt[n.TEXTURE_3D]=_t(n.TEXTURE_3D,n.TEXTURE_3D,1,1),r.setClear(0,0,0,1),o.setClear(1),a.setClear(0),rt(n.DEPTH_TEST),o.setFunc(Fs),Se(!1),ie(fh),rt(n.CULL_FACE),Kt(wi);function rt(N){h[N]!==!0&&(n.enable(N),h[N]=!0)}function Pt(N){h[N]!==!1&&(n.disable(N),h[N]=!1)}function Qt(N,j){return d[N]!==j?(n.bindFramebuffer(N,j),d[N]=j,N===n.DRAW_FRAMEBUFFER&&(d[n.FRAMEBUFFER]=j),N===n.FRAMEBUFFER&&(d[n.DRAW_FRAMEBUFFER]=j),!0):!1}function ot(N,j){let Y=m,ut=!1;if(N){Y=f.get(j),Y===void 0&&(Y=[],f.set(j,Y));let at=N.textures;if(Y.length!==at.length||Y[0]!==n.COLOR_ATTACHMENT0){for(let ct=0,It=at.length;ct<It;ct++)Y[ct]=n.COLOR_ATTACHMENT0+ct;Y.length=at.length,ut=!0}}else Y[0]!==n.BACK&&(Y[0]=n.BACK,ut=!0);ut&&n.drawBuffers(Y)}function Ct(N){return x!==N?(n.useProgram(N),x=N,!0):!1}let Bt={[gn]:n.FUNC_ADD,[Ed]:n.FUNC_SUBTRACT,[Td]:n.FUNC_REVERSE_SUBTRACT};Bt[Ad]=n.MIN,Bt[Rd]=n.MAX;let Nt={[Ka]:n.ZERO,[tr]:n.ONE,[Cd]:n.SRC_COLOR,[gh]:n.SRC_ALPHA,[Ud]:n.SRC_ALPHA_SATURATE,[Ld]:n.DST_COLOR,[Id]:n.DST_ALPHA,[Pd]:n.ONE_MINUS_SRC_COLOR,[xh]:n.ONE_MINUS_SRC_ALPHA,[Nd]:n.ONE_MINUS_DST_COLOR,[Dd]:n.ONE_MINUS_DST_ALPHA,[Fd]:n.CONSTANT_COLOR,[Od]:n.ONE_MINUS_CONSTANT_COLOR,[Bd]:n.CONSTANT_ALPHA,[zd]:n.ONE_MINUS_CONSTANT_ALPHA};function Kt(N,j,Y,ut,at,ct,It,qt,nt,Dt){if(N===wi){p===!0&&(Pt(n.BLEND),p=!1);return}if(p===!1&&(rt(n.BLEND),p=!0),N!==Za){if(N!==g||Dt!==P){if((y!==gn||T!==gn)&&(n.blendEquation(n.FUNC_ADD),y=gn,T=gn),Dt)switch(N){case Qs:n.blendFuncSeparate(n.ONE,n.ONE_MINUS_SRC_ALPHA,n.ONE,n.ONE_MINUS_SRC_ALPHA);break;case Qr:n.blendFunc(n.ONE,n.ONE);break;case ph:n.blendFuncSeparate(n.ZERO,n.ONE_MINUS_SRC_COLOR,n.ZERO,n.ONE);break;case mh:n.blendFuncSeparate(n.DST_COLOR,n.ONE_MINUS_SRC_ALPHA,n.ZERO,n.ONE);break;default:Ot("WebGLState: Invalid blending: ",N);break}else switch(N){case Qs:n.blendFuncSeparate(n.SRC_ALPHA,n.ONE_MINUS_SRC_ALPHA,n.ONE,n.ONE_MINUS_SRC_ALPHA);break;case Qr:n.blendFuncSeparate(n.SRC_ALPHA,n.ONE,n.ONE,n.ONE);break;case ph:Ot("WebGLState: SubtractiveBlending requires material.premultipliedAlpha = true");break;case mh:Ot("WebGLState: MultiplyBlending requires material.premultipliedAlpha = true");break;default:Ot("WebGLState: Invalid blending: ",N);break}v=null,S=null,b=null,_=null,M.set(0,0,0),D=0,g=N,P=Dt}return}at=at||j,ct=ct||Y,It=It||ut,(j!==y||at!==T)&&(n.blendEquationSeparate(Bt[j],Bt[at]),y=j,T=at),(Y!==v||ut!==S||ct!==b||It!==_)&&(n.blendFuncSeparate(Nt[Y],Nt[ut],Nt[ct],Nt[It]),v=Y,S=ut,b=ct,_=It),(qt.equals(M)===!1||nt!==D)&&(n.blendColor(qt.r,qt.g,qt.b,nt),M.copy(qt),D=nt),g=N,P=!1}function Yt(N,j){N.side===Ue?Pt(n.CULL_FACE):rt(n.CULL_FACE);let Y=N.side===Ze;j&&(Y=!Y),Se(Y),N.blending===Qs&&N.transparent===!1?Kt(wi):Kt(N.blending,N.blendEquation,N.blendSrc,N.blendDst,N.blendEquationAlpha,N.blendSrcAlpha,N.blendDstAlpha,N.blendColor,N.blendAlpha,N.premultipliedAlpha),o.setFunc(N.depthFunc),o.setTest(N.depthTest),o.setMask(N.depthWrite),r.setMask(N.colorWrite);let ut=N.stencilWrite;a.setTest(ut),ut&&(a.setMask(N.stencilWriteMask),a.setFunc(N.stencilFunc,N.stencilRef,N.stencilFuncMask),a.setOp(N.stencilFail,N.stencilZFail,N.stencilZPass)),_e(N.polygonOffset,N.polygonOffsetFactor,N.polygonOffsetUnits),N.alphaToCoverage===!0?rt(n.SAMPLE_ALPHA_TO_COVERAGE):Pt(n.SAMPLE_ALPHA_TO_COVERAGE)}function Se(N){A!==N&&(N?n.frontFace(n.CW):n.frontFace(n.CCW),A=N)}function ie(N){N!==Sd?(rt(n.CULL_FACE),N!==R&&(N===fh?n.cullFace(n.BACK):N===bd?n.cullFace(n.FRONT):n.cullFace(n.FRONT_AND_BACK))):Pt(n.CULL_FACE),R=N}function Re(N){N!==U&&(z&&n.lineWidth(N),U=N)}function _e(N,j,Y){N?(rt(n.POLYGON_OFFSET_FILL),(L!==j||B!==Y)&&(L=j,B=Y,o.getReversed()&&(j=-j,Y=-Y),n.polygonOffset(j,Y))):Pt(n.POLYGON_OFFSET_FILL)}function Ee(N){N?rt(n.SCISSOR_TEST):Pt(n.SCISSOR_TEST)}function fi(N){N===void 0&&(N=n.TEXTURE0+H-1),Z!==N&&(n.activeTexture(N),Z=N)}function ri(N,j,Y){Y===void 0&&(Z===null?Y=n.TEXTURE0+H-1:Y=Z);let ut=$[Y];ut===void 0&&(ut={type:void 0,texture:void 0},$[Y]=ut),(ut.type!==N||ut.texture!==j)&&(Z!==Y&&(n.activeTexture(Y),Z=Y),n.bindTexture(N,j||wt[N]),ut.type=N,ut.texture=j)}function O(){let N=$[Z];N!==void 0&&N.type!==void 0&&(n.bindTexture(N.type,null),N.type=void 0,N.texture=void 0)}function oi(){try{n.compressedTexImage2D(...arguments)}catch(N){Ot("WebGLState:",N)}}function ue(){try{n.compressedTexImage3D(...arguments)}catch(N){Ot("WebGLState:",N)}}function de(){try{n.texSubImage2D(...arguments)}catch(N){Ot("WebGLState:",N)}}function dt(){try{n.texSubImage3D(...arguments)}catch(N){Ot("WebGLState:",N)}}function Ne(){try{n.compressedTexSubImage2D(...arguments)}catch(N){Ot("WebGLState:",N)}}function Ft(){try{n.compressedTexSubImage3D(...arguments)}catch(N){Ot("WebGLState:",N)}}function kt(){try{n.texStorage2D(...arguments)}catch(N){Ot("WebGLState:",N)}}function I(){try{n.texStorage3D(...arguments)}catch(N){Ot("WebGLState:",N)}}function w(){try{n.texImage2D(...arguments)}catch(N){Ot("WebGLState:",N)}}function V(){try{n.texImage3D(...arguments)}catch(N){Ot("WebGLState:",N)}}function it(N){return u[N]!==void 0?u[N]:n.getParameter(N)}function tt(N,j){u[N]!==j&&(n.pixelStorei(N,j),u[N]=j)}function lt(N){q.equals(N)===!1&&(n.scissor(N.x,N.y,N.z,N.w),q.copy(N))}function mt(N){st.equals(N)===!1&&(n.viewport(N.x,N.y,N.z,N.w),st.copy(N))}function K(N,j){let Y=c.get(j);Y===void 0&&(Y=new WeakMap,c.set(j,Y));let ut=Y.get(N);ut===void 0&&(ut=n.getUniformBlockIndex(j,N.name),n.uniformBlockBinding(j,ut,ut),Y.set(N,ut))}function J(N,j,Y){let at=c.get(j).get(N);l[at]!==Y&&(n.bindBufferBase(n.UNIFORM_BUFFER,at,Y),l[at]=Y)}function ht(){n.disable(n.BLEND),n.disable(n.CULL_FACE),n.disable(n.DEPTH_TEST),n.disable(n.POLYGON_OFFSET_FILL),n.disable(n.SCISSOR_TEST),n.disable(n.STENCIL_TEST),n.disable(n.SAMPLE_ALPHA_TO_COVERAGE),n.blendEquation(n.FUNC_ADD),n.blendFunc(n.ONE,n.ZERO),n.blendFuncSeparate(n.ONE,n.ZERO,n.ONE,n.ZERO),n.blendColor(0,0,0,0),n.colorMask(!0,!0,!0,!0),n.clearColor(0,0,0,0),n.depthMask(!0),n.depthFunc(n.LESS),o.setReversed(!1),n.clearDepth(1),n.stencilMask(4294967295),n.stencilFunc(n.ALWAYS,0,4294967295),n.stencilOp(n.KEEP,n.KEEP,n.KEEP),n.clearStencil(0),n.cullFace(n.BACK),n.frontFace(n.CCW),n.polygonOffset(0,0),n.activeTexture(n.TEXTURE0),n.bindFramebuffer(n.FRAMEBUFFER,null),n.bindFramebuffer(n.DRAW_FRAMEBUFFER,null),n.bindFramebuffer(n.READ_FRAMEBUFFER,null),n.useProgram(null),n.lineWidth(1),n.scissor(0,0,n.canvas.width,n.canvas.height),n.viewport(0,0,n.canvas.width,n.canvas.height),n.pixelStorei(n.PACK_ALIGNMENT,4),n.pixelStorei(n.UNPACK_ALIGNMENT,4),n.pixelStorei(n.UNPACK_FLIP_Y_WEBGL,!1),n.pixelStorei(n.UNPACK_PREMULTIPLY_ALPHA_WEBGL,!1),n.pixelStorei(n.UNPACK_COLORSPACE_CONVERSION_WEBGL,n.BROWSER_DEFAULT_WEBGL),n.pixelStorei(n.PACK_ROW_LENGTH,0),n.pixelStorei(n.PACK_SKIP_PIXELS,0),n.pixelStorei(n.PACK_SKIP_ROWS,0),n.pixelStorei(n.UNPACK_ROW_LENGTH,0),n.pixelStorei(n.UNPACK_IMAGE_HEIGHT,0),n.pixelStorei(n.UNPACK_SKIP_PIXELS,0),n.pixelStorei(n.UNPACK_SKIP_ROWS,0),n.pixelStorei(n.UNPACK_SKIP_IMAGES,0),h={},u={},Z=null,$={},d={},l={},f=new WeakMap,m=[],x=null,p=!1,g=null,y=null,v=null,S=null,T=null,b=null,_=null,M=new bt(0,0,0),D=0,P=!1,A=null,R=null,U=null,L=null,B=null,q.set(0,0,n.canvas.width,n.canvas.height),st.set(0,0,n.canvas.width,n.canvas.height),r.reset(),o.reset(),a.reset()}return{buffers:{color:r,depth:o,stencil:a},enable:rt,disable:Pt,bindFramebuffer:Qt,drawBuffers:ot,useProgram:Ct,setBlending:Kt,setMaterial:Yt,setFlipSided:Se,setCullFace:ie,setLineWidth:Re,setPolygonOffset:_e,setScissorTest:Ee,activeTexture:fi,bindTexture:ri,unbindTexture:O,compressedTexImage2D:oi,compressedTexImage3D:ue,texImage2D:w,texImage3D:V,pixelStorei:tt,getParameter:it,updateUBOMapping:K,uniformBlockBinding:J,texStorage2D:kt,texStorage3D:I,texSubImage2D:de,texSubImage3D:dt,compressedTexSubImage2D:Ne,compressedTexSubImage3D:Ft,scissor:lt,viewport:mt,reset:ht}}function F_(n,t,e,i,s,r,o){let a=t.has("WEBGL_multisampled_render_to_texture")?t.get("WEBGL_multisampled_render_to_texture"):null,l=typeof navigator>"u"?!1:/OculusBrowser/g.test(navigator.userAgent),c=new et,h=new WeakMap,u=new Set,d,f=new WeakMap,m=new Set,x=new FinalizationRegistry(I=>m.delete(I)),p=!1;try{p=typeof OffscreenCanvas<"u"&&new OffscreenCanvas(1,1).getContext("2d")!==null}catch{}function g(I,w){return p?new OffscreenCanvas(I,w):Dr("canvas")}function y(I,w,V){let it=1,tt=kt(I);if((tt.width>V||tt.height>V)&&(it=V/Math.max(tt.width,tt.height)),it<1)if(typeof HTMLImageElement<"u"&&I instanceof HTMLImageElement||typeof HTMLCanvasElement<"u"&&I instanceof HTMLCanvasElement||typeof ImageBitmap<"u"&&I instanceof ImageBitmap||typeof VideoFrame<"u"&&I instanceof VideoFrame){let lt=Math.floor(it*tt.width),mt=Math.floor(it*tt.height);d===void 0&&(d=g(lt,mt));let K=w?g(lt,mt):d;return K.width=lt,K.height=mt,K.getContext("2d").drawImage(I,0,0,lt,mt),Lt("WebGLRenderer: Texture has been resized from ("+tt.width+"x"+tt.height+") to ("+lt+"x"+mt+")."),K}else return"data"in I&&Lt("WebGLRenderer: Image in DataTexture is too big ("+tt.width+"x"+tt.height+")."),I;return I}function v(I){return I.generateMipmaps}function S(I){n.generateMipmap(I)}function T(I){return I.isWebGLCubeRenderTarget?n.TEXTURE_CUBE_MAP:I.isWebGL3DRenderTarget?n.TEXTURE_3D:I.isWebGLArrayRenderTarget||I.isCompressedArrayTexture?n.TEXTURE_2D_ARRAY:n.TEXTURE_2D}function b(I,w,V,it,tt,lt=!1){if(I!==null){if(n[I]!==void 0)return n[I];Lt("WebGLRenderer: Attempt to use non-existing WebGL internal format '"+I+"'")}let mt;it&&(mt=t.get("EXT_texture_norm16"),mt||Lt("WebGLRenderer: Unable to use normalized textures without EXT_texture_norm16 extension"));let K=w;if(w===n.RED&&(V===n.FLOAT&&(K=n.R32F),V===n.HALF_FLOAT&&(K=n.R16F),V===n.UNSIGNED_BYTE&&(K=n.R8),V===n.UNSIGNED_SHORT&&mt&&(K=mt.R16_EXT),V===n.SHORT&&mt&&(K=mt.R16_SNORM_EXT)),w===n.RED_INTEGER&&(V===n.UNSIGNED_BYTE&&(K=n.R8UI),V===n.UNSIGNED_SHORT&&(K=n.R16UI),V===n.UNSIGNED_INT&&(K=n.R32UI),V===n.BYTE&&(K=n.R8I),V===n.SHORT&&(K=n.R16I),V===n.INT&&(K=n.R32I)),w===n.RG&&(V===n.FLOAT&&(K=n.RG32F),V===n.HALF_FLOAT&&(K=n.RG16F),V===n.UNSIGNED_BYTE&&(K=n.RG8),V===n.UNSIGNED_SHORT&&mt&&(K=mt.RG16_EXT),V===n.SHORT&&mt&&(K=mt.RG16_SNORM_EXT)),w===n.RG_INTEGER&&(V===n.UNSIGNED_BYTE&&(K=n.RG8UI),V===n.UNSIGNED_SHORT&&(K=n.RG16UI),V===n.UNSIGNED_INT&&(K=n.RG32UI),V===n.BYTE&&(K=n.RG8I),V===n.SHORT&&(K=n.RG16I),V===n.INT&&(K=n.RG32I)),w===n.RGB_INTEGER&&(V===n.UNSIGNED_BYTE&&(K=n.RGB8UI),V===n.UNSIGNED_SHORT&&(K=n.RGB16UI),V===n.UNSIGNED_INT&&(K=n.RGB32UI),V===n.BYTE&&(K=n.RGB8I),V===n.SHORT&&(K=n.RGB16I),V===n.INT&&(K=n.RGB32I)),w===n.RGBA_INTEGER&&(V===n.UNSIGNED_BYTE&&(K=n.RGBA8UI),V===n.UNSIGNED_SHORT&&(K=n.RGBA16UI),V===n.UNSIGNED_INT&&(K=n.RGBA32UI),V===n.BYTE&&(K=n.RGBA8I),V===n.SHORT&&(K=n.RGBA16I),V===n.INT&&(K=n.RGBA32I)),w===n.RGB&&(V===n.UNSIGNED_SHORT&&mt&&(K=mt.RGB16_EXT),V===n.SHORT&&mt&&(K=mt.RGB16_SNORM_EXT),V===n.UNSIGNED_INT_5_9_9_9_REV&&(K=n.RGB9_E5),V===n.UNSIGNED_INT_10F_11F_11F_REV&&(K=n.R11F_G11F_B10F)),w===n.RGBA){let J=lt?Ir:Zt.getTransfer(tt);V===n.FLOAT&&(K=n.RGBA32F),V===n.HALF_FLOAT&&(K=n.RGBA16F),V===n.UNSIGNED_BYTE&&(K=J===ne?n.SRGB8_ALPHA8:n.RGBA8),V===n.UNSIGNED_SHORT&&mt&&(K=mt.RGBA16_EXT),V===n.SHORT&&mt&&(K=mt.RGBA16_SNORM_EXT),V===n.UNSIGNED_SHORT_4_4_4_4&&(K=n.RGBA4),V===n.UNSIGNED_SHORT_5_5_5_1&&(K=n.RGB5_A1)}return(K===n.R16F||K===n.R32F||K===n.RG16F||K===n.RG32F||K===n.RGBA16F||K===n.RGBA32F)&&t.get("EXT_color_buffer_float"),K}function _(I,w){let V;return I?w===null||w===hi||w===us?V=n.DEPTH24_STENCIL8:w===gi?V=n.DEPTH32F_STENCIL8:w===ir&&(V=n.DEPTH24_STENCIL8,Lt("DepthTexture: 16 bit depth attachment is not supported with stencil. Using 24-bit attachment.")):w===null||w===hi||w===us?V=n.DEPTH_COMPONENT24:w===gi?V=n.DEPTH_COMPONENT32F:w===ir&&(V=n.DEPTH_COMPONENT16),V}function M(I,w){return v(I)===!0||I.isFramebufferTexture&&I.minFilter!==De&&I.minFilter!==ce?Math.log2(Math.max(w.width,w.height))+1:I.mipmaps.length>0?I.mipmaps.length:I.isCompressedTexture&&Array.isArray(I.image)?w.mipmaps.length:1}function D(I){let w=new WeakRef(I);i.get(I).__ref=w,m.add(w),x.register(I,w,w)}function P(I){let w=i.get(I).__ref;w!==void 0&&(m.delete(w),x.unregister(w))}function A(I){U(I.target)}function R(I){L(I.target)}function U(I){I.removeEventListener("dispose",A),P(I),H(I),I.isVideoTexture&&h.delete(I),I.isHTMLTexture&&u.delete(I)}function L(I){I.removeEventListener("dispose",R),P(I),Q(I)}function B(){for(let I of m){let w=I.deref();w!==void 0&&(w.isRenderTarget===!0?L(w):U(w))}m.clear(),u.clear()}function H(I){let w=i.get(I);if(w.__webglInit===void 0)return;let V=I.source,it=f.get(V);if(it){let tt=it[w.__cacheKey];tt.usedTimes--,tt.usedTimes===0&&z(I),Object.keys(it).length===0&&f.delete(V)}i.remove(I)}function z(I){let w=i.get(I);n.deleteTexture(w.__webglTexture);let V=I.source,it=f.get(V);delete it[w.__cacheKey],o.memory.textures--}function Q(I){let w=i.get(I);w.__depthDisposeCallback&&w.__depthDisposeCallback();let V=I.depthTexture;if(V&&V.renderTarget===I&&U(V),I.isWebGLCubeRenderTarget)for(let tt=0;tt<6;tt++){if(Array.isArray(w.__webglFramebuffer[tt]))for(let lt=0;lt<w.__webglFramebuffer[tt].length;lt++)n.deleteFramebuffer(w.__webglFramebuffer[tt][lt]);else n.deleteFramebuffer(w.__webglFramebuffer[tt]);w.__webglDepthbuffer&&n.deleteRenderbuffer(w.__webglDepthbuffer[tt])}else{if(Array.isArray(w.__webglFramebuffer))for(let tt=0;tt<w.__webglFramebuffer.length;tt++)n.deleteFramebuffer(w.__webglFramebuffer[tt]);else n.deleteFramebuffer(w.__webglFramebuffer);if(w.__webglDepthbuffer&&n.deleteRenderbuffer(w.__webglDepthbuffer),w.__webglMultisampledFramebuffer&&n.deleteFramebuffer(w.__webglMultisampledFramebuffer),w.__webglColorRenderbuffer)for(let tt=0;tt<w.__webglColorRenderbuffer.length;tt++)w.__webglColorRenderbuffer[tt]&&n.deleteRenderbuffer(w.__webglColorRenderbuffer[tt]);w.__webglDepthRenderbuffer&&n.deleteRenderbuffer(w.__webglDepthRenderbuffer)}let it=I.textures;for(let tt=0,lt=it.length;tt<lt;tt++){let mt=i.get(it[tt]);mt.__webglTexture&&(n.deleteTexture(mt.__webglTexture),o.memory.textures--),i.remove(it[tt])}i.remove(I)}let W=0;function Z(){W=0}function $(){return W}function xt(I){W=I}function Mt(){let I=W;return I>=s.maxTextures&&Lt("WebGLTextures: Trying to use "+(I+1)+" texture units while this GPU supports only "+s.maxTextures),W+=1,I}function q(I){let w=[];return w.push(I.wrapS),w.push(I.wrapT),w.push(I.wrapR||0),w.push(I.magFilter),w.push(I.minFilter),w.push(I.anisotropy),w.push(I.internalFormat),w.push(I.format),w.push(I.type),w.push(I.generateMipmaps),w.push(I.mipmapsAutoUpdate),w.push(I.premultiplyAlpha),w.push(I.flipY),w.push(I.unpackAlignment),w.push(I.colorSpace),w.join()}function st(I,w){let V=i.get(I);if(I.isVideoTexture&&Ne(I),I.isRenderTargetTexture===!1&&I.isExternalTexture!==!0&&I.version>0&&V.__version!==I.version){let it=I.image;if(it===null)Lt("WebGLRenderer: Texture marked for update but no image data found.");else if(it.complete===!1)Lt("WebGLRenderer: Texture marked for update but image is incomplete");else{Yt(V,I,w);return}}else I.isExternalTexture&&(V.__webglTexture=I.sourceTexture?I.sourceTexture:null);e.bindTexture(n.TEXTURE_2D,V.__webglTexture,n.TEXTURE0+w)}function _t(I,w){let V=i.get(I);if(I.isRenderTargetTexture===!1&&I.version>0&&V.__version!==I.version){Yt(V,I,w);return}else I.isExternalTexture&&(V.__webglTexture=I.sourceTexture?I.sourceTexture:null);e.bindTexture(n.TEXTURE_2D_ARRAY,V.__webglTexture,n.TEXTURE0+w)}function wt(I,w){let V=i.get(I);if(I.isRenderTargetTexture===!1&&I.version>0&&V.__version!==I.version){Yt(V,I,w);return}e.bindTexture(n.TEXTURE_3D,V.__webglTexture,n.TEXTURE0+w)}function rt(I,w){let V=i.get(I);if(I.version>0&&V.__version!==I.version){Se(V,I,w);return}e.bindTexture(n.TEXTURE_CUBE_MAP,V.__webglTexture,n.TEXTURE0+w)}let Pt={[un]:n.REPEAT,[Wi]:n.CLAMP_TO_EDGE,[Sa]:n.MIRRORED_REPEAT},Qt={[De]:n.NEAREST,[Vd]:n.NEAREST_MIPMAP_NEAREST,[oo]:n.NEAREST_MIPMAP_LINEAR,[ce]:n.LINEAR,[Qa]:n.LINEAR_MIPMAP_NEAREST,[zi]:n.LINEAR_MIPMAP_LINEAR},ot={[qd]:n.NEVER,[Th]:n.ALWAYS,[Yd]:n.LESS,[nr]:n.LEQUAL,[Zd]:n.EQUAL,[Eh]:n.GEQUAL,[Kd]:n.GREATER,[Jd]:n.NOTEQUAL};function Ct(I,w){if(w.type===gi&&t.has("OES_texture_float_linear")===!1&&(w.magFilter===ce||w.magFilter===Qa||w.magFilter===oo||w.magFilter===zi||w.minFilter===ce||w.minFilter===Qa||w.minFilter===oo||w.minFilter===zi)&&Lt("WebGLRenderer: Unable to use linear filtering with floating point textures. OES_texture_float_linear not supported on this device."),n.texParameteri(I,n.TEXTURE_WRAP_S,Pt[w.wrapS]),n.texParameteri(I,n.TEXTURE_WRAP_T,Pt[w.wrapT]),(I===n.TEXTURE_3D||I===n.TEXTURE_2D_ARRAY)&&n.texParameteri(I,n.TEXTURE_WRAP_R,Pt[w.wrapR]),n.texParameteri(I,n.TEXTURE_MAG_FILTER,Qt[w.magFilter]),n.texParameteri(I,n.TEXTURE_MIN_FILTER,Qt[w.minFilter]),w.compareFunction&&(n.texParameteri(I,n.TEXTURE_COMPARE_MODE,n.COMPARE_REF_TO_TEXTURE),n.texParameteri(I,n.TEXTURE_COMPARE_FUNC,ot[w.compareFunction])),t.has("EXT_texture_filter_anisotropic")===!0){if(w.magFilter===De||w.minFilter!==oo&&w.minFilter!==zi||w.type===gi&&t.has("OES_texture_float_linear")===!1)return;if(w.anisotropy>1||i.get(w).__currentAnisotropy){let V=t.get("EXT_texture_filter_anisotropic");n.texParameterf(I,V.TEXTURE_MAX_ANISOTROPY_EXT,Math.min(w.anisotropy,s.getMaxAnisotropy())),i.get(w).__currentAnisotropy=w.anisotropy}}}function Bt(I,w){let V=!1;I.__webglInit===void 0&&(I.__webglInit=!0,w.addEventListener("dispose",A),D(w));let it=w.source,tt=f.get(it);tt===void 0&&(tt={},f.set(it,tt));let lt=q(w);if(lt!==I.__cacheKey){tt[lt]===void 0&&(tt[lt]={texture:n.createTexture(),usedTimes:0},o.memory.textures++,V=!0),tt[lt].usedTimes++;let mt=tt[I.__cacheKey];mt!==void 0&&(tt[I.__cacheKey].usedTimes--,mt.usedTimes===0&&z(w)),I.__cacheKey=lt,I.__webglTexture=tt[lt].texture}return V}function Nt(I,w,V){return Math.floor(Math.floor(I/V)/w)}function Kt(I,w,V,it){let lt=I.updateRanges;if(lt.length===0)e.texSubImage2D(n.TEXTURE_2D,0,0,0,w.width,w.height,V,it,w.data);else{lt.sort((N,j)=>N.start-j.start);let mt=0;for(let N=1;N<lt.length;N++){let j=lt[mt],Y=lt[N],ut=j.start+j.count,at=Nt(Y.start,w.width,4),ct=Nt(j.start,w.width,4);Y.start<=ut+1&&at===ct&&Nt(Y.start+Y.count-1,w.width,4)===at?j.count=Math.max(j.count,Y.start+Y.count-j.start):(++mt,lt[mt]=Y)}lt.length=mt+1;let K=e.getParameter(n.UNPACK_ROW_LENGTH),J=e.getParameter(n.UNPACK_SKIP_PIXELS),ht=e.getParameter(n.UNPACK_SKIP_ROWS);e.pixelStorei(n.UNPACK_ROW_LENGTH,w.width);for(let N=0,j=lt.length;N<j;N++){let Y=lt[N],ut=Math.floor(Y.start/4),at=Math.ceil(Y.count/4),ct=ut%w.width,It=Math.floor(ut/w.width),qt=at,nt=1;e.pixelStorei(n.UNPACK_SKIP_PIXELS,ct),e.pixelStorei(n.UNPACK_SKIP_ROWS,It),e.texSubImage2D(n.TEXTURE_2D,0,ct,It,qt,nt,V,it,w.data)}I.clearUpdateRanges(),e.pixelStorei(n.UNPACK_ROW_LENGTH,K),e.pixelStorei(n.UNPACK_SKIP_PIXELS,J),e.pixelStorei(n.UNPACK_SKIP_ROWS,ht)}}function Yt(I,w,V){let it=n.TEXTURE_2D;(w.isDataArrayTexture||w.isCompressedArrayTexture)&&(it=n.TEXTURE_2D_ARRAY),w.isData3DTexture&&(it=n.TEXTURE_3D);let tt=Bt(I,w),lt=w.source;e.bindTexture(it,I.__webglTexture,n.TEXTURE0+V);let mt=i.get(lt);if(lt.version!==mt.__version||tt===!0){if(e.activeTexture(n.TEXTURE0+V),(typeof ImageBitmap<"u"&&w.image instanceof ImageBitmap)===!1){let nt=Zt.getPrimaries(Zt.workingColorSpace),Dt=w.colorSpace===ki?null:Zt.getPrimaries(w.colorSpace),Wt=w.colorSpace===ki||nt===Dt?n.NONE:n.BROWSER_DEFAULT_WEBGL;e.pixelStorei(n.UNPACK_FLIP_Y_WEBGL,w.flipY),e.pixelStorei(n.UNPACK_PREMULTIPLY_ALPHA_WEBGL,w.premultiplyAlpha),e.pixelStorei(n.UNPACK_COLORSPACE_CONVERSION_WEBGL,Wt)}e.pixelStorei(n.UNPACK_ALIGNMENT,w.unpackAlignment);let J=y(w.image,!1,s.maxTextureSize);J=Ft(w,J);let ht=r.convert(w.format,w.colorSpace),N=r.convert(w.type),j=b(w.internalFormat,ht,N,w.normalized,w.colorSpace,w.isVideoTexture);Ct(it,w);let Y,ut=w.mipmaps,at=w.isVideoTexture!==!0,ct=mt.__version===void 0||tt===!0,It=lt.dataReady,qt=M(w,J);if(w.isDepthTexture)j=_(w.format===xn,w.type),ct&&(at?e.texStorage2D(n.TEXTURE_2D,1,j,J.width,J.height):e.texImage2D(n.TEXTURE_2D,0,j,J.width,J.height,0,ht,N,null));else if(w.isDataTexture)if(ut.length>0){at&&ct&&e.texStorage2D(n.TEXTURE_2D,qt,j,ut[0].width,ut[0].height);for(let nt=0,Dt=ut.length;nt<Dt;nt++)Y=ut[nt],at?It&&e.texSubImage2D(n.TEXTURE_2D,nt,0,0,Y.width,Y.height,ht,N,Y.data):e.texImage2D(n.TEXTURE_2D,nt,j,Y.width,Y.height,0,ht,N,Y.data);w.generateMipmaps=!1}else at?(ct&&e.texStorage2D(n.TEXTURE_2D,qt,j,J.width,J.height),It&&Kt(w,J,ht,N)):e.texImage2D(n.TEXTURE_2D,0,j,J.width,J.height,0,ht,N,J.data);else if(w.isCompressedTexture)if(w.isCompressedArrayTexture){at&&ct&&e.texStorage3D(n.TEXTURE_2D_ARRAY,qt,j,ut[0].width,ut[0].height,J.depth);for(let nt=0,Dt=ut.length;nt<Dt;nt++)if(Y=ut[nt],w.format!==ui)if(ht!==null)if(at){if(It)if(w.layerUpdates.size>0){let Wt=Uh(Y.width,Y.height,w.format,w.type);for(let yt of w.layerUpdates){let ae=Y.data.subarray(yt*Wt/Y.data.BYTES_PER_ELEMENT,(yt+1)*Wt/Y.data.BYTES_PER_ELEMENT);e.compressedTexSubImage3D(n.TEXTURE_2D_ARRAY,nt,0,0,yt,Y.width,Y.height,1,ht,ae)}}else e.compressedTexSubImage3D(n.TEXTURE_2D_ARRAY,nt,0,0,0,Y.width,Y.height,J.depth,ht,Y.data)}else e.compressedTexImage3D(n.TEXTURE_2D_ARRAY,nt,j,Y.width,Y.height,J.depth,0,Y.data,0,0);else Lt("WebGLRenderer: Attempt to load unsupported compressed texture format in .uploadTexture()");else at?It&&e.texSubImage3D(n.TEXTURE_2D_ARRAY,nt,0,0,0,Y.width,Y.height,J.depth,ht,N,Y.data):e.texImage3D(n.TEXTURE_2D_ARRAY,nt,j,Y.width,Y.height,J.depth,0,ht,N,Y.data);w.layerUpdates.size>0&&w.clearLayerUpdates()}else{at&&ct&&e.texStorage2D(n.TEXTURE_2D,qt,j,ut[0].width,ut[0].height);for(let nt=0,Dt=ut.length;nt<Dt;nt++)Y=ut[nt],w.format!==ui?ht!==null?at?It&&e.compressedTexSubImage2D(n.TEXTURE_2D,nt,0,0,Y.width,Y.height,ht,Y.data):e.compressedTexImage2D(n.TEXTURE_2D,nt,j,Y.width,Y.height,0,Y.data):Lt("WebGLRenderer: Attempt to load unsupported compressed texture format in .uploadTexture()"):at?It&&e.texSubImage2D(n.TEXTURE_2D,nt,0,0,Y.width,Y.height,ht,N,Y.data):e.texImage2D(n.TEXTURE_2D,nt,j,Y.width,Y.height,0,ht,N,Y.data)}else if(w.isDataArrayTexture)if(at){if(ct&&e.texStorage3D(n.TEXTURE_2D_ARRAY,qt,j,J.width,J.height,J.depth),It)if(w.layerUpdates.size>0){let nt=Uh(J.width,J.height,w.format,w.type);for(let Dt of w.layerUpdates){let Wt=J.data.subarray(Dt*nt/J.data.BYTES_PER_ELEMENT,(Dt+1)*nt/J.data.BYTES_PER_ELEMENT);e.texSubImage3D(n.TEXTURE_2D_ARRAY,0,0,0,Dt,J.width,J.height,1,ht,N,Wt)}w.clearLayerUpdates()}else e.texSubImage3D(n.TEXTURE_2D_ARRAY,0,0,0,0,J.width,J.height,J.depth,ht,N,J.data)}else e.texImage3D(n.TEXTURE_2D_ARRAY,0,j,J.width,J.height,J.depth,0,ht,N,J.data);else if(w.isData3DTexture)at?(ct&&e.texStorage3D(n.TEXTURE_3D,qt,j,J.width,J.height,J.depth),It&&e.texSubImage3D(n.TEXTURE_3D,0,0,0,0,J.width,J.height,J.depth,ht,N,J.data)):e.texImage3D(n.TEXTURE_3D,0,j,J.width,J.height,J.depth,0,ht,N,J.data);else if(w.isFramebufferTexture){if(ct)if(at)e.texStorage2D(n.TEXTURE_2D,qt,j,J.width,J.height);else{let nt=J.width,Dt=J.height;for(let Wt=0;Wt<qt;Wt++)e.texImage2D(n.TEXTURE_2D,Wt,j,nt,Dt,0,ht,N,null),nt>>=1,Dt>>=1}}else if(w.isHTMLTexture){if("texElementImage2D"in n){let nt=n.canvas;if(nt.hasAttribute("layoutsubtree")||nt.setAttribute("layoutsubtree","true"),J.parentNode!==nt){nt.appendChild(J),u.add(w),nt.onpaint=Dt=>{let Wt=Dt.changedElements;for(let yt of u)Wt.includes(yt.image)&&(yt.needsUpdate=!0)},nt.requestPaint();return}if(n.texElementImage2D.length===3)n.texElementImage2D(n.TEXTURE_2D,n.RGBA8,J);else{let Wt=n.RGBA,yt=n.RGBA,ae=n.UNSIGNED_BYTE;n.texElementImage2D(n.TEXTURE_2D,0,Wt,yt,ae,J)}n.texParameteri(n.TEXTURE_2D,n.TEXTURE_MIN_FILTER,n.LINEAR),n.texParameteri(n.TEXTURE_2D,n.TEXTURE_WRAP_S,n.CLAMP_TO_EDGE),n.texParameteri(n.TEXTURE_2D,n.TEXTURE_WRAP_T,n.CLAMP_TO_EDGE)}}else if(ut.length>0){if(at&&ct){let nt=kt(ut[0]);e.texStorage2D(n.TEXTURE_2D,qt,j,nt.width,nt.height)}for(let nt=0,Dt=ut.length;nt<Dt;nt++)Y=ut[nt],at?It&&e.texSubImage2D(n.TEXTURE_2D,nt,0,0,ht,N,Y):e.texImage2D(n.TEXTURE_2D,nt,j,ht,N,Y);w.generateMipmaps=!1}else if(at){if(ct){let nt=kt(J);e.texStorage2D(n.TEXTURE_2D,qt,j,nt.width,nt.height)}It&&e.texSubImage2D(n.TEXTURE_2D,0,0,0,ht,N,J)}else e.texImage2D(n.TEXTURE_2D,0,j,ht,N,J);v(w)&&w.mipmapsAutoUpdate===!0&&S(it),mt.__version=lt.version,w.onUpdate&&w.onUpdate(w)}I.__version=w.version}function Se(I,w,V){if(w.image.length!==6)return;let it=Bt(I,w),tt=w.source;e.bindTexture(n.TEXTURE_CUBE_MAP,I.__webglTexture,n.TEXTURE0+V);let lt=i.get(tt);if(tt.version!==lt.__version||it===!0){e.activeTexture(n.TEXTURE0+V);let mt=Zt.getPrimaries(Zt.workingColorSpace),K=w.colorSpace===ki?null:Zt.getPrimaries(w.colorSpace),J=w.colorSpace===ki||mt===K?n.NONE:n.BROWSER_DEFAULT_WEBGL;e.pixelStorei(n.UNPACK_FLIP_Y_WEBGL,w.flipY),e.pixelStorei(n.UNPACK_PREMULTIPLY_ALPHA_WEBGL,w.premultiplyAlpha),e.pixelStorei(n.UNPACK_ALIGNMENT,w.unpackAlignment),e.pixelStorei(n.UNPACK_COLORSPACE_CONVERSION_WEBGL,J);let ht=w.isCompressedTexture||w.image[0].isCompressedTexture,N=w.image[0]&&w.image[0].isDataTexture,j=[];for(let yt=0;yt<6;yt++)!ht&&!N?j[yt]=y(w.image[yt],!0,s.maxCubemapSize):j[yt]=N?w.image[yt].image:w.image[yt],j[yt]=Ft(w,j[yt]);let Y=j[0],ut=r.convert(w.format,w.colorSpace),at=r.convert(w.type),ct=b(w.internalFormat,ut,at,w.normalized,w.colorSpace),It=w.isVideoTexture!==!0,qt=lt.__version===void 0||it===!0,nt=tt.dataReady,Dt=M(w,Y);Ct(n.TEXTURE_CUBE_MAP,w);let Wt;if(w.isDepthTexture){if(ct=_(w.format===xn,w.type),qt)if(It)e.texStorage2D(n.TEXTURE_CUBE_MAP,1,ct,Y.width,Y.height);else for(let yt=0;yt<6;yt++)e.texImage2D(n.TEXTURE_CUBE_MAP_POSITIVE_X+yt,0,ct,Y.width,Y.height,0,ut,at,null)}else if(ht){It&&qt&&e.texStorage2D(n.TEXTURE_CUBE_MAP,Dt,ct,Y.width,Y.height);for(let yt=0;yt<6;yt++){Wt=j[yt].mipmaps;for(let ae=0;ae<Wt.length;ae++){let He=Wt[ae];w.format!==ui?ut!==null?It?nt&&e.compressedTexSubImage2D(n.TEXTURE_CUBE_MAP_POSITIVE_X+yt,ae,0,0,He.width,He.height,ut,He.data):e.compressedTexImage2D(n.TEXTURE_CUBE_MAP_POSITIVE_X+yt,ae,ct,He.width,He.height,0,He.data):Lt("WebGLRenderer: Attempt to load unsupported compressed texture format in .setTextureCube()"):It?nt&&e.texSubImage2D(n.TEXTURE_CUBE_MAP_POSITIVE_X+yt,ae,0,0,He.width,He.height,ut,at,He.data):e.texImage2D(n.TEXTURE_CUBE_MAP_POSITIVE_X+yt,ae,ct,He.width,He.height,0,ut,at,He.data)}}}else{if(Wt=w.mipmaps,It&&qt){Wt.length>0&&Dt++;let yt=kt(j[0]);e.texStorage2D(n.TEXTURE_CUBE_MAP,Dt,ct,yt.width,yt.height)}for(let yt=0;yt<6;yt++)if(N){It?nt&&e.texSubImage2D(n.TEXTURE_CUBE_MAP_POSITIVE_X+yt,0,0,0,j[yt].width,j[yt].height,ut,at,j[yt].data):e.texImage2D(n.TEXTURE_CUBE_MAP_POSITIVE_X+yt,0,ct,j[yt].width,j[yt].height,0,ut,at,j[yt].data);for(let ae=0;ae<Wt.length;ae++){let bn=Wt[ae].image[yt].image;It?nt&&e.texSubImage2D(n.TEXTURE_CUBE_MAP_POSITIVE_X+yt,ae+1,0,0,bn.width,bn.height,ut,at,bn.data):e.texImage2D(n.TEXTURE_CUBE_MAP_POSITIVE_X+yt,ae+1,ct,bn.width,bn.height,0,ut,at,bn.data)}}else{It?nt&&e.texSubImage2D(n.TEXTURE_CUBE_MAP_POSITIVE_X+yt,0,0,0,ut,at,j[yt]):e.texImage2D(n.TEXTURE_CUBE_MAP_POSITIVE_X+yt,0,ct,ut,at,j[yt]);for(let ae=0;ae<Wt.length;ae++){let He=Wt[ae];It?nt&&e.texSubImage2D(n.TEXTURE_CUBE_MAP_POSITIVE_X+yt,ae+1,0,0,ut,at,He.image[yt]):e.texImage2D(n.TEXTURE_CUBE_MAP_POSITIVE_X+yt,ae+1,ct,ut,at,He.image[yt])}}}v(w)&&w.mipmapsAutoUpdate===!0&&S(n.TEXTURE_CUBE_MAP),lt.__version=tt.version,w.onUpdate&&w.onUpdate(w)}I.__version=w.version}function ie(I,w,V,it,tt,lt){let mt=r.convert(V.format,V.colorSpace),K=r.convert(V.type),J=b(V.internalFormat,mt,K,V.normalized,V.colorSpace),ht=i.get(w),N=i.get(V);if(N.__renderTarget=w,!ht.__hasExternalTextures){let j=Math.max(1,w.width>>lt),Y=Math.max(1,w.height>>lt);tt===n.TEXTURE_3D||tt===n.TEXTURE_2D_ARRAY?e.texImage3D(tt,lt,J,j,Y,w.depth,0,mt,K,null):e.texImage2D(tt,lt,J,j,Y,0,mt,K,null)}e.bindFramebuffer(n.FRAMEBUFFER,I),dt(w)?a.framebufferTexture2DMultisampleEXT(n.FRAMEBUFFER,it,tt,N.__webglTexture,0,de(w)):(tt===n.TEXTURE_2D||tt>=n.TEXTURE_CUBE_MAP_POSITIVE_X&&tt<=n.TEXTURE_CUBE_MAP_NEGATIVE_Z)&&n.framebufferTexture2D(n.FRAMEBUFFER,it,tt,N.__webglTexture,lt),e.bindFramebuffer(n.FRAMEBUFFER,null)}function Re(I,w,V){if(n.bindRenderbuffer(n.RENDERBUFFER,I),w.depthBuffer){let it=w.depthTexture,tt=it&&it.isDepthTexture?it.type:null,lt=_(w.stencilBuffer,tt),mt=w.stencilBuffer?n.DEPTH_STENCIL_ATTACHMENT:n.DEPTH_ATTACHMENT;dt(w)?a.renderbufferStorageMultisampleEXT(n.RENDERBUFFER,de(w),lt,w.width,w.height):V?n.renderbufferStorageMultisample(n.RENDERBUFFER,de(w),lt,w.width,w.height):n.renderbufferStorage(n.RENDERBUFFER,lt,w.width,w.height),n.framebufferRenderbuffer(n.FRAMEBUFFER,mt,n.RENDERBUFFER,I)}else{let it=w.textures;for(let tt=0;tt<it.length;tt++){let lt=it[tt],mt=r.convert(lt.format,lt.colorSpace),K=r.convert(lt.type),J=b(lt.internalFormat,mt,K,lt.normalized,lt.colorSpace);dt(w)?a.renderbufferStorageMultisampleEXT(n.RENDERBUFFER,de(w),J,w.width,w.height):V?n.renderbufferStorageMultisample(n.RENDERBUFFER,de(w),J,w.width,w.height):n.renderbufferStorage(n.RENDERBUFFER,J,w.width,w.height)}}n.bindRenderbuffer(n.RENDERBUFFER,null)}function _e(I,w,V){let it=w.isWebGLCubeRenderTarget===!0;if(e.bindFramebuffer(n.FRAMEBUFFER,I),!(w.depthTexture&&w.depthTexture.isDepthTexture))throw new Error("THREE.WebGLTextures: renderTarget.depthTexture must be an instance of THREE.DepthTexture.");let tt=i.get(w.depthTexture);tt.__renderTarget=w;let lt=it?w.depthTexture.image[0]:w.depthTexture.image;(!tt.__webglTexture||lt.width!==w.width||lt.height!==w.height)&&(lt.width=w.width,lt.height=w.height,w.depthTexture.needsUpdate=!0),it?rt(w.depthTexture,0):st(w.depthTexture,0);let mt=tt.__webglTexture,K=de(w),J=it?n.TEXTURE_CUBE_MAP_POSITIVE_X+V:n.TEXTURE_2D,ht=w.depthTexture.format===xn?n.DEPTH_STENCIL_ATTACHMENT:n.DEPTH_ATTACHMENT;if(w.depthTexture.format===Li)dt(w)?a.framebufferTexture2DMultisampleEXT(n.FRAMEBUFFER,ht,J,mt,0,K):n.framebufferTexture2D(n.FRAMEBUFFER,ht,J,mt,0);else if(w.depthTexture.format===xn)dt(w)?a.framebufferTexture2DMultisampleEXT(n.FRAMEBUFFER,ht,J,mt,0,K):n.framebufferTexture2D(n.FRAMEBUFFER,ht,J,mt,0);else throw new Error("THREE.WebGLTextures: Unknown depthTexture format.")}function Ee(I){let w=i.get(I),V=I.isWebGLCubeRenderTarget===!0;if(w.__boundDepthTexture!==I.depthTexture){let it=I.depthTexture;if(w.__depthDisposeCallback&&w.__depthDisposeCallback(),it){let tt=()=>{delete w.__boundDepthTexture,delete w.__depthDisposeCallback,it.removeEventListener("dispose",tt)};it.addEventListener("dispose",tt),w.__depthDisposeCallback=tt}w.__boundDepthTexture=it}if(I.depthTexture&&!w.__autoAllocateDepthBuffer)if(V)for(let it=0;it<6;it++)_e(w.__webglFramebuffer[it],I,it);else I.texture.mipmaps.length>0?_e(w.__webglFramebuffer[0],I,0):_e(w.__webglFramebuffer,I,0);else if(V){w.__webglDepthbuffer=[];for(let it=0;it<6;it++)if(e.bindFramebuffer(n.FRAMEBUFFER,w.__webglFramebuffer[it]),w.__webglDepthbuffer[it]===void 0)w.__webglDepthbuffer[it]=n.createRenderbuffer(),Re(w.__webglDepthbuffer[it],I,!1);else{let tt=I.stencilBuffer?n.DEPTH_STENCIL_ATTACHMENT:n.DEPTH_ATTACHMENT,lt=w.__webglDepthbuffer[it];n.bindRenderbuffer(n.RENDERBUFFER,lt),n.framebufferRenderbuffer(n.FRAMEBUFFER,tt,n.RENDERBUFFER,lt)}}else if(I.texture.mipmaps.length>0?e.bindFramebuffer(n.FRAMEBUFFER,w.__webglFramebuffer[0]):e.bindFramebuffer(n.FRAMEBUFFER,w.__webglFramebuffer),w.__webglDepthbuffer===void 0)w.__webglDepthbuffer=n.createRenderbuffer(),Re(w.__webglDepthbuffer,I,!1);else{let tt=I.stencilBuffer?n.DEPTH_STENCIL_ATTACHMENT:n.DEPTH_ATTACHMENT,lt=w.__webglDepthbuffer;n.bindRenderbuffer(n.RENDERBUFFER,lt),n.framebufferRenderbuffer(n.FRAMEBUFFER,tt,n.RENDERBUFFER,lt)}e.bindFramebuffer(n.FRAMEBUFFER,null)}function fi(I,w,V){let it=i.get(I);w!==void 0&&ie(it.__webglFramebuffer,I,I.texture,n.COLOR_ATTACHMENT0,n.TEXTURE_2D,0),V!==void 0&&Ee(I)}function ri(I){let w=I.texture,V=i.get(I),it=i.get(w);I.addEventListener("dispose",R),D(I);let tt=I.textures,lt=I.isWebGLCubeRenderTarget===!0,mt=tt.length>1;if(mt||(it.__webglTexture===void 0&&(it.__webglTexture=n.createTexture()),it.__version=w.version,o.memory.textures++),lt){V.__webglFramebuffer=[];for(let K=0;K<6;K++)if(w.mipmaps.length>0){V.__webglFramebuffer[K]=[];for(let J=0;J<w.mipmaps.length;J++)V.__webglFramebuffer[K][J]=n.createFramebuffer()}else V.__webglFramebuffer[K]=n.createFramebuffer()}else{if(w.mipmaps.length>0){V.__webglFramebuffer=[];for(let K=0;K<w.mipmaps.length;K++)V.__webglFramebuffer[K]=n.createFramebuffer()}else V.__webglFramebuffer=n.createFramebuffer();if(mt)for(let K=0,J=tt.length;K<J;K++){let ht=i.get(tt[K]);ht.__webglTexture===void 0&&(ht.__webglTexture=n.createTexture(),o.memory.textures++)}if(I.samples>0&&dt(I)===!1){V.__webglMultisampledFramebuffer=n.createFramebuffer(),V.__webglColorRenderbuffer=[],e.bindFramebuffer(n.FRAMEBUFFER,V.__webglMultisampledFramebuffer);for(let K=0;K<tt.length;K++){let J=tt[K];V.__webglColorRenderbuffer[K]=n.createRenderbuffer(),n.bindRenderbuffer(n.RENDERBUFFER,V.__webglColorRenderbuffer[K]);let ht=r.convert(J.format,J.colorSpace),N=r.convert(J.type),j=b(J.internalFormat,ht,N,J.normalized,J.colorSpace,I.isXRRenderTarget===!0),Y=de(I);n.renderbufferStorageMultisample(n.RENDERBUFFER,Y,j,I.width,I.height),n.framebufferRenderbuffer(n.FRAMEBUFFER,n.COLOR_ATTACHMENT0+K,n.RENDERBUFFER,V.__webglColorRenderbuffer[K])}n.bindRenderbuffer(n.RENDERBUFFER,null),I.depthBuffer&&(V.__webglDepthRenderbuffer=n.createRenderbuffer(),Re(V.__webglDepthRenderbuffer,I,!0)),e.bindFramebuffer(n.FRAMEBUFFER,null)}}if(lt){e.bindTexture(n.TEXTURE_CUBE_MAP,it.__webglTexture),Ct(n.TEXTURE_CUBE_MAP,w);for(let K=0;K<6;K++)if(w.mipmaps.length>0)for(let J=0;J<w.mipmaps.length;J++)ie(V.__webglFramebuffer[K][J],I,w,n.COLOR_ATTACHMENT0,n.TEXTURE_CUBE_MAP_POSITIVE_X+K,J);else ie(V.__webglFramebuffer[K],I,w,n.COLOR_ATTACHMENT0,n.TEXTURE_CUBE_MAP_POSITIVE_X+K,0);v(w)?S(n.TEXTURE_CUBE_MAP):w.mipmaps.length>0&&n.texParameteri(n.TEXTURE_CUBE_MAP,n.TEXTURE_MAX_LEVEL,w.mipmaps.length-1),e.unbindTexture()}else if(mt){for(let K=0,J=tt.length;K<J;K++){let ht=tt[K],N=i.get(ht),j=n.TEXTURE_2D;(I.isWebGL3DRenderTarget||I.isWebGLArrayRenderTarget)&&(j=I.isWebGL3DRenderTarget?n.TEXTURE_3D:n.TEXTURE_2D_ARRAY),e.bindTexture(j,N.__webglTexture),Ct(j,ht),ie(V.__webglFramebuffer,I,ht,n.COLOR_ATTACHMENT0+K,j,0),v(ht)&&S(j)}e.unbindTexture()}else{let K=n.TEXTURE_2D;if((I.isWebGL3DRenderTarget||I.isWebGLArrayRenderTarget)&&(K=I.isWebGL3DRenderTarget?n.TEXTURE_3D:n.TEXTURE_2D_ARRAY),e.bindTexture(K,it.__webglTexture),Ct(K,w),w.mipmaps.length>0)for(let J=0;J<w.mipmaps.length;J++)ie(V.__webglFramebuffer[J],I,w,n.COLOR_ATTACHMENT0,K,J);else ie(V.__webglFramebuffer,I,w,n.COLOR_ATTACHMENT0,K,0);v(w)?S(K):w.mipmaps.length>0&&n.texParameteri(K,n.TEXTURE_MAX_LEVEL,w.mipmaps.length-1),e.unbindTexture()}I.depthBuffer&&Ee(I)}function O(I){let w=I.textures;for(let V=0,it=w.length;V<it;V++){let tt=w[V];if(v(tt)&&tt.mipmapsAutoUpdate===!0){let lt=T(I),mt=i.get(tt).__webglTexture;e.bindTexture(lt,mt),S(lt),e.unbindTexture()}}}let oi=[];function ue(I){if(I.samples>0){if(dt(I)===!1){let w=I.textures,V=I.width,it=I.height,tt=n.COLOR_BUFFER_BIT,lt=I.stencilBuffer?n.DEPTH_STENCIL_ATTACHMENT:n.DEPTH_ATTACHMENT,mt=i.get(I),K=w.length>1;if(K)for(let ht=0;ht<w.length;ht++)e.bindFramebuffer(n.FRAMEBUFFER,mt.__webglMultisampledFramebuffer),n.framebufferRenderbuffer(n.FRAMEBUFFER,n.COLOR_ATTACHMENT0+ht,n.RENDERBUFFER,null),e.bindFramebuffer(n.FRAMEBUFFER,mt.__webglFramebuffer),n.framebufferTexture2D(n.DRAW_FRAMEBUFFER,n.COLOR_ATTACHMENT0+ht,n.TEXTURE_2D,null,0);e.bindFramebuffer(n.READ_FRAMEBUFFER,mt.__webglMultisampledFramebuffer),I.texture.mipmaps.length>0?e.bindFramebuffer(n.DRAW_FRAMEBUFFER,mt.__webglFramebuffer[0]):e.bindFramebuffer(n.DRAW_FRAMEBUFFER,mt.__webglFramebuffer);for(let ht=0;ht<w.length;ht++){if(I.resolveDepthBuffer&&(I.depthBuffer&&(tt|=n.DEPTH_BUFFER_BIT),I.stencilBuffer&&I.resolveStencilBuffer&&(tt|=n.STENCIL_BUFFER_BIT)),K){n.framebufferRenderbuffer(n.READ_FRAMEBUFFER,n.COLOR_ATTACHMENT0,n.RENDERBUFFER,mt.__webglColorRenderbuffer[ht]);let N=i.get(w[ht]).__webglTexture;n.framebufferTexture2D(n.DRAW_FRAMEBUFFER,n.COLOR_ATTACHMENT0,n.TEXTURE_2D,N,0)}n.blitFramebuffer(0,0,V,it,0,0,V,it,tt,n.NEAREST),l===!0&&(oi.length=0,oi.push(n.COLOR_ATTACHMENT0+ht),I.depthBuffer&&I.storeMultisampledDepthBuffer===!1&&oi.push(lt),n.invalidateFramebuffer(n.READ_FRAMEBUFFER,oi))}if(e.bindFramebuffer(n.READ_FRAMEBUFFER,null),e.bindFramebuffer(n.DRAW_FRAMEBUFFER,null),K)for(let ht=0;ht<w.length;ht++){e.bindFramebuffer(n.FRAMEBUFFER,mt.__webglMultisampledFramebuffer),n.framebufferRenderbuffer(n.FRAMEBUFFER,n.COLOR_ATTACHMENT0+ht,n.RENDERBUFFER,mt.__webglColorRenderbuffer[ht]);let N=i.get(w[ht]).__webglTexture;e.bindFramebuffer(n.FRAMEBUFFER,mt.__webglFramebuffer),n.framebufferTexture2D(n.DRAW_FRAMEBUFFER,n.COLOR_ATTACHMENT0+ht,n.TEXTURE_2D,N,0)}e.bindFramebuffer(n.DRAW_FRAMEBUFFER,mt.__webglMultisampledFramebuffer)}else if(I.depthBuffer&&I.storeMultisampledDepthBuffer===!1&&l){let w=I.stencilBuffer?n.DEPTH_STENCIL_ATTACHMENT:n.DEPTH_ATTACHMENT;n.invalidateFramebuffer(n.DRAW_FRAMEBUFFER,[w])}}}function de(I){return Math.min(s.maxSamples,I.samples)}function dt(I){let w=i.get(I);return I.samples>0&&t.has("WEBGL_multisampled_render_to_texture")===!0&&w.__useRenderToTexture!==!1}function Ne(I){let w=o.render.frame;h.get(I)!==w&&(h.set(I,w),I.update())}function Ft(I,w){let V=I.colorSpace,it=I.format,tt=I.type;return I.isCompressedTexture===!0||I.isVideoTexture===!0||V!==In&&V!==ki&&(Zt.getTransfer(V)===ne?(it!==ui||tt!==ei)&&Lt("WebGLTextures: sRGB encoded textures have to use RGBAFormat and UnsignedByteType."):Ot("WebGLTextures: Unsupported texture color space:",V)),w}function kt(I){return typeof HTMLImageElement<"u"&&I instanceof HTMLImageElement?(c.width=I.naturalWidth||I.width,c.height=I.naturalHeight||I.height):typeof VideoFrame<"u"&&I instanceof VideoFrame?(c.width=I.displayWidth,c.height=I.displayHeight):(c.width=I.width,c.height=I.height),c}this.allocateTextureUnit=Mt,this.resetTextureUnits=Z,this.getTextureUnits=$,this.setTextureUnits=xt,this.setTexture2D=st,this.setTexture2DArray=_t,this.setTexture3D=wt,this.setTextureCube=rt,this.rebindTextures=fi,this.setupRenderTarget=ri,this.updateRenderTargetMipmap=O,this.updateMultisampleRenderTarget=ue,this.setupDepthRenderbuffer=Ee,this.setupFrameBufferTexture=ie,this.useMultisampledRTT=dt,this.dispose=B}function O_(n,t){function e(i,s=ki){let r,o=Zt.getTransfer(s);if(i===ei)return n.UNSIGNED_BYTE;if(i===el)return n.UNSIGNED_SHORT_4_4_4_4;if(i===il)return n.UNSIGNED_SHORT_5_5_5_1;if(i===Mh)return n.UNSIGNED_INT_5_9_9_9_REV;if(i===Sh)return n.UNSIGNED_INT_10F_11F_11F_REV;if(i===yh)return n.BYTE;if(i===vh)return n.SHORT;if(i===ir)return n.UNSIGNED_SHORT;if(i===tl)return n.INT;if(i===hi)return n.UNSIGNED_INT;if(i===gi)return n.FLOAT;if(i===xe)return n.HALF_FLOAT;if(i===bh)return n.ALPHA;if(i===wh)return n.RGB;if(i===ui)return n.RGBA;if(i===Li)return n.DEPTH_COMPONENT;if(i===xn)return n.DEPTH_STENCIL;if(i===zn)return n.RED;if(i===nl)return n.RED_INTEGER;if(i===kn)return n.RG;if(i===sl)return n.RG_INTEGER;if(i===rl)return n.RGBA_INTEGER;if(i===ao||i===lo||i===co||i===ho)if(o===ne)if(r=t.get("WEBGL_compressed_texture_s3tc_srgb"),r!==null){if(i===ao)return r.COMPRESSED_SRGB_S3TC_DXT1_EXT;if(i===lo)return r.COMPRESSED_SRGB_ALPHA_S3TC_DXT1_EXT;if(i===co)return r.COMPRESSED_SRGB_ALPHA_S3TC_DXT3_EXT;if(i===ho)return r.COMPRESSED_SRGB_ALPHA_S3TC_DXT5_EXT}else return null;else if(r=t.get("WEBGL_compressed_texture_s3tc"),r!==null){if(i===ao)return r.COMPRESSED_RGB_S3TC_DXT1_EXT;if(i===lo)return r.COMPRESSED_RGBA_S3TC_DXT1_EXT;if(i===co)return r.COMPRESSED_RGBA_S3TC_DXT3_EXT;if(i===ho)return r.COMPRESSED_RGBA_S3TC_DXT5_EXT}else return null;if(i===ol||i===al||i===ll||i===cl)if(r=t.get("WEBGL_compressed_texture_pvrtc"),r!==null){if(i===ol)return r.COMPRESSED_RGB_PVRTC_4BPPV1_IMG;if(i===al)return r.COMPRESSED_RGB_PVRTC_2BPPV1_IMG;if(i===ll)return r.COMPRESSED_RGBA_PVRTC_4BPPV1_IMG;if(i===cl)return r.COMPRESSED_RGBA_PVRTC_2BPPV1_IMG}else return null;if(i===hl||i===ul||i===dl||i===fl||i===pl||i===uo||i===ml)if(r=t.get("WEBGL_compressed_texture_etc"),r!==null){if(i===hl||i===ul)return o===ne?r.COMPRESSED_SRGB8_ETC2:r.COMPRESSED_RGB8_ETC2;if(i===dl)return o===ne?r.COMPRESSED_SRGB8_ALPHA8_ETC2_EAC:r.COMPRESSED_RGBA8_ETC2_EAC;if(i===fl)return r.COMPRESSED_R11_EAC;if(i===pl)return r.COMPRESSED_SIGNED_R11_EAC;if(i===uo)return r.COMPRESSED_RG11_EAC;if(i===ml)return r.COMPRESSED_SIGNED_RG11_EAC}else return null;if(i===gl||i===xl||i===_l||i===yl||i===vl||i===Ml||i===Sl||i===bl||i===wl||i===El||i===Tl||i===Al||i===Rl||i===Cl)if(r=t.get("WEBGL_compressed_texture_astc"),r!==null){if(i===gl)return o===ne?r.COMPRESSED_SRGB8_ALPHA8_ASTC_4x4_KHR:r.COMPRESSED_RGBA_ASTC_4x4_KHR;if(i===xl)return o===ne?r.COMPRESSED_SRGB8_ALPHA8_ASTC_5x4_KHR:r.COMPRESSED_RGBA_ASTC_5x4_KHR;if(i===_l)return o===ne?r.COMPRESSED_SRGB8_ALPHA8_ASTC_5x5_KHR:r.COMPRESSED_RGBA_ASTC_5x5_KHR;if(i===yl)return o===ne?r.COMPRESSED_SRGB8_ALPHA8_ASTC_6x5_KHR:r.COMPRESSED_RGBA_ASTC_6x5_KHR;if(i===vl)return o===ne?r.COMPRESSED_SRGB8_ALPHA8_ASTC_6x6_KHR:r.COMPRESSED_RGBA_ASTC_6x6_KHR;if(i===Ml)return o===ne?r.COMPRESSED_SRGB8_ALPHA8_ASTC_8x5_KHR:r.COMPRESSED_RGBA_ASTC_8x5_KHR;if(i===Sl)return o===ne?r.COMPRESSED_SRGB8_ALPHA8_ASTC_8x6_KHR:r.COMPRESSED_RGBA_ASTC_8x6_KHR;if(i===bl)return o===ne?r.COMPRESSED_SRGB8_ALPHA8_ASTC_8x8_KHR:r.COMPRESSED_RGBA_ASTC_8x8_KHR;if(i===wl)return o===ne?r.COMPRESSED_SRGB8_ALPHA8_ASTC_10x5_KHR:r.COMPRESSED_RGBA_ASTC_10x5_KHR;if(i===El)return o===ne?r.COMPRESSED_SRGB8_ALPHA8_ASTC_10x6_KHR:r.COMPRESSED_RGBA_ASTC_10x6_KHR;if(i===Tl)return o===ne?r.COMPRESSED_SRGB8_ALPHA8_ASTC_10x8_KHR:r.COMPRESSED_RGBA_ASTC_10x8_KHR;if(i===Al)return o===ne?r.COMPRESSED_SRGB8_ALPHA8_ASTC_10x10_KHR:r.COMPRESSED_RGBA_ASTC_10x10_KHR;if(i===Rl)return o===ne?r.COMPRESSED_SRGB8_ALPHA8_ASTC_12x10_KHR:r.COMPRESSED_RGBA_ASTC_12x10_KHR;if(i===Cl)return o===ne?r.COMPRESSED_SRGB8_ALPHA8_ASTC_12x12_KHR:r.COMPRESSED_RGBA_ASTC_12x12_KHR}else return null;if(i===Pl||i===Il||i===Dl)if(r=t.get("EXT_texture_compression_bptc"),r!==null){if(i===Pl)return o===ne?r.COMPRESSED_SRGB_ALPHA_BPTC_UNORM_EXT:r.COMPRESSED_RGBA_BPTC_UNORM_EXT;if(i===Il)return r.COMPRESSED_RGB_BPTC_SIGNED_FLOAT_EXT;if(i===Dl)return r.COMPRESSED_RGB_BPTC_UNSIGNED_FLOAT_EXT}else return null;if(i===Ll||i===Nl||i===fo||i===Ul)if(r=t.get("EXT_texture_compression_rgtc"),r!==null){if(i===Ll)return r.COMPRESSED_RED_RGTC1_EXT;if(i===Nl)return r.COMPRESSED_SIGNED_RED_RGTC1_EXT;if(i===fo)return r.COMPRESSED_RED_GREEN_RGTC2_EXT;if(i===Ul)return r.COMPRESSED_SIGNED_RED_GREEN_RGTC2_EXT}else return null;return i===us?n.UNSIGNED_INT_24_8:n[i]!==void 0?n[i]:null}return{convert:e}}var B_=`
void main() {

	gl_Position = vec4( position, 1.0 );

}`,z_=`
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

}`,Wh=class{constructor(){this.texture=null,this.mesh=null,this.depthNear=0,this.depthFar=0}init(t,e){if(this.texture===null){let i=new zr(t.texture);(t.depthNear!==e.depthNear||t.depthFar!==e.depthFar)&&(this.depthNear=t.depthNear,this.depthFar=t.depthFar),this.texture=i}}getMesh(t){if(this.texture!==null&&this.mesh===null){let e=t.cameras[0].viewport,i=new fe({vertexShader:B_,fragmentShader:z_,uniforms:{depthColor:{value:this.texture},depthWidth:{value:e.z},depthHeight:{value:e.w}}});this.mesh=new Ut(new Si(20,20),i)}return this.mesh}reset(){this.texture=null,this.mesh=null}getDepthTexture(){return this.texture}},Xh=class extends Ni{constructor(t,e){super();let i=this,s=null,r=1,o=null,a="local-floor",l=1,c=null,h=null,u=null,d=null,f=null,m=null,x=typeof XRWebGLBinding<"u",p=new Wh,g={},y=e.getContextAttributes(),v=null,S=null,T=[],b=[],_=new et,M=null,D=null,P=new Xe;P.viewport=new oe;let A=new Xe;A.viewport=new oe;let R=[P,A],U=new Ya,L=null,B=null;this.cameraAutoUpdate=!0,this.enabled=!1,this.isPresenting=!1,this.getController=function(q){let st=T[q];return st===void 0&&(st=new Vs,T[q]=st),st.getTargetRaySpace()},this.getControllerGrip=function(q){let st=T[q];return st===void 0&&(st=new Vs,T[q]=st),st.getGripSpace()},this.getHand=function(q){let st=T[q];return st===void 0&&(st=new Vs,T[q]=st),st.getHandSpace()};function H(q){let st=b.indexOf(q.inputSource);if(st===-1)return;let _t=T[st];_t!==void 0&&(_t.update(q.inputSource,q.frame,c||o),_t.dispatchEvent({type:q.type,data:q.inputSource}))}function z(){s.removeEventListener("select",H),s.removeEventListener("selectstart",H),s.removeEventListener("selectend",H),s.removeEventListener("squeeze",H),s.removeEventListener("squeezestart",H),s.removeEventListener("squeezeend",H),s.removeEventListener("end",z),s.removeEventListener("inputsourceschange",Q);for(let q=0;q<T.length;q++){let st=b[q];st!==null&&(b[q]=null,T[q].disconnect(st))}L=null,B=null,p.reset();for(let q in g)delete g[q];if(t.setRenderTarget(v),f=null,d=null,u=null,s=null,S=null,Mt.stop(),i.isPresenting=!1,t.setPixelRatio(M),t.setSize(_.width,_.height,!1),D!==null){let q=D.camera;q.fov=D.fov,q.zoom=D.zoom,q.updateProjectionMatrix(),D=null}i.dispatchEvent({type:"sessionend"})}this.setFramebufferScaleFactor=function(q){r=q,i.isPresenting===!0&&Lt("WebXRManager: Cannot change framebuffer scale while presenting.")},this.setReferenceSpaceType=function(q){a=q,i.isPresenting===!0&&Lt("WebXRManager: Cannot change reference space type while presenting.")},this.getReferenceSpace=function(){return c||o},this.setReferenceSpace=function(q){c=q},this.getBaseLayer=function(){return d!==null?d:f},this.getBinding=function(){return u===null&&x&&(u=new XRWebGLBinding(s,e)),u},this.getFrame=function(){return m},this.getSession=function(){return s},this.setSession=async function(q){if(s=q,s!==null){if(v=t.getRenderTarget(),s.addEventListener("select",H),s.addEventListener("selectstart",H),s.addEventListener("selectend",H),s.addEventListener("squeeze",H),s.addEventListener("squeezestart",H),s.addEventListener("squeezeend",H),s.addEventListener("end",z),s.addEventListener("inputsourceschange",Q),y.xrCompatible!==!0&&await e.makeXRCompatible(),M=t.getPixelRatio(),t.getSize(_),x&&"createProjectionLayer"in XRWebGLBinding.prototype){let _t=null,wt=null,rt=null;y.depth&&(rt=y.stencil?e.DEPTH24_STENCIL8:e.DEPTH_COMPONENT24,_t=y.stencil?xn:Li,wt=y.stencil?us:hi);let Pt={colorFormat:e.RGBA8,depthFormat:rt,scaleFactor:r};u=this.getBinding(),d=u.createProjectionLayer(Pt),s.updateRenderState({layers:[d]}),t.setPixelRatio(1),t.setSize(d.textureWidth,d.textureHeight,!1),S=new me(d.textureWidth,d.textureHeight,{format:ui,type:ei,depthTexture:new Oi(d.textureWidth,d.textureHeight,wt,void 0,void 0,void 0,void 0,void 0,void 0,_t),stencilBuffer:y.stencil,colorSpace:t.outputColorSpace,samples:y.antialias?4:0,resolveDepthBuffer:d.ignoreDepthValues===!1,resolveStencilBuffer:d.ignoreDepthValues===!1,storeMultisampledDepthBuffer:d.ignoreDepthValues===!1,storeMultisampledStencilBuffer:d.ignoreDepthValues===!1})}else{let _t={antialias:y.antialias,alpha:!0,depth:y.depth,stencil:y.stencil,framebufferScaleFactor:r};f=new XRWebGLLayer(s,e,_t),s.updateRenderState({baseLayer:f}),t.setPixelRatio(1),t.setSize(f.framebufferWidth,f.framebufferHeight,!1),S=new me(f.framebufferWidth,f.framebufferHeight,{format:ui,type:ei,colorSpace:t.outputColorSpace,stencilBuffer:y.stencil,resolveDepthBuffer:f.ignoreDepthValues===!1,resolveStencilBuffer:f.ignoreDepthValues===!1,storeMultisampledDepthBuffer:f.ignoreDepthValues===!1,storeMultisampledStencilBuffer:f.ignoreDepthValues===!1})}S.isXRRenderTarget=!0,this.setFoveation(l),c=null,o=await s.requestReferenceSpace(a),Mt.setContext(s),Mt.start(),i.isPresenting=!0,i.dispatchEvent({type:"sessionstart"})}},this.getEnvironmentBlendMode=function(){if(s!==null)return s.environmentBlendMode},this.getDepthTexture=function(){return p.getDepthTexture()};function Q(q){for(let st=0;st<q.removed.length;st++){let _t=q.removed[st],wt=b.indexOf(_t);wt>=0&&(b[wt]=null,T[wt].disconnect(_t))}for(let st=0;st<q.added.length;st++){let _t=q.added[st],wt=b.indexOf(_t);if(wt===-1){for(let Pt=0;Pt<T.length;Pt++)if(Pt>=b.length){b.push(_t),wt=Pt;break}else if(b[Pt]===null){b[Pt]=_t,wt=Pt;break}if(wt===-1)break}let rt=T[wt];rt&&rt.connect(_t)}}function W(q,st){st===null?q.matrixWorld.copy(q.matrix):q.matrixWorld.multiplyMatrices(st.matrixWorld,q.matrix),q.matrixWorldInverse.copy(q.matrixWorld).invert()}this.updateCamera=function(q){if(s===null)return;let st=q.near,_t=q.far;p.texture!==null&&(p.depthNear>0&&(st=p.depthNear),p.depthFar>0&&(_t=p.depthFar)),U.near=A.near=P.near=st,U.far=A.far=P.far=_t,(L!==U.near||B!==U.far)&&(s.updateRenderState({depthNear:U.near,depthFar:U.far}),L=U.near,B=U.far),U.layers.mask=q.layers.mask|6,P.layers.mask=U.layers.mask&-5,A.layers.mask=U.layers.mask&-3;let wt=q.parent,rt=U.cameras;W(U,wt);for(let Pt=0;Pt<rt.length;Pt++)W(rt[Pt],wt);U.projectionMatrix.copy(P.projectionMatrix),U.projectionMatrixInverse.copy(P.projectionMatrixInverse),D===null&&q.isPerspectiveCamera&&(D={camera:q,fov:q.fov,zoom:q.zoom}),Z(q,U,wt)};function Z(q,st,_t){_t===null?q.matrix.copy(st.matrixWorld):(q.matrix.copy(_t.matrixWorld),q.matrix.invert(),q.matrix.multiply(st.matrixWorld)),q.matrix.decompose(q.position,q.quaternion,q.scale),q.updateMatrixWorld(!0),q.projectionMatrix.copy(st.projectionMatrix),q.projectionMatrixInverse.copy(st.projectionMatrixInverse),q.isPerspectiveCamera&&(q.fov=jn*2*Math.atan(1/q.projectionMatrix.elements[5]),q.zoom=1)}this.getCamera=function(){return U},this.getFoveation=function(){if(!(d===null&&f===null))return l},this.setFoveation=function(q){l=q,d!==null&&(d.fixedFoveation=q),f!==null&&f.fixedFoveation!==void 0&&(f.fixedFoveation=q)},this.hasDepthSensing=function(){return p.texture!==null},this.getDepthSensingMesh=function(){return p.getMesh(U)},this.getCameraTexture=function(q){return g[q]};let $=null;function xt(q,st){if(h=st.getViewerPose(c||o),m=st,h!==null){let _t=h.views;f!==null&&(t.setRenderTargetFramebuffer(S,f.framebuffer),t.setRenderTarget(S));let wt=!1;_t.length!==U.cameras.length&&(U.cameras.length=0,wt=!0);for(let ot=0;ot<_t.length;ot++){let Ct=_t[ot],Bt=null;if(f!==null)Bt=f.getViewport(Ct);else{let Kt=u.getViewSubImage(d,Ct);Bt=Kt.viewport,ot===0&&(t.setRenderTargetTextures(S,Kt.colorTexture,Kt.depthStencilTexture),t.setRenderTarget(S))}let Nt=R[ot];Nt===void 0&&(Nt=new Xe,Nt.layers.enable(ot),Nt.viewport=new oe,R[ot]=Nt),Nt.matrix.fromArray(Ct.transform.matrix),Nt.matrix.decompose(Nt.position,Nt.quaternion,Nt.scale),Nt.projectionMatrix.fromArray(Ct.projectionMatrix),Nt.projectionMatrixInverse.copy(Nt.projectionMatrix).invert(),Nt.viewport.set(Bt.x,Bt.y,Bt.width,Bt.height),ot===0&&(U.matrix.copy(Nt.matrix),U.matrix.decompose(U.position,U.quaternion,U.scale)),wt===!0&&U.cameras.push(Nt)}let rt=s.enabledFeatures;if(rt&&rt.includes("depth-sensing")&&s.depthUsage=="gpu-optimized"&&x){u=i.getBinding();let ot=u.getDepthInformation(_t[0]);ot&&ot.isValid&&ot.texture&&p.init(ot,s.renderState)}if(rt&&rt.includes("camera-access")&&x){t.state.unbindTexture(),u=i.getBinding();for(let ot=0;ot<_t.length;ot++){let Ct=_t[ot].camera;if(Ct){let Bt=g[Ct];Bt||(Bt=new zr,g[Ct]=Bt);let Nt=u.getCameraImage(Ct);Bt.sourceTexture=Nt}}}}for(let _t=0;_t<T.length;_t++){let wt=b[_t],rt=T[_t];wt!==null&&rt!==void 0&&rt.update(wt,st,c||o)}$&&$(q,st),st.detectedPlanes&&i.dispatchEvent({type:"planesdetected",data:st}),m=null}let Mt=new Cf;Mt.setAnimationLoop(xt),this.setAnimationLoop=function(q){$=q},this.dispose=function(){}}},k_=new Xt,zf=new zt;zf.set(-1,0,0,0,1,0,0,0,1);function H_(n,t){function e(p,g){p.matrixAutoUpdate===!0&&p.updateMatrix(),g.value.copy(p.matrix)}function i(p,g){g.color.getRGB(p.fogColor.value,Dh(n)),g.isFog?(p.fogNear.value=g.near,p.fogFar.value=g.far):g.isFogExp2&&(p.fogDensity.value=g.density)}function s(p,g,y,v,S){g.isNodeMaterial?g.uniformsNeedUpdate=!1:g.isMeshBasicMaterial?r(p,g):g.isMeshLambertMaterial?(r(p,g),g.envMap&&(p.envMapIntensity.value=g.envMapIntensity)):g.isMeshToonMaterial?(r(p,g),u(p,g)):g.isMeshPhongMaterial?(r(p,g),h(p,g),g.envMap&&(p.envMapIntensity.value=g.envMapIntensity)):g.isMeshStandardMaterial?(r(p,g),d(p,g),g.isMeshPhysicalMaterial&&f(p,g,S)):g.isMeshMatcapMaterial?(r(p,g),m(p,g)):g.isMeshDepthMaterial?r(p,g):g.isMeshDistanceMaterial?(r(p,g),x(p,g)):g.isMeshNormalMaterial?r(p,g):g.isLineBasicMaterial?(o(p,g),g.isLineDashedMaterial&&a(p,g)):g.isPointsMaterial?l(p,g,y,v):g.isSpriteMaterial?c(p,g):g.isShadowMaterial?(p.color.value.copy(g.color),p.opacity.value=g.opacity):g.isShaderMaterial&&(g.uniformsNeedUpdate=!1)}function r(p,g){p.opacity.value=g.opacity,g.color&&p.diffuse.value.copy(g.color),g.emissive&&p.emissive.value.copy(g.emissive).multiplyScalar(g.emissiveIntensity),g.map&&(p.map.value=g.map,e(g.map,p.mapTransform)),g.alphaMap&&(p.alphaMap.value=g.alphaMap,e(g.alphaMap,p.alphaMapTransform)),g.bumpMap&&(p.bumpMap.value=g.bumpMap,e(g.bumpMap,p.bumpMapTransform),p.bumpScale.value=g.bumpScale,g.side===Ze&&(p.bumpScale.value*=-1)),g.normalMap&&(p.normalMap.value=g.normalMap,e(g.normalMap,p.normalMapTransform),p.normalScale.value.copy(g.normalScale),g.side===Ze&&p.normalScale.value.negate()),g.displacementMap&&(p.displacementMap.value=g.displacementMap,e(g.displacementMap,p.displacementMapTransform),p.displacementScale.value=g.displacementScale,p.displacementBias.value=g.displacementBias),g.emissiveMap&&(p.emissiveMap.value=g.emissiveMap,e(g.emissiveMap,p.emissiveMapTransform)),g.specularMap&&(p.specularMap.value=g.specularMap,e(g.specularMap,p.specularMapTransform)),g.alphaTest>0&&(p.alphaTest.value=g.alphaTest);let y=t.get(g),v=y.envMap,S=y.envMapRotation;v&&(p.envMap.value=v,p.envMapRotation.value.setFromMatrix4(k_.makeRotationFromEuler(S)).transpose(),v.isCubeTexture&&v.isRenderTargetTexture===!1&&p.envMapRotation.value.premultiply(zf),p.reflectivity.value=g.reflectivity,p.ior.value=g.ior,p.refractionRatio.value=g.refractionRatio),g.lightMap&&(p.lightMap.value=g.lightMap,p.lightMapIntensity.value=g.lightMapIntensity,e(g.lightMap,p.lightMapTransform)),g.aoMap&&(p.aoMap.value=g.aoMap,p.aoMapIntensity.value=g.aoMapIntensity,e(g.aoMap,p.aoMapTransform))}function o(p,g){p.diffuse.value.copy(g.color),p.opacity.value=g.opacity,g.map&&(p.map.value=g.map,e(g.map,p.mapTransform))}function a(p,g){p.dashSize.value=g.dashSize,p.totalSize.value=g.dashSize+g.gapSize,p.scale.value=g.scale}function l(p,g,y,v){p.diffuse.value.copy(g.color),p.opacity.value=g.opacity,p.size.value=g.size*y,p.scale.value=v*.5,g.map&&(p.map.value=g.map,e(g.map,p.uvTransform)),g.alphaMap&&(p.alphaMap.value=g.alphaMap,e(g.alphaMap,p.alphaMapTransform)),g.alphaTest>0&&(p.alphaTest.value=g.alphaTest)}function c(p,g){p.diffuse.value.copy(g.color),p.opacity.value=g.opacity,p.rotation.value=g.rotation,g.map&&(p.map.value=g.map,e(g.map,p.mapTransform)),g.alphaMap&&(p.alphaMap.value=g.alphaMap,e(g.alphaMap,p.alphaMapTransform)),g.alphaTest>0&&(p.alphaTest.value=g.alphaTest)}function h(p,g){p.specular.value.copy(g.specular),p.shininess.value=Math.max(g.shininess,1e-4)}function u(p,g){g.gradientMap&&(p.gradientMap.value=g.gradientMap)}function d(p,g){p.metalness.value=g.metalness,g.metalnessMap&&(p.metalnessMap.value=g.metalnessMap,e(g.metalnessMap,p.metalnessMapTransform)),p.roughness.value=g.roughness,g.roughnessMap&&(p.roughnessMap.value=g.roughnessMap,e(g.roughnessMap,p.roughnessMapTransform)),g.envMap&&(p.envMapIntensity.value=g.envMapIntensity)}function f(p,g,y){p.ior.value=g.ior,g.diffuseRoughness>0&&(p.diffuseRoughness.value=g.diffuseRoughness,g.diffuseRoughnessMap&&(p.diffuseRoughnessMap.value=g.diffuseRoughnessMap,e(g.diffuseRoughnessMap,p.diffuseRoughnessMapTransform))),g.sheen>0&&(p.sheenColor.value.copy(g.sheenColor).multiplyScalar(g.sheen),p.sheenRoughness.value=g.sheenRoughness,g.sheenColorMap&&(p.sheenColorMap.value=g.sheenColorMap,e(g.sheenColorMap,p.sheenColorMapTransform)),g.sheenRoughnessMap&&(p.sheenRoughnessMap.value=g.sheenRoughnessMap,e(g.sheenRoughnessMap,p.sheenRoughnessMapTransform))),g.clearcoat>0&&(p.clearcoat.value=g.clearcoat,p.clearcoatRoughness.value=g.clearcoatRoughness,g.clearcoatMap&&(p.clearcoatMap.value=g.clearcoatMap,e(g.clearcoatMap,p.clearcoatMapTransform)),g.clearcoatRoughnessMap&&(p.clearcoatRoughnessMap.value=g.clearcoatRoughnessMap,e(g.clearcoatRoughnessMap,p.clearcoatRoughnessMapTransform)),g.clearcoatNormalMap&&(p.clearcoatNormalMap.value=g.clearcoatNormalMap,e(g.clearcoatNormalMap,p.clearcoatNormalMapTransform),p.clearcoatNormalScale.value.copy(g.clearcoatNormalScale),g.side===Ze&&p.clearcoatNormalScale.value.negate())),g.dispersion>0&&(p.dispersion.value=g.dispersion),g.retroreflectivity>0&&(p.retroreflectivity.value=g.retroreflectivity),g.iridescence>0&&(p.iridescence.value=g.iridescence,p.iridescenceIOR.value=g.iridescenceIOR,p.iridescenceThicknessMinimum.value=g.iridescenceThicknessMinimum,p.iridescenceThicknessMaximum.value=g.iridescenceThicknessMaximum,g.iridescenceMap&&(p.iridescenceMap.value=g.iridescenceMap,e(g.iridescenceMap,p.iridescenceMapTransform)),g.iridescenceThicknessMap&&(p.iridescenceThicknessMap.value=g.iridescenceThicknessMap,e(g.iridescenceThicknessMap,p.iridescenceThicknessMapTransform))),g.transmission>0&&(p.transmission.value=g.transmission,p.transmissionSamplerMap.value=y.texture,p.transmissionSamplerSize.value.set(y.width,y.height),g.transmissionMap&&(p.transmissionMap.value=g.transmissionMap,e(g.transmissionMap,p.transmissionMapTransform)),p.thickness.value=g.thickness,g.thicknessMap&&(p.thicknessMap.value=g.thicknessMap,e(g.thicknessMap,p.thicknessMapTransform)),p.attenuationDistance.value=g.attenuationDistance,p.attenuationColor.value.copy(g.attenuationColor)),g.anisotropy>0&&(p.anisotropyVector.value.set(g.anisotropy*Math.cos(g.anisotropyRotation),g.anisotropy*Math.sin(g.anisotropyRotation)),g.anisotropyMap&&(p.anisotropyMap.value=g.anisotropyMap,e(g.anisotropyMap,p.anisotropyMapTransform))),p.specularIntensity.value=g.specularIntensity,p.specularColor.value.copy(g.specularColor),g.specularColorMap&&(p.specularColorMap.value=g.specularColorMap,e(g.specularColorMap,p.specularColorMapTransform)),g.specularIntensityMap&&(p.specularIntensityMap.value=g.specularIntensityMap,e(g.specularIntensityMap,p.specularIntensityMapTransform))}function m(p,g){g.matcap&&(p.matcap.value=g.matcap)}function x(p,g){let y=t.get(g).light;p.referencePosition.value.setFromMatrixPosition(y.matrixWorld),p.nearDistance.value=y.shadow.camera.near,p.farDistance.value=y.shadow.camera.far}return{refreshFogUniforms:i,refreshMaterialUniforms:s}}function V_(n,t,e,i){let s={},r={};function o(y,v){let S=v.program;i.uniformBlockBinding(y,S,s[y.id])}function a(y,v){let S=s[y.id];S===void 0&&(m(y),S=l(y),s[y.id]=S,y.addEventListener("dispose",p));let T=v.program;i.updateUBOMapping(y,T);let b=t.render.frame;r[y.id]!==b&&(c(y),r[y.id]=b)}function l(y){let v=n.createBuffer(),S=y.__size,T=y.usage;return n.bindBuffer(n.UNIFORM_BUFFER,v),n.bufferData(n.UNIFORM_BUFFER,S,T),n.bindBuffer(n.UNIFORM_BUFFER,null),v}function c(y){let v=s[y.id],S=y.uniforms,T=y.__cache;n.bindBuffer(n.UNIFORM_BUFFER,v);for(let b=0,_=S.length;b<_;b++){let M=S[b];if(Array.isArray(M))for(let D=0,P=M.length;D<P;D++)h(M[D],b,D,T);else h(M,b,0,T)}n.bindBuffer(n.UNIFORM_BUFFER,null)}function h(y,v,S,T){if(d(y,v,S,T)===!0){let b=y.__offset,_=y.value;if(Array.isArray(_)){let M=0;for(let D=0;D<_.length;D++){let P=_[D],A=x(P,!0);u(P,y.__data,M),M+=A.storage/Float32Array.BYTES_PER_ELEMENT}}else u(_,y.__data,0);n.bufferSubData(n.UNIFORM_BUFFER,b,y.__data)}}function u(y,v,S){typeof y=="number"||typeof y=="boolean"?v[S]=y:y.isMatrix3?(v[S]=y.elements[0],v[S+1]=y.elements[1],v[S+2]=y.elements[2],v[S+3]=0,v[S+4]=y.elements[3],v[S+5]=y.elements[4],v[S+6]=y.elements[5],v[S+7]=0,v[S+8]=y.elements[6],v[S+9]=y.elements[7],v[S+10]=y.elements[8],v[S+11]=0):ArrayBuffer.isView(y)?v.set(y.subarray(0,v.length-S),S):y.toArray(v,S)}function d(y,v,S,T){let b=y.value,_=v+"_"+S;if(Array.isArray(b)){T[_]===void 0&&(T[_]=[]);let M=T[_],D=!1;for(let P=0;P<b.length;P++)f(b[P],M,P)===!0&&(D=!0);return D}return f(b,T,_)}function f(y,v,S){if(v[S]===void 0)return typeof y=="number"||typeof y=="boolean"?v[S]=y:ArrayBuffer.isView(y)?v[S]=y.slice():v[S]=y.clone(),!0;{let T=v[S];if(typeof y=="number"||typeof y=="boolean"){if(T!==y)return v[S]=y,!0}else{if(ArrayBuffer.isView(y))return!0;if(T.equals(y)===!1)return T.copy(y),!0}}return!1}function m(y){let v=y.uniforms,S=0,T=16;for(let _=0,M=v.length;_<M;_++){let D=Array.isArray(v[_])?v[_]:[v[_]];for(let P=0,A=D.length;P<A;P++){let R=D[P],U=Array.isArray(R.value)?R.value:[R.value],L=Array.isArray(v[_])||Array.isArray(R.value),B=S;for(let z=0,Q=U.length;z<Q;z++){let W=U[z],Z=x(W,L),$=S%T,xt=$%Z.boundary,Mt=$+xt;S+=xt,Mt!==0&&T-Mt<Z.storage&&(S+=T-Mt),z===0&&(B=S),S+=Z.storage}let H=R.type==="int"?Int32Array:R.type==="uint"?Uint32Array:Float32Array;R.__data=new H((S-B)/Float32Array.BYTES_PER_ELEMENT),R.__offset=B}}let b=S%T;return b>0&&(S+=T-b),y.__size=S,y.__cache={},this}function x(y,v){let S={boundary:0,storage:0};return typeof y=="number"||typeof y=="boolean"?(S.boundary=4,S.storage=4):y.isVector2?(S.boundary=8,S.storage=8):y.isVector3||y.isColor?(S.boundary=16,S.storage=12):y.isVector4?(S.boundary=16,S.storage=16):y.isMatrix3?(S.boundary=16,S.storage=48):y.isMatrix4?(S.boundary=16,S.storage=64):y.isTexture?Lt("WebGLRenderer: Texture samplers can not be part of an uniforms group."):ArrayBuffer.isView(y)?(S.boundary=16,S.storage=y.byteLength):Lt("WebGLRenderer: Unsupported uniform value type.",y),v===!0&&(S.boundary=16,S.storage=Math.ceil(S.storage/16)*16),S}function p(y){let v=y.target;v.removeEventListener("dispose",p),n.deleteBuffer(s[v.id]),delete s[v.id],delete r[v.id]}function g(){for(let y in s)n.deleteBuffer(s[y]);s={},r={}}return{bind:o,update:a,dispose:g}}var G_=new Uint16Array([12469,15057,12620,14925,13266,14620,13807,14376,14323,13990,14545,13625,14713,13328,14840,12882,14931,12528,14996,12233,15039,11829,15066,11525,15080,11295,15085,10976,15082,10705,15073,10495,13880,14564,13898,14542,13977,14430,14158,14124,14393,13732,14556,13410,14702,12996,14814,12596,14891,12291,14937,11834,14957,11489,14958,11194,14943,10803,14921,10506,14893,10278,14858,9960,14484,14039,14487,14025,14499,13941,14524,13740,14574,13468,14654,13106,14743,12678,14818,12344,14867,11893,14889,11509,14893,11180,14881,10751,14852,10428,14812,10128,14765,9754,14712,9466,14764,13480,14764,13475,14766,13440,14766,13347,14769,13070,14786,12713,14816,12387,14844,11957,14860,11549,14868,11215,14855,10751,14825,10403,14782,10044,14729,9651,14666,9352,14599,9029,14967,12835,14966,12831,14963,12804,14954,12723,14936,12564,14917,12347,14900,11958,14886,11569,14878,11247,14859,10765,14828,10401,14784,10011,14727,9600,14660,9289,14586,8893,14508,8533,15111,12234,15110,12234,15104,12216,15092,12156,15067,12010,15028,11776,14981,11500,14942,11205,14902,10752,14861,10393,14812,9991,14752,9570,14682,9252,14603,8808,14519,8445,14431,8145,15209,11449,15208,11451,15202,11451,15190,11438,15163,11384,15117,11274,15055,10979,14994,10648,14932,10343,14871,9936,14803,9532,14729,9218,14645,8742,14556,8381,14461,8020,14365,7603,15273,10603,15272,10607,15267,10619,15256,10631,15231,10614,15182,10535,15118,10389,15042,10167,14963,9787,14883,9447,14800,9115,14710,8665,14615,8318,14514,7911,14411,7507,14279,7198,15314,9675,15313,9683,15309,9712,15298,9759,15277,9797,15229,9773,15166,9668,15084,9487,14995,9274,14898,8910,14800,8539,14697,8234,14590,7790,14479,7409,14367,7067,14178,6621,15337,8619,15337,8631,15333,8677,15325,8769,15305,8871,15264,8940,15202,8909,15119,8775,15022,8565,14916,8328,14804,8009,14688,7614,14569,7287,14448,6888,14321,6483,14088,6171,15350,7402,15350,7419,15347,7480,15340,7613,15322,7804,15287,7973,15229,8057,15148,8012,15046,7846,14933,7611,14810,7357,14682,7069,14552,6656,14421,6316,14251,5948,14007,5528,15356,5942,15356,5977,15353,6119,15348,6294,15332,6551,15302,6824,15249,7044,15171,7122,15070,7050,14949,6861,14818,6611,14679,6349,14538,6067,14398,5651,14189,5311,13935,4958,15359,4123,15359,4153,15356,4296,15353,4646,15338,5160,15311,5508,15263,5829,15188,6042,15088,6094,14966,6001,14826,5796,14678,5543,14527,5287,14377,4985,14133,4586,13869,4257,15360,1563,15360,1642,15358,2076,15354,2636,15341,3350,15317,4019,15273,4429,15203,4732,15105,4911,14981,4932,14836,4818,14679,4621,14517,4386,14359,4156,14083,3795,13808,3437,15360,122,15360,137,15358,285,15355,636,15344,1274,15322,2177,15281,2765,15215,3223,15120,3451,14995,3569,14846,3567,14681,3466,14511,3305,14344,3121,14037,2800,13753,2467,15360,0,15360,1,15359,21,15355,89,15346,253,15325,479,15287,796,15225,1148,15133,1492,15008,1749,14856,1882,14685,1886,14506,1783,14324,1608,13996,1398,13702,1183]),Ki=null;function W_(){return Ki===null&&(Ki=new Fi(G_,16,16,kn,xe),Ki.name="DFG_LUT",Ki.minFilter=ce,Ki.magFilter=ce,Ki.wrapS=Wi,Ki.wrapT=Wi,Ki.generateMipmaps=!1,Ki.needsUpdate=!0),Ki}var Bl=class{constructor(t={}){let{canvas:e=jd(),context:i=null,depth:s=!0,stencil:r=!1,alpha:o=!1,antialias:a=!1,premultipliedAlpha:l=!0,preserveDrawingBuffer:c=!1,powerPreference:h="default",failIfMajorPerformanceCaveat:u=!1,reversedDepthBuffer:d=!1,outputBufferType:f=ei}=t;this.isWebGLRenderer=!0;let m;if(i!==null){if(typeof WebGLRenderingContext<"u"&&i instanceof WebGLRenderingContext)throw new Error("THREE.WebGLRenderer: WebGL 1 is not supported since r163.");m=i.getContextAttributes().alpha}else m=o;let x=f,p=new Set([rl,sl,nl]),g=new Set([ei,hi,ir,us,el,il]),y=new Uint32Array(4),v=new Int32Array(4),S={textures:[],min:[],max:[],resolution:[]},T=null,b=null,_=[],M=[],D=null;this.domElement=e,this.debug={checkShaderErrors:!0,diagnostics:{keywords:!1},onShaderError:null},this.autoClear=!0,this.autoClearColor=!0,this.autoClearDepth=!0,this.autoClearStencil=!0,this.sortObjects=!0,this.clippingPlanes=[],this.localClippingEnabled=!1,this.toneMapping=Yi,this.toneMappingExposure=1,this.transmissionResolutionScale=1;let P=this,A=!1,R=null,U=null,L=null,B=null;this._outputColorSpace=Ie;let H=0,z=0,Q=null,W=-1,Z=null,$=new oe,xt=new oe,Mt=null,q=new bt(0),st=0,_t=e.width,wt=e.height,rt=1,Pt=null,Qt=null,ot=new oe(0,0,_t,wt),Ct=new oe(0,0,_t,wt),Bt=!1,Nt=new Dn,Kt=new Ta,Yt=!1,Se=!1,ie=new Xt,Re=new C,_e=new oe,Ee={background:null,fog:null,environment:null,overrideMaterial:null,isScene:!0},fi=!1;function ri(){return Q===null?rt:1}let O=i;function oi(E,F){return e.getContext(E,F)}let ue,de,dt,Ne,Ft,kt,I,w,V,it,tt,lt,mt,K,J,ht,N,j,Y,ut,at,ct,It;try{let E={alpha:!0,depth:s,stencil:r,antialias:a,premultipliedAlpha:l,preserveDrawingBuffer:c,powerPreference:h,failIfMajorPerformanceCaveat:u};if("setAttribute"in e&&e.setAttribute("data-engine",`three.js r${"187"}`),e.addEventListener("webglcontextlost",Dt,!1),e.addEventListener("webglcontextrestored",Wt,!1),e.addEventListener("webglcontextcreationerror",yt,!1),O===null){let F="webgl2";if(O=oi(F,E),O===null)throw oi(F)?new Error("THREE.WebGLRenderer: Error creating WebGL context with your selected attributes."):new Error("THREE.WebGLRenderer: Error creating WebGL context.")}qt()}catch(E){throw e.removeEventListener("webglcontextlost",Dt,!1),e.removeEventListener("webglcontextrestored",Wt,!1),e.removeEventListener("webglcontextcreationerror",yt,!1),Ot("WebGLRenderer: "+E.message),E}function qt(){ue=new e1(O),ue.init(),at=new O_(O,ue),de=new Wx(O,ue,t,at),dt=new U_(O,ue),de.reversedDepthBuffer&&d&&dt.buffers.depth.setReversed(!0),U=O.createFramebuffer(),L=O.createFramebuffer(),B=O.createFramebuffer(),Ne=new s1(O),Ft=new M_,kt=new F_(O,ue,dt,Ft,de,at,Ne),I=new t1(P),w=new og(O),ct=new Vx(O,w),V=new i1(O,w,Ne,ct),it=new o1(O,V,w,ct,Ne),j=new r1(O,de,kt),J=new Xx(Ft),tt=new v_(P,I,ue,de,ct,J),lt=new H_(P,Ft),mt=new b_,K=new C_(ue),N=new Hx(P,I,dt,it,m,l),ht=new N_(P,it,de),It=new V_(O,Ne,de,dt),Y=new Gx(O,ue,Ne),ut=new n1(O,ue,Ne),Ne.programs=tt.programs,P.capabilities=de,P.extensions=ue,P.properties=Ft,P.renderLists=mt,P.shadowMap=ht,P.state=dt,P.info=Ne}x!==ei&&(D=new l1(x,e.width,e.height,a,s,r));let nt=new Xh(P,O);this.xr=nt,this.getContext=function(){return O},this.getContextAttributes=function(){return O.getContextAttributes()},this.forceContextLoss=function(){let E=ue.get("WEBGL_lose_context");E&&E.loseContext()},this.forceContextRestore=function(){let E=ue.get("WEBGL_lose_context");E&&E.restoreContext()},this.getPixelRatio=function(){return rt},this.setPixelRatio=function(E){E!==void 0&&(rt=E,this.setSize(_t,wt,!1))},this.getSize=function(E){return E.set(_t,wt)},this.setSize=function(E,F,X=!0){if(nt.isPresenting){Lt("WebGLRenderer: Can't change size while VR device is presenting.");return}_t=E,wt=F,e.width=Math.floor(E*rt),e.height=Math.floor(F*rt),X===!0&&(e.style.width=E+"px",e.style.height=F+"px"),D!==null&&D.setSize(e.width,e.height),this.setViewport(0,0,E,F)},this.getDrawingBufferSize=function(E){return E.set(_t*rt,wt*rt).floor()},this.setDrawingBufferSize=function(E,F,X){_t=E,wt=F,rt=X,e.width=Math.floor(E*X),e.height=Math.floor(F*X),this.setViewport(0,0,E,F)},this.setEffects=function(E){if(x===ei){Ot("WebGLRenderer: setEffects() requires outputBufferType set to HalfFloatType or FloatType.");return}if(E){for(let F=0;F<E.length;F++)if(E[F].isOutputPass===!0){Lt("WebGLRenderer: OutputPass is not needed in setEffects(). Tone mapping and color space conversion are applied automatically.");break}}D.setEffects(E||[])},this.getCurrentViewport=function(E){return E.copy($)},this.getViewport=function(E){return E.copy(ot)},this.setViewport=function(E,F,X,k){E.isVector4?ot.set(E.x,E.y,E.z,E.w):ot.set(E,F,X,k),dt.viewport($.copy(ot).multiplyScalar(ri()).round())},this.getScissor=function(E){return E.copy(Ct)},this.setScissor=function(E,F,X,k){E.isVector4?Ct.set(E.x,E.y,E.z,E.w):Ct.set(E,F,X,k),dt.scissor(xt.copy(Ct).multiplyScalar(ri()).round())},this.getScissorTest=function(){return Bt},this.setScissorTest=function(E){dt.setScissorTest(Bt=E)},this.setOpaqueSort=function(E){Pt=E},this.setTransparentSort=function(E){Qt=E},this.getClearColor=function(E){return E.copy(N.getClearColor())},this.setClearColor=function(){N.setClearColor(...arguments)},this.getClearAlpha=function(){return N.getClearAlpha()},this.setClearAlpha=function(){N.setClearAlpha(...arguments)},this.clear=function(E=!0,F=!0,X=!0){let k=0;if(E){let G=!1;if(Q!==null){let gt=Q.texture.format;G=p.has(gt)}if(G){let gt=Q.texture.type,Et=g.has(gt),ft=N.getClearColor(),St=N.getClearAlpha(),Rt=ft.r,Ht=ft.g,$t=ft.b;Et?(y[0]=Rt,y[1]=Ht,y[2]=$t,y[3]=St,O.clearBufferuiv(O.COLOR,0,y)):(v[0]=Rt,v[1]=Ht,v[2]=$t,v[3]=St,O.clearBufferiv(O.COLOR,0,v))}else k|=O.COLOR_BUFFER_BIT}F&&(k|=O.DEPTH_BUFFER_BIT,this.state.buffers.depth.setMask(!0)),X&&(k|=O.STENCIL_BUFFER_BIT,this.state.buffers.stencil.setMask(4294967295)),k!==0&&O.clear(k)},this.clearColor=function(){this.clear(!0,!1,!1)},this.clearDepth=function(){this.clear(!1,!0,!1)},this.clearStencil=function(){this.clear(!1,!1,!0)},this.setNodesHandler=function(E){E.setRenderer(this),R=E},this.dispose=function(){e.removeEventListener("webglcontextlost",Dt,!1),e.removeEventListener("webglcontextrestored",Wt,!1),e.removeEventListener("webglcontextcreationerror",yt,!1),N.dispose(),mt.dispose(),K.dispose(),kt.dispose(),Ft.dispose(),I.dispose(),it.dispose(),V.dispose(),ct.dispose(),It.dispose(),tt.dispose(),nt.dispose(),nt.removeEventListener("sessionstart",Ru),nt.removeEventListener("sessionend",Cu),Wn.stop()};function Dt(E){E.preventDefault(),Ah("WebGLRenderer: Context Lost."),A=!0}function Wt(){Ah("WebGLRenderer: Context Restored."),A=!1;let E=Ne.autoReset,F=ht.enabled,X=ht.autoUpdate,k=ht.needsUpdate,G=ht.type;qt(),Ne.autoReset=E,ht.enabled=F,ht.autoUpdate=X,ht.needsUpdate=k,ht.type=G}function yt(E){Ot("WebGLRenderer: A WebGL context could not be created. Reason: ",E.statusMessage)}function ae(E){let F=E.target;F.removeEventListener("dispose",ae),He(F)}function He(E){bn(E),Ft.remove(E)}function bn(E){let F=Ft.get(E).programs;F!==void 0&&(F.forEach(function(X){tt.releaseProgram(X)}),E.isShaderMaterial&&tt.releaseShaderCache(E))}this.renderBufferDirect=function(E,F,X,k,G,gt){F===null&&(F=Ee);let Et=G.isMesh&&G.matrixWorld.determinantAffine()<0,ft=Jp(E,F,X,k,G);dt.setMaterial(k,Et);let St=X.index,Rt=1;if(k.wireframe===!0){if(St=V.getWireframeAttribute(X),St===void 0)return;Rt=2}let Ht=X.drawRange,$t=X.attributes.position,Tt=Ht.start*Rt,jt=(Ht.start+Ht.count)*Rt;gt!==null&&(Tt=Math.max(Tt,gt.start*Rt),jt=Math.min(jt,(gt.start+gt.count)*Rt)),St!==null?(Tt=Math.max(Tt,0),jt=Math.min(jt,St.count)):$t!=null&&(Tt=Math.max(Tt,0),jt=Math.min(jt,$t.count));let Ce=jt-Tt;if(Ce<0||Ce===1/0)return;ct.setup(G,k,ft,X,St);let ge,pe=Y;if(St!==null&&(ge=w.get(St),pe=ut,pe.setIndex(ge)),G.isMesh)k.wireframe===!0?(dt.setLineWidth(k.wireframeLinewidth*ri()),pe.setMode(O.LINES)):pe.setMode(O.TRIANGLES);else if(G.isLine){let Je=k.linewidth;Je===void 0&&(Je=1),dt.setLineWidth(Je*ri()),G.isLineSegments?pe.setMode(O.LINES):G.isLineLoop?pe.setMode(O.LINE_LOOP):pe.setMode(O.LINE_STRIP)}else G.isPoints?pe.setMode(O.POINTS):G.isSprite&&pe.setMode(O.TRIANGLES);if(G.isBatchedMesh)if(ue.get("WEBGL_multi_draw"))pe.renderMultiDraw(G._multiDrawStarts,G._multiDrawCounts,G._multiDrawCount);else{let Je=G._multiDrawStarts,At=G._multiDrawCounts,ai=G._multiDrawCount,se=St?w.get(St).bytesPerElement:1,Xn=Ft.get(k).currentProgram.getUniforms();for(let $e=0;$e<ai;$e++)Xn.setValue(O,"_gl_DrawID",$e),pe.render(Je[$e]/se,At[$e])}else if(G.isInstancedMesh)pe.renderInstances(Tt,Ce,G.count);else if(X.isInstancedBufferGeometry){let Je=X._maxInstanceCount!==void 0?X._maxInstanceCount:1/0,At=Math.min(X.instanceCount,Je);pe.renderInstances(Tt,Ce,At)}else pe.render(Tt,Ce)};function Au(E,F,X,k){R!==null&&E.isNodeMaterial&&R.setObject(k,E),Yt===!0&&J.setState(E,X,!1),E.transparent===!0&&E.side===Ue&&E.forceSinglePass===!1?(E.side=Ze,E.needsUpdate=!0,yr(E,F,k),E.side=Bn,E.needsUpdate=!0,yr(E,F,k),E.side=Ue):yr(E,F,k)}this.compile=function(E,F,X=null){X===null&&(X=E),R!==null&&R.renderStart(E,F,X),b=K.get(X),b.init(F),M.push(b),X.traverseVisible(function(G){G.isLight&&G.layers.test(F.layers)&&(b.pushLight(G),G.castShadow&&b.pushShadow(G))}),E!==X&&E.traverseVisible(function(G){G.isLight&&G.layers.test(F.layers)&&(b.pushLight(G),G.castShadow&&b.pushShadow(G))}),b.setupLights(),R!==null&&R.updateLights(b.state.lightsArray),Se=this.localClippingEnabled,Yt=J.init(this.clippingPlanes,Se),Yt===!0&&J.setGlobalState(this.clippingPlanes,F),R!==null&&ht.render(b.state.shadowsArray,X,F);let k=new Set;return E.traverse(function(G){if(!(G.isMesh||G.isPoints||G.isLine||G.isSprite))return;let gt=G.material;if(gt)if(Array.isArray(gt))for(let Et=0;Et<gt.length;Et++){let ft=gt[Et];Au(ft,X,F,G),k.add(ft)}else Au(gt,X,F,G),k.add(gt)}),b=M.pop(),R!==null&&R.renderEnd(),k},this.compileAsync=function(E,F,X=null){let k=this.compile(E,F,X);return new Promise(G=>{function gt(){if(k.forEach(function(Et){let St=Ft.get(Et).currentProgram;(St===void 0||St.isReady())&&k.delete(Et)}),k.size===0){G(E);return}setTimeout(gt,10)}ue.get("KHR_parallel_shader_compile")!==null?gt():setTimeout(gt,10)})};let Rc=null;function Zp(E){Rc&&Rc(E)}function Ru(){Wn.stop()}function Cu(){Wn.start()}let Wn=new Cf;Wn.setAnimationLoop(Zp),typeof self<"u"&&Wn.setContext(self),this.setAnimationLoop=function(E){Rc=E,nt.setAnimationLoop(E),E===null?Wn.stop():Wn.start()},nt.addEventListener("sessionstart",Ru),nt.addEventListener("sessionend",Cu),this.render=function(E,F){if(F!==void 0&&F.isCamera!==!0){Ot("WebGLRenderer.render: camera is not an instance of THREE.Camera.");return}if(A===!0)return;R!==null&&R.renderStart(E,F);let X=nt.enabled===!0&&nt.isPresenting===!0,k=D!==null&&(Q===null||X)&&D.begin(P,Q);if(E.matrixWorldAutoUpdate===!0&&E.updateMatrixWorld(),F.parent===null&&F.matrixWorldAutoUpdate===!0&&F.updateMatrixWorld(),nt.enabled===!0&&nt.isPresenting===!0&&(D===null||D.isCompositing()===!1)&&(nt.cameraAutoUpdate===!0&&nt.updateCamera(F),F=nt.getCamera()),E.isScene===!0&&E.onBeforeRender(P,E,F,Q),b=K.get(E,M.length),b.init(F),b.state.textureUnits=kt.getTextureUnits(),M.push(b),ie.multiplyMatrices(F.projectionMatrix,F.matrixWorldInverse),F.isArrayCamera?Kt.setFromArrayCamera(F):Nt.setFromProjectionMatrix(ie,Ti,F.reversedDepth),Se=this.localClippingEnabled,Yt=J.init(this.clippingPlanes,Se),T=mt.get(E,_.length),T.init(),_.push(T),this.info.render.frame++,nt.enabled===!0&&nt.isPresenting===!0){let Et=P.xr.getDepthSensingMesh();Et!==null&&Cc(Et,F,-1/0,P.sortObjects)}Cc(E,F,0,P.sortObjects),T.finish(),R!==null&&R.updateLights(b.state.lightsArray),P.sortObjects===!0&&T.sort(Pt,Qt),fi=nt.enabled===!1||nt.isPresenting===!1||nt.hasDepthSensing()===!1,fi&&N.addToRenderList(T,E),this.info.autoReset===!0&&this.info.reset(),Yt===!0&&J.beginShadows();let G=b.state.shadowsArray;if(ht.render(G,E,F),Yt===!0&&J.endShadows(),(k&&D.hasRenderPass())===!1){let Et=T.opaque,ft=T.transmissive;if(b.setupLights(),F.isArrayCamera){let St=F.cameras;if(ft.length>0)for(let Rt=0,Ht=St.length;Rt<Ht;Rt++){let $t=St[Rt];Iu(Et,ft,E,$t)}fi&&N.render(E);for(let Rt=0,Ht=St.length;Rt<Ht;Rt++){let $t=St[Rt];Pu(T,E,$t,$t.viewport)}}else ft.length>0&&Iu(Et,ft,E,F),fi&&N.render(E),Pu(T,E,F)}Q!==null&&z===0&&(kt.updateMultisampleRenderTarget(Q),kt.updateRenderTargetMipmap(Q)),k&&D.end(P),E.isScene===!0&&E.onAfterRender(P,E,F),ct.resetDefaultState(),W=-1,Z=null,M.pop(),M.length>0?(b=M[M.length-1],kt.setTextureUnits(b.state.textureUnits),Yt===!0&&J.setGlobalState(P.clippingPlanes,b.state.camera)):b=null,_.pop(),_.length>0?T=_[_.length-1]:T=null,R!==null&&R.renderEnd()};function Cc(E,F,X,k){if(E.visible===!1)return;if(E.layers.test(F.layers)){if(E.isGroup)X=E.renderOrder;else if(E.isLOD)E.autoUpdate===!0&&E.update(F);else if(E.isLight)b.pushLight(E),E.castShadow&&b.pushShadow(E);else if(E.isSprite){let Et=F.isArrayCamera?Kt:Nt;if(!E.frustumCulled||E.intersectsFrustum(Et)){k&&_e.setFromMatrixPosition(E.matrixWorld).applyMatrix4(ie);let ft=it.update(E),St=E.material;St.visible&&T.push(E,ft,St,X,_e.z,null,F)}}else if(E.isMesh||E.isLine||E.isPoints){let Et=F.isArrayCamera?Kt:Nt;if(!E.frustumCulled||E.intersectsFrustum(Et)){let ft=it.update(E),St=E.material;if(k&&(E.boundingSphere!==void 0?(E.boundingSphere===null&&E.computeBoundingSphere(),_e.copy(E.boundingSphere.center)):(ft.boundingSphere===null&&ft.computeBoundingSphere(),_e.copy(ft.boundingSphere.center)),_e.applyMatrix4(E.matrixWorld).applyMatrix4(ie)),Array.isArray(St)){let Rt=ft.groups;for(let Ht=0,$t=Rt.length;Ht<$t;Ht++){let Tt=Rt[Ht],jt=St[Tt.materialIndex];jt&&jt.visible&&T.push(E,ft,jt,X,_e.z,Tt,F)}}else St.visible&&T.push(E,ft,St,X,_e.z,null,F)}}}let gt=E.children;for(let Et=0,ft=gt.length;Et<ft;Et++)Cc(gt[Et],F,X,k)}function Pu(E,F,X,k){let{opaque:G,transmissive:gt,transparent:Et}=E;b.setupLightsView(X),Yt===!0&&J.setGlobalState(P.clippingPlanes,X),k&&dt.viewport($.copy(k)),G.length>0&&Uo(G,F,X),gt.length>0&&Uo(gt,F,X),Et.length>0&&Uo(Et,F,X),dt.buffers.depth.setTest(!0),dt.buffers.depth.setMask(!0),dt.buffers.color.setMask(!0),dt.setPolygonOffset(!1)}function Iu(E,F,X,k){if((X.isScene===!0?X.overrideMaterial:null)!==null)return;if(b.state.transmissionRenderTarget[k.id]===void 0){let Tt=ue.has("EXT_color_buffer_half_float")||ue.has("EXT_color_buffer_float");b.state.transmissionRenderTarget[k.id]=new me(1,1,{generateMipmaps:!0,type:Tt?xe:ei,minFilter:zi,samples:Math.max(4,de.samples),stencilBuffer:r,resolveDepthBuffer:!1,resolveStencilBuffer:!1,storeMultisampledDepthBuffer:!1,storeMultisampledStencilBuffer:!1,colorSpace:Zt.workingColorSpace})}let gt=b.state.transmissionRenderTarget[k.id],Et=k.viewport||$;gt.setSize(Et.z*P.transmissionResolutionScale,Et.w*P.transmissionResolutionScale);let ft=P.getRenderTarget(),St=P.getActiveCubeFace(),Rt=P.getActiveMipmapLevel();P.setRenderTarget(gt),P.getClearColor(q),st=P.getClearAlpha(),st<1&&P.setClearColor(16777215,.5),P.clear(),fi&&N.render(X);let Ht=P.toneMapping;P.toneMapping=Yi;let $t=k.viewport;if(k.viewport!==void 0&&(k.viewport=void 0),b.setupLightsView(k),Yt===!0&&J.setGlobalState(P.clippingPlanes,k),Uo(E,X,k),kt.updateMultisampleRenderTarget(gt),kt.updateRenderTargetMipmap(gt),ue.has("WEBGL_multisampled_render_to_texture")===!1){let Tt=!1;for(let jt=0,Ce=F.length;jt<Ce;jt++){let ge=F[jt],{object:pe,geometry:Je,material:At,group:ai}=ge;if(At.side===Ue&&pe.layers.test(k.layers)){let se=At.side;At.side=Ze,At.needsUpdate=!0,Du(pe,X,k,Je,At,ai),At.side=se,At.needsUpdate=!0,Tt=!0}}Tt===!0&&(kt.updateMultisampleRenderTarget(gt),kt.updateRenderTargetMipmap(gt))}P.setRenderTarget(ft,St,Rt),P.setClearColor(q,st),$t!==void 0&&(k.viewport=$t),P.toneMapping=Ht}function Uo(E,F,X){let k=F.isScene===!0?F.overrideMaterial:null;for(let G=0,gt=E.length;G<gt;G++){let Et=E[G],{object:ft,geometry:St,group:Rt}=Et,Ht=Et.material;Ht.allowOverride===!0&&k!==null&&(Ht=k),ft.layers.test(X.layers)&&Du(ft,F,X,St,Ht,Rt)}}function Du(E,F,X,k,G,gt){R!==null&&G.isNodeMaterial&&R.setObject(E,G),E.onBeforeRender(P,F,X,k,G,gt),E.modelViewMatrix.multiplyMatrices(X.matrixWorldInverse,E.matrixWorld),E.normalMatrix.getNormalMatrix(E.modelViewMatrix),G.onBeforeRender(P,F,X,k,E,gt),G.transparent===!0&&G.side===Ue&&G.forceSinglePass===!1?(G.side=Ze,G.needsUpdate=!0,P.renderBufferDirect(X,F,k,G,E,gt),G.side=Bn,G.needsUpdate=!0,P.renderBufferDirect(X,F,k,G,E,gt),G.side=Ue):P.renderBufferDirect(X,F,k,G,E,gt),E.onAfterRender(P,F,X,k,G,gt)}function Lu(E){let F=0;for(let X of E)X.texture!==null&&F++;return F}function yr(E,F,X){F.isScene!==!0&&(F=Ee);let k=Ft.get(E),G=b.state.lights,gt=b.state.shadowsArray,Et=G.state.version,ft=tt.getParameters(E,G.state,gt,F,X,b.state.lightProbeGridArray),St=tt.getProgramCacheKey(ft),Rt=k.programs;k.environment=E.isMeshStandardMaterial||E.isMeshLambertMaterial||E.isMeshPhongMaterial?F.environment:null,k.fog=F.fog;let Ht=E.isMeshStandardMaterial||E.isMeshLambertMaterial&&!E.envMap||E.isMeshPhongMaterial&&!E.envMap;k.envMap=I.get(E.envMap||k.environment,Ht),k.envMapRotation=k.environment!==null&&E.envMap===null?F.environmentRotation:E.envMapRotation,Rt===void 0&&(E.addEventListener("dispose",ae),Rt=new Map,k.programs=Rt,k.programVariants=new Map);let $t=Rt.get(St);if($t!==void 0){if(k.currentProgram===$t&&k.lightsStateVersion===Et)return Uu(E,ft),$t}else ft.uniforms=tt.getUniforms(E),R!==null&&E.isNodeMaterial&&R.build(E,X,ft),E.onBeforeCompile(ft,P),$t=tt.acquireProgram(ft,St),Rt.set(St,$t),k.uniforms=ft.uniforms;let Tt=k.uniforms;return(!E.isShaderMaterial&&!E.isRawShaderMaterial||E.clipping===!0)&&(Tt.clippingPlanes=J.uniform),Uu(E,ft),k.needsLights=jp(E),k.lightsStateVersion=Et,k.needsLights&&(Tt.ambientLightColor.value=G.state.ambient,Tt.lightProbe.value=G.state.probe,Tt.sunLights.value=G.state.sun,Tt.sunLightShadows.value=G.state.sunShadow,Tt.directionalLights.value=G.state.directional,Tt.directionalLightShadows.value=G.state.directionalShadow,Tt.spotLights.value=G.state.spot,Tt.spotLightShadows.value=G.state.spotShadow,Tt.rectAreaLights.value=G.state.rectArea,Tt.ltc_1.value=G.state.rectAreaLTC1,Tt.ltc_2.value=G.state.rectAreaLTC2,Tt.pointLights.value=G.state.point,Tt.pointLightShadows.value=G.state.pointShadow,Tt.hemisphereLights.value=G.state.hemi,Tt.sunShadowMatrix.value=G.state.sunShadowMatrix,Tt.sunShadowCascade.value=G.state.sunShadowCascade,Tt.directionalShadowMatrix.value=G.state.directionalShadowMatrix,Tt.spotLightMatrix.value=G.state.spotLightMatrix,Tt.spotLightMap.value=G.state.spotLightMap,Tt.pointShadowMatrix.value=G.state.pointShadowMatrix),k.numLightProbeGrids=Lu(b.state.lightProbeGridArray),k.currentProgram=$t,k.uniformsList=null,$t}function Kp(E,F,X,k,G){let gt=Ft.get(E),Et=gt.currentProgram.id+","+k+","+G,ft=gt.programVariants.get(Et);return ft===void 0?(ft=yr(E,F,X),gt.programVariants.set(Et,ft)):(gt.currentProgram=ft,gt.uniformsList=null,gt.outputColorSpace=k,gt.toneMapping=G),ft}function Nu(E){if(E.uniformsList===null){let F=E.currentProgram.getUniforms();E.uniformsList=or.seqWithValue(F.seq,E.uniforms)}return E.uniformsList}function Uu(E,F){let X=Ft.get(E);X.outputColorSpace=F.outputColorSpace,X.batching=F.batching,X.batchingColor=F.batchingColor,X.instancing=F.instancing,X.instancingColor=F.instancingColor,X.instancingMorph=F.instancingMorph,X.skinning=F.skinning,X.morphTargets=F.morphTargets,X.morphNormals=F.morphNormals,X.morphColors=F.morphColors,X.morphTargetsCount=F.morphTargetsCount,X.numClippingPlanes=F.numClippingPlanes,X.numIntersection=F.numClipIntersection,X.vertexAlphas=F.vertexAlphas,X.vertexTangents=F.vertexTangents,X.toneMapping=F.toneMapping}function Jp(E,F,X,k,G){F.isScene!==!0&&(F=Ee),kt.resetTextureUnits();let gt=F.fog,Et=k.isMeshStandardMaterial||k.isMeshLambertMaterial||k.isMeshPhongMaterial?F.environment:null,ft=Q===null?P.outputColorSpace:Q.isXRRenderTarget===!0?Q.texture.colorSpace:Zt.workingColorSpace,St=k.isMeshStandardMaterial||k.isMeshLambertMaterial&&!k.envMap||k.isMeshPhongMaterial&&!k.envMap,Rt=I.get(k.envMap||Et,St),Ht=k.vertexColors===!0&&!!X.attributes.color&&X.attributes.color.itemSize===4,$t=!!X.attributes.tangent&&(!!k.normalMap||k.anisotropy>0),Tt=!!X.morphAttributes.position,jt=!!X.morphAttributes.normal,Ce=!!X.morphAttributes.color,ge=Yi;k.toneMapped&&(Q===null||Q.isXRRenderTarget===!0)&&(ge=P.toneMapping);let pe=X.morphAttributes.position||X.morphAttributes.normal||X.morphAttributes.color,Je=pe!==void 0?pe.length:0,At=Ft.get(k),ai=b.state.lights;if(Yt===!0&&(Se===!0||E!==Z)){let be=E===Z&&k.id===W;J.setState(k,E,be)}let se=!1,Xn=!1;k.version===At.__version?At.needsLights&&At.lightsStateVersion!==ai.state.version||G.isBatchedMesh&&At.batching===!1||!G.isBatchedMesh&&At.batching===!0||G.isBatchedMesh&&At.batchingColor===!0&&G._colorsTexture===null||G.isBatchedMesh&&At.batchingColor===!1&&G._colorsTexture!==null||G.isInstancedMesh&&At.instancing===!1||!G.isInstancedMesh&&At.instancing===!0||G.isSkinnedMesh&&At.skinning===!1||!G.isSkinnedMesh&&At.skinning===!0||G.isInstancedMesh&&At.instancingColor===!0&&G.instanceColor===null||G.isInstancedMesh&&At.instancingColor===!1&&G.instanceColor!==null||G.isInstancedMesh&&At.instancingMorph===!0&&G.morphTexture===null||G.isInstancedMesh&&At.instancingMorph===!1&&G.morphTexture!==null||At.envMap!==Rt||k.fog===!0&&At.fog!==gt||At.numClippingPlanes!==void 0&&(At.numClippingPlanes!==J.numPlanes||At.numIntersection!==J.numIntersection)||At.vertexAlphas!==Ht||At.vertexTangents!==$t||At.morphTargets!==Tt||At.morphNormals!==jt||At.morphColors!==Ce||At.morphTargetsCount!==Je||At.numLightProbeGrids!==Lu(b.state.lightProbeGridArray)?se=!0:(At.outputColorSpace!==ft||At.toneMapping!==ge)&&(Xn=!0):(se=!0,At.__version=k.version);let $e=At.currentProgram;(se===!0||Xn===!0)&&($e=Xn===!0?Kp(k,F,G,ft,ge):yr(k,F,G),R&&k.isNodeMaterial&&R.onUpdateProgram(k,$e,At));let Fo=!1,wn=!1,qn=!1,te=$e.getUniforms(),We=At.uniforms;if(dt.useProgram($e.program)&&(Fo=!0,wn=!0,qn=!0),k.id!==W&&(W=k.id,wn=!0),Fo||Z!==E){dt.buffers.depth.getReversed()&&E.reversedDepth!==!0&&(E._reversedDepth=!0,E.updateProjectionMatrix()),te.setValue(O,"projectionMatrix",E.projectionMatrix),te.setValue(O,"viewMatrix",E.matrixWorldInverse);let Ve=te.map.cameraPosition;Ve!==void 0&&Ve.setValue(O,Re.setFromMatrixPosition(E.matrixWorld)),de.logarithmicDepthBuffer&&te.setValue(O,"logDepthBufFC",2/(Math.log(E.far+1)/Math.LN2)),(k.isMeshPhongMaterial||k.isMeshToonMaterial||k.isMeshLambertMaterial||k.isMeshBasicMaterial||k.isMeshStandardMaterial||k.isShaderMaterial)&&te.setValue(O,"isOrthographic",E.isOrthographicCamera===!0),Z!==E&&(Z=E,wn=!0,qn=!0)}if(At.needsLights&&(ai.state.sunShadowMap.length>0&&te.setValue(O,"sunShadowMap",ai.state.sunShadowMap,kt),ai.state.directionalShadowMap.length>0&&te.setValue(O,"directionalShadowMap",ai.state.directionalShadowMap,kt),ai.state.spotShadowMap.length>0&&te.setValue(O,"spotShadowMap",ai.state.spotShadowMap,kt),ai.state.pointShadowMap.length>0&&te.setValue(O,"pointShadowMap",ai.state.pointShadowMap,kt)),G.isSkinnedMesh){te.setOptional(O,G,"bindMatrix"),te.setOptional(O,G,"bindMatrixInverse");let be=G.skeleton;be&&(be.boneTexture===null&&be.computeBoneTexture(),te.setValue(O,"boneTexture",be.boneTexture,kt))}G.isBatchedMesh&&(te.setOptional(O,G,"batchingTexture"),te.setValue(O,"batchingTexture",G._matricesTexture,kt),te.setOptional(O,G,"batchingIdTexture"),te.setValue(O,"batchingIdTexture",G._indirectTexture,kt),te.setOptional(O,G,"batchingColorTexture"),G._colorsTexture!==null&&te.setValue(O,"batchingColorTexture",G._colorsTexture,kt));let Ci=X.morphAttributes;if((Ci.position!==void 0||Ci.normal!==void 0||Ci.color!==void 0)&&j.update(G,X,$e),(wn||At.receiveShadow!==G.receiveShadow)&&(At.receiveShadow=G.receiveShadow,te.setValue(O,"receiveShadow",G.receiveShadow)),(k.isMeshStandardMaterial||k.isMeshLambertMaterial||k.isMeshPhongMaterial)&&k.envMap===null&&F.environment!==null&&(We.envMapIntensity.value=F.environmentIntensity),We.dfgLUT!==void 0&&(We.dfgLUT.value=W_()),wn){if(te.setValue(O,"toneMappingExposure",P.toneMappingExposure),At.needsLights&&$p(We,qn),gt&&k.fog===!0&&lt.refreshFogUniforms(We,gt),lt.refreshMaterialUniforms(We,k,rt,wt,b.state.transmissionRenderTarget[E.id]),At.needsLights&&At.numLightProbeGrids>0){let be=S,Ve=0;for(let en of b.state.lightProbeGridArray)en.texture!==null&&(be.textures[Ve]=en.texture,be.min[Ve]=en.boundingBox.min,be.max[Ve]=en.boundingBox.max,be.resolution[Ve]=en.resolution,Ve++);for(let en in be)be[en].length=Ve;We.probesSH.value=be.textures,We.probesMin.value=be.min,We.probesMax.value=be.max,We.probesResolution.value=be.resolution}or.upload(O,Nu(At),We,kt)}if(k.isShaderMaterial&&k.uniformsNeedUpdate===!0&&(or.upload(O,Nu(At),We,kt),k.uniformsNeedUpdate=!1),k.isSpriteMaterial&&te.setValue(O,"center",G.center),te.setValue(O,"modelViewMatrix",G.modelViewMatrix),te.setValue(O,"normalMatrix",G.normalMatrix),te.setValue(O,"modelMatrix",G.matrixWorld),k.uniformsGroups!==void 0){let be=k.uniformsGroups;for(let Ve=0,en=be.length;Ve<en;Ve++){let Ou=be[Ve];It.update(Ou,$e),It.bind(Ou,$e)}}return $e}function $p(E,F){E.ambientLightColor.needsUpdate=F,E.lightProbe.needsUpdate=F,E.sunLights.needsUpdate=F,E.sunLightShadows.needsUpdate=F,E.directionalLights.needsUpdate=F,E.directionalLightShadows.needsUpdate=F,E.pointLights.needsUpdate=F,E.pointLightShadows.needsUpdate=F,E.spotLights.needsUpdate=F,E.spotLightShadows.needsUpdate=F,E.rectAreaLights.needsUpdate=F,E.hemisphereLights.needsUpdate=F}function jp(E){return E.isMeshLambertMaterial||E.isMeshToonMaterial||E.isMeshPhongMaterial||E.isMeshStandardMaterial||E.isShadowMaterial||E.isShaderMaterial&&E.lights===!0}this.getActiveCubeFace=function(){return H},this.getActiveMipmapLevel=function(){return z},this.getRenderTarget=function(){return Q},this.setRenderTargetTextures=function(E,F,X){let k=Ft.get(E);k.__autoAllocateDepthBuffer=E.resolveDepthBuffer===!1,k.__autoAllocateDepthBuffer===!1&&(k.__useRenderToTexture=!1),Ft.get(E.texture).__webglTexture=F,Ft.get(E.depthTexture).__webglTexture=k.__autoAllocateDepthBuffer?void 0:X,k.__hasExternalTextures=!0},this.setRenderTargetFramebuffer=function(E,F){let X=Ft.get(E);X.__webglFramebuffer=F,X.__useDefaultFramebuffer=F===void 0},this.setRenderTarget=function(E,F=0,X=0){Q=E,H=F,z=X;let k=null,G=!1,gt=!1;if(E){let ft=Ft.get(E);if(ft.__useDefaultFramebuffer!==void 0){dt.bindFramebuffer(O.FRAMEBUFFER,ft.__webglFramebuffer),$.copy(E.viewport),xt.copy(E.scissor),Mt=E.scissorTest,dt.viewport($),dt.scissor(xt),dt.setScissorTest(Mt),W=-1;return}else if(ft.__webglFramebuffer===void 0)kt.setupRenderTarget(E);else if(ft.__hasExternalTextures)kt.rebindTextures(E,Ft.get(E.texture).__webglTexture,Ft.get(E.depthTexture).__webglTexture);else if(E.depthBuffer){let Ht=E.depthTexture;if(ft.__boundDepthTexture!==Ht){if(Ht!==null&&Ft.has(Ht)&&(E.width!==Ht.image.width||E.height!==Ht.image.height))throw new Error("THREE.WebGLRenderer: Attached DepthTexture is initialized to the incorrect size.");kt.setupDepthRenderbuffer(E)}}let St=E.texture;(St.isData3DTexture||St.isDataArrayTexture||St.isCompressedArrayTexture)&&(gt=!0);let Rt=Ft.get(E).__webglFramebuffer;E.isWebGLCubeRenderTarget?(Array.isArray(Rt[F])?k=Rt[F][X]:k=Rt[F],G=!0):E.samples>0&&kt.useMultisampledRTT(E)===!1?k=Ft.get(E).__webglMultisampledFramebuffer:Array.isArray(Rt)?k=Rt[X]:k=Rt,$.copy(E.viewport),xt.copy(E.scissor),Mt=E.scissorTest}else $.copy(ot).multiplyScalar(rt).floor(),xt.copy(Ct).multiplyScalar(rt).floor(),Mt=Bt;if(X!==0&&(k=U),dt.bindFramebuffer(O.FRAMEBUFFER,k)&&dt.drawBuffers(E,k),dt.viewport($),dt.scissor(xt),dt.setScissorTest(Mt),G){let ft=Ft.get(E.texture);O.framebufferTexture2D(O.FRAMEBUFFER,O.COLOR_ATTACHMENT0,O.TEXTURE_CUBE_MAP_POSITIVE_X+F,ft.__webglTexture,X)}else if(gt){let ft=F;for(let St=0;St<E.textures.length;St++){let Rt=Ft.get(E.textures[St]);O.framebufferTextureLayer(O.FRAMEBUFFER,O.COLOR_ATTACHMENT0+St,Rt.__webglTexture,X,ft)}}else if(E!==null&&X!==0){let ft=Ft.get(E.texture);O.framebufferTexture2D(O.FRAMEBUFFER,O.COLOR_ATTACHMENT0,O.TEXTURE_2D,ft.__webglTexture,X)}W=-1};function Fu(E){let F=Ft.get(E);return(F.__readFormat!==E.format||F.__readType!==E.type)&&(F.__readFormat=E.format,F.__readType=E.type,F.__formatReadable=de.textureFormatReadable(E.format),F.__typeReadable=de.textureTypeReadable(E.type)),F}this.readRenderTargetPixels=function(E,F,X,k,G,gt,Et,ft=0){if(!(E&&E.isWebGLRenderTarget)){Ot("WebGLRenderer.readRenderTargetPixels: renderTarget is not THREE.WebGLRenderTarget.");return}let St=Ft.get(E).__webglFramebuffer;if(E.isWebGLCubeRenderTarget&&Et!==void 0&&(St=St[Et],Array.isArray(St)&&(St=St[0])),St){dt.bindFramebuffer(O.FRAMEBUFFER,St);try{let Rt=E.textures[ft],Ht=Rt.format,$t=Rt.type;E.textures.length>1&&O.readBuffer(O.COLOR_ATTACHMENT0+ft);let Tt=Fu(Rt);if(Tt.__formatReadable===!1){Ot("WebGLRenderer.readRenderTargetPixels: renderTarget is not in RGBA or implementation defined format.");return}if(Tt.__typeReadable===!1){Ot("WebGLRenderer.readRenderTargetPixels: renderTarget is not in UnsignedByteType or implementation defined type.");return}F>=0&&F<=E.width-k&&X>=0&&X<=E.height-G&&O.readPixels(F,X,k,G,at.convert(Ht),at.convert($t),gt)}finally{let Rt=Q!==null?Ft.get(Q).__webglFramebuffer:null;dt.bindFramebuffer(O.FRAMEBUFFER,Rt)}}},this.readRenderTargetPixelsAsync=async function(E,F,X,k,G,gt,Et,ft=0){if(!(E&&E.isWebGLRenderTarget))throw new Error("THREE.WebGLRenderer.readRenderTargetPixels: renderTarget is not THREE.WebGLRenderTarget.");let St=Ft.get(E).__webglFramebuffer;if(E.isWebGLCubeRenderTarget&&Et!==void 0&&(St=St[Et],Array.isArray(St)&&(St=St[0])),St)if(F>=0&&F<=E.width-k&&X>=0&&X<=E.height-G){dt.bindFramebuffer(O.FRAMEBUFFER,St);let Rt=E.textures[ft],Ht=Rt.format,$t=Rt.type;E.textures.length>1&&O.readBuffer(O.COLOR_ATTACHMENT0+ft);let Tt=Fu(Rt);if(Tt.__formatReadable===!1)throw new Error("THREE.WebGLRenderer.readRenderTargetPixelsAsync: renderTarget is not in RGBA or implementation defined format.");if(Tt.__typeReadable===!1)throw new Error("THREE.WebGLRenderer.readRenderTargetPixelsAsync: renderTarget is not in UnsignedByteType or implementation defined type.");let jt=O.createBuffer();O.bindBuffer(O.PIXEL_PACK_BUFFER,jt),O.bufferData(O.PIXEL_PACK_BUFFER,gt.byteLength,O.STREAM_READ),O.readPixels(F,X,k,G,at.convert(Ht),at.convert($t),0),O.bindBuffer(O.PIXEL_PACK_BUFFER,null);let Ce=Q!==null?Ft.get(Q).__webglFramebuffer:null;dt.bindFramebuffer(O.FRAMEBUFFER,Ce);let ge=O.fenceSync(O.SYNC_GPU_COMMANDS_COMPLETE,0);return O.flush(),await tf(O,ge,4),O.bindBuffer(O.PIXEL_PACK_BUFFER,jt),O.getBufferSubData(O.PIXEL_PACK_BUFFER,0,gt),O.bindBuffer(O.PIXEL_PACK_BUFFER,null),O.deleteBuffer(jt),O.deleteSync(ge),gt}else throw new Error("THREE.WebGLRenderer.readRenderTargetPixelsAsync: requested read bounds are out of range.")},this.copyFramebufferToTexture=function(E,F=null,X=0){let k=Math.pow(2,-X),G=Math.floor(E.image.width*k),gt=Math.floor(E.image.height*k),Et=F!==null?F.x:0,ft=F!==null?F.y:0;kt.setTexture2D(E,0),O.copyTexSubImage2D(O.TEXTURE_2D,X,0,0,Et,ft,G,gt),dt.unbindTexture()},this.copyTextureToTexture=function(E,F,X=null,k=null,G=0,gt=0){let Et,ft,St,Rt,Ht,$t,Tt,jt,Ce,ge=E.isCompressedTexture?E.mipmaps[gt]:E.image;if(X!==null)Et=X.max.x-X.min.x,ft=X.max.y-X.min.y,St=X.isBox3?X.max.z-X.min.z:1,Rt=X.min.x,Ht=X.min.y,$t=X.isBox3?X.min.z:0;else{let te=Math.pow(2,-G);Et=Math.floor(ge.width*te),ft=Math.floor(ge.height*te),E.isDataArrayTexture?St=ge.depth:E.isData3DTexture?St=Math.floor(ge.depth*te):St=1,Rt=0,Ht=0,$t=0}k!==null?(Tt=k.x,jt=k.y,Ce=k.z):(Tt=0,jt=0,Ce=0);let pe=at.convert(F.format),Je=at.convert(F.type),At;F.isData3DTexture?(kt.setTexture3D(F,0),At=O.TEXTURE_3D):F.isDataArrayTexture||F.isCompressedArrayTexture?(kt.setTexture2DArray(F,0),At=O.TEXTURE_2D_ARRAY):(kt.setTexture2D(F,0),At=O.TEXTURE_2D),dt.activeTexture(O.TEXTURE0),dt.pixelStorei(O.UNPACK_FLIP_Y_WEBGL,F.flipY),dt.pixelStorei(O.UNPACK_PREMULTIPLY_ALPHA_WEBGL,F.premultiplyAlpha),dt.pixelStorei(O.UNPACK_ALIGNMENT,F.unpackAlignment);let ai=dt.getParameter(O.UNPACK_ROW_LENGTH),se=dt.getParameter(O.UNPACK_IMAGE_HEIGHT),Xn=dt.getParameter(O.UNPACK_SKIP_PIXELS),$e=dt.getParameter(O.UNPACK_SKIP_ROWS),Fo=dt.getParameter(O.UNPACK_SKIP_IMAGES);dt.pixelStorei(O.UNPACK_ROW_LENGTH,ge.width),dt.pixelStorei(O.UNPACK_IMAGE_HEIGHT,ge.height),dt.pixelStorei(O.UNPACK_SKIP_PIXELS,Rt),dt.pixelStorei(O.UNPACK_SKIP_ROWS,Ht),dt.pixelStorei(O.UNPACK_SKIP_IMAGES,$t);let wn=E.isDataArrayTexture||E.isData3DTexture,qn=F.isDataArrayTexture||F.isData3DTexture;if(E.isDepthTexture){let te=Ft.get(E),We=Ft.get(F),Ci=Ft.get(te.__renderTarget),be=Ft.get(We.__renderTarget);dt.bindFramebuffer(O.READ_FRAMEBUFFER,Ci.__webglFramebuffer),dt.bindFramebuffer(O.DRAW_FRAMEBUFFER,be.__webglFramebuffer);for(let Ve=0;Ve<St;Ve++)wn&&(O.framebufferTextureLayer(O.READ_FRAMEBUFFER,O.COLOR_ATTACHMENT0,Ft.get(E).__webglTexture,G,$t+Ve),O.framebufferTextureLayer(O.DRAW_FRAMEBUFFER,O.COLOR_ATTACHMENT0,Ft.get(F).__webglTexture,gt,Ce+Ve)),O.blitFramebuffer(Rt,Ht,Rt+Et,Ht+ft,Tt,jt,Tt+Et,jt+ft,O.DEPTH_BUFFER_BIT,O.NEAREST);dt.bindFramebuffer(O.READ_FRAMEBUFFER,null),dt.bindFramebuffer(O.DRAW_FRAMEBUFFER,null)}else if(G!==0||E.isRenderTargetTexture||Ft.has(E)){let te=Ft.get(E),We=Ft.get(F);dt.bindFramebuffer(O.READ_FRAMEBUFFER,L),dt.bindFramebuffer(O.DRAW_FRAMEBUFFER,B);for(let Ci=0;Ci<St;Ci++)wn?O.framebufferTextureLayer(O.READ_FRAMEBUFFER,O.COLOR_ATTACHMENT0,te.__webglTexture,G,$t+Ci):O.framebufferTexture2D(O.READ_FRAMEBUFFER,O.COLOR_ATTACHMENT0,O.TEXTURE_2D,te.__webglTexture,G),qn?O.framebufferTextureLayer(O.DRAW_FRAMEBUFFER,O.COLOR_ATTACHMENT0,We.__webglTexture,gt,Ce+Ci):O.framebufferTexture2D(O.DRAW_FRAMEBUFFER,O.COLOR_ATTACHMENT0,O.TEXTURE_2D,We.__webglTexture,gt),G!==0?O.blitFramebuffer(Rt,Ht,Rt+Et,Ht+ft,Tt,jt,Tt+Et,jt+ft,O.COLOR_BUFFER_BIT,O.NEAREST):qn?O.copyTexSubImage3D(At,gt,Tt,jt,Ce+Ci,Rt,Ht,Et,ft):O.copyTexSubImage2D(At,gt,Tt,jt,Rt,Ht,Et,ft);dt.bindFramebuffer(O.READ_FRAMEBUFFER,null),dt.bindFramebuffer(O.DRAW_FRAMEBUFFER,null)}else qn?E.isDataTexture||E.isData3DTexture?O.texSubImage3D(At,gt,Tt,jt,Ce,Et,ft,St,pe,Je,ge.data):F.isCompressedArrayTexture?O.compressedTexSubImage3D(At,gt,Tt,jt,Ce,Et,ft,St,pe,ge.data):O.texSubImage3D(At,gt,Tt,jt,Ce,Et,ft,St,pe,Je,ge):E.isDataTexture?O.texSubImage2D(O.TEXTURE_2D,gt,Tt,jt,Et,ft,pe,Je,ge.data):E.isCompressedTexture?O.compressedTexSubImage2D(O.TEXTURE_2D,gt,Tt,jt,ge.width,ge.height,pe,ge.data):O.texSubImage2D(O.TEXTURE_2D,gt,Tt,jt,Et,ft,pe,Je,ge);dt.pixelStorei(O.UNPACK_ROW_LENGTH,ai),dt.pixelStorei(O.UNPACK_IMAGE_HEIGHT,se),dt.pixelStorei(O.UNPACK_SKIP_PIXELS,Xn),dt.pixelStorei(O.UNPACK_SKIP_ROWS,$e),dt.pixelStorei(O.UNPACK_SKIP_IMAGES,Fo),gt===0&&F.generateMipmaps===!0&&F.mipmapsAutoUpdate===!0&&O.generateMipmap(At),dt.unbindTexture()},this.initRenderTarget=function(E){Ft.get(E).__webglFramebuffer===void 0&&kt.setupRenderTarget(E)},this.initTexture=function(E){E.isCubeTexture?kt.setTextureCube(E,0):E.isData3DTexture?kt.setTexture3D(E,0):E.isDataArrayTexture||E.isCompressedArrayTexture?kt.setTexture2DArray(E,0):kt.setTexture2D(E,0),dt.unbindTexture()},this.resetState=function(){H=0,z=0,Q=null,dt.reset(),ct.reset()},typeof __THREE_DEVTOOLS__<"u"&&__THREE_DEVTOOLS__.dispatchEvent(new CustomEvent("observe",{detail:this}))}get coordinateSystem(){return Ti}get outputColorSpace(){return this._outputColorSpace}set outputColorSpace(t){this._outputColorSpace=t;let e=this.getContext();e.drawingBufferColorSpace=Zt._getDrawingBufferColorSpace(t),e.unpackColorSpace=Zt._getUnpackColorSpace()}};var X_=[15432046,9231257,9285362];function Yh(n){if(!n?.center)return[];let t=[{position:n.center,kind:"centre"}];for(let e=0;e<3;e++){let i=(n.max[e]-n.min[e])/2,s=Math.max(.32,i),r=[...n.center];r[e]+=s,t.push({position:r,kind:"stretch",axis:e,extent:i})}return t}var cr=class{constructor(){this.group=new ve,this.key=null}clear(){for(let t of this.group.children)t.geometry.dispose(),t.material.dispose();this.group.clear()}update(t){if(!t){this.group.children.length&&this.clear(),this.key=null;return}if(t.key===this.key)return;this.key=t.key,this.clear();let e=(s,r,o=1)=>{if(!s?.length)return;let a=new Float32Array(s.length*3);s.forEach((h,u)=>a.set(h,u*3));let l=new Jt;l.setAttribute("position",new le(a,3));let c=new vi({color:r,opacity:o,transparent:o<1,depthTest:!1,toneMapped:!1});this.group.add(new fn(l,c))},i=new bt().fromArray(t.color||[.4,.9,.8]);if(e(t.outer,i,.28),e(t.surface,i,.9),t["handles?"]!==!1)for(let{position:s,kind:r,axis:o}of Yh(t)){let a=r==="centre",l=a?16767920:X_[o];a||e([t.center,s],l);let c=a?new pn(.11,12,8):new Te(.13,.13,.13),h=new Ut(c,new pi({color:l,depthTest:!1,toneMapped:!1}));h.position.fromArray(s),this.group.add(h)}}dispose(){this.clear(),this.key=null}};var Ke=(n,t,e)=>Math.max(t,Math.min(e,n)),Hl=n=>n*Math.PI/180,yo=n=>Array.isArray(n)&&n.length===3&&n.every(Number.isFinite);function kf(n){return[n?.width,n?.height,n?.depth].map((t,e)=>Number.isFinite(t)&&t>0?t:[12,5,10][e])}function Zh(n){let t=Math.hypot(...n);return t>1e-9?n.map(e=>e/t):[0,-1,0]}function Hn(n,t){let e=n[t]??n.channels?.[t];return Number.isFinite(e)&&e>=0&&e<=1?e:void 0}function yn(n,t,e){let i=e+"-deg";if(Number.isFinite(t.pose?.[i]))return t.pose[i];let s=Hn(t,e);if(s===void 0&&Number.isFinite(t[i]))return t[i];let r=s??0,o=n.calibration?.[e];if(Array.isArray(o)&&o.length===2&&o[1]!==0)return(r*255-o[0])/o[1]*180;let a=o?.["min-deg"]??(e==="pan"?-180:-90),l=o?.["max-deg"]??(e==="pan"?180:90);return a+r*(l-a)}function Vn(n,t=[0,0,0]){let[e,i,s]=n;for(let r=0;r<3;r++){let o=Hl(Number(t[r])||0),a=Math.cos(o),l=Math.sin(o);r===0&&([i,s]=[i*a-s*l,i*l+s*a]),r===1&&([e,s]=[e*a+s*l,-e*l+s*a]),r===2&&([e,i]=[e*a-i*l,e*l+i*a])}return[e,i,s]}function Kh(n,t={}){if(yo(t.pose?.direction))return t.pose.direction;if(!(Hn(t,"pan")!==void 0&&Hn(t,"tilt")!==void 0||Number.isFinite(t.pose?.["pan-deg"])&&Number.isFinite(t.pose?.["tilt-deg"]))){if(yo(t.direction)&&Math.hypot(...t.direction)>1e-9)return Zh(t.direction);if(yo(t.aim)&&yo(n.position))return Zh(t.aim.map((r,o)=>r-n.position[o]))}let i=Hl(yn(n,t,"pan")),s=Hl(yn(n,t,"tilt"));return Zh(Vn([Math.sin(i)*Math.cos(s),Math.sin(s),Math.cos(i)*Math.cos(s)],n["rotation-deg"]))}function fs(n={}){return yo(n["emitted-color"])?n["emitted-color"].map(t=>Ke(t,0,8)):[0,0,0]}function Jh(n,t={}){let e=n["zoom-range-deg"],i=Ke(Number(t.zoom??t.channels?.zoom)||0,0,1),s=o=>Number.isFinite(o)&&o>0&&o<180,r=Array.isArray(e)&&e.length===2&&e.every(s)?e[0]+i*(e[1]-e[0]):s(n["beam-angle"])?n["beam-angle"]:12+i*34;return Hl(r/2)}function Hf(n){let t=n?.fixtures||[];return JSON.stringify({room:n?.room,surfaces:n?.surfaces,reflections:t.filter(e=>e["pseudo-fixture"]).length,fixtures:t.map(e=>({id:e.id,type:e["fixture-type"],aperture:e["lens-radius-m"],diffuser:e["diffuser-radius-m"]&&[e["diffuser-radius-m"],e["pixel-layout-id"],e["pixel-index"],e.position]})),trusses:(n?.trusses||[]).map(e=>({id:e.id,length:e.length}))})}var q_=(n,t,e,i)=>{n.updateMatrixWorld();let s=new qi;return s.setFromCamera(new et((e-t.left)*2/t.width-1,1-(i-t.top)*2/t.height),n),s.ray};function Y_(n){let t=Number(n?.width)||12,e=Number(n?.height)||5,i=Number(n?.depth)||10;return{id:"ceiling",point:[0,e,0],normal:[0,-1,0],"u-axis":[1,0,0],size:[t,i]}}function Z_(n,t){let e=new C().fromArray(t.point),i=new C().fromArray(t.normal).normalize(),s=i.dot(n.direction);if(s>=-1e-8)return null;let r=e.clone().sub(n.origin).dot(i)/s;if(!(r>0))return null;let o=n.at(r,new C),a=o.clone().sub(e),l=new C().fromArray(t["u-axis"]).projectOnPlane(i).normalize(),c=new C().crossVectors(i,l);return Math.abs(a.dot(l))>t.size[0]/2+1e-8||Math.abs(a.dot(c))>t.size[1]/2+1e-8?null:{distance:r,point:o.toArray(),normal:i.toArray(),kind:"surface","surface-id":t.id}}function K_(n,t,e){let i=Math.min(40,Number(t.length)||1)/2,s=new C(...Vn([1,0,0],t["rotation-deg"])),r=new C().fromArray(t.position),o=n.origin.clone().sub(r),a=n.direction.dot(s),l=n.direction.dot(o),c=s.dot(o),h=1-a*a,u=Math.max(-i,Math.min(i,h>1e-9?(c-a*l)/h:c)),d=r.clone().addScaledVector(s,u),f=n.direction.dot(d.clone().sub(n.origin)),x=n.at(Math.max(0,f),new C).distanceTo(d);return!(f>0)||x>e?null:{distance:f-e,point:d.toArray(),normal:Vn([0,-1,0],t["rotation-deg"]),kind:"truss","truss-id":String(t.id)}}function Vf(n,t,e,i,s,r=.25){if(!(e.width>0&&e.height>0))return null;let o=q_(t,e,i,s),a=[];for(let u of[...n?.["receiver-surfaces"]||[],Y_(n?.rig?.room)]){let d=Z_(o,u);d&&a.push(d)}for(let u of(n?.rig?.trusses||[]).slice(0,24)){if(!Array.isArray(u.position)||u.position.length!==3)continue;let d=K_(o,u,r);d&&a.push(d)}let l=a.sort((u,d)=>u.distance-d.distance)[0];if(!l)return null;let{distance:c,...h}=l;return h}var Gf=n=>({forward:Vn([0,0,1],n),up:Vn([0,1,0],n)});var vo=new Set(["aim","place","calibrate"]),So=n=>({xz:1,xy:2,yz:0})[n]??1,J_=n=>JSON.stringify([n?.["runtime-id"],n?.["show-id"],n?.epoch]),Ae=n=>Array.isArray(n)&&n.length===3&&n.every(t=>Number.isFinite(t)&&Math.abs(t)<=1e4);function hr(n,t,e,i,s="xz",r=0,o=0){if(!(t.width>0&&t.height>0)||!Number.isFinite(r))return null;n.updateMatrixWorld();let a=new qi;a.setFromCamera(new et(2*(e-t.left)/t.width-1,1-2*(i-t.top)/t.height),n);let l=So(s),c=new C().setComponent(l,1),h=new C;if(!a.ray.intersectPlane(new li(c,-r),h))return null;let u=h.toArray().map((d,f)=>f===l?r:o>0?Math.round(d/o)*o:d);return Ae(u)?u:null}function $_(n,t,e,i,s){if(!(e.width>0&&e.height>0))return null;t.updateMatrixWorld();let r=new qi;r.setFromCamera(new et((i-e.left)*2/e.width-1,1-(s-e.top)*2/e.height),t);let{origin:o,direction:a}=r.ray,l=1/0,c=null;for(let h of n?.["receiver-surfaces"]||[]){let u=new C().fromArray(h.point),d=new C().fromArray(h.normal).normalize(),f=d.dot(a);if(f>=-1e-8)continue;let m=u.clone().sub(o).dot(d)/f;if(m<=0||m>=l)continue;let x=r.ray.at(m,new C),p=x.clone().sub(u),g=new C().fromArray(h["u-axis"]).projectOnPlane(d).normalize(),y=new C().crossVectors(d,g);Math.abs(p.dot(g))<=h.size[0]/2+1e-8&&Math.abs(p.dot(y))<=h.size[1]/2+1e-8&&(l=m,c={point:x.toArray(),"surface-id":h.id})}return c}function Mo(n,t,e){if(!Ae(e))return null;let i=new C(...e).project(n);return i.z>=-1&&i.z<=1?{x:t.left+(i.x+1)*t.width/2,y:t.top+(1-i.y)*t.height/2,depth:i.z}:null}var j_=(n,t,e,i)=>{let s=i.x-e.x,r=i.y-e.y,o=Math.max(0,Math.min(1,((n-e.x)*s+(t-e.y)*r)/(s*s+r*r||1)));return Math.hypot(n-e.x-o*s,t-e.y-o*r)};function Q_(n,t,e,i,s){t.updateMatrixWorld();let r=n?.editor||{},o=[],a=(l,c,h=24)=>{let u=Mo(t,e,l);if(!u)return;let d=Math.hypot(u.x-i,u.y-s);d<=h&&o.push({...c,position:l,score:d/h+u.depth*.001})};if(r.tool==="sculpt-volume")for(let l of Yh(n["sculpt-guide"]))a(l.position,l,22);else if(r.tool==="volume-select")for(let l of n["spatial-volumes"]||[]){let c=n["volume-guides"]?.[l.id],h=(c?.["moving?"]?l.signal?.position||l.position:c?.center)||l.signal?.position||l.position||(Ae(l.min)&&Ae(l.max)?l.min.map((u,d)=>(u+l.max[d])/2):null);h&&a(h,{kind:"volumes",id:l.id},32)}else if(r.tool==="pixels"&&r["pixel-mode"]==="pixel"){let l=r["pixel-indices"]||[];(r["pixel-positions"]||[]).forEach((c,h)=>a(c,{kind:"pixels",index:l[h]},22)),a(r["pixel-position"],{kind:"pixels",index:r["selected-pixel"]},22)}else r.tool==="draw"||r.tool==="pixels"?(r.points||[]).slice(0,256).forEach((l,c)=>a(l,{kind:"points",index:r["point-indices"]?.[c]??c},22)):((n?.rig?.fixtures||[]).slice(0,2048).filter(l=>l["pixel-layout-id"]==null).forEach(l=>a(l.position,{kind:"fixtures",id:String(l.id)})),(n?.rig?.trusses||[]).slice(0,24).forEach(l=>{if(!Ae(l.position))return;let c=Vn([Math.min(40,l.length||1)/2,0,0],l["rotation-deg"]),h=Mo(t,e,l.position.map((d,f)=>d-c[f])),u=Mo(t,e,l.position.map((d,f)=>d+c[f]));if(h&&u){let d=j_(i,s,h,u);d<=18&&o.push({kind:"trusses",id:String(l.id),position:l.position,score:d/18+h.depth*.001+.2})}}));return o.sort((l,c)=>l.score-c.score)[0]||null}var Vl=class{constructor(t){this.engine=t,this.canvas=t.canvas,this.abort=new AbortController,this.sequence=0;let e={signal:this.abort.signal,capture:!0};this.canvas.addEventListener("pointerdown",i=>this.down(i),e),this.canvas.addEventListener("pointermove",i=>this.move(i),e),this.canvas.addEventListener("pointerup",i=>this.finish(i,!1),e),this.canvas.addEventListener("pointercancel",i=>this.finish(i,!0),e),this.canvas.addEventListener("lostpointercapture",i=>this.finish(i,!0),e),this.canvas.addEventListener("keydown",i=>{i.key==="Escape"&&this.snapshot?.editor&&(this.cancel(),this.emit("navigate"))},e),globalThis.addEventListener?.("blur",()=>this.cancel(),{signal:this.abort.signal}),globalThis.document?.addEventListener("visibilitychange",()=>{document.hidden&&this.cancel()},{signal:this.abort.signal})}emit(t,e={}){this.canvas.dispatchEvent(new CustomEvent("tolglow-editor",{detail:{...e,type:t,identity:this.identity,token:this.gesture?.token}}))}update(t){let e=J_(t),i=t?.editor,s=this.snapshot?.editor;(this.identity!==e||i?.tool!==s?.tool||i?.["layout-id"]!==s?.["layout-id"]||i?.["pixel-mode"]!==s?.["pixel-mode"]||i?.plane!==s?.plane)&&this.cancel(),this.identity=e,this.snapshot=t;let r=!!(t?.editor&&t.editor.tool!=="navigate");r&&!this.interacting?(this.previousInteraction=!!this.engine.getNavigationState?.().interacting,this.engine.setInteractionActive?.(!0)):!r&&this.interacting&&this.engine.setInteractionActive?.(this.previousInteraction),this.interacting=r,this.canvas.style.cursor=r?["draw","pixels",...vo].includes(t.editor.tool)?"crosshair":"grab":""}down(t){let e=this.snapshot?.editor;if(vo.has(this.gesture?.tool)&&t.pointerId!==this.gesture.pointer){this.gesture.dragged=!0,t.preventDefault(),t.stopImmediatePropagation();return}if(!e||e.tool==="navigate"||t.button!==0||this.gesture)return;if(t.preventDefault(),t.stopImmediatePropagation(),this.canvas.focus(),vo.has(e.tool)){this.gesture={tool:e.tool,token:++this.sequence,pointer:t.pointerId,x:t.clientX,y:t.clientY,dragged:!1},this.canvas.setPointerCapture(t.pointerId);return}if(e.tool==="throw"){let a=hr(this.engine.camera,this.canvas.getBoundingClientRect(),t.clientX,t.clientY,e.plane,e["plane-value"],e.snap);if(!a){this.emit("unreachable");return}this.gesture={tool:"throw",token:++this.sequence,pointer:t.pointerId,point:a,start:t.timeStamp,x:t.clientX,y:t.clientY,plane:e.plane,value:e["plane-value"]},this.canvas.setPointerCapture(t.pointerId);return}let i=this.canvas.getBoundingClientRect(),s=Q_(this.snapshot,this.engine.camera,i,t.clientX,t.clientY);if(e.tool==="sculpt-volume"){if(!s)return;let a=s.position[So(e.plane)],l=hr(this.engine.camera,i,t.clientX,t.clientY,e.plane,a,e.snap),c=Mo(this.engine.camera,i,s.position),h=[...s.position];s.axis!=null&&(h[s.axis]+=1);let u=Mo(this.engine.camera,i,h);if(!l||!c||!u){this.emit("unreachable");return}let d=u.x-c.x,f=u.y-c.y;if(s.kind==="stretch"&&d*d+f*f<4){this.emit("unreachable");return}this.gesture={token:++this.sequence,tool:e.tool,pointer:t.pointerId,x:t.clientX,y:t.clientY,axis:s.axis,extent:s.extent,value:a,plane:e.plane,snap:e.snap,dx:d,dy:f},this.canvas.setPointerCapture(t.pointerId),this.emit("sculpt-begin",{point:l});return}if(e.tool==="volume-select"){s&&this.emit("select-volume",{"volume-id":s.id});return}if(e.tool==="pixels"&&!s&&!e["append?"])return;if(e.tool!=="draw"&&e.tool!=="pixels"&&!s){this.emit("select",{selection:null});return}let r=s?s.position[So(e.plane)]:e["plane-value"],o=hr(this.engine.camera,i,t.clientX,t.clientY,e.plane,r,e.snap);if(!o){this.emit("unreachable");return}this.gesture={token:++this.sequence,pointer:t.pointerId,hit:s,point:o,x:t.clientX,value:r,tool:e.tool,offset:s?s.position.map((a,l)=>a-o[l]):[0,0,0]},this.canvas.setPointerCapture(t.pointerId),this.emit("begin",{selection:s,point:o})}move(t){let e=this.gesture;if(!e||t.pointerId!==e.pointer||(t.preventDefault(),t.stopImmediatePropagation(),e.tool==="throw"))return;if(e.tool==="sculpt-volume"){if(e.axis!=null){let r=((t.clientX-e.x)*e.dx+(t.clientY-e.y)*e.dy)/(e.dx*e.dx+e.dy*e.dy),o=e.snap>0?Math.round(r/e.snap)*e.snap:r;this.emit("sculpt-move",{axis:e.axis,delta:o,"half-extent":e.extent})}else{let r=hr(this.engine.camera,this.canvas.getBoundingClientRect(),t.clientX,t.clientY,e.plane,e.value,e.snap);r&&this.emit("sculpt-move",{point:r})}return}if(vo.has(e.tool)){Math.hypot(t.clientX-e.x,t.clientY-e.y)>10&&(e.dragged=!0);return}let i=this.snapshot.editor,s=hr(this.engine.camera,this.canvas.getBoundingClientRect(),t.clientX,t.clientY,i.plane,e.value,i.snap);s&&this.emit("move",{point:e.hit&&["move","draw","pixels"].includes(e.tool)?s.map((r,o)=>r+e.offset[o]):s,angle:(t.clientX-e.x)*.5})}finish(t,e){if(!this.gesture||t.pointerId!==this.gesture.pointer)return;t.preventDefault(),t.stopImmediatePropagation();let i=this.gesture;if(vo.has(i.tool)){if(!e&&!i.dragged&&Math.hypot(t.clientX-i.x,t.clientY-i.y)<=10){let r=this.canvas.getBoundingClientRect();if(i.tool==="place"){let o=Vf(this.snapshot,this.engine.camera,r,t.clientX,t.clientY);this.emit(o?"place":"missed-place",o||{})}else{let o=$_(this.snapshot,this.engine.camera,r,t.clientX,t.clientY);this.emit(o?"surface":"missed-surface",o||{})}}}else if(i.tool==="throw"){if(!e&&Math.hypot(t.clientX-i.x,t.clientY-i.y)>=8){let r=hr(this.engine.camera,this.canvas.getBoundingClientRect(),t.clientX,t.clientY,i.plane,i.value);if(r){let o=Math.max(.08,(t.timeStamp-i.start)/1e3),a=r.map((c,h)=>(c-i.point[h])/o),l=Math.min(1,50/(Math.hypot(...a)||1));this.emit("throw",{point:i.point,velocity:a.map(c=>c*l)})}}}else i.tool==="sculpt-volume"?this.emit(e?"sculpt-cancel":"sculpt-end"):this.emit(e?"cancel":"end");let s=i.pointer;this.gesture=null,this.canvas.hasPointerCapture?.(s)&&this.canvas.releasePointerCapture(s)}cancel(){if(!this.gesture)return;this.emit(this.gesture.tool==="sculpt-volume"?"sculpt-cancel":"cancel");let t=this.gesture.pointer;this.gesture=null,this.canvas.hasPointerCapture?.(t)&&this.canvas.releasePointerCapture(t)}dispose(){this.cancel(),this.abort.abort(),this.interacting&&this.engine.setInteractionActive?.(this.previousInteraction),this.interacting=!1,this.canvas.style.cursor=""}};function ty(n){return Ae(n.signal?.position)?n.signal.position:Ae(n.position)?n.position:n.shape==="box"&&Ae(n.min)&&Ae(n.max)?n.min.map((t,e)=>(t+n.max[e])/2):null}function ey(n){let t=ty(n),e=n.signal?.radius??n.radius;if(!t)return null;if(n.shape==="box"&&Ae(n.min)&&Ae(n.max))return{position:t,scale:n.max.map((r,o)=>Math.max(0,r-n.min[o]))};if(!["ball","sphere","cylinder"].includes(n.shape)||!Number.isFinite(e)||e<=0)return null;if(n.shape==="cylinder")return{position:t,scale:[e,n.height,e]};let i=Math.min(.5,Math.max(0,n.signal?.deformation||0)),s=1/Math.sqrt(1-i);return{position:t,scale:[e*s,e*(1-i),e*s]}}function iy(n){if(n==="box"||n==="cylinder"){let e=n==="box"?new Te(1,1,1):new mi(1,1,1,40),i=new ns(e);return e.dispose(),i}let t=[];for(let e=0;e<3;e++)for(let i=0;i<48;i++)for(let s of[i,i+1]){let r=[0,0,0];r[(e+1)%3]=Math.cos(s*Math.PI/24),r[(e+2)%3]=Math.sin(s*Math.PI/24),t.push(new C(...r))}return new Jt().setFromPoints(t)}var Gl=class{constructor(){this.group=new ve,this.byId=new Map,this.fields=new Map}remove(t){let e=this.byId.get(t);e.geometry.dispose(),e.material.dispose(),this.group.remove(e),this.byId.delete(t)}update(t=[],e){let i=new Set,s=new Set;for(let r of t){let o=e?.[r.id];if(o){let c=this.fields.get(r.id);c||(c=new cr,this.fields.set(r.id,c),this.group.add(c.group)),c.update(o),o["moving?"]&&c.group.position.fromArray(r.signal?.position||r.position),s.add(r.id);continue}let a=ey(r);if(!a)continue;i.add(r.id);let l=this.byId.get(r.id);l&&l.userData.shape!==r.shape&&(this.remove(r.id),l=null),l||(l=new fn(iy(r.shape),new vi({color:r.shape==="ball"?16760184:9427403,transparent:!0,opacity:.84,depthTest:!1,toneMapped:!1})),l.userData.shape=r.shape,this.group.add(l),this.byId.set(r.id,l)),l.position.fromArray(a.position),l.scale.fromArray(a.scale),Ae(r.signal?.rotation)&&l.rotation.set(...r.signal.rotation)}for(let r of this.byId.keys())i.has(r)||this.remove(r);for(let[r,o]of this.fields)s.has(r)||(o.dispose(),this.group.remove(o.group),this.fields.delete(r))}dispose(){this.update([])}};function ny(n){return`${n.columns}x${n.rows}|${n.outline===!1?"closed":"open"}`}function sy(n,t){let e=n+1,i=[];for(let s=0;s<=n;s++)i.push(s);for(let s=1;s<=t;s++)i.push(s*e+n);for(let s=n-1;s>=0;s--)i.push(t*e+s);for(let s=t-1;s>0;s--)i.push(s*e);return i}function ry(n){return!Array.isArray(n)||n.length<3?[0,0,0,0]:[...n.slice(0,3).map(t=>{let e=Math.min(1,Math.max(0,Number(t)||0));return e<=.0031308?12.92*e:1.055*Math.pow(e,1/2.4)-.055}),1]}function oy(n){let t=new Float32Array(n.length*3);return n.forEach((e,i)=>{let s=.35+.65*Math.min(1,Math.max(0,Number(e)||0));t.set([s,s*.86,s*.32],i*3)}),t}function Wl(n,t){return Array.isArray(n)&&n.length%t===0&&n.every(Number.isFinite)}function ay(){return new fe({transparent:!0,depthTest:!1,depthWrite:!1,toneMapped:!1,uniforms:{size:{value:16}},vertexShader:`attribute vec3 markerColor; attribute vec4 headColor; varying vec3 vColor; varying vec4 vHead;
      uniform float size;
      void main() { vColor = markerColor; vHead = headColor;
        gl_Position = projectionMatrix * modelViewMatrix * vec4(position, 1.0); gl_PointSize = size; }`,fragmentShader:`varying vec3 vColor; varying vec4 vHead;
      void main() { float r = length(gl_PointCoord - .5) * 2.; if (r > 1. || (r <= .62 && vHead.a < .5)) discard;
        gl_FragColor = vec4(r > .62 ? vColor : vHead.rgb, 1.); }`})}async function ly(n){let t=await fetch(n,{cache:"no-store"});if(!t.ok)throw new Error(`Video frame ${t.status}`);return createImageBitmap(await t.blob())}var Xl=class{constructor(t=ly){this.group=new ve,this.byId=new Map,this.textures=new Map,this.load=t}texture(t){if(!t)return null;let e=this.textures.get(t);return e||(e={texture:null,users:0,failed:!1},this.textures.set(t,e),this.load(t).then(i=>{if(this.textures.get(t)!==e){i.close?.();return}let s=new qe(i);s.flipY=!1,s.colorSpace=Ie,s.needsUpdate=!0,e.texture=s;for(let r of this.byId.values())r.wantUrl===t&&this.show(r,t,s);this.releaseTextures()}).catch(()=>{e.failed=!0})),e}show(t,e,i){t.shownUrl=e,t.mesh.material.map=i,t.mesh.material.color.set(16777215),t.mesh.material.needsUpdate=!0}releaseTextures(){let t=new Set;for(let e of this.byId.values())t.add(e.wantUrl),t.add(e.shownUrl);for(let[e,i]of this.textures)t.has(e)||(i.texture?.image?.close?.(),i.texture?.dispose(),this.textures.delete(e))}remove(t){let e=this.byId.get(t);for(let i of[e.mesh,e.edge,e.markers])i.geometry.dispose(),i.material.dispose();this.group.remove(e.root),this.byId.delete(t)}build(t){let e=new ve,i=new Ut(new Jt,new pi({color:5792622,side:Ue,transparent:!0,opacity:.5,toneMapped:!1,depthWrite:!1})),s=new Fr(new Jt,new vi({color:9427403,transparent:!0,opacity:.7,toneMapped:!1})),r=new is(new Jt,ay());return e.add(i,s,r),this.group.add(e),{root:e,mesh:i,edge:s,markers:r,wantUrl:null,shownUrl:null}}shape(t,e){let i=ny(e),s=e.positions.length/3;if(t.key!==i||t.mesh.geometry.getAttribute("position")?.count!==s){t.key=i,t.mesh.geometry.dispose(),t.edge.geometry.dispose();let c=t.mesh.geometry=new Jt;c.setAttribute("position",new le(new Float32Array(s*3),3)),c.setAttribute("uv",new le(new Float32Array(s*2),2)),c.setIndex(e.indices),t.border=e.outline===!1?[]:sy(e.columns,e.rows),t.edge.geometry=new Jt,t.edge.geometry.setAttribute("position",new le(new Float32Array(t.border.length*3),3))}let r=t.mesh.geometry,o=r.getAttribute("position"),a=r.getAttribute("uv");o.array.set(e.positions),a.array.set(e.uvs),o.needsUpdate=a.needsUpdate=!0,r.computeBoundingSphere();let l=t.edge.geometry.getAttribute("position");t.border.forEach((c,h)=>l.array.set(e.positions.slice(c*3,c*3+3),h*3)),l.needsUpdate=!0,t.edge.geometry.computeBoundingSphere()}update(t=[],e=null){let i=new Set;for(let s of t||[]){if(!s||!Wl(s.positions,3)||!Wl(s.uvs,2))continue;let r=String(s.id);i.add(r);let o=this.byId.get(r);if(o||(o=this.build(s),this.byId.set(r,o)),o.positions!==s.positions&&(o.positions=s.positions,this.shape(o,s)),o.contacts!==s.contacts){o.contacts=s.contacts;let a=(s.contacts||[]).filter(u=>Wl(u,4)),l=o.markers.geometry;l.getAttribute("position")?.count!==a.length&&(l.dispose(),l=o.markers.geometry=new Jt,l.setAttribute("position",new le(new Float32Array(a.length*3),3)),l.setAttribute("markerColor",new le(new Float32Array(a.length*3),3)),l.setAttribute("headColor",new le(new Float32Array(a.length*4),4)));let c=l.getAttribute("position"),h=l.getAttribute("markerColor");a.forEach((u,d)=>c.array.set(u.slice(0,3),d*3)),h.array.set(oy(a.map(u=>u[3]))),c.needsUpdate=h.needsUpdate=!0,l.computeBoundingSphere(),o.ids=(s["contact-ids"]||[]).filter((u,d)=>Wl(s.contacts[d],4)),o.colored=null}if(o.colored!==e){o.colored=e;let a=o.markers.geometry.getAttribute("headColor");(o.ids||[]).forEach((l,c)=>a.array.set(ry(e?.[l]?.["emitted-color"]),c*4)),a.needsUpdate=!0}if(o.wantUrl!==s.texture){o.wantUrl=s.texture;let a=this.texture(s.texture);a?.texture?this.show(o,s.texture,a.texture):s.texture||(o.shownUrl=null,o.mesh.material.map=null,o.mesh.material.color.set(5792622),o.mesh.material.needsUpdate=!0)}}for(let s of[...this.byId.keys()])i.has(s)||this.remove(s);this.releaseTextures()}dispose(){this.update([])}};var ql=class{constructor(){this.scene=new Ye,this.rig=new ve,this.volumes=new Gl,this.sculpt=new cr,this.videoSurfaces=new Xl,this.scene.add(this.rig,this.volumes.group,this.sculpt.group,this.videoSurfaces.group),this.key=null}clear(){this.rig.traverse(t=>{t.geometry?.dispose(),t.material?.dispose()}),this.rig.clear()}update(t){this.volumes.update(t?.["spatial-volumes"],t?.["volume-guides"]),this.sculpt.update(t?.["sculpt-guide"]),this.videoSurfaces.update(t?.["video-surfaces"],t?.output?.values);let e=t?.editor,i=e?.selection;if(!e){this.key!==null&&(this.clear(),this.key=null);return}this.markerRig!==t.rig&&(this.markerRig=t.rig,this.markers=(t.rig?.fixtures||[]).slice(0,2048).filter(c=>c["pixel-layout-id"]==null).map(c=>c.position).filter(Ae),this.key=null);let s=t?.rig?.[i?.kind]?.find(c=>String(c.id)===String(i.id)),r=this.markers;if(this.key===e&&this.item===s)return;this.key=e,this.item=s,this.clear();let o=(c,h,u=1)=>{let d=new Jt().setFromPoints(c.map(f=>new C(...f)));this.rig.add(new es(d,new vi({color:h,transparent:u<1,opacity:u,depthTest:!1,toneMapped:!1})))},a=(c,h,u)=>{if(!c.length)return;let d=new Jt().setFromPoints(c.map(f=>new C(...f)));this.rig.add(new is(d,new Ws({color:h,size:u,sizeAttenuation:!1,depthTest:!1,toneMapped:!1})))};a(r,10663625,6),e.tool==="pixels"&&(a(e["pixel-positions"]||[],7923154,5),e["pixel-mode"]==="pixel"&&Ae(e["pixel-position"])&&a([e["pixel-position"]],16763264,12));let l=(e.points||[]).filter(Ae).slice(0,256);if(l.length>1&&o(e["closed?"]?[...l,l[0]]:l,11979177),e["pixel-mode"]!=="pixel"&&l.forEach((c,h)=>{let u=new Ut(new pn(h===e["selected-point"]?.11:.075,10,6),new pi({color:h===e["selected-point"]?16763264:11979177,depthTest:!1,toneMapped:!1}));u.position.fromArray(c),this.rig.add(u)}),s&&Ae(s.position)){let c=new Te(i.kind==="trusses"?Math.min(40,s.length||1):.7,.9,.65),h=new ns(c);c.dispose();let u=new fn(h,new vi({color:16763264,depthTest:!1,toneMapped:!1}));if(u.position.fromArray(s.position),u.rotation.set(...(s["rotation-deg"]||[0,0,0]).map(d=>d*Math.PI/180),"ZYX"),this.rig.add(u),i.kind==="fixtures"&&["mount","place","calibrate"].includes(e.tool)){let{forward:d,up:f}=Gf(s["rotation-deg"]||[0,0,0]),m=(x,p)=>s.position.map((g,y)=>g+x[y]*p);o([s.position,m(d,1.6)],16763264),a([m(d,1.6)],16763264,11),o([s.position,m(f,.55)],7910629)}i.kind==="fixtures"&&e.tool==="calibrate"&&Ae(e["calibration-target"])&&o([s.position,e["calibration-target"]],7923154,.8)}if(e.tool==="calibrate"&&(a((e["calibration-points"]||[]).filter(Ae).slice(0,64),11979177,9),Ae(e["calibration-target"])&&a([e["calibration-target"]],7923154,13)),e.tool==="draw"||e.tool==="pixels"){let c=So(e.plane),h=[0,1,2].filter(d=>d!==c),u=Number(e["plane-value"])||0;for(let d=-10;d<=10;d++)for(let f=0;f<2;f++){let m=[0,0,0],x=[0,0,0];m[c]=x[c]=u,m[h[f]]=x[h[f]]=d,m[h[1-f]]=-10,x[h[1-f]]=10,o([m,x],11979177,.14)}}}render(t,e){if(!this.rig.children.length&&!this.volumes.group.children.length&&!this.sculpt.group.children.length&&!this.videoSurfaces.group.children.length)return;let i=t.autoClear,s=t.getRenderTarget();try{t.setRenderTarget(null),t.autoClear=!1,t.clearDepth(),t.render(this.scene,e)}finally{t.autoClear=i,t.setRenderTarget(s)}}dispose(){this.clear(),this.volumes.dispose(),this.sculpt.dispose(),this.videoSurfaces.dispose()}};var Wf={type:"change"},jh={type:"start"},qf={type:"end"},Yl=new dn,Xf=new li,cy=Math.cos(70*rr.DEG2RAD),ze=new C,xi=2*Math.PI,re={NONE:-1,ROTATE:0,DOLLY:1,PAN:2,TOUCH_ROTATE:3,TOUCH_PAN:4,TOUCH_DOLLY_PAN:5,TOUCH_DOLLY_ROTATE:6},$h=1e-6,Zl=class extends jr{constructor(t,e=null){super(t,e),this.state=re.NONE,this.target=new C,this.cursor=new C,this.minDistance=0,this.maxDistance=1/0,this.minZoom=0,this.maxZoom=1/0,this.minTargetRadius=0,this.maxTargetRadius=1/0,this.minPolarAngle=0,this.maxPolarAngle=Math.PI,this.minAzimuthAngle=-1/0,this.maxAzimuthAngle=1/0,this.enableDamping=!1,this.dampingFactor=.05,this.enableZoom=!0,this.zoomSpeed=1,this.enableRotate=!0,this.rotateSpeed=1,this.keyRotateSpeed=1,this.enablePan=!0,this.panSpeed=1,this.screenSpacePanning=!0,this.keyPanSpeed=7,this.zoomToCursor=!1,this.autoRotate=!1,this.autoRotateSpeed=2,this.keys={LEFT:"ArrowLeft",UP:"ArrowUp",RIGHT:"ArrowRight",BOTTOM:"ArrowDown"},this.mouseButtons={LEFT:On.ROTATE,MIDDLE:On.DOLLY,RIGHT:On.PAN},this.touches={ONE:Bi.ROTATE,TWO:Bi.DOLLY_PAN},this.target0=this.target.clone(),this.position0=this.object.position.clone(),this.zoom0=this.object.zoom,this._cursorStyle="auto",this._domElementKeyEvents=null,this._lastPosition=new C,this._lastQuaternion=new ti,this._lastTargetPosition=new C,this._quat=new ti().setFromUnitVectors(t.up,new C(0,1,0)),this._quatInverse=this._quat.clone().invert(),this._spherical=new $s,this._sphericalDelta=new $s,this._scale=1,this._panOffset=new C,this._rotateStart=new et,this._rotateEnd=new et,this._rotateDelta=new et,this._panStart=new et,this._panEnd=new et,this._panDelta=new et,this._dollyStart=new et,this._dollyEnd=new et,this._dollyDelta=new et,this._dollyDirection=new C,this._mouse=new et,this._performCursorZoom=!1,this._pointers=[],this._pointerPositions={},this._controlActive=!1,this._onPointerMove=uy.bind(this),this._onPointerDown=hy.bind(this),this._onPointerUp=dy.bind(this),this._onContextMenu=yy.bind(this),this._onMouseWheel=my.bind(this),this._onKeyDown=gy.bind(this),this._onTouchStart=xy.bind(this),this._onTouchMove=_y.bind(this),this._onMouseDown=fy.bind(this),this._onMouseMove=py.bind(this),this._interceptControlDown=vy.bind(this),this._interceptControlUp=My.bind(this),this.domElement!==null&&this.connect(this.domElement),this.update()}set cursorStyle(t){this._cursorStyle=t,t==="grab"?this.domElement.style.cursor="grab":this.domElement.style.cursor="auto"}get cursorStyle(){return this._cursorStyle}connect(t){super.connect(t),this.domElement.addEventListener("pointerdown",this._onPointerDown),this.domElement.addEventListener("pointercancel",this._onPointerUp),this.domElement.addEventListener("contextmenu",this._onContextMenu),this.domElement.addEventListener("wheel",this._onMouseWheel,{passive:!1}),this.domElement.ownerDocument.addEventListener("keydown",this._interceptControlDown,{passive:!0,capture:!0}),this.domElement.style.touchAction="none"}disconnect(){this.state=re.NONE,this.domElement.removeEventListener("pointerdown",this._onPointerDown),this.domElement.ownerDocument.removeEventListener("pointermove",this._onPointerMove),this.domElement.ownerDocument.removeEventListener("pointerup",this._onPointerUp),this.domElement.removeEventListener("pointercancel",this._onPointerUp),this.domElement.removeEventListener("wheel",this._onMouseWheel),this.domElement.removeEventListener("contextmenu",this._onContextMenu),this.stopListenToKeyEvents(),this.domElement.ownerDocument.removeEventListener("keydown",this._interceptControlDown,{capture:!0}),this.domElement.ownerDocument.removeEventListener("keyup",this._interceptControlUp,{capture:!0}),this._controlActive=!1,this._pointers.length=0,this._pointerPositions={},this.domElement.style.touchAction="",this.domElement.style.cursor="auto"}dispose(){this.disconnect()}getPolarAngle(){return this._spherical.phi}getAzimuthalAngle(){return this._spherical.theta}getDistance(){return this.object.position.distanceTo(this.target)}listenToKeyEvents(t){t.addEventListener("keydown",this._onKeyDown),this._domElementKeyEvents=t}stopListenToKeyEvents(){this._domElementKeyEvents!==null&&(this._domElementKeyEvents.removeEventListener("keydown",this._onKeyDown),this._domElementKeyEvents=null)}saveState(){this.target0.copy(this.target),this.position0.copy(this.object.position),this.zoom0=this.object.zoom}reset(){this.target.copy(this.target0),this.object.position.copy(this.position0),this.object.zoom=this.zoom0,this.object.updateProjectionMatrix(),this.dispatchEvent(Wf),this.update(),this.state=re.NONE}pan(t,e){this._pan(t,e),this.update()}dollyIn(t){this._dollyIn(t),this.update()}dollyOut(t){this._dollyOut(t),this.update()}rotateLeft(t){this._rotateLeft(t),this.update()}rotateUp(t){this._rotateUp(t),this.update()}update(t=null){let e=this.object.position;ze.copy(e).sub(this.target),ze.applyQuaternion(this._quat),this._spherical.setFromVector3(ze),this.autoRotate&&this.state===re.NONE&&this._rotateLeft(this._getAutoRotationAngle(t)),this.enableDamping?(this._spherical.theta+=this._sphericalDelta.theta*this.dampingFactor,this._spherical.phi+=this._sphericalDelta.phi*this.dampingFactor):(this._spherical.theta+=this._sphericalDelta.theta,this._spherical.phi+=this._sphericalDelta.phi);let i=this.minAzimuthAngle,s=this.maxAzimuthAngle;isFinite(i)&&isFinite(s)&&(i<-Math.PI?i+=xi:i>Math.PI&&(i-=xi),s<-Math.PI?s+=xi:s>Math.PI&&(s-=xi),i<=s?this._spherical.theta=Math.max(i,Math.min(s,this._spherical.theta)):this._spherical.theta=this._spherical.theta>(i+s)/2?Math.max(i,this._spherical.theta):Math.min(s,this._spherical.theta)),this._spherical.phi=Math.max(this.minPolarAngle,Math.min(this.maxPolarAngle,this._spherical.phi)),this._spherical.makeSafe(),this.enableDamping===!0?this.target.addScaledVector(this._panOffset,this.dampingFactor):this.target.add(this._panOffset),this.target.sub(this.cursor),this.target.clampLength(this.minTargetRadius,this.maxTargetRadius),this.target.add(this.cursor);let r=!1;if(this.zoomToCursor&&this._performCursorZoom||this.object.isOrthographicCamera)this._spherical.radius=this._clampDistance(this._spherical.radius);else{let o=this._spherical.radius;this._spherical.radius=this._clampDistance(this._spherical.radius*this._scale),r=o!=this._spherical.radius}if(ze.setFromSpherical(this._spherical),ze.applyQuaternion(this._quatInverse),e.copy(this.target).add(ze),this.object.lookAt(this.target),this.enableDamping===!0?(this._sphericalDelta.theta*=1-this.dampingFactor,this._sphericalDelta.phi*=1-this.dampingFactor,this._panOffset.multiplyScalar(1-this.dampingFactor)):(this._sphericalDelta.set(0,0,0),this._panOffset.set(0,0,0)),this.zoomToCursor&&this._performCursorZoom){let o=null;if(this.object.isPerspectiveCamera){let a=ze.length();o=this._clampDistance(a*this._scale);let l=a-o;this.object.position.addScaledVector(this._dollyDirection,l),this.object.updateMatrixWorld(),r=!!l}else if(this.object.isOrthographicCamera){let a=new C(this._mouse.x,this._mouse.y,0);a.unproject(this.object);let l=this.object.zoom;this.object.zoom=Math.max(this.minZoom,Math.min(this.maxZoom,this.object.zoom/this._scale)),this.object.updateProjectionMatrix(),r=l!==this.object.zoom;let c=new C(this._mouse.x,this._mouse.y,0);c.unproject(this.object),this.object.position.sub(c).add(a),this.object.updateMatrixWorld(),o=ze.length()}else console.warn("WARNING: OrbitControls.js encountered an unknown camera type - zoom to cursor disabled."),this.zoomToCursor=!1;o!==null&&(this.screenSpacePanning?this.target.set(0,0,-1).transformDirection(this.object.matrix).multiplyScalar(o).add(this.object.position):(Yl.origin.copy(this.object.position),Yl.direction.set(0,0,-1).transformDirection(this.object.matrix),Math.abs(this.object.up.dot(Yl.direction))<cy?this.object.lookAt(this.target):(Xf.setFromNormalAndCoplanarPoint(this.object.up,this.target),Yl.intersectPlane(Xf,this.target))))}else if(this.object.isOrthographicCamera){let o=this.object.zoom;this.object.zoom=Math.max(this.minZoom,Math.min(this.maxZoom,this.object.zoom/this._scale)),o!==this.object.zoom&&(this.object.updateProjectionMatrix(),r=!0)}return this._scale=1,this._performCursorZoom=!1,r||this._lastPosition.distanceToSquared(this.object.position)>$h||8*(1-this._lastQuaternion.dot(this.object.quaternion))>$h||this._lastTargetPosition.distanceToSquared(this.target)>$h?(this.dispatchEvent(Wf),this._lastPosition.copy(this.object.position),this._lastQuaternion.copy(this.object.quaternion),this._lastTargetPosition.copy(this.target),!0):!1}_getAutoRotationAngle(t){return t!==null?xi/60*this.autoRotateSpeed*t:xi/60/60*this.autoRotateSpeed}_getZoomScale(t){let e=Math.abs(t*.01);return Math.pow(.95,this.zoomSpeed*e)}_rotateLeft(t){this._sphericalDelta.theta-=t}_rotateUp(t){this._sphericalDelta.phi-=t}_panLeft(t,e){ze.setFromMatrixColumn(e,0),ze.multiplyScalar(-t),this._panOffset.add(ze)}_panUp(t,e){this.screenSpacePanning===!0?ze.setFromMatrixColumn(e,1):(ze.setFromMatrixColumn(e,0),ze.crossVectors(this.object.up,ze)),ze.multiplyScalar(t),this._panOffset.add(ze)}_pan(t,e){let i=this.domElement;if(this.object.isPerspectiveCamera){let s=this.object.position;ze.copy(s).sub(this.target);let r=ze.length();r*=Math.tan(this.object.fov/2*Math.PI/180),this._panLeft(2*t*r/i.clientHeight,this.object.matrix),this._panUp(2*e*r/i.clientHeight,this.object.matrix)}else this.object.isOrthographicCamera?(this._panLeft(t*(this.object.right-this.object.left)/this.object.zoom/i.clientWidth,this.object.matrix),this._panUp(e*(this.object.top-this.object.bottom)/this.object.zoom/i.clientHeight,this.object.matrix)):(console.warn("WARNING: OrbitControls.js encountered an unknown camera type - pan disabled."),this.enablePan=!1)}_dollyOut(t){this.object.isPerspectiveCamera||this.object.isOrthographicCamera?this._scale/=t:(console.warn("WARNING: OrbitControls.js encountered an unknown camera type - dolly/zoom disabled."),this.enableZoom=!1)}_dollyIn(t){this.object.isPerspectiveCamera||this.object.isOrthographicCamera?this._scale*=t:(console.warn("WARNING: OrbitControls.js encountered an unknown camera type - dolly/zoom disabled."),this.enableZoom=!1)}_updateZoomParameters(t,e){if(!this.zoomToCursor)return;this._performCursorZoom=!0;let i=this.domElement.getBoundingClientRect(),s=t-i.left,r=e-i.top,o=i.width,a=i.height;this._mouse.x=s/o*2-1,this._mouse.y=-(r/a)*2+1,this._dollyDirection.set(this._mouse.x,this._mouse.y,1).unproject(this.object).sub(this.object.position).normalize()}_clampDistance(t){return Math.max(this.minDistance,Math.min(this.maxDistance,t))}_handleMouseDownRotate(t){this._rotateStart.set(t.clientX,t.clientY)}_handleMouseDownDolly(t){this._updateZoomParameters(t.clientX,t.clientY),this._dollyStart.set(t.clientX,t.clientY)}_handleMouseDownPan(t){this._panStart.set(t.clientX,t.clientY)}_handleMouseMoveRotate(t){this._rotateEnd.set(t.clientX,t.clientY),this._rotateDelta.subVectors(this._rotateEnd,this._rotateStart).multiplyScalar(this.rotateSpeed);let e=this.domElement;this._rotateLeft(xi*this._rotateDelta.x/e.clientHeight),this._rotateUp(xi*this._rotateDelta.y/e.clientHeight),this._rotateStart.copy(this._rotateEnd),this.update()}_handleMouseMoveDolly(t){this._dollyEnd.set(t.clientX,t.clientY),this._dollyDelta.subVectors(this._dollyEnd,this._dollyStart),this._dollyDelta.y>0?this._dollyOut(this._getZoomScale(this._dollyDelta.y)):this._dollyDelta.y<0&&this._dollyIn(this._getZoomScale(this._dollyDelta.y)),this._dollyStart.copy(this._dollyEnd),this.zoomToCursor&&(this._performCursorZoom=!0,this._dollyDirection.set(this._mouse.x,this._mouse.y,1).unproject(this.object).sub(this.object.position).normalize()),this.update()}_handleMouseMovePan(t){this._panEnd.set(t.clientX,t.clientY),this._panDelta.subVectors(this._panEnd,this._panStart).multiplyScalar(this.panSpeed),this._pan(this._panDelta.x,this._panDelta.y),this._panStart.copy(this._panEnd),this.update()}_handleMouseWheel(t){this._updateZoomParameters(t.clientX,t.clientY),t.deltaY<0?this._dollyIn(this._getZoomScale(t.deltaY)):t.deltaY>0&&this._dollyOut(this._getZoomScale(t.deltaY)),this.update()}_handleKeyDown(t){let e=!1;switch(t.code){case this.keys.UP:t.ctrlKey||t.metaKey||t.shiftKey?this.enableRotate&&this._rotateUp(xi*this.keyRotateSpeed/this.domElement.clientHeight):this.enablePan&&this._pan(0,this.keyPanSpeed),e=!0;break;case this.keys.BOTTOM:t.ctrlKey||t.metaKey||t.shiftKey?this.enableRotate&&this._rotateUp(-xi*this.keyRotateSpeed/this.domElement.clientHeight):this.enablePan&&this._pan(0,-this.keyPanSpeed),e=!0;break;case this.keys.LEFT:t.ctrlKey||t.metaKey||t.shiftKey?this.enableRotate&&this._rotateLeft(xi*this.keyRotateSpeed/this.domElement.clientHeight):this.enablePan&&this._pan(this.keyPanSpeed,0),e=!0;break;case this.keys.RIGHT:t.ctrlKey||t.metaKey||t.shiftKey?this.enableRotate&&this._rotateLeft(-xi*this.keyRotateSpeed/this.domElement.clientHeight):this.enablePan&&this._pan(-this.keyPanSpeed,0),e=!0;break}e&&(t.preventDefault(),this.update())}_handleTouchStartRotate(t){if(this._pointers.length===1)this._rotateStart.set(t.pageX,t.pageY);else{let e=this._getSecondPointerPosition(t),i=.5*(t.pageX+e.x),s=.5*(t.pageY+e.y);this._rotateStart.set(i,s)}}_handleTouchStartPan(t){if(this._pointers.length===1)this._panStart.set(t.pageX,t.pageY);else{let e=this._getSecondPointerPosition(t),i=.5*(t.pageX+e.x),s=.5*(t.pageY+e.y);this._panStart.set(i,s)}}_handleTouchStartDolly(t){let e=this._getSecondPointerPosition(t),i=t.pageX-e.x,s=t.pageY-e.y,r=Math.sqrt(i*i+s*s);this._dollyStart.set(0,r)}_handleTouchStartDollyPan(t){this.enableZoom&&this._handleTouchStartDolly(t),this.enablePan&&this._handleTouchStartPan(t)}_handleTouchStartDollyRotate(t){this.enableZoom&&this._handleTouchStartDolly(t),this.enableRotate&&this._handleTouchStartRotate(t)}_handleTouchMoveRotate(t){if(this._pointers.length==1)this._rotateEnd.set(t.pageX,t.pageY);else{let i=this._getSecondPointerPosition(t),s=.5*(t.pageX+i.x),r=.5*(t.pageY+i.y);this._rotateEnd.set(s,r)}this._rotateDelta.subVectors(this._rotateEnd,this._rotateStart).multiplyScalar(this.rotateSpeed);let e=this.domElement;this._rotateLeft(xi*this._rotateDelta.x/e.clientHeight),this._rotateUp(xi*this._rotateDelta.y/e.clientHeight),this._rotateStart.copy(this._rotateEnd)}_handleTouchMovePan(t){if(this._pointers.length===1)this._panEnd.set(t.pageX,t.pageY);else{let e=this._getSecondPointerPosition(t),i=.5*(t.pageX+e.x),s=.5*(t.pageY+e.y);this._panEnd.set(i,s)}this._panDelta.subVectors(this._panEnd,this._panStart).multiplyScalar(this.panSpeed),this._pan(this._panDelta.x,this._panDelta.y),this._panStart.copy(this._panEnd)}_handleTouchMoveDolly(t){let e=this._getSecondPointerPosition(t),i=t.pageX-e.x,s=t.pageY-e.y,r=Math.sqrt(i*i+s*s);this._dollyEnd.set(0,r),this._dollyDelta.set(0,Math.pow(this._dollyEnd.y/this._dollyStart.y,this.zoomSpeed)),this._dollyOut(this._dollyDelta.y),this._dollyStart.copy(this._dollyEnd);let o=(t.pageX+e.x)*.5,a=(t.pageY+e.y)*.5;this._updateZoomParameters(o,a)}_handleTouchMoveDollyPan(t){this.enableZoom&&this._handleTouchMoveDolly(t),this.enablePan&&this._handleTouchMovePan(t)}_handleTouchMoveDollyRotate(t){this.enableZoom&&this._handleTouchMoveDolly(t),this.enableRotate&&this._handleTouchMoveRotate(t)}_addPointer(t){this._pointers.push(t.pointerId)}_removePointer(t){delete this._pointerPositions[t.pointerId];for(let e=0;e<this._pointers.length;e++)if(this._pointers[e]==t.pointerId){this._pointers.splice(e,1);return}}_isTrackingPointer(t){for(let e=0;e<this._pointers.length;e++)if(this._pointers[e]==t.pointerId)return!0;return!1}_trackPointer(t){let e=this._pointerPositions[t.pointerId];e===void 0&&(e=new et,this._pointerPositions[t.pointerId]=e),e.set(t.pageX,t.pageY)}_getSecondPointerPosition(t){let e=t.pointerId===this._pointers[0]?this._pointers[1]:this._pointers[0];return this._pointerPositions[e]}_customWheelEvent(t){let e=t.deltaMode,i={clientX:t.clientX,clientY:t.clientY,deltaY:t.deltaY};switch(e){case 1:i.deltaY*=16;break;case 2:i.deltaY*=100;break}return t.ctrlKey&&!this._controlActive&&(i.deltaY*=10),i}};function hy(n){this.enabled!==!1&&(this._pointers.length===0&&(this.domElement.setPointerCapture(n.pointerId),this.domElement.ownerDocument.addEventListener("pointermove",this._onPointerMove),this.domElement.ownerDocument.addEventListener("pointerup",this._onPointerUp)),!this._isTrackingPointer(n)&&(this._addPointer(n),n.pointerType==="touch"?this._onTouchStart(n):this._onMouseDown(n),this._cursorStyle==="grab"&&(this.domElement.style.cursor="grabbing")))}function uy(n){this.enabled!==!1&&(n.pointerType==="touch"?this._onTouchMove(n):this._onMouseMove(n))}function dy(n){switch(this._removePointer(n),this._pointers.length){case 0:this.domElement.releasePointerCapture(n.pointerId),this.domElement.ownerDocument.removeEventListener("pointermove",this._onPointerMove),this.domElement.ownerDocument.removeEventListener("pointerup",this._onPointerUp),this.dispatchEvent(qf),this.state=re.NONE,this._cursorStyle==="grab"&&(this.domElement.style.cursor="grab");break;case 1:let t=this._pointers[0],e=this._pointerPositions[t];this._onTouchStart({pointerId:t,pageX:e.x,pageY:e.y});break}}function fy(n){let t;switch(n.button){case 0:t=this.mouseButtons.LEFT;break;case 1:t=this.mouseButtons.MIDDLE;break;case 2:t=this.mouseButtons.RIGHT;break;default:t=-1}switch(t){case On.DOLLY:if(this.enableZoom===!1)return;this._handleMouseDownDolly(n),this.state=re.DOLLY;break;case On.ROTATE:if(n.ctrlKey||n.metaKey||n.shiftKey){if(this.enablePan===!1)return;this._handleMouseDownPan(n),this.state=re.PAN}else{if(this.enableRotate===!1)return;this._handleMouseDownRotate(n),this.state=re.ROTATE}break;case On.PAN:if(n.ctrlKey||n.metaKey||n.shiftKey){if(this.enableRotate===!1)return;this._handleMouseDownRotate(n),this.state=re.ROTATE}else{if(this.enablePan===!1)return;this._handleMouseDownPan(n),this.state=re.PAN}break;default:this.state=re.NONE}this.state!==re.NONE&&this.dispatchEvent(jh)}function py(n){switch(this.state){case re.ROTATE:if(this.enableRotate===!1)return;this._handleMouseMoveRotate(n);break;case re.DOLLY:if(this.enableZoom===!1)return;this._handleMouseMoveDolly(n);break;case re.PAN:if(this.enablePan===!1)return;this._handleMouseMovePan(n);break}}function my(n){this.enabled===!1||this.enableZoom===!1||this.state!==re.NONE||(n.preventDefault(),this.dispatchEvent(jh),this._handleMouseWheel(this._customWheelEvent(n)),this.dispatchEvent(qf))}function gy(n){this.enabled!==!1&&this._handleKeyDown(n)}function xy(n){switch(this._trackPointer(n),this._pointers.length){case 1:switch(this.touches.ONE){case Bi.ROTATE:if(this.enableRotate===!1){this.state=re.NONE;break}this._handleTouchStartRotate(n),this.state=re.TOUCH_ROTATE;break;case Bi.PAN:if(this.enablePan===!1){this.state=re.NONE;break}this._handleTouchStartPan(n),this.state=re.TOUCH_PAN;break;default:this.state=re.NONE}break;case 2:switch(this.touches.TWO){case Bi.DOLLY_PAN:if(this.enableZoom===!1&&this.enablePan===!1)return;this._handleTouchStartDollyPan(n),this.state=re.TOUCH_DOLLY_PAN;break;case Bi.DOLLY_ROTATE:if(this.enableZoom===!1&&this.enableRotate===!1)return;this._handleTouchStartDollyRotate(n),this.state=re.TOUCH_DOLLY_ROTATE;break;default:this.state=re.NONE}break;default:this.state=re.NONE}this.state!==re.NONE&&this.dispatchEvent(jh)}function _y(n){switch(this._trackPointer(n),this.state){case re.TOUCH_ROTATE:if(this.enableRotate===!1)return;this._handleTouchMoveRotate(n),this.update();break;case re.TOUCH_PAN:if(this.enablePan===!1)return;this._handleTouchMovePan(n),this.update();break;case re.TOUCH_DOLLY_PAN:if(this.enableZoom===!1&&this.enablePan===!1)return;this._handleTouchMoveDollyPan(n),this.update();break;case re.TOUCH_DOLLY_ROTATE:if(this.enableZoom===!1&&this.enableRotate===!1)return;this._handleTouchMoveDollyRotate(n),this.update();break;default:this.state=re.NONE}}function yy(n){this.enabled!==!1&&n.preventDefault()}function vy(n){n.key==="Control"&&(this._controlActive=!0,this.domElement.ownerDocument.addEventListener("keyup",this._interceptControlUp,{passive:!0,capture:!0}))}function My(n){n.key==="Control"&&(this._controlActive=!1,this.domElement.ownerDocument.removeEventListener("keyup",this._interceptControlUp,{passive:!0,capture:!0}))}function Yf(n,t){let e=new Zl(n,t);return e.enableDamping=!0,e.dampingFactor=.1,e.minDistance=1,e.maxDistance=45,e.maxPolarAngle=Math.PI*.6,e.enablePan=!0,e.screenSpacePanning=!0,e.touches.ONE=Bi.ROTATE,e.touches.TWO=Bi.DOLLY_PAN,e}function Qh(n,t,e,i){let[s,r,o]=e;if(i==="overview")n.position.set(s*.58,r*1.25,o*.77),t.target.set(0,r*.3,-o*.08);else if(i==="audience")n.position.set(s*.09,r*.48,o*.76),t.target.set(0,r*.38,-o*.28);else throw new Error("Unknown preview camera");t.update()}function Zf(n,t,e,i){return i==="Home"?(Qh(n,t,e,"audience"),!0):["+","=","-"].includes(i)?(t.dollyIn(i==="-"?1.1:.9),!0):!1}function Kf(n){let[t,e]=n.values();return{x:(t.x+e.x)/2,y:(t.y+e.y)/2,span:Math.hypot(e.x-t.x,e.y-t.y)}}var Kl=class{points=new Map;pair=null;kind=null;add(t,e,i){this.points.set(t,{x:e,y:i}),this.rebase()}remove(t){this.points.delete(t),this.rebase()}rebase(){this.pair=this.points.size===2?Kf(this.points):null,this.kind=null}sample(){if(!this.pair)return null;let t=Kf(this.points),e=t.x-this.pair.x,i=t.y-this.pair.y,s=Math.abs(t.span-this.pair.span)/2,r=Math.hypot(e,i);if(!this.kind){if(Math.max(r,s)<4)return null;this.kind=s>r?"pinch":"walk"}let o=Math.log(Math.max(1,t.span)/Math.max(1,this.pair.span));return this.pair=t,{kind:this.kind,x:e,y:i,zoom:o}}};var tu={KeyW:"forward",KeyK:"forward",ArrowUp:"forward",KeyS:"backward",KeyJ:"backward",ArrowDown:"backward",KeyA:"left",KeyH:"left",ArrowLeft:"left",KeyD:"right",KeyL:"right",ArrowRight:"right",KeyQ:"down",KeyE:"up"},Sy=new Set(Object.values(tu)),Jf=[];function by(n,t,e){let i=t.lengthSq()?13:19,s=-Math.expm1(-i*e),r=n.clone().sub(t).multiplyScalar(s/i).addScaledVector(t,e);return n.lerp(t,s),r}var eu=n=>Array.isArray(n)&&n.length===3&&n.every(Number.isFinite),jf=n=>[n?.[0]||12,n?.[1]||5,n?.[2]||10].map(t=>Ke(t,1,100)),wy=n=>n?.isContentEditable||/^(INPUT|TEXTAREA|SELECT)$/.test(n?.tagName||""),$f=(n,t,e)=>`${n},${t},${e}`;function Qf(n){return new Map(n.map(t=>[String(t.id),t]))}function Ey(n,t=Qf(n?.rig?.fixtures||[]),e){let i=n?.output?.values||{},s=[];for(let r of e||Object.keys(i)){let o=t.get(r)?.position;if(!eu(o))continue;let a=fs(i[r]),l=a[0]*.2126+a[1]*.7152+a[2]*.0722;l>1e-4&&s.push({id:r,position:o.slice(),energy:l})}return s.sort((r,o)=>o.energy-r.energy||r.id.localeCompare(o.id)).slice(0,16)}function tp(n,t,e=[],i=n){let[s,r,o]=jf(t),a=[-s/2+.25,.2,-o/2+.25],l=[s/2-.25,r-.2,o/2-.25],c=n.map((h,u)=>Ke(h,Math.min(a[u],i[u]),Math.max(l[u],i[u])));for(let h of e){if(!eu(h.position))continue;let u=c.map((f,m)=>f-h.position[m]),d=Math.hypot(...u);if(d<.45){let f=d>1e-5?u.map(m=>m/d):[0,-1,0];c=h.position.map((m,x)=>m+f[x]*.45)}}return c.map((h,u)=>Ke(h,Math.min(a[u],i[u]),Math.max(l[u],i[u])))}function Ty(n,t,e=0){let[i,s,r]=jf(t);if(!n)return{position:[i*.28,s*.55,r*.38],target:[0,s*.4,-r*.12]};let[o,a,l]=n.position,c=Math.atan2(-l||1,-o)+Math.sin(e)*.32,h=Ke(Math.min(i,r)*.22,1.1,2.4);return{position:tp([o+Math.cos(c)*h,Ke(a-.65,.5,s-.4),l+Math.sin(c)*h],t,[],[0,s/2,0]),target:n.position.slice()}}var Jl=class{constructor({camera:t,controls:e,canvas:i,room:s,snapshot:r,visibleLights:o,active:a,reducedMotion:l,changed:c=()=>{}}){Object.assign(this,{camera:t,controls:e,canvas:i,room:s,snapshot:r,visibleLights:o,active:a,reducedMotion:l,changed:c}),this.mode="orbit",this.speed=2,this.keys=new Set,this.buttons=new Set,this.listeners=new Set,this.velocity=new C,this.swipe=new et,this.look=new et,this.gesture=new Kl,this.zoom=0,this.interacting=!1,this.disposed=!1,this.clock=0,this.subjectId=null,this.visitStarted=0,this.abort=new AbortController;let h=this.abort.signal;i.addEventListener("keydown",u=>this.keydown(u),{signal:h}),i.addEventListener("keyup",u=>{this.keys.delete(u.code),this.boost=u.shiftKey},{signal:h}),i.addEventListener("blur",()=>this.clearInput(),{signal:h}),window.addEventListener("blur",()=>this.clearInput(),{signal:h}),document.addEventListener("visibilitychange",()=>{this.clearInput(),this.lastTime=null},{signal:h}),i.addEventListener("pointerdown",u=>this.pointerDown(u),{signal:h,capture:!0}),i.addEventListener("pointermove",u=>this.pointerMove(u),{signal:h,capture:!0});for(let u of["pointerup","pointercancel","lostpointercapture"])i.addEventListener(u,d=>this.pointerUp(d),{signal:h,capture:!0});i.addEventListener("wheel",u=>this.wheel(u),{signal:h,capture:!0,passive:!1}),this.animate=u=>{this.frame=null,!(this.disposed||!this.active())&&(this.tick(u),this.refreshActivity())},this.refreshActivity()}refreshActivity(){this.disposed||!this.active()?(this.frame!=null&&cancelAnimationFrame(this.frame),this.frame=null,this.lastTime=null,this.clearInput(),this.controls.enabled=!1):this.frame==null&&(this.controls.enabled=this.mode==="orbit"&&!this.interacting,this.frame=requestAnimationFrame(this.animate))}state(){return{mode:this.mode,speed:this.speed,interacting:this.interacting,"reduced-motion":!!this.reducedMotion(),"tour-fixture":this.subjectId,"tour-waiting":this.mode==="tour"&&!this.subjectId}}notify(){let t=this.state(),e=JSON.stringify(t);if(e!==this.lastNotice){this.lastNotice=e;for(let i of this.listeners)i(t)}}subscribe(t){return this.listeners.add(t),t(this.state()),()=>this.listeners.delete(t)}setMode(t,e=!1){if(!["orbit","walk","tour"].includes(t))throw new Error("Unknown camera navigation mode");this.clearInput(e),this.mode=t,this.lastTime=null,this.subjectId=null,this.visitStarted=this.clock,this.controls.enabled=t==="orbit"&&!this.interacting,this.notify()}setSpeed(t){if(!Number.isFinite(t)||t<.25||t>8)throw new Error("Camera speed must be 0.25\u20138 metres per second");this.speed=t,this.notify()}setInteractionActive(t){this.interacting=!!t,this.clearInput(),this.lastTime=null,this.controls.enabled=this.mode==="orbit"&&!this.interacting,this.notify()}clearInput(t=!1){if(!t){let e=[...this.gesture.points.keys()];this.gesture.points.clear(),this.gesture.rebase();for(let i of e)this.canvas.hasPointerCapture?.(i)&&this.canvas.releasePointerCapture(i)}this.velocity.set(0,0,0),this.swipe.set(0,0),this.look.set(0,0),this.zoom=0,this.keys.clear(),this.buttons.clear(),this.boost=!1,this.pointer&&this.canvas.hasPointerCapture?.(this.pointer.id)&&this.canvas.releasePointerCapture(this.pointer.id),this.pointer=null}move(t,e){if(!Sy.has(t))throw new Error("Unknown camera direction");if(!e){this.buttons.delete(t);return}this.interacting||!this.active()||(this.mode!=="walk"&&this.setMode("walk"),this.buttons.add(t))}keydown(t){if(!(wy(t.target)||t.altKey||t.metaKey||t.ctrlKey||this.interacting||!this.active())){if(tu[t.code]){t.preventDefault(),this.mode!=="walk"&&this.setMode("walk"),this.keys.add(t.code),this.boost=t.shiftKey;return}if(t.key==="Escape"){this.setMode("orbit");return}["+","=","-","Home"].includes(t.key)&&(this.mode!=="orbit"&&this.setMode("orbit"),Zf(this.camera,this.controls,this.room(),t.key)&&(t.preventDefault(),this.changed()))}}queueSwipe(t,e){let i=this.speed*2/Math.max(1,this.canvas.clientHeight);this.swipe.x+=t*i,this.swipe.y-=e*i}wheel(t){if(!this.active()||this.interacting||t.altKey||t.metaKey||!t.ctrlKey&&t.deltaMode!==0&&this.mode==="orbit")return;if(t.preventDefault(),t.stopImmediatePropagation(),t.ctrlKey){this.mode==="tour"&&this.setMode("orbit"),this.zoom-=Ke(t.deltaY*.01,-1,1);return}this.mode!=="walk"&&this.setMode("walk");let e=t.deltaMode===1?16:t.deltaMode===2?this.canvas.clientHeight:1;this.queueSwipe(t.deltaX*e,t.deltaY*e)}pointerDown(t){if(!(this.interacting||!this.active()||t.button!==0)){if(this.canvas.focus({preventScroll:!0}),this.mode==="tour"&&this.setMode("walk"),t.pointerType==="touch"){t.preventDefault(),t.stopImmediatePropagation(),this.gesture.add(t.pointerId,t.clientX,t.clientY),this.canvas.setPointerCapture(t.pointerId);return}this.mode!=="walk"||this.pointer||(t.preventDefault(),t.stopImmediatePropagation(),this.pointer={id:t.pointerId,x:t.clientX,y:t.clientY},this.canvas.setPointerCapture(t.pointerId))}}pointerMove(t){let e=this.gesture.points.get(t.pointerId),i=e||(this.pointer?.id===t.pointerId?this.pointer:null);!i||this.interacting||(t.preventDefault(),t.stopImmediatePropagation(),(!e||this.gesture.points.size===1)&&(this.look.x+=t.clientX-i.x,this.look.y+=t.clientY-i.y),i.x=t.clientX,i.y=t.clientY)}pointerUp(t){let e=this.gesture.points.has(t.pointerId);if(!e&&this.pointer?.id!==t.pointerId)return;t.stopImmediatePropagation();let i=t.type!=="pointerup";e?(i||this.sampleGesture(),this.gesture.remove(t.pointerId)):this.pointer=null,i&&(this.swipe.set(0,0),this.zoom=0,this.look.set(0,0)),this.canvas.hasPointerCapture?.(t.pointerId)&&this.canvas.releasePointerCapture(t.pointerId)}sampleGesture(){let t=this.gesture.sample();if(t){if(t.kind==="pinch"){this.zoom+=t.zoom;return}this.mode!=="walk"&&this.setMode("walk",!0),this.queueSwipe(t.x,t.y)}}updateView(t,e){let i=this.reducedMotion()?1:-Math.expm1(-24*t);if(Math.abs(this.zoom)>1e-4){let o=this.zoom*i;this.zoom-=o,this.mode==="orbit"?this.controls.dollyIn(Math.exp(-o)):this.translate(this.camera.getWorldDirection(new C).multiplyScalar(o*this.speed),e),this.changed()}if(this.look.lengthSq()<1e-4)return;let s=this.look.x*i,r=this.look.y*i;if(this.look.multiplyScalar(1-i),this.mode==="orbit"){let o=2*Math.PI/Math.max(1,this.canvas.clientHeight);this.controls.rotateLeft(s*o),this.controls.rotateUp(r*o)}else{let o=new Ai().setFromQuaternion(this.camera.quaternion,"YXZ");o.y-=s*.004,o.x=Ke(o.x-r*.004,-Math.PI*.48,Math.PI*.48),this.camera.quaternion.setFromEuler(o),this.controls.target.copy(this.camera.position).add(this.camera.getWorldDirection(new C).multiplyScalar(2))}this.changed()}walk(t,e){if(!this.keys.size&&!this.buttons.size&&this.velocity.lengthSq()<1e-7&&this.swipe.lengthSq()<1e-9)return;let i=new Set([...this.buttons,...Array.from(this.keys,h=>tu[h])]),s=new C(Number(i.has("right"))-Number(i.has("left")),Number(i.has("up"))-Number(i.has("down")),Number(i.has("forward"))-Number(i.has("backward")));s.normalize().multiplyScalar(this.speed*(this.boost?2:1));let r=by(this.velocity,s,t),o=this.reducedMotion()?1:-Math.expm1(-24*t);r.x+=this.swipe.x*o,r.z+=this.swipe.y*o,this.swipe.multiplyScalar(1-o);let a=this.camera.getWorldDirection(new C);a.y=0,a.normalize();let l=new C().crossVectors(a,this.camera.up).normalize(),c=a.multiplyScalar(r.z).addScaledVector(l,r.x);c.y=r.y,this.translate(c,e),this.changed()}tick(t){let e=this.active()&&!this.interacting;!e&&this.wasActive&&this.clearInput(),this.wasActive=e;let i=e&&this.lastTime!=null?Ke((t-this.lastTime)/1e3,0,.05):0;if(this.lastTime=e?t:null,this.controls.enabled=e&&this.mode==="orbit",!e||(this.sampleGesture(),!i))return;if(this.mode==="orbit"){this.updateView(i,Jf),this.controls.update(i);return}let s=this.snapshot(),r=s?.rig?.fixtures||Jf;if(this.updateView(i,r),this.mode==="walk"){this.walk(i,r);return}if(this.reducedMotion())return;this.clock+=i;let o=this.visibleLights?.();(s!==this.subjectSnapshot||o!==this.subjectLights)&&(r!==this.subjectFixtures&&(this.subjectFixtures=r,this.fixtureIndex=Qf(r)),this.subjectLights=o,this.subjectSnapshot=s,this.subjects=Ey(s,this.fixtureIndex,o));let a=this.subjects||[],l=a.find(f=>f.id===this.subjectId);if(!l||this.clock-this.visitStarted>=8){let f=l?(a.indexOf(l)+1)%a.length:0;l=a[f],this.subjectId=l?.id||null,this.visitStarted=this.clock,this.notify()}let c=Ty(l,this.room(),this.clock*.22),u=new C(...c.position).sub(this.camera.position),d=u.length();d>0&&u.multiplyScalar(Math.min(1,this.speed*i/d)),this.translate(u,r),this.controls.target.lerp(new C(...c.target),1-Math.exp(-i*2)),this.camera.lookAt(this.controls.target),this.changed()}translate(t,e){if(e!==this.collisionFixtures){this.collisionFixtures=e,this.collisionGrid=new Map;for(let r of e)if(eu(r.position)){let o=$f(...r.position.map(Math.floor));this.collisionGrid.has(o)||this.collisionGrid.set(o,[]),this.collisionGrid.get(o).push(r)}}let i=Math.max(1,Math.ceil(t.length()/.15)),s=t.clone().divideScalar(i);for(let r=0;r<i;r++){let o=this.camera.position.clone(),a=o.clone().add(s),[l,c,h]=a.toArray().map(Math.floor),u=[];for(let m=-1;m<=1;m++)for(let x=-1;x<=1;x++)for(let p=-1;p<=1;p++)u.push(...this.collisionGrid.get($f(l+m,c+x,h+p))||[]);let f=new C(...tp(a.toArray(),this.room(),u,o.toArray())).sub(o).clampLength(0,s.length());this.camera.position.copy(o).add(f),this.controls.target.add(this.camera.position.clone().sub(o))}}dispose(){this.disposed||(this.disposed=!0,this.clearInput(),this.abort.abort(),cancelAnimationFrame(this.frame),this.listeners.clear())}};var $l=class extends Ye{constructor(){super(),this.name="RoomEnvironment",this.position.y=-3.5;let t=new Te;t.deleteAttribute("uv");let e=new Le({side:Ze}),i=new Le,s=new Zr(16777215,900,28,2);s.position.set(.418,16.199,.3),this.add(s);let r=new Ut(t,e);r.position.set(-.757,13.219,.717),r.scale.set(31.713,28.305,28.591),this.add(r);let o=new ts(t,i,6),a=new Me;a.position.set(-10.906,2.009,1.846),a.rotation.set(0,-.195,0),a.scale.set(2.328,7.905,4.651),a.updateMatrix(),o.setMatrixAt(0,a.matrix),a.position.set(-5.607,-.754,-.758),a.rotation.set(0,.994,0),a.scale.set(1.97,1.534,3.955),a.updateMatrix(),o.setMatrixAt(1,a.matrix),a.position.set(6.167,.857,7.803),a.rotation.set(0,.561,0),a.scale.set(3.927,6.285,3.687),a.updateMatrix(),o.setMatrixAt(2,a.matrix),a.position.set(-2.017,.018,6.124),a.rotation.set(0,.333,0),a.scale.set(2.002,4.566,2.064),a.updateMatrix(),o.setMatrixAt(3,a.matrix),a.position.set(2.291,-.756,-2.621),a.rotation.set(0,-.286,0),a.scale.set(1.546,1.552,1.496),a.updateMatrix(),o.setMatrixAt(4,a.matrix),a.position.set(-2.193,-.369,-5.547),a.rotation.set(0,.516,0),a.scale.set(3.875,3.487,2.986),a.updateMatrix(),o.setMatrixAt(5,a.matrix),this.add(o);let l=new Ut(t,ur(50));l.position.set(-16.116,14.37,8.208),l.scale.set(.1,2.428,2.739),this.add(l);let c=new Ut(t,ur(50));c.position.set(-16.109,18.021,-8.207),c.scale.set(.1,2.425,2.751),this.add(c);let h=new Ut(t,ur(17));h.position.set(14.904,12.198,-1.832),h.scale.set(.15,4.265,6.331),this.add(h);let u=new Ut(t,ur(43));u.position.set(-.462,8.89,14.52),u.scale.set(4.38,5.441,.088),this.add(u);let d=new Ut(t,ur(20));d.position.set(3.235,11.486,-12.541),d.scale.set(2.5,2,.1),this.add(d);let f=new Ut(t,ur(100));f.position.set(0,20,0),f.scale.set(1,.1,1),this.add(f)}dispose(){let t=new Set;this.traverse(e=>{e.isMesh&&(t.add(e.geometry),t.add(e.material))});for(let e of t)e.dispose()}};function ur(n){return new qr({color:0,emissive:16777215,emissiveIntensity:n})}var Gn={name:"CopyShader",uniforms:{tDiffuse:{value:null},opacity:{value:1}},vertexShader:`

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


		}`};var di=class{constructor(){this.isPass=!0,this.enabled=!0,this.needsSwap=!0,this.clear=!1,this.renderToScreen=!1}setSize(){}render(){console.error("THREE.Pass: .render() must be implemented in derived pass.")}dispose(){}},Ay=new as(-1,1,1,-1,0,1),iu=class extends Jt{constructor(){super(),this.setAttribute("position",new ee([-1,3,0,-1,-1,0,3,-1,0],3)),this.setAttribute("uv",new ee([0,2,0,0,2,0],2))}},Ry=new iu,$i=class{constructor(t){this._mesh=new Ut(Ry,t)}dispose(){this._mesh.geometry.dispose()}render(t){t.render(this._mesh,Ay)}get material(){return this._mesh.material}set material(t){this._mesh.material=t}};var ps=class extends di{constructor(t,e="tDiffuse"){super(),this.textureID=e,this.uniforms=null,this.material=null,t instanceof fe?(this.uniforms=t.uniforms,this.material=t):t&&(this.uniforms=Hi.clone(t.uniforms),this.material=new fe({name:t.name!==void 0?t.name:"unspecified",defines:Object.assign({},t.defines),uniforms:this.uniforms,vertexShader:t.vertexShader,fragmentShader:t.fragmentShader})),this._fsQuad=new $i(this.material)}render(t,e,i){this.uniforms[this.textureID]&&(this.uniforms[this.textureID].value=i.texture),this._fsQuad.material=this.material,this.renderToScreen?(t.setRenderTarget(null),this._fsQuad.render(t)):(t.setRenderTarget(e),this.clear&&t.clear(t.autoClearColor,t.autoClearDepth,t.autoClearStencil),this._fsQuad.render(t))}dispose(){this.material.dispose(),this._fsQuad.dispose()}};var bo=class extends di{constructor(t,e){super(),this.scene=t,this.camera=e,this.clear=!0,this.needsSwap=!1,this.inverse=!1}render(t,e,i){let s=t.getContext(),r=t.state;r.buffers.color.setMask(!1),r.buffers.depth.setMask(!1),r.buffers.color.setLocked(!0),r.buffers.depth.setLocked(!0);let o,a;this.inverse?(o=0,a=1):(o=1,a=0),r.buffers.stencil.setTest(!0),r.buffers.stencil.setOp(s.REPLACE,s.REPLACE,s.REPLACE),r.buffers.stencil.setFunc(s.ALWAYS,o,4294967295),r.buffers.stencil.setClear(a),r.buffers.stencil.setLocked(!0),t.setRenderTarget(i),this.clear&&t.clear(),t.render(this.scene,this.camera),t.setRenderTarget(e),this.clear&&t.clear(),t.render(this.scene,this.camera),r.buffers.color.setLocked(!1),r.buffers.depth.setLocked(!1),r.buffers.color.setMask(!0),r.buffers.depth.setMask(!0),r.buffers.stencil.setLocked(!1),r.buffers.stencil.setFunc(s.EQUAL,1,4294967295),r.buffers.stencil.setOp(s.KEEP,s.KEEP,s.KEEP),r.buffers.stencil.setLocked(!0)}},jl=class extends di{constructor(){super(),this.needsSwap=!1}render(t){t.state.buffers.stencil.setLocked(!1),t.state.buffers.stencil.setTest(!1)}};var Ql=class{constructor(t,e){if(this.renderer=t,this._pixelRatio=t.getPixelRatio(),e===void 0){let i=t.getSize(new et);this._width=i.width,this._height=i.height,e=new me(this._width*this._pixelRatio,this._height*this._pixelRatio,{type:xe}),e.texture.name="EffectComposer.rt1"}else this._width=e.width,this._height=e.height;this.renderTarget1=e,this.renderTarget2=e.clone(),this.renderTarget2.texture.name="EffectComposer.rt2",this.writeBuffer=this.renderTarget1,this.readBuffer=this.renderTarget2,this.renderToScreen=!0,this.passes=[],this.copyPass=new ps(Gn),this.copyPass.material.blending=wi,this.timer=new $r}swapBuffers(){let t=this.readBuffer;this.readBuffer=this.writeBuffer,this.writeBuffer=t}addPass(t){this.passes.push(t),t.setSize(this._width*this._pixelRatio,this._height*this._pixelRatio)}insertPass(t,e){this.passes.splice(e,0,t),t.setSize(this._width*this._pixelRatio,this._height*this._pixelRatio)}removePass(t){let e=this.passes.indexOf(t);e!==-1&&this.passes.splice(e,1)}isLastEnabledPass(t){for(let e=t+1;e<this.passes.length;e++)if(this.passes[e].enabled)return!1;return!0}render(t){this.timer.update(),t===void 0&&(t=this.timer.getDelta());let e=this.renderer.getRenderTarget(),i=!1;for(let s=0,r=this.passes.length;s<r;s++){let o=this.passes[s];if(o.enabled!==!1){if(o.renderToScreen=this.renderToScreen&&this.isLastEnabledPass(s),o.render(this.renderer,this.writeBuffer,this.readBuffer,t,i),o.needsSwap){if(i){let a=this.renderer.getContext(),l=this.renderer.state.buffers.stencil;l.setFunc(a.NOTEQUAL,1,4294967295),this.copyPass.render(this.renderer,this.writeBuffer,this.readBuffer,t),l.setFunc(a.EQUAL,1,4294967295)}this.swapBuffers()}bo!==void 0&&(o instanceof bo?i=!0:o instanceof jl&&(i=!1))}}this.renderer.setRenderTarget(e)}reset(t){if(t===void 0){let e=this.renderer.getSize(new et);this._pixelRatio=this.renderer.getPixelRatio(),this._width=e.width,this._height=e.height,t=this.renderTarget1.clone(),t.setSize(this._width*this._pixelRatio,this._height*this._pixelRatio)}this.renderTarget1.dispose(),this.renderTarget2.dispose(),this.renderTarget1=t,this.renderTarget2=t.clone(),this.writeBuffer=this.renderTarget1,this.readBuffer=this.renderTarget2}setSize(t,e){this._width=t,this._height=e;let i=this._width*this._pixelRatio,s=this._height*this._pixelRatio;this.renderTarget1.setSize(i,s),this.renderTarget2.setSize(i,s);for(let r=0;r<this.passes.length;r++)this.passes[r].setSize(i,s)}setPixelRatio(t){this._pixelRatio=t,this.setSize(this._width,this._height)}dispose(){this.renderTarget1.dispose(),this.renderTarget2.dispose(),this.copyPass.dispose()}};var tc=class extends di{constructor(t,e=1){super();let i=Gn;this.map=t,this.opacity=e,this.needsSwap=!1,this.uniforms=Hi.clone(i.uniforms),this.material=new fe({uniforms:this.uniforms,vertexShader:i.vertexShader,fragmentShader:i.fragmentShader,depthTest:!1,depthWrite:!1,premultipliedAlpha:!0}),this._fsQuad=new $i(null)}render(t,e,i){let s=t.autoClear;t.autoClear=!1,this._fsQuad.material=this.material,this.uniforms.opacity.value=this.opacity,this.uniforms.tDiffuse.value=this.map,this.material.transparent=this.opacity<1,t.setRenderTarget(this.renderToScreen?null:i),this.clear&&t.clear(),this._fsQuad.render(t),t.autoClear=s}dispose(){this.material.dispose(),this._fsQuad.dispose()}};var ep={name:"LuminosityHighPassShader",uniforms:{tDiffuse:{value:null},luminosityThreshold:{value:1},smoothWidth:{value:1},defaultColor:{value:new bt(0)},defaultOpacity:{value:0}},vertexShader:`

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

		}`};var dr=class n extends di{constructor(t,e=1,i,s){super(),this.strength=e,this.radius=i,this.threshold=s,this.resolution=t!==void 0?new et(t.x,t.y):new et(256,256),this.clearColor=new bt(0,0,0),this.needsSwap=!1,this.renderTargetsHorizontal=[],this.renderTargetsVertical=[],this.nMips=5;let r=Math.round(this.resolution.x/2),o=Math.round(this.resolution.y/2);this.renderTargetBright=new me(r,o,{type:xe,depthBuffer:!1}),this.renderTargetBright.texture.name="UnrealBloomPass.bright",this.renderTargetBright.texture.generateMipmaps=!1;for(let h=0;h<this.nMips;h++){let u=new me(r,o,{type:xe,depthBuffer:!1});u.texture.name="UnrealBloomPass.h"+h,u.texture.generateMipmaps=!1,this.renderTargetsHorizontal.push(u);let d=new me(r,o,{type:xe,depthBuffer:!1});d.texture.name="UnrealBloomPass.v"+h,d.texture.generateMipmaps=!1,this.renderTargetsVertical.push(d),r=Math.round(r/2),o=Math.round(o/2)}let a=ep;this.highPassUniforms=Hi.clone(a.uniforms),this.highPassUniforms.luminosityThreshold.value=s,this.highPassUniforms.smoothWidth.value=.01,this.materialHighPassFilter=new fe({uniforms:this.highPassUniforms,vertexShader:a.vertexShader,fragmentShader:a.fragmentShader}),this.separableBlurMaterials=[];let l=[6,10,14,18,22];r=Math.round(this.resolution.x/2),o=Math.round(this.resolution.y/2);for(let h=0;h<this.nMips;h++)this.separableBlurMaterials.push(this._getSeparableBlurMaterial(l[h])),this.separableBlurMaterials[h].uniforms.invSize.value=new et(1/r,1/o),r=Math.round(r/2),o=Math.round(o/2);this.compositeMaterial=this._getCompositeMaterial(this.nMips),this.compositeMaterial.uniforms.blurTexture1.value=this.renderTargetsVertical[0].texture,this.compositeMaterial.uniforms.blurTexture2.value=this.renderTargetsVertical[1].texture,this.compositeMaterial.uniforms.blurTexture3.value=this.renderTargetsVertical[2].texture,this.compositeMaterial.uniforms.blurTexture4.value=this.renderTargetsVertical[3].texture,this.compositeMaterial.uniforms.blurTexture5.value=this.renderTargetsVertical[4].texture,this.compositeMaterial.uniforms.bloomStrength.value=e,this.compositeMaterial.uniforms.bloomRadius.value=.1;let c=[1,.8,.6,.4,.2];this.compositeMaterial.uniforms.bloomFactors.value=c,this.bloomTintColors=[new C(1,1,1),new C(1,1,1),new C(1,1,1),new C(1,1,1),new C(1,1,1)],this.compositeMaterial.uniforms.bloomTintColors.value=this.bloomTintColors,this.copyUniforms=Hi.clone(Gn.uniforms),this.blendMaterial=new fe({uniforms:this.copyUniforms,vertexShader:Gn.vertexShader,fragmentShader:Gn.fragmentShader,premultipliedAlpha:!0,blending:Qr,depthTest:!1,depthWrite:!1,transparent:!0}),this._oldClearColor=new bt,this._oldClearAlpha=1,this._basic=new pi,this._fsQuad=new $i(null)}dispose(){for(let t=0;t<this.renderTargetsHorizontal.length;t++)this.renderTargetsHorizontal[t].dispose();for(let t=0;t<this.renderTargetsVertical.length;t++)this.renderTargetsVertical[t].dispose();this.renderTargetBright.dispose();for(let t=0;t<this.separableBlurMaterials.length;t++)this.separableBlurMaterials[t].dispose();this.compositeMaterial.dispose(),this.blendMaterial.dispose(),this._basic.dispose(),this._fsQuad.dispose()}setSize(t,e){let i=Math.round(t/2),s=Math.round(e/2);this.renderTargetBright.setSize(i,s);for(let r=0;r<this.nMips;r++)this.renderTargetsHorizontal[r].setSize(i,s),this.renderTargetsVertical[r].setSize(i,s),this.separableBlurMaterials[r].uniforms.invSize.value=new et(1/i,1/s),i=Math.round(i/2),s=Math.round(s/2)}render(t,e,i,s,r){t.getClearColor(this._oldClearColor),this._oldClearAlpha=t.getClearAlpha();let o=t.autoClear;t.autoClear=!1,t.setClearColor(this.clearColor,0),r&&t.state.buffers.stencil.setTest(!1),this.renderToScreen&&(this._fsQuad.material=this._basic,this._basic.map=i.texture,t.setRenderTarget(null),t.clear(),this._fsQuad.render(t)),this.highPassUniforms.tDiffuse.value=i.texture,this.highPassUniforms.luminosityThreshold.value=this.threshold,this._fsQuad.material=this.materialHighPassFilter,t.setRenderTarget(this.renderTargetBright),t.clear(),this._fsQuad.render(t);let a=this.renderTargetBright;for(let l=0;l<this.nMips;l++)this._fsQuad.material=this.separableBlurMaterials[l],this.separableBlurMaterials[l].uniforms.colorTexture.value=a.texture,this.separableBlurMaterials[l].uniforms.direction.value=n.BlurDirectionX,t.setRenderTarget(this.renderTargetsHorizontal[l]),t.clear(),this._fsQuad.render(t),this.separableBlurMaterials[l].uniforms.colorTexture.value=this.renderTargetsHorizontal[l].texture,this.separableBlurMaterials[l].uniforms.direction.value=n.BlurDirectionY,t.setRenderTarget(this.renderTargetsVertical[l]),t.clear(),this._fsQuad.render(t),a=this.renderTargetsVertical[l];this._fsQuad.material=this.compositeMaterial,this.compositeMaterial.uniforms.bloomStrength.value=this.strength,this.compositeMaterial.uniforms.bloomRadius.value=this.radius,this.compositeMaterial.uniforms.bloomTintColors.value=this.bloomTintColors,t.setRenderTarget(this.renderTargetsHorizontal[0]),t.clear(),this._fsQuad.render(t),this._fsQuad.material=this.blendMaterial,this.copyUniforms.tDiffuse.value=this.renderTargetsHorizontal[0].texture,r&&t.state.buffers.stencil.setTest(!0),this.renderToScreen?(t.setRenderTarget(null),this._fsQuad.render(t)):(t.setRenderTarget(i),this._fsQuad.render(t)),t.setClearColor(this._oldClearColor,this._oldClearAlpha),t.autoClear=o}_getSeparableBlurMaterial(t){let e=[],i=t/3;for(let o=0;o<t;o++)e.push(.39894*Math.exp(-.5*o*o/(i*i))/i);let s=[],r=[];for(let o=1;o<t;o+=2){let a=e[o],l=o+1<t?e[o+1]:0,c=a+l;s.push((o*a+(o+1)*l)/c),r.push(c)}return new fe({defines:{KERNEL_PAIRS:s.length},uniforms:{colorTexture:{value:null},invSize:{value:new et(.5,.5)},direction:{value:new et(.5,.5)},centerWeight:{value:e[0]},gaussianOffsets:{value:s},gaussianWeights:{value:r}},vertexShader:`

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

				}`})}_getCompositeMaterial(t){return new fe({defines:{NUM_MIPS:t},uniforms:{blurTexture1:{value:null},blurTexture2:{value:null},blurTexture3:{value:null},blurTexture4:{value:null},blurTexture5:{value:null},bloomStrength:{value:1},bloomFactors:{value:null},bloomTintColors:{value:null},bloomRadius:{value:0}},vertexShader:`

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

				}`})}};dr.BlurDirectionX=new et(1,0);dr.BlurDirectionY=new et(0,1);var nu={name:"FXAAShader",uniforms:{tDiffuse:{value:null},resolution:{value:new et(1/1024,1/512)}},vertexShader:`

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

		}`};var wo={name:"OutputShader",uniforms:{tDiffuse:{value:null},toneMappingExposure:{value:1}},vertexShader:`
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

		}`};var ec=class extends di{constructor(){super(),this.isOutputPass=!0,this.uniforms=Hi.clone(wo.uniforms),this.material=new Ln({name:wo.name,uniforms:this.uniforms,vertexShader:wo.vertexShader,fragmentShader:wo.fragmentShader}),this._fsQuad=new $i(this.material),this._outputColorSpace=null,this._toneMapping=null}render(t,e,i){this.uniforms.tDiffuse.value=i.texture,this.uniforms.toneMappingExposure.value=t.toneMappingExposure,(this._outputColorSpace!==t.outputColorSpace||this._toneMapping!==t.toneMapping)&&(this._outputColorSpace=t.outputColorSpace,this._toneMapping=t.toneMapping,this.material.defines={},Zt.getTransfer(this._outputColorSpace)===ne&&(this.material.defines.SRGB_TRANSFER=""),this._toneMapping===to?this.material.defines.LINEAR_TONE_MAPPING="":this._toneMapping===eo?this.material.defines.REINHARD_TONE_MAPPING="":this._toneMapping===io?this.material.defines.CINEON_TONE_MAPPING="":this._toneMapping===cs?this.material.defines.ACES_FILMIC_TONE_MAPPING="":this._toneMapping===so?this.material.defines.AGX_TONE_MAPPING="":this._toneMapping===ro?this.material.defines.NEUTRAL_TONE_MAPPING="":this._toneMapping===no&&(this.material.defines.CUSTOM_TONE_MAPPING=""),this.material.needsUpdate=!0),this.renderToScreen===!0?(t.setRenderTarget(null),this._fsQuad.render(t)):(t.setRenderTarget(e),this.clear&&t.clear(t.autoClearColor,t.autoClearDepth,t.autoClearStencil),this._fsQuad.render(t))}dispose(){this.material.dispose(),this._fsQuad.dispose()}};function ip(){let n=new ec;return n.uniforms.extendedDisplay={value:!1},n.material.fragmentShader=`uniform bool extendedDisplay;
`+n.material.fragmentShader.replace("gl_FragColor.rgb = ACESFilmicToneMapping( gl_FragColor.rgb );",`if (extendedDisplay) {
       vec3 exposed = max(vec3(0.0), gl_FragColor.rgb * toneMappingExposure);
       gl_FragColor.rgb = exposed / (1.0 + max(exposed.r, max(exposed.g, exposed.b)) * 0.25);
     } else {
       gl_FragColor.rgb = ACESFilmicToneMapping(gl_FragColor.rgb);
     }`),n}var ic=class extends ps{constructor(){super(nu)}setSize(t,e){this.uniforms.resolution.value.set(1/t,1/e)}},nc=class extends ps{constructor(){super({uniforms:{tDiffuse:{value:null},grain:{value:.0015},phase:{value:0}},vertexShader:nu.vertexShader,fragmentShader:`
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
        }`})}};var ji={low:{dpr:.75,maxPixels:18e4,shadows:1,shadowSize:256,fps:10,fixtures:32,fullLights:4,fog:"phone"},mobile:{dpr:2,maxPixels:15e5,shadows:2,shadowSize:512,fps:120,fixtures:32,fullLights:8},desktop:{dpr:2,maxPixels:6e6,shadows:4,shadowSize:1024,fps:120,fixtures:128,fullLights:8},high:{dpr:4,maxPixels:8294400,shadows:4,shadowSize:2048,fps:120,fixtures:128,fullLights:16}};function np(n,t=null){let e=t??n?.fullLights??16;return Math.max(0,Math.min(16,Math.floor(Number(e)||0)))}function sp(n,t,e,i){let s=n==="low"&&t==="auto"&&!e&&i?ji.mobile.fps:ji[n].fps;return Math.min(s,i||s)}function sc(n,t,e){return ji[n==="low"&&t==="auto"&&!e?"mobile":n]}function rp(n,t,e,i,s,r){let o=Math.min(e||1,Math.sqrt(8294400/(n*t)),r/Math.max(n,t)),a=Math.min(o,i.dpr,Math.sqrt(i.maxPixels/(n*t)))*s,l=c=>[n,t].map(h=>Math.max(1,Math.round(h*c)));return{scene:l(a),display:l(o)}}function op(n){let t=i=>Array.from(n.getInternalformatParameter(n.RENDERBUFFER,i,n.SAMPLES)),e=t(n.DEPTH_COMPONENT24);return Math.max(0,...t(n.RGBA16F).filter(i=>i<=4&&e.includes(i)))}function ap(n,t,e,i){if(n==="low")return 0;for(let s of[i,2])if(s>=2&&s<=i&&t*e*s<=16e6)return s;return 0}function lp(n,t){let e=Object.keys(ji);return e[Math.max(0,Math.min(e.length-1,e.indexOf(n)+t))]}function cp(){let n=globalThis.WebGL2RenderingContext?.prototype;return!!(globalThis.matchMedia?.("(dynamic-range: high)").matches&&typeof n?.drawingBufferStorage=="function"&&typeof n?.drawingBufferToneMapping=="function")}function hp(n,t,e,i){if(!i||!n.getExtension("EXT_color_buffer_float"))return"sdr";try{if(n.drawingBufferStorage(n.RGBA16F,t,e),n.drawingBufferFormat===n.RGBA16F&&n.drawingBufferToneMapping({mode:"extended"})?.mode==="extended")return"extended"}catch{}if(n.drawingBufferFormat===n.RGBA16F){n.drawingBufferStorage(n.RGBA8,t,e);try{n.drawingBufferToneMapping({mode:"standard"})}catch{}}return"sdr"}var Eo=class{constructor(t){this.gl=t,this.extension=t.getExtension("EXT_disjoint_timer_query_webgl2")}sample(){let t=this.gl;if(!this.extension){let s=this.elapsed;return this.elapsed=null,s}if(!this.query)return null;let e=t.getParameter(this.extension.GPU_DISJOINT_EXT);if(!e&&!t.getQueryParameter(this.query,t.QUERY_RESULT_AVAILABLE))return null;let i=e?null:t.getQueryParameter(this.query,t.QUERY_RESULT)/1e6;return this.dispose(),i}begin(){return this.extension?this.query||(this.query=this.gl.createQuery(),!this.query)?!1:(this.gl.beginQuery(this.extension.TIME_ELAPSED_EXT,this.query),!0):this.fence?!1:(this.started=performance.now(),!0)}end(){let t=this.gl;if(this.extension){t.endQuery(this.extension.TIME_ELAPSED_EXT);return}if(this.fence=t.fenceSync(t.SYNC_GPU_COMMANDS_COMPLETE,0),!this.fence)return;t.flush();let e=()=>{let i=t.clientWaitSync(this.fence,0,0),s=performance.now()-this.started;if(i===t.TIMEOUT_EXPIRED&&s<3e4){this.timeout=setTimeout(e,2);return}this.elapsed=i===t.CONDITION_SATISFIED||i===t.ALREADY_SIGNALED?s:null,t.deleteSync(this.fence),this.fence=null};this.timeout=setTimeout(e,0)}dispose(){this.query&&this.gl.deleteQuery(this.query),this.fence&&this.gl.deleteSync(this.fence),clearTimeout(this.timeout),this.query=this.fence=this.elapsed=null}};var dp=(n,t)=>{if(typeof document>"u")return new OffscreenCanvas(n,t);let e=document.createElement("canvas");return e.width=n,e.height=t,e};function fr(n,t){let e=Math.sin(n*127.1+t*311.7)*43758.5453123;return e-Math.floor(e)}function up(n,t){let e=dp(n,n),i=e.getContext("2d"),s=i.createImageData(n,n);for(let a=0;a<n;a++)for(let l=0;l<n;l++){let c=fr(l,a),h=fr(Math.floor(l/24),Math.floor(a/24)),u=t==="floor"?108+c*28+h*10:139+c*28+h*10;t==="floor"&&(l<2||a<2)&&(u*=.62);let d=(a*n+l)*4;s.data[d]=u,s.data[d+1]=u*.98,s.data[d+2]=u*.94,s.data[d+3]=255}i.putImageData(s,0,0),i.strokeStyle="rgba(26,26,25,.12)",i.lineWidth=1;for(let a=0;a<40;a++){let l=fr(a,6)*n,c=fr(a,8)*n;i.beginPath(),i.moveTo(l,c),i.lineTo(l+fr(a,2)*80,c+fr(a,1)*5),i.stroke()}let r=new Xs(e);r.wrapS=r.wrapT=un,r.colorSpace=Ie,r.anisotropy=4;let o=r.clone();return o.colorSpace=ki,{map:r,bump:o}}function Cy(){let n=dp(64,64),t=n.getContext("2d");t.fillStyle="#353a3c",t.fillRect(0,0,64,64),t.fillStyle="#080a0b";for(let i=0;i<64;i+=8)for(let s=0;s<64;s+=8)t.beginPath(),t.arc(s+(i%16?4:0),i,2.5,0,Math.PI*2),t.fill();let e=new Xs(n);return e.wrapS=e.wrapT=un,e.repeat.set(3,5),e.colorSpace=Ie,e}function fp(){let n=up(512,"floor"),t=up(512,"wall");n.map.repeat.set(5,4),n.bump.repeat.copy(n.map.repeat),t.map.repeat.set(3,2),t.bump.repeat.copy(t.map.repeat);let e={floor:new Le({color:12960184,map:n.map,bumpMap:n.bump,bumpScale:.028,roughness:.42,metalness:.12}),wall:new Le({color:11972772,map:t.map,bumpMap:t.bump,bumpScale:.035,roughness:.91,metalness:.02}),metal:new Le({color:9147033,roughness:.28,metalness:.88}),blackMetal:new Le({color:2303786,roughness:.34,metalness:.75}),housing:new Le({color:1777186,roughness:.43,metalness:.32}),rubber:new Le({color:1118997,roughness:.85,metalness:0}),curtain:new Le({color:2632495,roughness:1,metalness:0,side:Ue}),stage:new Le({color:2105894,roughness:.73,metalness:.14}),grille:new Le({color:9606807,map:Cy(),roughness:.7,metalness:.55}),yellow:new Le({color:10189117,roughness:.65,metalness:.05})};return e.floor.userData.meanMapReflectance=.22,e.wall.userData.meanMapReflectance=.34,e.wall.shadowSide=Ue,e}function pp(n){let t=new Set;for(let e of Object.values(n)){for(let i of Object.values(e))i?.isTexture&&t.add(i);e.dispose()}for(let e of t)e.dispose()}function ms(n){let t=n["lens-radius-m"];return Number.isFinite(t)&&t>=0?t:.119}function su(n,t,e,i){let s=new C(...t).normalize(),r=new C(...n),o=i/Math.tan(e);return{optical:r,virtual:r.clone().addScaledVector(s,-o),axis:s,apexDistance:o,radius:i}}function ru(n,t,e){n.onBeforeCompile=i=>{i.uniforms.tolglowApexDepth=t,i.uniforms.tolglowPatchRadius=e,i.fragmentShader=`uniform float tolglowApexDepth[18]; uniform float tolglowPatchRadius[18];
`+i.fragmentShader;let s=Gt.lights_fragment_begin.replace("getSpotLightInfo( spotLight, geometryPosition, directLight );",`getSpotLightInfo( spotLight, geometryPosition, directLight );
       directLight.color *= step(tolglowApexDepth[i], dot(spotLight.position - geometryPosition, spotLight.direction));
       { float patchD2 = dot(spotLight.position - geometryPosition, spotLight.position - geometryPosition);
       directLight.color *= patchD2 / max(1.e-9, patchD2 + tolglowPatchRadius[i]*tolglowPatchRadius[i]); }`);i.fragmentShader=i.fragmentShader.replace("#include <lights_fragment_begin>",s)},n.customProgramCacheKey=()=>"tolglow-finite-aperture-patch-v2"}var To=new C;function Ri(n,t,e,i,s,r){let o=2*Math.PI*s/4,a=Math.max(r-2*s,0),l=Math.PI/4;To.copy(t),To[i]=0,To.normalize();let c=.5*o/(o+a),h=1-To.angleTo(n)/l;return Math.sign(To[e])===1?h*c:a/(o+a)+c+c*(1-h)}var pr=class n extends Te{constructor(t=1,e=1,i=1,s=2,r=.1){let o=s*2+1;if(r=Math.min(t/2,e/2,i/2,r),super(1,1,1,o,o,o),this.type="RoundedBoxGeometry",this.parameters={width:t,height:e,depth:i,segments:s,radius:r},o===1)return;let a=this.toNonIndexed();this.index=null,this.attributes.position=a.attributes.position,this.attributes.normal=a.attributes.normal,this.attributes.uv=a.attributes.uv;let l=new C,c=new C,h=new C(t,e,i).divideScalar(2).subScalar(r),u=this.attributes.position.array,d=this.attributes.normal.array,f=this.attributes.uv.array,m=u.length/6,x=new C,p=.5/o;for(let g=0,y=0;g<u.length;g+=3,y+=2)switch(l.fromArray(u,g),c.copy(l),c.x-=Math.sign(c.x)*p,c.y-=Math.sign(c.y)*p,c.z-=Math.sign(c.z)*p,c.normalize(),u[g+0]=h.x*Math.sign(l.x)+c.x*r,u[g+1]=h.y*Math.sign(l.y)+c.y*r,u[g+2]=h.z*Math.sign(l.z)+c.z*r,d[g+0]=c.x,d[g+1]=c.y,d[g+2]=c.z,Math.floor(g/m)){case 0:x.set(1,0,0),f[y+0]=Ri(x,c,"z","y",r,i),f[y+1]=1-Ri(x,c,"y","z",r,e);break;case 1:x.set(-1,0,0),f[y+0]=1-Ri(x,c,"z","y",r,i),f[y+1]=1-Ri(x,c,"y","z",r,e);break;case 2:x.set(0,1,0),f[y+0]=1-Ri(x,c,"x","z",r,t),f[y+1]=Ri(x,c,"z","x",r,i);break;case 3:x.set(0,-1,0),f[y+0]=1-Ri(x,c,"x","z",r,t),f[y+1]=1-Ri(x,c,"z","x",r,i);break;case 4:x.set(0,0,1),f[y+0]=1-Ri(x,c,"x","y",r,t),f[y+1]=1-Ri(x,c,"y","x",r,e);break;case 5:x.set(0,0,-1),f[y+0]=Ri(x,c,"x","y",r,t),f[y+1]=1-Ri(x,c,"y","x",r,e);break}}static fromJSON(t){return new n(t.width,t.height,t.depth,t.segments,t.radius)}};function we(n,t,e,i,s=0){let r=s?new pr(...t,2,s):new Te(...t),o=new Ut(r,i);return o.position.set(...e),o.castShadow=o.receiveShadow=!0,n.add(o),o}function ou(n,t,e,i,s,r=8){let o=new C().subVectors(new C(...e),new C(...t)),a=new Ut(new mi(i,i,o.length(),r),s);return a.position.set(...t).addScaledVector(o,.5),a.quaternion.setFromUnitVectors(new C(0,1,0),o.normalize()),a.castShadow=a.receiveShadow=!0,n.add(a),a}function mp(n,t){let e=new ve;e.name=`truss:${n.id}`,e.position.fromArray(n.position);let i=(n["rotation-deg"]||[0,0,0]).map(m=>m*Math.PI/180);e.rotation.set(...i,"ZYX");let s=Math.min(40,Math.max(.3,n.length)),r=s/2,o=.17,a=[];for(let m of[-o,o])for(let x of[-o,o])a.push([[-r,m,x],[r,m,x],.025]);let l=Math.ceil(s/.55),c=s/l;for(let m=0;m<l;m++){let x=-r+m*c,p=x+c,g=m%2?-1:1;for(let y of[-o,o])a.push([[x,-o*g,y],[p,o*g,y],.012]),a.push([[x,y,-o*g],[p,y,o*g],.012])}for(let m=0;m<=l;m++){let x=-r+m*c;a.push([[x,-o,-o],[x,o,-o],.012],[[x,-o,o],[x,o,o],.012],[[x,-o,-o],[x,-o,o],.012],[[x,o,-o],[x,o,o],.012])}let h=new ts(new mi(1,1,1,8),t.metal,a.length),u=new Me,d=new C,f=new C(0,1,0);return a.forEach(([m,x,p],g)=>{d.set(x[0]-m[0],x[1]-m[1],x[2]-m[2]);let y=d.length();u.position.set((m[0]+x[0])/2,(m[1]+x[1])/2,(m[2]+x[2])/2),u.quaternion.setFromUnitVectors(f,d.normalize()),u.scale.set(p,y,p),u.updateMatrix(),h.setMatrixAt(g,u.matrix)}),h.castShadow=h.receiveShadow=!0,e.add(h),e}function gp(n,t){let e=ms({}),i=new ve;i.name=`fixture:${n.id}`,i.position.fromArray(n.position||[0,0,0]);let s=we(i,[.52,.15,.36],[0,.62,0],t.housing,.025);we(i,[.44,.035,.27],[0,.71,0],t.blackMetal,.012);for(let d of[-.17,.17])we(i,[.055,.05,.11],[d,.73,0],t.rubber,.008);let r=new ve;i.add(r);for(let d of[-.22,.22]){we(r,[.065,.36,.11],[d,.39,0],t.housing,.02);let f=new Ut(new mi(.055,.055,.085,16),t.metal);f.rotation.z=Math.PI/2,f.position.set(d,.23,0),r.add(f)}let o=new ve;i.add(o),we(o,[.34,.36,.3],[0,.24,0],t.housing,.055);let a=new Ut(new mi(e*1.22,e*1.01,.16,32),t.blackMetal);a.position.y=.081,a.castShadow=a.receiveShadow=!0,o.add(a);let l=new Xr({color:2109754,metalness:.35,roughness:.1,clearcoat:1,clearcoatRoughness:.04,emissive:new bt(0,0,0),emissiveIntensity:1}),c=new Ut(new kr(e,40),l);c.rotation.x=Math.PI/2,o.add(c);let h=new Ut(new Wr(e+.014,.012,8,40),t.metal);h.rotation.x=Math.PI/2,h.position.y=-.002,o.add(h);for(let d=0;d<6;d++)we(o,[.27,.012,.012],[0,.17+d*.034,.156],t.rubber);we(s,[.12,.035,.004],[0,-.015,.182],t.rubber,.003);let u={root:i,head:o,yoke:r,lensMaterial:l,setAperture(d){let f=ms(d),m=f/e;this.lensRadius=f,a.scale.set(m,1,m),c.scale.set(m,m,1);let x=(f+.014)/(e+.014);h.scale.set(x,x,1)},dispose(){l.dispose()}};return u.setAperture(n),u}function au(n){let t=new Set;n.traverse(e=>{e.geometry&&t.add(e.geometry),e.userData.ownsReceiverMaterial&&e.material.dispose()});for(let e of t)e.dispose()}function Py(n,t){if(t?.method!=="bracketed-neutral-card"||!Number.isFinite(t.estimate)||t.estimate<0)return n;let e=n.clone();return e.map=null,e.metalness=0,e.roughness=1,e.color.setScalar(Math.min(1,t.estimate)),e.userData.meanMapReflectance=1,e.onBeforeCompile=n.onBeforeCompile,e.customProgramCacheKey=n.customProgramCacheKey,e}function lu([n,,t]){let e=Math.min(2.3,t*.27);return{width:n*.68,height:.42,depth:e,z:-t/2+e/2+.16}}function xp(n,t,e=[]){let[i,s,r]=n,o=new ve;o.name="venue-context";let a=new Ut(new Si(i,r),t.floor);a.userData.receiver="floor",a.rotation.x=-Math.PI/2,a.receiveShadow=!0,o.add(a),we(o,[i+.2,.16,r+.2],[0,-.09,0],t.blackMetal),we(o,[i,s,.18],[0,s/2,-r/2-.09],t.wall).userData.receiver="back";for(let v of[-i/2,i/2]){we(o,[.18,s,r],[v+Math.sign(v)*.09,s/2,0],t.wall).userData.receiver=v<0?"left":"right";for(let S of[-r*.4,r*.12,r*.4])we(o,[.2,s,.2],[v-Math.sign(v)*.13,s/2,S],t.blackMetal,.008),we(o,[.36,.08,.36],[v-Math.sign(v)*.13,.04,S],t.metal,.008);ou(o,[v-Math.sign(v)*.22,s-.25,-r/2],[v-Math.sign(v)*.22,s-.25,r/2],.075,t.blackMetal,12)}let{width:l,height:c,depth:h,z:u}=lu(n);we(o,[l,c,h],[0,c/2,u],t.stage,.025).userData.receiver="stage",we(o,[l,.025,.045],[0,.435,u+h/2-.02],t.stage,.008);for(let v=-l/2+1;v<l/2;v+=Math.max(1.2,l/64))we(o,[.008,.003,h-.08],[v,.423,u],t.rubber);for(let v=0;v<2;v++)we(o,[.85,.14*(v+1),.34],[l/2+.35,.07*(v+1),u+h/2-v*.34],t.stage,.015);let d=i*.7,f=s*.72,m=new Si(d,f,120,12),x=m.attributes.position;for(let v=0;v<x.count;v++){let S=x.getX(v),T=x.getY(v);x.setZ(v,.08*Math.sin(S*19)+.018*Math.sin(S*37+T*.5))}m.computeVertexNormals();let p=new Ut(m,t.curtain);p.position.set(0,.45+f/2,-r/2+.15),p.castShadow=p.receiveShadow=!0,o.add(p);for(let v of[-1,1]){let S=v*i*.395,T=-r*.31;for(let b=0;b<2;b++){we(o,[.68,.86,.58],[S,.43+b*.88,T],t.housing,.035),we(o,[.59,.72,.014],[S,.43+b*.88,T+.3],t.grille,.01);for(let _ of[-.25,.25])we(o,[.02,.07,.018],[S+_,.18+b*.88,T+.315],t.metal)}we(o,[.85,.5,.8],[S,.25,T+1],t.rubber,.035),we(o,[.72,.37,.015],[S,.26,T+1.41],t.grille,.012)}for(let v of[-r*.38,r*.05,r*.4])we(o,[i,.17,.1],[0,s-.12,v],t.blackMetal),ou(o,[-i/2,s-.3,v+.14],[i/2,s-.3,v+.14],.025,t.metal);let g=new qs([new C(-i*.4,.025,-r*.1),new C(-i*.39,.025,r*.2),new C(-i*.28,.025,r*.35),new C(-i*.1,.025,r*.38)]),y=new Ut(new ss(g,40,.012,5,!1),t.rubber);o.add(y);for(let v of e){let{id:S,point:T,normal:b,size:_}=v;if(!Array.isArray(_)||!_.every(R=>Number.isFinite(R)&&R>0))continue;let M=new C(...b).normalize(),D=new C(...v["u-axis"]);D.addScaledVector(M,-D.dot(M)).normalize();let P=Py(t.wall,v.reflectance),A=new Ut(new Si(..._),P);P!==t.wall&&(A.userData.ownsReceiverMaterial=!0),A.position.fromArray(T),A.quaternion.setFromRotationMatrix(new Xt().makeBasis(D,M.clone().cross(D),M)),A.userData.receiver=String(S),A.receiveShadow=A.castShadow=!0,o.add(A)}return o}function Iy(n,t){if(n.length<2)return null;let e=n.length-1,i=Math.min(256,e),s=Array.from({length:i+1},(f,m)=>new C(...n[Math.round(e*m/i)].position)),r=new Gr;for(let f=1;f<s.length;f++)s[f-1].equals(s[f])||r.add(new Ys(s[f-1],s[f]));if(!r.curves.length)return null;let o=i*2,a=12,l=new ss(r,o,n[0]["diffuser-radius-m"],a),c=new le(new Float32Array(l.attributes.position.count*3),3);c.setUsage(Zi),l.setAttribute("color",c);let h=new Le({color:8488587,roughness:.72,vertexColors:!0});h.onBeforeCompile=f=>{f.fragmentShader=f.fragmentShader.replace("#include <color_fragment>","").replace("#include <emissivemap_fragment>","totalEmissiveRadiance += 5.0 * vColor.rgb;")};let u=new Ut(l,h);u.name="diffuser:"+n[0]["pixel-layout-id"],u.receiveShadow=!0,t.add(u);let d=n.map(f=>({id:String(f.id),fixture:f}));return{rows:d,update(f){let m=d.map(({id:x})=>fs(f[x]));for(let x=0;x<=o;x++){let p=e*x/o,g=Math.floor(p),y=p-g,v=m[Math.max(0,g-1)],S=m[g],T=m[Math.min(e,g+1)],b=m[Math.min(e,g+2)],_=S.map((M,D)=>(1-y)*(.16*v[D]+.68*M+.16*T[D])+y*(.16*M+.68*T[D]+.16*b[D]));for(let M=0;M<=a;M++)c.setXYZ(x*(a+1)+M,..._)}c.needsUpdate=!0},dispose(){h.dispose()}}}function _p(n,t){let e=new Map;for(let i of n){let s=i["pixel-layout-id"],r=e.get(s);r?r.push(i):e.set(s,[i])}return[...e.values()].map(i=>Iy(i.sort((s,r)=>s["pixel-index"]-r["pixel-index"]),t)).filter(Boolean)}var{exp:Dy,max:cu}=Math;function hu(n,t,e){return Dy(-(cu(0,n)*cu(0,t)*cu(0,e)))}var Ao=`float fogTransmission(float extinction, float density, float distance) {
  return exp((-((max(0.0, extinction) * max(0.0, density)) * max(0.0, distance))));
}`;var yp=(n,t,e)=>Math.max(t,Math.min(e,n)),uu=n=>Number.isFinite(n)&&n>=0,vp=n=>Array.isArray(n)&&n.length===3&&n.every(Number.isFinite);var Mp=(n,t)=>n===t||Array.isArray(n)&&Array.isArray(t)&&n.length===t.length&&n.every((e,i)=>e===t[i]);function Ly(n,t){return n===t||n["extinction-per-m"]===t["extinction-per-m"]&&n["reference-path-m"]===t["reference-path-m"]&&n["fog-level"]===t["fog-level"]&&n["volume-m3"]===t["volume-m3"]&&n["mixed-m3"]===t["mixed-m3"]&&n["mixed-density"]===t["mixed-density"]&&n.sources.length===t.sources.length&&n.sources.every((e,i)=>{let s=t.sources[i];return e["fixture-id"]===s["fixture-id"]&&e["flow-m3-s"]===s["flow-m3-s"]&&e["local-m3"]===s["local-m3"]&&e["emitted-m3"]===s["emitted-m3"]&&Mp(e.position,s.position)&&Mp(e.direction,s.direction)})}function du(n,t,e){return{position:[...n.position],direction:[...n.direction],flow:n["flow-m3-s"],time:t,mass:e}}function Sp(n,t){let e=n.mass+t.mass,i=t.mass/e,s=(r,o)=>r+i*(o-r);return{mass:e,time:s(n.time,t.time),flow:s(n.flow,t.flow),position:n.position.map((r,o)=>s(r,t.position[o])),direction:n.direction.map((r,o)=>s(r,t.direction[o]))}}function Ny(n,t,e){let i=n["local-m3"],s=n["emitted-m3"];if(!i)return{emitted:s,parcels:[]};if(!t||s<t.emitted){let c=n["flow-m3-s"]>0?[.15,1,2.8]:[1,2.8,4.5];return{emitted:s,parcels:c.map((h,u)=>du(n,e-h*1e3,i*[.25,.45,.3][u])).reverse()}}let r=Math.min(i,s-t.emitted),o=i-r,a=t.parcels,l=a.reduce((c,h)=>c+h.mass,0);if(!o)a.length=0;else if(l)for(let c of a)c.mass*=o/l;else a.push(du(n,e-1500,o));if(r){let c=du(n,e,r),h=a.at(-1);h&&Math.floor(e/180)===Math.floor(h.time/180)?a[a.length-1]=Sp(h,c):a.push(c)}return a.length>8&&a.splice(0,2,Sp(a[0],a[1])),{emitted:s,parcels:a}}function Uy(n,t,e){let i=Math.max(0,(e-t.time)*.001),s=n.grid,r=n.spacing,o=n.room,a=Math.hypot(...t.direction)||1,l=t.direction.map(A=>A/a),h=Math.min(4.5,.45+Math.sqrt(t.flow))*(1-Math.exp(-i/1.4))+.12,u=.12+.15*Math.sqrt(i)+.035*Math.min(i,15),d=u+.3*Math.exp(-i/3),f=Math.min(i,12),m=t.position[0]*.9+t.position[2]*.65,x=[.07*f+.1*Math.sin(m+f)*Math.min(1,f),.11*f,.05*f+.09*Math.sin(m+f*.6)*Math.min(1,f)],p=[-o[0]/2,0,-o[2]/2],g=t.position.map((A,R)=>yp(A+h*l[R]+x[R],p[R],p[R]+o[R])),y=r.map(A=>Math.max(u,.48*A)),v=y.map(A=>3*Math.max(A,d)),S=(A,R)=>yp(Math.floor((R-p[A])/r[A]),0,s[A]-1),T=g.map((A,R)=>S(R,A-v[R])),b=g.map((A,R)=>S(R,A+v[R])),_=n.advected,M=Math.max(d,.48*Math.hypot(...r)),D=0;for(let A=T[2];A<=b[2];A++)for(let R=T[1];R<=b[1];R++)for(let U=T[0];U<=b[0];U++){let L=(U+.5)*r[0]+p[0]-g[0],B=(R+.5)*r[1]+p[1]-g[1],H=(A+.5)*r[2]+p[2]-g[2],z=L*l[0]+B*l[1]+H*l[2],Q=Math.exp(-.5*(((L-z*l[0])/y[0])**2+((B-z*l[1])/y[1])**2+((H-z*l[2])/y[2])**2+(z/M)**2));_[U+s[0]*(R+s[1]*A)]=Q,D+=Q}if(!D)return;let P=t.mass/(D*r[0]*r[1]*r[2]);for(let A=T[2];A<=b[2];A++)for(let R=T[1];R<=b[1];R++)for(let U=T[0];U<=b[0];U++){let L=U+s[0]*(R+s[1]*A);n.data[L]+=P*_[L]}}var rc=class{constructor(){this.sources=new Map,this.sample=null,this.rebase=!0,this.paintAt=null}suspend(){this.rebase=!0}accept(t,e){let i=e["sample-time-ms"];if(!Number.isFinite(i)||!uu(e["mixed-density"])||!Array.isArray(e.sources))return!1;let s=JSON.stringify([t,e.epoch]),r=this.sample?.["sample-time-ms"];if(s===this.epoch&&(i<r||!this.rebase&&i===r&&Ly(e,this.sample)))return!1;let o=this.rebase||s!==this.epoch||i-r>1e3,a=new Map,l=this.localVolume,c=this.sample?.["mixed-density"];this.localVolume=0;for(let h of e.sources){let u=h["fixture-id"];u==null||!vp(h.position)||!vp(h.direction)||!["local-m3","emitted-m3","flow-m3-s"].every(d=>uu(h[d]))||(this.localVolume+=h["local-m3"],a.set(u,Ny(h,o?null:this.sources.get(u),i)))}return this.sources=a,this.epoch=s,this.sample=e,this.rebase=!1,this.elapsed=0,this.dirty||=o||this.localVolume>0||l>0||c!==e["mixed-density"],!0}paint(t,e){if(!this.sample)return!1;this.elapsed=Math.min(.1,this.elapsed+Math.max(0,e));let i=this.sample["sample-time-ms"]+1e3*this.elapsed;if(!this.dirty&&this.field===t&&(!this.localVolume||Math.abs(i-this.paintAt)<49.99))return!1;t.data.fill(this.sample["mixed-density"]),t.temperature.fill(0),t.sources=[];for(let{parcels:s}of this.sources.values())for(let r of s)Uy(t,r,i);return t.time=i*.001%1e4,t.steps++,this.dirty=!1,this.paintAt=i,this.field=t,!0}get extinctionPerM(){let t=this.sample?.["extinction-per-m"];return uu(t)?t:1.25}stats(){return{sampleTimeMs:this.sample?.["sample-time-ms"],mixedDensity:this.sample?.["mixed-density"],extinctionPerM:this.extinctionPerM,referencePathM:this.sample?.["reference-path-m"],fogLevel:this.sample?.["fog-level"],mixedVolumeM3:this.sample?.["mixed-m3"],roomVolumeM3:this.sample?.["volume-m3"],localVolumeM3:this.localVolume,foggerCount:this.sources.size}}};var bp=[[0,0],[.68,0],[-.68,0],[0,.68],[0,-.68]],Fy=n=>Array.isArray(n)&&n.length===3&&n.every(Number.isFinite),fu=n=>Math.max(n.r,n.g,n.b);function Oy(n,t,e,i,s=1.25){if(t<=0||s<=0)return 1;let r=6,[o,a,l]=n.room,[c,h,u]=n.grid,d=0;for(let f=0;f<r;f++){let m=(f+.5)/r,x=(e.x+(i.x-e.x)*m)/o+.5,p=(e.y+(i.y-e.y)*m)/a,g=(e.z+(i.z-e.z)*m)/l+.5;d+=n.sample(x*c-.5,p*h-.5,g*u-.5)}return hu(s,t*d/r,e.distanceTo(i))}function By(n,t,e){if(!e||!["hit","center","normal","major-axis","minor-axis"].every(d=>Fy(e[d])))return;let i=e["major-radius-m"],s=e["minor-radius-m"];if(!(i>0&&s>0&&Number.isFinite(i*s)))return;let r=new C(...e.normal).normalize(),o=-t.direction.dot(r);if(o<=1e-5)return;let a=new C(...e.hit),l=a.clone().sub(t.position).dot(r)/-o;if(!(l>0&&l<t.distance-t.apexDistance))return;let c=t.position.clone().addScaledVector(t.direction,l),h=new C(...e.center).add(c.clone().sub(a)),u=t.intensity*o/(l+t.apexDistance)**2;return{id:n.id,fixture:n.root,surface:String(e["surface-id"]),origin:t.position,center:h,normal:r,major:new C(...e["major-axis"]).multiplyScalar(i),minor:new C(...e["minor-axis"]).multiplyScalar(s),radius:Math.sqrt(i*s),power:u*i*s,color:t.color}}var oc=class{constructor(t,e,i,s=[]){if(this.chunks=[],this.targets=[],e){t.updateMatrixWorld(!0);let r=new Map(s.map(o=>[o.root,o]));for(let o of t.children){let a=[];if(o.traverse(l=>{l.isMesh&&a.push(l)}),!!a.length)if(r.has(o))this.chunks.push({meshes:a,root:o,fixture:r.get(o),sphere:new ci});else for(let l of a){let c=l.isInstancedMesh?l:l.geometry;c.boundingSphere||c.computeBoundingSphere(),this.chunks.push({meshes:[l],mesh:l,local:c.boundingSphere,sphere:new ci})}}}this.ray=new qi,this.hits=[],this.lights=Array.from({length:e},()=>{let r=new os(0,0,15,Math.PI*.47,1,2);return r.castShadow=!0,r.shadow.autoUpdate=!1,r.shadow.needsUpdate=!0,r.shadow.mapSize.set(i,i),r.shadow.camera.near=.015,r.shadow.bias=-1e-4,r.shadow.normalBias=.005,t.add(r,r.target),r}),this.patches=[]}addCandidate(t,e,i,s){if(!this.lights.length||!e.fixture["pseudo-fixture"])return;let r=By(e,i,s);r&&t.push(r)}sample(t,e){let i=e.clone().sub(t.origin),s=i.length();this.ray.set(t.origin,i.normalize()),this.ray.near=.003,this.ray.far=s+.04,this.hits.length=0,this.targets.length=0;for(let{root:o,meshes:a,sphere:l}of this.chunks)o===t.fixture||t.origin.distanceToSquared(l.center)>(l.radius+this.ray.far)**2||this.ray.ray.intersectsSphere(l)&&this.targets.push(...a);let[r]=this.ray.intersectObjects(this.targets,!1,this.hits);if(!(r?.object.userData.receiver!==t.surface||Math.abs(r.distance-s)>.04))return r.object.material}resolve(t){if(this.patches.length=0,!!t.length){for(let e of this.chunks)e.fixture?(e.sphere.center.setFromMatrixPosition(e.root.matrixWorld),e.sphere.radius=Math.max(1,1.5*ms(e.fixture.fixture)+.3)):e.sphere.copy(e.local).applyMatrix4(e.mesh.matrixWorld);t.sort((e,i)=>fu(i.color)*i.power-fu(e.color)*e.power);for(let e of t){let i=new C,s=new bt(0,0,0),r=0;for(let[o,a]of bp){let l=e.center.clone().addScaledVector(e.major,o).addScaledVector(e.minor,a),c=this.sample(e,l);if(!c)continue;let h=c.color.clone().multiplyScalar((1-c.metalness)*(c.userData.meanMapReflectance??1));s.add(h),i.add(l),r++}if(!(!r||fu(s)<=0)&&(e.position=i.divideScalar(r),e.reflected=s.multiplyScalar(1/bp.length).multiply(e.color),this.patches.push(e),this.patches.length===this.lights.length))break}for(let e=0;e<this.patches.length;e++)this.lights[e].shadow.needsUpdate=!0}}illuminate(t,e,i){this.lights.forEach((s,r)=>{let o=this.patches[r];if(!o){s.intensity=0,s.color.setRGB(0,0,0);return}s.color.copy(o.reflected),s.intensity=o.power*Oy(t,e,o.origin,o.position,i),s.userData.patchRadius=o.radius,s.position.copy(o.position).addScaledVector(o.normal,.018),s.target.position.copy(s.position).add(o.normal),s.target.updateMatrixWorld()})}dispose(){for(let t of this.lights)t.dispose()}};var cc=class extends Ut{constructor(t,e,i=!1,s=!1,r=1e4){let o=new Jt;super(o,e),this.isMarchingCubes=!0;let a=this,l=new Float32Array(36),c=new Float32Array(36),h=new Float32Array(36);this.enableUvs=i,this.enableColors=s,this.init=function(y){this.resolution=y,this.isolation=80,this.size=y,this.size2=this.size*this.size,this.size3=this.size2*this.size,this.halfsize=this.size/2,this.delta=2/this.size,this.yd=this.size,this.zd=this.size2,this.field=new Float32Array(this.size3),this.normal_cache=new Float32Array(this.size3*3),this.palette=new Float32Array(this.size3*3),this.count=0;let v=r*3;this.positionArray=new Float32Array(v*3);let S=new le(this.positionArray,3);S.setUsage(Zi),o.setAttribute("position",S),this.normalArray=new Float32Array(v*3);let T=new le(this.normalArray,3);if(T.setUsage(Zi),o.setAttribute("normal",T),this.enableUvs){this.uvArray=new Float32Array(v*2);let b=new le(this.uvArray,2);b.setUsage(Zi),o.setAttribute("uv",b)}if(this.enableColors){this.colorArray=new Float32Array(v*3);let b=new le(this.colorArray,3);b.setUsage(Zi),o.setAttribute("color",b)}o.boundingSphere=new ci(new C,1)};function u(y,v,S){return y+(v-y)*S}function d(y,v,S,T,b,_,M,D,P,A){let R=(S-M)/(D-M),U=a.normal_cache;l[v+0]=T+R*a.delta,l[v+1]=b,l[v+2]=_,c[v+0]=u(U[y+0],U[y+3],R),c[v+1]=u(U[y+1],U[y+4],R),c[v+2]=u(U[y+2],U[y+5],R),h[v+0]=u(a.palette[P*3+0],a.palette[A*3+0],R),h[v+1]=u(a.palette[P*3+1],a.palette[A*3+1],R),h[v+2]=u(a.palette[P*3+2],a.palette[A*3+2],R)}function f(y,v,S,T,b,_,M,D,P,A){let R=(S-M)/(D-M),U=a.normal_cache;l[v+0]=T,l[v+1]=b+R*a.delta,l[v+2]=_;let L=y+a.yd*3;c[v+0]=u(U[y+0],U[L+0],R),c[v+1]=u(U[y+1],U[L+1],R),c[v+2]=u(U[y+2],U[L+2],R),h[v+0]=u(a.palette[P*3+0],a.palette[A*3+0],R),h[v+1]=u(a.palette[P*3+1],a.palette[A*3+1],R),h[v+2]=u(a.palette[P*3+2],a.palette[A*3+2],R)}function m(y,v,S,T,b,_,M,D,P,A){let R=(S-M)/(D-M),U=a.normal_cache;l[v+0]=T,l[v+1]=b,l[v+2]=_+R*a.delta;let L=y+a.zd*3;c[v+0]=u(U[y+0],U[L+0],R),c[v+1]=u(U[y+1],U[L+1],R),c[v+2]=u(U[y+2],U[L+2],R),h[v+0]=u(a.palette[P*3+0],a.palette[A*3+0],R),h[v+1]=u(a.palette[P*3+1],a.palette[A*3+1],R),h[v+2]=u(a.palette[P*3+2],a.palette[A*3+2],R)}function x(y){let v=y*3;a.normal_cache[v]===0&&(a.normal_cache[v+0]=a.field[y-1]-a.field[y+1],a.normal_cache[v+1]=a.field[y-a.yd]-a.field[y+a.yd],a.normal_cache[v+2]=a.field[y-a.zd]-a.field[y+a.zd])}function p(y,v,S,T,b){let _=T+1,M=T+a.yd,D=T+a.zd,P=_+a.yd,A=_+a.zd,R=T+a.yd+a.zd,U=_+a.yd+a.zd,L=0,B=a.field[T],H=a.field[_],z=a.field[M],Q=a.field[P],W=a.field[D],Z=a.field[A],$=a.field[R],xt=a.field[U];B<b&&(L|=1),H<b&&(L|=2),z<b&&(L|=8),Q<b&&(L|=4),W<b&&(L|=16),Z<b&&(L|=32),$<b&&(L|=128),xt<b&&(L|=64);let Mt=zy[L];if(Mt===0)return 0;let q=a.delta,st=y+q,_t=v+q,wt=S+q;Mt&1&&(x(T),x(_),d(T*3,0,b,y,v,S,B,H,T,_)),Mt&2&&(x(_),x(P),f(_*3,3,b,st,v,S,H,Q,_,P)),Mt&4&&(x(M),x(P),d(M*3,6,b,y,_t,S,z,Q,M,P)),Mt&8&&(x(T),x(M),f(T*3,9,b,y,v,S,B,z,T,M)),Mt&16&&(x(D),x(A),d(D*3,12,b,y,v,wt,W,Z,D,A)),Mt&32&&(x(A),x(U),f(A*3,15,b,st,v,wt,Z,xt,A,U)),Mt&64&&(x(R),x(U),d(R*3,18,b,y,_t,wt,$,xt,R,U)),Mt&128&&(x(D),x(R),f(D*3,21,b,y,v,wt,W,$,D,R)),Mt&256&&(x(T),x(D),m(T*3,24,b,y,v,S,B,W,T,D)),Mt&512&&(x(_),x(A),m(_*3,27,b,st,v,S,H,Z,_,A)),Mt&1024&&(x(P),x(U),m(P*3,30,b,st,_t,S,Q,xt,P,U)),Mt&2048&&(x(M),x(R),m(M*3,33,b,y,_t,S,z,$,M,R)),L<<=4;let rt,Pt,Qt,ot=0,Ct=0;for(;lc[L+Ct]!=-1;)rt=L+Ct,Pt=rt+1,Qt=rt+2,g(l,c,h,3*lc[rt],3*lc[Pt],3*lc[Qt]),Ct+=3,ot++;return ot}function g(y,v,S,T,b,_){let M=a.count*3;if(a.positionArray[M+0]=y[T],a.positionArray[M+1]=y[T+1],a.positionArray[M+2]=y[T+2],a.positionArray[M+3]=y[b],a.positionArray[M+4]=y[b+1],a.positionArray[M+5]=y[b+2],a.positionArray[M+6]=y[_],a.positionArray[M+7]=y[_+1],a.positionArray[M+8]=y[_+2],a.material.flatShading===!0){let D=(v[T+0]+v[b+0]+v[_+0])/3,P=(v[T+1]+v[b+1]+v[_+1])/3,A=(v[T+2]+v[b+2]+v[_+2])/3;a.normalArray[M+0]=D,a.normalArray[M+1]=P,a.normalArray[M+2]=A,a.normalArray[M+3]=D,a.normalArray[M+4]=P,a.normalArray[M+5]=A,a.normalArray[M+6]=D,a.normalArray[M+7]=P,a.normalArray[M+8]=A}else a.normalArray[M+0]=v[T+0],a.normalArray[M+1]=v[T+1],a.normalArray[M+2]=v[T+2],a.normalArray[M+3]=v[b+0],a.normalArray[M+4]=v[b+1],a.normalArray[M+5]=v[b+2],a.normalArray[M+6]=v[_+0],a.normalArray[M+7]=v[_+1],a.normalArray[M+8]=v[_+2];if(a.enableUvs){let D=a.count*2;a.uvArray[D+0]=y[T+0],a.uvArray[D+1]=y[T+2],a.uvArray[D+2]=y[b+0],a.uvArray[D+3]=y[b+2],a.uvArray[D+4]=y[_+0],a.uvArray[D+5]=y[_+2]}a.enableColors&&(a.colorArray[M+0]=S[T+0],a.colorArray[M+1]=S[T+1],a.colorArray[M+2]=S[T+2],a.colorArray[M+3]=S[b+0],a.colorArray[M+4]=S[b+1],a.colorArray[M+5]=S[b+2],a.colorArray[M+6]=S[_+0],a.colorArray[M+7]=S[_+1],a.colorArray[M+8]=S[_+2]),a.count+=3}this.addBall=function(y,v,S,T,b,_){let M=Math.sign(T);T=Math.abs(T);let D=_!=null,P=new bt(y,v,S);if(D)try{P=_ instanceof bt?_:Array.isArray(_)?new bt(Math.min(Math.abs(_[0]),1),Math.min(Math.abs(_[1]),1),Math.min(Math.abs(_[2]),1)):new bt(_)}catch{P=new bt(y,v,S)}let A=this.size*Math.sqrt(T/b),R=S*this.size,U=v*this.size,L=y*this.size,B=Math.floor(R-A);B<1&&(B=1);let H=Math.floor(R+A);H>this.size-1&&(H=this.size-1);let z=Math.floor(U-A);z<1&&(z=1);let Q=Math.floor(U+A);Q>this.size-1&&(Q=this.size-1);let W=Math.floor(L-A);W<1&&(W=1);let Z=Math.floor(L+A);Z>this.size-1&&(Z=this.size-1);let $,xt,Mt,q,st,_t,wt,rt,Pt,Qt,ot;for(Mt=B;Mt<H;Mt++)for(st=this.size2*Mt,rt=Mt/this.size-S,Pt=rt*rt,xt=z;xt<Q;xt++)for(q=st+this.size*xt,wt=xt/this.size-v,Qt=wt*wt,$=W;$<Z;$++)if(_t=$/this.size-y,ot=T/(1e-6+_t*_t+Qt+Pt)-b,ot>0){this.field[q+$]+=ot*M;let Ct=Math.sqrt(($-L)*($-L)+(xt-U)*(xt-U)+(Mt-R)*(Mt-R))/A,Bt=1-Ct*Ct*Ct*(Ct*(Ct*6-15)+10);this.palette[(q+$)*3+0]+=P.r*Bt,this.palette[(q+$)*3+1]+=P.g*Bt,this.palette[(q+$)*3+2]+=P.b*Bt}},this.addPlaneX=function(y,v){let S=this.size,T=this.yd,b=this.zd,_=this.field,M,D,P,A,R,U,L,B=S*Math.sqrt(y/v);for(B>S&&(B=S),M=0;M<B;M++)if(U=M/S,A=U*U,R=y/(1e-4+A)-v,R>0)for(D=0;D<S;D++)for(L=M+D*T,P=0;P<S;P++)_[b*P+L]+=R},this.addPlaneY=function(y,v){let S=this.size,T=this.yd,b=this.zd,_=this.field,M,D,P,A,R,U,L,B,H=S*Math.sqrt(y/v);for(H>S&&(H=S),D=0;D<H;D++)if(U=D/S,A=U*U,R=y/(1e-4+A)-v,R>0)for(L=D*T,M=0;M<S;M++)for(B=L+M,P=0;P<S;P++)_[b*P+B]+=R},this.addPlaneZ=function(y,v){let S=this.size,T=this.yd,b=this.zd,_=this.field,M,D,P,A,R,U,L,B,H=S*Math.sqrt(y/v);for(H>S&&(H=S),P=0;P<H;P++)if(U=P/S,A=U*U,R=y/(1e-4+A)-v,R>0)for(L=b*P,D=0;D<S;D++)for(B=L+D*T,M=0;M<S;M++)_[B+M]+=R},this.setCell=function(y,v,S,T){let b=this.size2*S+this.size*v+y;this.field[b]=T},this.getCell=function(y,v,S){let T=this.size2*S+this.size*v+y;return this.field[T]},this.blur=function(y=1){let v=this.field,S=v.slice(),T=this.size,b=this.size2;for(let _=0;_<T;_++)for(let M=0;M<T;M++)for(let D=0;D<T;D++){let P=b*D+T*M+_,A=S[P],R=1;for(let U=-1;U<=1;U+=2){let L=U+_;if(!(L<0||L>=T))for(let B=-1;B<=1;B+=2){let H=B+M;if(!(H<0||H>=T))for(let z=-1;z<=1;z+=2){let Q=z+D;if(Q<0||Q>=T)continue;let W=b*Q+T*H+L,Z=S[W];R++,A+=y*(Z-A)/R}}}v[P]=A}},this.reset=function(){for(let y=0;y<this.size3;y++)this.normal_cache[y*3]=0,this.field[y]=0,this.palette[y*3]=this.palette[y*3+1]=this.palette[y*3+2]=0},this.update=function(){this.count=0;let y=this.size-2;for(let v=1;v<y;v++){let S=this.size2*v,T=(v-this.halfsize)/this.halfsize;for(let b=1;b<y;b++){let _=S+this.size*b,M=(b-this.halfsize)/this.halfsize;for(let D=1;D<y;D++){let P=(D-this.halfsize)/this.halfsize,A=_+D;p(P,M,T,A,this.isolation)}}}this.geometry.setDrawRange(0,this.count),o.getAttribute("position").needsUpdate=!0,o.getAttribute("normal").needsUpdate=!0,this.enableUvs&&(o.getAttribute("uv").needsUpdate=!0),this.enableColors&&(o.getAttribute("color").needsUpdate=!0),this.count/3>r&&console.warn("THREE.MarchingCubes: Geometry buffers too small for rendering. Please create an instance with a higher poly count.")},this.init(t)}},zy=new Int32Array([0,265,515,778,1030,1295,1541,1804,2060,2309,2575,2822,3082,3331,3593,3840,400,153,915,666,1430,1183,1941,1692,2460,2197,2975,2710,3482,3219,3993,3728,560,825,51,314,1590,1855,1077,1340,2620,2869,2111,2358,3642,3891,3129,3376,928,681,419,170,1958,1711,1445,1196,2988,2725,2479,2214,4010,3747,3497,3232,1120,1385,1635,1898,102,367,613,876,3180,3429,3695,3942,2154,2403,2665,2912,1520,1273,2035,1786,502,255,1013,764,3580,3317,4095,3830,2554,2291,3065,2800,1616,1881,1107,1370,598,863,85,348,3676,3925,3167,3414,2650,2899,2137,2384,1984,1737,1475,1226,966,719,453,204,4044,3781,3535,3270,3018,2755,2505,2240,2240,2505,2755,3018,3270,3535,3781,4044,204,453,719,966,1226,1475,1737,1984,2384,2137,2899,2650,3414,3167,3925,3676,348,85,863,598,1370,1107,1881,1616,2800,3065,2291,2554,3830,4095,3317,3580,764,1013,255,502,1786,2035,1273,1520,2912,2665,2403,2154,3942,3695,3429,3180,876,613,367,102,1898,1635,1385,1120,3232,3497,3747,4010,2214,2479,2725,2988,1196,1445,1711,1958,170,419,681,928,3376,3129,3891,3642,2358,2111,2869,2620,1340,1077,1855,1590,314,51,825,560,3728,3993,3219,3482,2710,2975,2197,2460,1692,1941,1183,1430,666,915,153,400,3840,3593,3331,3082,2822,2575,2309,2060,1804,1541,1295,1030,778,515,265,0]),lc=new Int32Array([-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,0,8,3,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,0,1,9,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,1,8,3,9,8,1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,1,2,10,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,0,8,3,1,2,10,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,9,2,10,0,2,9,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,2,8,3,2,10,8,10,9,8,-1,-1,-1,-1,-1,-1,-1,3,11,2,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,0,11,2,8,11,0,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,1,9,0,2,3,11,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,1,11,2,1,9,11,9,8,11,-1,-1,-1,-1,-1,-1,-1,3,10,1,11,10,3,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,0,10,1,0,8,10,8,11,10,-1,-1,-1,-1,-1,-1,-1,3,9,0,3,11,9,11,10,9,-1,-1,-1,-1,-1,-1,-1,9,8,10,10,8,11,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,4,7,8,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,4,3,0,7,3,4,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,0,1,9,8,4,7,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,4,1,9,4,7,1,7,3,1,-1,-1,-1,-1,-1,-1,-1,1,2,10,8,4,7,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,3,4,7,3,0,4,1,2,10,-1,-1,-1,-1,-1,-1,-1,9,2,10,9,0,2,8,4,7,-1,-1,-1,-1,-1,-1,-1,2,10,9,2,9,7,2,7,3,7,9,4,-1,-1,-1,-1,8,4,7,3,11,2,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,11,4,7,11,2,4,2,0,4,-1,-1,-1,-1,-1,-1,-1,9,0,1,8,4,7,2,3,11,-1,-1,-1,-1,-1,-1,-1,4,7,11,9,4,11,9,11,2,9,2,1,-1,-1,-1,-1,3,10,1,3,11,10,7,8,4,-1,-1,-1,-1,-1,-1,-1,1,11,10,1,4,11,1,0,4,7,11,4,-1,-1,-1,-1,4,7,8,9,0,11,9,11,10,11,0,3,-1,-1,-1,-1,4,7,11,4,11,9,9,11,10,-1,-1,-1,-1,-1,-1,-1,9,5,4,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,9,5,4,0,8,3,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,0,5,4,1,5,0,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,8,5,4,8,3,5,3,1,5,-1,-1,-1,-1,-1,-1,-1,1,2,10,9,5,4,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,3,0,8,1,2,10,4,9,5,-1,-1,-1,-1,-1,-1,-1,5,2,10,5,4,2,4,0,2,-1,-1,-1,-1,-1,-1,-1,2,10,5,3,2,5,3,5,4,3,4,8,-1,-1,-1,-1,9,5,4,2,3,11,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,0,11,2,0,8,11,4,9,5,-1,-1,-1,-1,-1,-1,-1,0,5,4,0,1,5,2,3,11,-1,-1,-1,-1,-1,-1,-1,2,1,5,2,5,8,2,8,11,4,8,5,-1,-1,-1,-1,10,3,11,10,1,3,9,5,4,-1,-1,-1,-1,-1,-1,-1,4,9,5,0,8,1,8,10,1,8,11,10,-1,-1,-1,-1,5,4,0,5,0,11,5,11,10,11,0,3,-1,-1,-1,-1,5,4,8,5,8,10,10,8,11,-1,-1,-1,-1,-1,-1,-1,9,7,8,5,7,9,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,9,3,0,9,5,3,5,7,3,-1,-1,-1,-1,-1,-1,-1,0,7,8,0,1,7,1,5,7,-1,-1,-1,-1,-1,-1,-1,1,5,3,3,5,7,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,9,7,8,9,5,7,10,1,2,-1,-1,-1,-1,-1,-1,-1,10,1,2,9,5,0,5,3,0,5,7,3,-1,-1,-1,-1,8,0,2,8,2,5,8,5,7,10,5,2,-1,-1,-1,-1,2,10,5,2,5,3,3,5,7,-1,-1,-1,-1,-1,-1,-1,7,9,5,7,8,9,3,11,2,-1,-1,-1,-1,-1,-1,-1,9,5,7,9,7,2,9,2,0,2,7,11,-1,-1,-1,-1,2,3,11,0,1,8,1,7,8,1,5,7,-1,-1,-1,-1,11,2,1,11,1,7,7,1,5,-1,-1,-1,-1,-1,-1,-1,9,5,8,8,5,7,10,1,3,10,3,11,-1,-1,-1,-1,5,7,0,5,0,9,7,11,0,1,0,10,11,10,0,-1,11,10,0,11,0,3,10,5,0,8,0,7,5,7,0,-1,11,10,5,7,11,5,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,10,6,5,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,0,8,3,5,10,6,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,9,0,1,5,10,6,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,1,8,3,1,9,8,5,10,6,-1,-1,-1,-1,-1,-1,-1,1,6,5,2,6,1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,1,6,5,1,2,6,3,0,8,-1,-1,-1,-1,-1,-1,-1,9,6,5,9,0,6,0,2,6,-1,-1,-1,-1,-1,-1,-1,5,9,8,5,8,2,5,2,6,3,2,8,-1,-1,-1,-1,2,3,11,10,6,5,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,11,0,8,11,2,0,10,6,5,-1,-1,-1,-1,-1,-1,-1,0,1,9,2,3,11,5,10,6,-1,-1,-1,-1,-1,-1,-1,5,10,6,1,9,2,9,11,2,9,8,11,-1,-1,-1,-1,6,3,11,6,5,3,5,1,3,-1,-1,-1,-1,-1,-1,-1,0,8,11,0,11,5,0,5,1,5,11,6,-1,-1,-1,-1,3,11,6,0,3,6,0,6,5,0,5,9,-1,-1,-1,-1,6,5,9,6,9,11,11,9,8,-1,-1,-1,-1,-1,-1,-1,5,10,6,4,7,8,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,4,3,0,4,7,3,6,5,10,-1,-1,-1,-1,-1,-1,-1,1,9,0,5,10,6,8,4,7,-1,-1,-1,-1,-1,-1,-1,10,6,5,1,9,7,1,7,3,7,9,4,-1,-1,-1,-1,6,1,2,6,5,1,4,7,8,-1,-1,-1,-1,-1,-1,-1,1,2,5,5,2,6,3,0,4,3,4,7,-1,-1,-1,-1,8,4,7,9,0,5,0,6,5,0,2,6,-1,-1,-1,-1,7,3,9,7,9,4,3,2,9,5,9,6,2,6,9,-1,3,11,2,7,8,4,10,6,5,-1,-1,-1,-1,-1,-1,-1,5,10,6,4,7,2,4,2,0,2,7,11,-1,-1,-1,-1,0,1,9,4,7,8,2,3,11,5,10,6,-1,-1,-1,-1,9,2,1,9,11,2,9,4,11,7,11,4,5,10,6,-1,8,4,7,3,11,5,3,5,1,5,11,6,-1,-1,-1,-1,5,1,11,5,11,6,1,0,11,7,11,4,0,4,11,-1,0,5,9,0,6,5,0,3,6,11,6,3,8,4,7,-1,6,5,9,6,9,11,4,7,9,7,11,9,-1,-1,-1,-1,10,4,9,6,4,10,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,4,10,6,4,9,10,0,8,3,-1,-1,-1,-1,-1,-1,-1,10,0,1,10,6,0,6,4,0,-1,-1,-1,-1,-1,-1,-1,8,3,1,8,1,6,8,6,4,6,1,10,-1,-1,-1,-1,1,4,9,1,2,4,2,6,4,-1,-1,-1,-1,-1,-1,-1,3,0,8,1,2,9,2,4,9,2,6,4,-1,-1,-1,-1,0,2,4,4,2,6,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,8,3,2,8,2,4,4,2,6,-1,-1,-1,-1,-1,-1,-1,10,4,9,10,6,4,11,2,3,-1,-1,-1,-1,-1,-1,-1,0,8,2,2,8,11,4,9,10,4,10,6,-1,-1,-1,-1,3,11,2,0,1,6,0,6,4,6,1,10,-1,-1,-1,-1,6,4,1,6,1,10,4,8,1,2,1,11,8,11,1,-1,9,6,4,9,3,6,9,1,3,11,6,3,-1,-1,-1,-1,8,11,1,8,1,0,11,6,1,9,1,4,6,4,1,-1,3,11,6,3,6,0,0,6,4,-1,-1,-1,-1,-1,-1,-1,6,4,8,11,6,8,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,7,10,6,7,8,10,8,9,10,-1,-1,-1,-1,-1,-1,-1,0,7,3,0,10,7,0,9,10,6,7,10,-1,-1,-1,-1,10,6,7,1,10,7,1,7,8,1,8,0,-1,-1,-1,-1,10,6,7,10,7,1,1,7,3,-1,-1,-1,-1,-1,-1,-1,1,2,6,1,6,8,1,8,9,8,6,7,-1,-1,-1,-1,2,6,9,2,9,1,6,7,9,0,9,3,7,3,9,-1,7,8,0,7,0,6,6,0,2,-1,-1,-1,-1,-1,-1,-1,7,3,2,6,7,2,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,2,3,11,10,6,8,10,8,9,8,6,7,-1,-1,-1,-1,2,0,7,2,7,11,0,9,7,6,7,10,9,10,7,-1,1,8,0,1,7,8,1,10,7,6,7,10,2,3,11,-1,11,2,1,11,1,7,10,6,1,6,7,1,-1,-1,-1,-1,8,9,6,8,6,7,9,1,6,11,6,3,1,3,6,-1,0,9,1,11,6,7,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,7,8,0,7,0,6,3,11,0,11,6,0,-1,-1,-1,-1,7,11,6,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,7,6,11,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,3,0,8,11,7,6,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,0,1,9,11,7,6,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,8,1,9,8,3,1,11,7,6,-1,-1,-1,-1,-1,-1,-1,10,1,2,6,11,7,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,1,2,10,3,0,8,6,11,7,-1,-1,-1,-1,-1,-1,-1,2,9,0,2,10,9,6,11,7,-1,-1,-1,-1,-1,-1,-1,6,11,7,2,10,3,10,8,3,10,9,8,-1,-1,-1,-1,7,2,3,6,2,7,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,7,0,8,7,6,0,6,2,0,-1,-1,-1,-1,-1,-1,-1,2,7,6,2,3,7,0,1,9,-1,-1,-1,-1,-1,-1,-1,1,6,2,1,8,6,1,9,8,8,7,6,-1,-1,-1,-1,10,7,6,10,1,7,1,3,7,-1,-1,-1,-1,-1,-1,-1,10,7,6,1,7,10,1,8,7,1,0,8,-1,-1,-1,-1,0,3,7,0,7,10,0,10,9,6,10,7,-1,-1,-1,-1,7,6,10,7,10,8,8,10,9,-1,-1,-1,-1,-1,-1,-1,6,8,4,11,8,6,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,3,6,11,3,0,6,0,4,6,-1,-1,-1,-1,-1,-1,-1,8,6,11,8,4,6,9,0,1,-1,-1,-1,-1,-1,-1,-1,9,4,6,9,6,3,9,3,1,11,3,6,-1,-1,-1,-1,6,8,4,6,11,8,2,10,1,-1,-1,-1,-1,-1,-1,-1,1,2,10,3,0,11,0,6,11,0,4,6,-1,-1,-1,-1,4,11,8,4,6,11,0,2,9,2,10,9,-1,-1,-1,-1,10,9,3,10,3,2,9,4,3,11,3,6,4,6,3,-1,8,2,3,8,4,2,4,6,2,-1,-1,-1,-1,-1,-1,-1,0,4,2,4,6,2,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,1,9,0,2,3,4,2,4,6,4,3,8,-1,-1,-1,-1,1,9,4,1,4,2,2,4,6,-1,-1,-1,-1,-1,-1,-1,8,1,3,8,6,1,8,4,6,6,10,1,-1,-1,-1,-1,10,1,0,10,0,6,6,0,4,-1,-1,-1,-1,-1,-1,-1,4,6,3,4,3,8,6,10,3,0,3,9,10,9,3,-1,10,9,4,6,10,4,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,4,9,5,7,6,11,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,0,8,3,4,9,5,11,7,6,-1,-1,-1,-1,-1,-1,-1,5,0,1,5,4,0,7,6,11,-1,-1,-1,-1,-1,-1,-1,11,7,6,8,3,4,3,5,4,3,1,5,-1,-1,-1,-1,9,5,4,10,1,2,7,6,11,-1,-1,-1,-1,-1,-1,-1,6,11,7,1,2,10,0,8,3,4,9,5,-1,-1,-1,-1,7,6,11,5,4,10,4,2,10,4,0,2,-1,-1,-1,-1,3,4,8,3,5,4,3,2,5,10,5,2,11,7,6,-1,7,2,3,7,6,2,5,4,9,-1,-1,-1,-1,-1,-1,-1,9,5,4,0,8,6,0,6,2,6,8,7,-1,-1,-1,-1,3,6,2,3,7,6,1,5,0,5,4,0,-1,-1,-1,-1,6,2,8,6,8,7,2,1,8,4,8,5,1,5,8,-1,9,5,4,10,1,6,1,7,6,1,3,7,-1,-1,-1,-1,1,6,10,1,7,6,1,0,7,8,7,0,9,5,4,-1,4,0,10,4,10,5,0,3,10,6,10,7,3,7,10,-1,7,6,10,7,10,8,5,4,10,4,8,10,-1,-1,-1,-1,6,9,5,6,11,9,11,8,9,-1,-1,-1,-1,-1,-1,-1,3,6,11,0,6,3,0,5,6,0,9,5,-1,-1,-1,-1,0,11,8,0,5,11,0,1,5,5,6,11,-1,-1,-1,-1,6,11,3,6,3,5,5,3,1,-1,-1,-1,-1,-1,-1,-1,1,2,10,9,5,11,9,11,8,11,5,6,-1,-1,-1,-1,0,11,3,0,6,11,0,9,6,5,6,9,1,2,10,-1,11,8,5,11,5,6,8,0,5,10,5,2,0,2,5,-1,6,11,3,6,3,5,2,10,3,10,5,3,-1,-1,-1,-1,5,8,9,5,2,8,5,6,2,3,8,2,-1,-1,-1,-1,9,5,6,9,6,0,0,6,2,-1,-1,-1,-1,-1,-1,-1,1,5,8,1,8,0,5,6,8,3,8,2,6,2,8,-1,1,5,6,2,1,6,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,1,3,6,1,6,10,3,8,6,5,6,9,8,9,6,-1,10,1,0,10,0,6,9,5,0,5,6,0,-1,-1,-1,-1,0,3,8,5,6,10,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,10,5,6,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,11,5,10,7,5,11,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,11,5,10,11,7,5,8,3,0,-1,-1,-1,-1,-1,-1,-1,5,11,7,5,10,11,1,9,0,-1,-1,-1,-1,-1,-1,-1,10,7,5,10,11,7,9,8,1,8,3,1,-1,-1,-1,-1,11,1,2,11,7,1,7,5,1,-1,-1,-1,-1,-1,-1,-1,0,8,3,1,2,7,1,7,5,7,2,11,-1,-1,-1,-1,9,7,5,9,2,7,9,0,2,2,11,7,-1,-1,-1,-1,7,5,2,7,2,11,5,9,2,3,2,8,9,8,2,-1,2,5,10,2,3,5,3,7,5,-1,-1,-1,-1,-1,-1,-1,8,2,0,8,5,2,8,7,5,10,2,5,-1,-1,-1,-1,9,0,1,5,10,3,5,3,7,3,10,2,-1,-1,-1,-1,9,8,2,9,2,1,8,7,2,10,2,5,7,5,2,-1,1,3,5,3,7,5,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,0,8,7,0,7,1,1,7,5,-1,-1,-1,-1,-1,-1,-1,9,0,3,9,3,5,5,3,7,-1,-1,-1,-1,-1,-1,-1,9,8,7,5,9,7,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,5,8,4,5,10,8,10,11,8,-1,-1,-1,-1,-1,-1,-1,5,0,4,5,11,0,5,10,11,11,3,0,-1,-1,-1,-1,0,1,9,8,4,10,8,10,11,10,4,5,-1,-1,-1,-1,10,11,4,10,4,5,11,3,4,9,4,1,3,1,4,-1,2,5,1,2,8,5,2,11,8,4,5,8,-1,-1,-1,-1,0,4,11,0,11,3,4,5,11,2,11,1,5,1,11,-1,0,2,5,0,5,9,2,11,5,4,5,8,11,8,5,-1,9,4,5,2,11,3,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,2,5,10,3,5,2,3,4,5,3,8,4,-1,-1,-1,-1,5,10,2,5,2,4,4,2,0,-1,-1,-1,-1,-1,-1,-1,3,10,2,3,5,10,3,8,5,4,5,8,0,1,9,-1,5,10,2,5,2,4,1,9,2,9,4,2,-1,-1,-1,-1,8,4,5,8,5,3,3,5,1,-1,-1,-1,-1,-1,-1,-1,0,4,5,1,0,5,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,8,4,5,8,5,3,9,0,5,0,3,5,-1,-1,-1,-1,9,4,5,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,4,11,7,4,9,11,9,10,11,-1,-1,-1,-1,-1,-1,-1,0,8,3,4,9,7,9,11,7,9,10,11,-1,-1,-1,-1,1,10,11,1,11,4,1,4,0,7,4,11,-1,-1,-1,-1,3,1,4,3,4,8,1,10,4,7,4,11,10,11,4,-1,4,11,7,9,11,4,9,2,11,9,1,2,-1,-1,-1,-1,9,7,4,9,11,7,9,1,11,2,11,1,0,8,3,-1,11,7,4,11,4,2,2,4,0,-1,-1,-1,-1,-1,-1,-1,11,7,4,11,4,2,8,3,4,3,2,4,-1,-1,-1,-1,2,9,10,2,7,9,2,3,7,7,4,9,-1,-1,-1,-1,9,10,7,9,7,4,10,2,7,8,7,0,2,0,7,-1,3,7,10,3,10,2,7,4,10,1,10,0,4,0,10,-1,1,10,2,8,7,4,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,4,9,1,4,1,7,7,1,3,-1,-1,-1,-1,-1,-1,-1,4,9,1,4,1,7,0,8,1,8,7,1,-1,-1,-1,-1,4,0,3,7,4,3,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,4,8,7,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,9,10,8,10,11,8,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,3,0,9,3,9,11,11,9,10,-1,-1,-1,-1,-1,-1,-1,0,1,10,0,10,8,8,10,11,-1,-1,-1,-1,-1,-1,-1,3,1,10,11,3,10,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,1,2,11,1,11,9,9,11,8,-1,-1,-1,-1,-1,-1,-1,3,0,9,3,9,11,1,2,9,2,11,9,-1,-1,-1,-1,0,2,11,8,0,11,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,3,2,11,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,2,3,8,2,8,10,10,8,9,-1,-1,-1,-1,-1,-1,-1,9,10,2,0,9,2,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,2,3,8,2,8,10,0,1,8,1,10,8,-1,-1,-1,-1,1,10,2,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,1,3,8,9,1,8,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,0,9,1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,0,3,8,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1]);var ky=`
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
}`,Hy=`
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
}`;function Vy({resolution:n,field:t,padding:e}){let i=new pi,s=new cc(n,i,!1,!1,18e3);s.field.set(t),s.isolation=0,s.update();let r=new Jt,o=Math.min(s.count,s.geometry.attributes.position.count)*3;for(let a of["position","normal"]){let l=s.geometry.attributes[a].array.slice(0,o);r.setAttribute(a,new le(l,3))}return s.geometry.dispose(),i.dispose(),r.scale(e,e,e),r.computeBoundingSphere(),r}function Gy(n){switch(n.kind){case"sphere":return new pn(1,48,32);case"box":return new Te(2,2,2);case"cylinder":return new mi(1,1,2,48);case"field":return Vy(n);case"rounded-box":{let[t,e,i]=n.size,s=Math.max(0,n.rounding)*Math.min(t,e,i)/2;return(s>0?new pr(t,e,i,5,s):new Te(t,e,i)).scale(2/t,2/e,2/i)}default:throw new Error("Unknown optical surface")}}var ke=n=>({value:n}),pu=()=>Array.from({length:4},()=>new C);function Wy(n,t,e,i){let s=new fe({vertexShader:ky,fragmentShader:Hy,side:Ue,uniforms:{...t,tint:ke(new bt),sizeM:ke(1),strength:ke(1),velocity:ke(0)}}),r=new Ut(n,s);return r.position.copy(e),{mesh:r,start:e.clone(),end:e.clone(),at:i,period:25,speed:0}}function Xy(n,t,e,i){let{mesh:s}=n;if(!n.end.equals(e)){let u=i-n.at;n.speed=Math.min(6,e.distanceTo(n.end)/(Math.max(1,u)/1e3)),n.start.copy(u>220?e:s.position),n.end.copy(e),n.at=i,n.period=rr.clamp(u,8,80)}let r=rr.clamp(t.deformation||0,0,.5),o=1/Math.sqrt(1-r),a=s.scale.fromArray(t["half-size"]);a.set(a.x*o,a.y*(1-r),a.z*o);let{sizeM:l,tint:c,strength:h}=s.material.uniforms;l.value=Math.max(.02,Math.min(a.x,a.y,a.z)),c.value.fromArray(t.color||[.65,.85,1]),h.value=t.strength??1}function qy(n,t){let e=t.filter(i=>i.intensity>0).sort((i,s)=>s.intensity-i.intensity).slice(0,4);for(let i=0;i<4;i++){let s=e[i],r=n.sourceColor.value[i];if(!s){r.set(0,0,0);continue}let o=n.sourcePos.value[i].setFromMatrixPosition(s.matrixWorld);n.sourceDir.value[i].setFromMatrixPosition(s.target.matrixWorld).sub(o).normalize(),r.set(s.color.r,s.color.g,s.color.b).multiplyScalar(s.intensity/50),n.coneCos.value.setComponent(i,Math.cos(s.angle))}}var hc=class extends di{constructor(t){super(),this.engine=t,this.enabled=!1,this.needsSwap=!1,this.scene=new Ye,this.geometry=new Map,this.instances=new Map,this.target=new C,this.uniforms={sceneColor:ke(null),roomEnvironment:ke(null),opaqueDepth:ke(null),atmosphere:ke(null),resolution:ke(new et(1,1)),lens:ke(new Xt),clockS:ke(0),workLight:ke(0),fogAmount:ke(0),nearZ:ke(.1),farZ:ke(100),sourcePos:ke(pu()),sourceColor:ke(pu()),sourceDir:ke(pu()),coneCos:ke(new oe)}}setSize(t,e){this.uniforms.resolution.value.set(t,e)}update(t,e,i=performance.now()){let s=new Set,r=new Set;for(let o of(e||[]).slice(0,12)){let a=t?.[o.mesh];if(!a)continue;this.geometry.has(o.mesh)||this.geometry.set(o.mesh,Gy(a));let l=this.geometry.get(o.mesh),c=this.target.fromArray(o.position),h=this.instances.get(o.id);h||(h=Wy(l,this.uniforms,c,i),this.instances.set(o.id,h),this.scene.add(h.mesh)),h.mesh.geometry=l,Xy(h,o,c,i),s.add(o.mesh),r.add(o.id)}for(let[o,{mesh:a}]of this.instances)r.has(o)||(this.scene.remove(a),a.material.dispose(),this.instances.delete(o));for(let[o,a]of this.geometry)s.has(o)||(a.dispose(),this.geometry.delete(o));this.enabled=r.size>0}render(t,e,i){let{camera:s,clock:r,compositeTarget:o,sceneTarget:a,fog:l,fogAmount:c,workLight:h,spots:u,environmentTarget:d}=this.engine,f=this.uniforms,m=performance.now();for(let g of this.instances.values()){let y=Math.max(0,m-g.at);g.mesh.position.lerpVectors(g.start,g.end,Math.min(1,y/g.period)),g.mesh.material.uniforms.velocity.value=g.speed*Math.exp(-Math.max(0,y-g.period)/220)}f.sceneColor.value=o.texture,f.opaqueDepth.value=a.depthTexture,f.atmosphere.value=l.target.texture,f.roomEnvironment.value=d.texture,f.clockS.value=r,f.fogAmount.value=c,f.workLight.value=h,f.lens.value.copy(s.projectionMatrix),f.nearZ.value=s.near,f.farZ.value=s.far,qy(f,u||[]);let x=t.autoClear,p=t.getRenderTarget();try{t.setRenderTarget(i),t.autoClear=!1,t.clearDepth(),t.render(this.scene,s)}finally{t.autoClear=x,t.setRenderTarget(p)}}dispose(){this.update({},[])}};var gs=(n,t,e)=>Math.max(t,Math.min(e,n)),uc=(n,t)=>{if(!Number.isFinite(n))throw new Error(`Invalid ${t}`);return n},Co=Object.freeze({high:Object.freeze({grid:[48,24,48],scale:.67,pixels:14e5,steps:72,shadows:4,mode:"raymarch"}),desktop:Object.freeze({grid:[32,18,32],scale:.5,pixels:52e4,steps:48,shadows:4,mode:"raymarch"}),mobile:Object.freeze({grid:[24,14,24],scale:.34,pixels:3e5,steps:32,shadows:2,mode:"raymarch"}),phone:Object.freeze({grid:[20,12,20],scale:.33,pixels:12e4,steps:24,shadows:1,mode:"haze"})});function wp(n,t,e){let i=Math.max(1,n*t);return Math.min(e.scale,e.pixels?Math.sqrt(e.pixels/i):e.scale)}var Ro=class n{constructor({grid:t=Co.desktop.grid,room:e=[12,5,10],initialHaze:i=.07}={}){if(!Array.isArray(t)||t.length!==3||t.some(r=>!Number.isInteger(r)||r<4||r>64))throw new Error("Invalid fog grid");this.grid=[...t],this.initialHaze=gs(uc(i,"haze"),0,1);let s=t[0]*t[1]*t[2];this.data=new Float32Array(s),this.temperature=new Float32Array(s),this.advected=new Float32Array(s),this.advectedHeat=new Float32Array(s),this.next=new Float32Array(s),this.nextHeat=new Float32Array(s),this.steps=0,this.time=0,this.accumulator=0,this.droppedSeconds=0,this.sources=[],this.setRoom(e)}setRoom(t){if(!Array.isArray(t)||t.length!==3||t.some(e=>!Number.isFinite(e)||e<=0))throw new Error("Invalid fog room");return this.room?.every((e,i)=>e===t[i])?!1:(this.room=[...t],this.spacing=t.map((e,i)=>e/this.grid[i]),this.clear(this.initialHaze),!0)}clear(t=0){uc(t,"haze"),this.data.fill(gs(t,0,6)),this.temperature.fill(0),this.sources=[],this.accumulator=0,this.time=0,this.steps=0,this.droppedSeconds=0}copyFrom(t){if(!(t instanceof n))throw new Error("Expected an owned fog field");this.setRoom(t.room);let[e,i,s]=this.grid,[r,o,a]=t.grid;for(let l=0;l<s;l++)for(let c=0;c<i;c++)for(let h=0;h<e;h++){let u=h+c*e+l*e*i,d=(h+.5)*r/e-.5,f=(c+.5)*o/i-.5,m=(l+.5)*a/s-.5;this.data[u]=t.sample(d,f,m),this.temperature[u]=t.sample(d,f,m,t.temperature)}this.sources=t.sources.map(l=>({...l,position:[...l.position],direction:[...l.direction]}));for(let l of["time","steps","accumulator","droppedSeconds"])this[l]=t[l]}burst({position:t=[-this.room[0]*.34,.22,-this.room[2]*.24],direction:e=[.7,.12,.7],duration:i=2.8,strength:s=1.2}={}){if(![t,e].every(c=>Array.isArray(c)&&c.length===3&&c.every(Number.isFinite)))throw new Error("Invalid fog source");let r=Math.hypot(...e);if(r<1e-6)throw new Error("Fog source needs a direction");let o=e.map(c=>c/r),a={position:[...t],direction:o,remaining:gs(uc(i,"burst duration"),0,8),strength:gs(uc(s,"burst strength"),0,8)},l=this.sources.find(c=>c.position.every((h,u)=>h===t[u]));l?(l.remaining=Math.min(8,l.remaining+a.remaining),l.strength=a.strength):(this.sources.length>=4&&this.sources.shift(),this.sources.push(a))}sample(t,e,i,s=this.data){let[r,o,a]=this.grid;t=gs(t,0,r-1),e=gs(e,0,o-1),i=gs(i,0,a-1);let l=Math.floor(t),c=Math.floor(e),h=Math.floor(i),u=Math.min(l+1,r-1),d=Math.min(c+1,o-1),f=Math.min(h+1,a-1),m=t-l,x=e-c,p=i-h,g=r*o,y=l+c*r+h*g,v=u+c*r+h*g,S=l+d*r+h*g,T=u+d*r+h*g,b=(f-h)*g,_=(s[y]*(1-m)+s[v]*m)*(1-x)+(s[S]*(1-m)+s[T]*m)*x,M=(s[y+b]*(1-m)+s[v+b]*m)*(1-x)+(s[S+b]*(1-m)+s[T+b]*m)*x;return _*(1-p)+M*p}advance(t){if(!Number.isFinite(t)||t<0)throw new Error("Fog dt must be finite and nonnegative");let e=Math.min(t,.1);this.droppedSeconds+=t-e,this.accumulator+=e;let i=0;for(;this.accumulator+1e-10>=.05&&i<2;)this.step(.05),this.accumulator=Math.max(0,this.accumulator-.05),i++;return i}step(t){let[e,i,s]=this.grid,[r,o,a]=this.spacing,l=e*i,c=this.data,h=this.temperature,u=this.advected,d=this.advectedHeat,f=this.time,m=this.room[0],x=this.room[2],p=Array.from({length:e},(D,P)=>{let A=(P+.5)*r-m/2;return[A,Math.cos(A*1.5+f*.43),Math.cos(A*1.5-f*.43),Math.cos(A*4.3-f*.8)]}),g=Array.from({length:i},(D,P)=>{let A=(P+.5)*o;return[A,Math.cos(A*1.7+f*.43),Math.sin(A*1.7+f*.31),Math.cos(A*4.1-f*.8),Math.sin(A*4.1+f*.7)]}),y=Array.from({length:s},(D,P)=>{let A=(P+.5)*a-x/2;return[A,Math.sin(A*1.3-f*.31),Math.sin(A*1.3+f*.31),Math.sin(A*3.7+f*.7)]});for(let D=0;D<s;D++)for(let P=0;P<i;P++)for(let A=0;A<e;A++){let R=A+P*e+D*l,[U,L,B,H]=p[A],[z,Q,W,Z,$]=g[P],[xt,Mt,q,st]=y[D],_t=Math.min(1,h[R])*.17,wt=.12+.11*(Q-Mt)+_t*(Z-st),rt=h[R]*.38+.035+.08*(q-L)+_t*(st-H),Pt=.025+.11*(B-W)+_t*(H-$),Qt=0;for(let ot of this.sources){let Ct=U-ot.position[0],Bt=z-ot.position[1],Nt=xt-ot.position[2],Kt=Ct*ot.direction[0]+Bt*ot.direction[1]+Nt*ot.direction[2];if(Kt<-.2||Kt>1.8)continue;let Yt=.2+Math.max(0,Kt)*.24,Se=Math.max(0,Ct*Ct+Bt*Bt+Nt*Nt-Kt*Kt),ie=Math.exp(-Se/(2*Yt*Yt)-((Kt-.45)/.85)**2),Re=Math.sin(Kt*8-f*5.5+z*3),_e=.72+.28*Re*Re,Ee=ie*_e*ot.strength;Qt+=Ee,wt+=ot.direction[0]*Ee*.9,rt+=ot.direction[1]*Ee*.9,Pt+=ot.direction[2]*Ee*.9}u[R]=Math.min(6,this.sample(A-t*wt/r,P-t*rt/o,D-t*Pt/a,c)+Qt*t*3.2),d[R]=Math.min(4,this.sample(A-t*wt/r,P-t*rt/o,D-t*Pt/a,h)+Qt*t*2.4)}let v=t*.025/(r*r),S=t*.025/(o*o),T=t*.025/(a*a),b=Math.min(1,.45/(v+S+T));v*=b,S*=b,T*=b;let _=Math.exp(-t/100),M=Math.exp(-t/4.5);for(let D=0;D<s;D++)for(let P=0;P<i;P++)for(let A=0;A<e;A++){let R=A+P*e+D*l,U=A?R-1:R,L=A<e-1?R+1:R,B=P?R-e:R,H=P<i-1?R+e:R,z=D?R-l:R,Q=D<s-1?R+l:R;this.next[R]=Math.max(0,(u[R]+v*(u[U]+u[L]-2*u[R])+S*(u[B]+u[H]-2*u[R])+T*(u[z]+u[Q]-2*u[R]))*_),this.nextHeat[R]=Math.max(0,(d[R]+v*(d[U]+d[L]-2*d[R])+S*(d[B]+d[H]-2*d[R])+T*(d[z]+d[Q]-2*d[R]))*M)}[this.data,this.next]=[this.next,this.data],[this.temperature,this.nextHeat]=[this.nextHeat,this.temperature];for(let D of this.sources)D.remaining-=t;this.sources=this.sources.filter(D=>D.remaining>1e-8),this.time+=t,this.steps++}stats(){let t=0,e=0,i=0;for(let s=0;s<this.data.length;s++)t+=this.data[s],e=Math.max(e,this.data[s]),i+=this.temperature[s];return{cells:this.data.length,steps:this.steps,time:this.time,droppedSeconds:this.droppedSeconds,meanDensity:t/this.data.length,maximumDensity:e,heat:i/this.data.length,activeSources:this.sources.length,massEstimate:t*this.spacing[0]*this.spacing[1]*this.spacing[2]}}};var Yy=`out vec2 vUv;
void main(){vUv=uv;gl_Position=vec4(position.xy,0.0,1.0);}`,Ap=`precision highp float;
precision highp sampler3D;
#include <packing>
${Ao}
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
}`,Zy=`${Ap}
out vec4 volumeOutput;
void main(){integrateFog(volumeOutput);}`,Ky=`${Ap}
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
}`,Jy=`precision highp float;
precision highp sampler3D;
${Ao}
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
}`,Ep=()=>globalThis.performance?.now?.()??Date.now(),$y=()=>{let n=new Fi(new Uint32Array([4294967295]),1,1,Li,hi);return n.internalFormat="DEPTH_COMPONENT24",n.compareFunction=nr,n.minFilter=n.magFilter=ce,n.needsUpdate=!0,n};function Tp(n){let t=new ks(new Uint16Array(n.data.length),...n.grid);return Object.assign(t,{format:zn,type:xe,internalFormat:"R16F",minFilter:ce,magFilter:ce,unpackAlignment:1})}var dc=class{constructor(t,{quality:e="desktop"}={}){if(!Co[e])throw new Error("Unknown fog quality");this.renderer=t,this.quality=e,this.budget=Co[e],this.disposed=!1,this.field=new Ro({grid:this.budget.grid}),this.field.burst(),this.amount=.5,this.density=Tp(this.field),this.packed=this.density.image.data,this.dummyShadow=$y(),this.target=new me(1,1,{type:xe,format:ui,depthBuffer:!1,stencilBuffer:!1,minFilter:ce,magFilter:ce}),this.lights=[],this.updateMs=0,this.uploads=0,this.width=1,this.height=1;let i=r=>Array.from({length:r},()=>new C);this.uniforms={sceneDepth:{value:null},densityTexture:{value:this.density},inverseProjection:{value:new Xt},cameraWorld:{value:new Xt},eye:{value:new C},room:{value:new C(...this.field.room)},detailMin:{value:new C},detailMax:{value:new C},amount:{value:this.amount},time:{value:0},mixedDensity:{value:0},extinctionPerM:{value:1.25},raySteps:{value:this.budget.steps},lightCount:{value:0},lightPosition:{value:i(16)},lightDirection:{value:i(16)},lightColor:{value:i(16)},lightShape:{value:i(16)},lightShadow:{value:new Int32Array(16).fill(-1)},lightProfile:{value:Array.from({length:16},()=>new oe)},lightStart:{value:new Float32Array(16)},ambientSky:{value:new C},ambientGround:{value:new C},shadowMatrix:{value:Array.from({length:4},()=>new Xt)},shadowTexel:{value:Array.from({length:4},()=>new et(1,1))},shadowBias:{value:new Float32Array(4)}};for(let r=0;r<4;r++)this.uniforms[`shadow${r}`]={value:this.dummyShadow};this.compositeUniforms={...this.uniforms,sceneColor:{value:null},volume:{value:this.target.texture},volumeSize:{value:new et(1,1)},nearPlane:{value:.08},farPlane:{value:80},beamLayer:{value:!1}};let s=(r,o)=>new fe({vertexShader:Yy,fragmentShader:r,uniforms:o,glslVersion:_n,depthTest:!1,depthWrite:!1,blending:wi,toneMapped:!1});this.volumeMaterial=s(Zy,this.uniforms),this.compositeMaterial=s(Ky,this.compositeUniforms),this.hazeMaterial=s(Jy,this.compositeUniforms),this.mode=this.budget.mode||"raymarch",this.quad=new Ut(new Si(2,2),this.volumeMaterial),this.quad.frustumCulled=!1,this.scene=new Ye,this.scene.add(this.quad),this.camera=new mn,this.upload()}upload(){let[t,e,i]=this.field.grid,[s,r,o]=this.field.room,a=this.showAtmosphere?this.showAtmosphere.sample?.["mixed-density"]||0:this.field.initialHaze,l=Gs.toHalfFloat(a+.22),c=t,h=e,u=i,d=-1,f=-1,m=-1;for(let g=0;g<this.packed.length;g++){let y=Gs.toHalfFloat(Math.min(65504,this.field.data[g]));if(this.packed[g]=y,y<l)continue;let v=g%t,S=Math.floor(g/t)%e,T=Math.floor(g/(t*e));c=Math.min(c,v),h=Math.min(h,S),u=Math.min(u,T),d=Math.max(d,v),f=Math.max(f,S),m=Math.max(m,T)}let x=this.uniforms.detailMin.value,p=this.uniforms.detailMax.value;d<0?(x.set(1,1,1),p.set(-1,-1,-1)):(x.set((Math.max(0,c-1)/t-.5)*s,Math.max(0,h-1)/e*r,(Math.max(0,u-1)/i-.5)*o),p.set((Math.min(t,d+2)/t-.5)*s,Math.min(e,f+2)/e*r,(Math.min(i,m+2)/i-.5)*o)),this.density.needsUpdate=!0,this.uploads++}setQuality(t){let e=Co[t];if(!e)throw new Error("Unknown fog quality");if(this.disposed||this.quality===t)return;if(e.grid.every((r,o)=>r===this.budget.grid[o])){this.quality=t,this.budget=e,this.mode=e.mode||"raymarch",this.uniforms.raySteps.value=e.steps;return}let i=new Ro({grid:e.grid});i.copyFrom(this.field);let s=Tp(i);this.density.dispose(),this.field=i,this.density=s,this.packed=s.image.data,this.quality=t,this.budget=e,this.mode=e.mode||"raymarch",this.uniforms.densityTexture.value=s,this.uniforms.raySteps.value=e.steps,this.upload()}setRoom(t){this.field.setRoom(t)&&(this.uniforms.room.value.fromArray(t),this.showAtmosphere?(this.showAtmosphere.suspend(),this.field.clear()):this.field.burst(),this.upload())}get extinctionPerM(){return this.showAtmosphere?.extinctionPerM??1.25}setAmount(t){if(!Number.isFinite(t))throw new Error("Invalid fog amount");this.amount=Math.max(0,Math.min(1,t))}setAmbient({color:t,intensity:e=0,groundColor:i=t}={}){if(!Number.isFinite(e)||e<0)throw new Error("Invalid atmosphere ambient intensity");let s=(r,o)=>{if(!o)return r.value.set(0,0,0);if(![o.r,o.g,o.b].every(a=>Number.isFinite(a)&&a>=0))throw new Error("Invalid atmosphere ambient color");r.value.set(o.r,o.g,o.b).multiplyScalar(e)};s(this.uniforms.ambientSky,t),s(this.uniforms.ambientGround,i)}setAtmosphere(t,e){e!=null?(this.showAtmosphere||(this.showAtmosphere=new rc,this.field.clear(0),this.upload()),this.showAtmosphere.accept(t,e)):this.showAtmosphere&&(this.showAtmosphere=null,this.field.clear(this.field.initialHaze),this.field.burst(),this.upload())}burst(t){return this.showAtmosphere?!1:(this.field.burst(t),!0)}setSize(t,e){if(![t,e].every(s=>Number.isFinite(s)&&s>0))return;let i=wp(t,e,this.budget);this.width=Math.max(1,Math.floor(t*i)),this.height=Math.max(1,Math.floor(e*i)),this.sceneWidth=t,this.sceneHeight=e,this.target.setSize(this.width,this.height),this.compositeUniforms.volumeSize.value.set(this.width,this.height)}update(t,e=[]){if(this.disposed)return;let i=Ep();this.lights=this.mode==="haze"?[]:e.slice(0,16),(this.showAtmosphere?this.showAtmosphere.paint(this.field,t):this.field.advance(t))&&this.upload(),this.updateMs=Ep()-i}render(t,e,i=null,s=null){if(this.disposed)return;if(!t?.depthTexture||!t?.texture)throw new Error("Fog requires opaque scene color and depth");let r=this.renderer,o=r.getRenderTarget(),a=r.autoClear,l=this.uniforms,c=this.compositeUniforms;l.sceneDepth.value=t.depthTexture,l.inverseProjection.value.copy(e.projectionMatrixInverse),l.cameraWorld.value.copy(e.matrixWorld),l.eye.value.setFromMatrixPosition(e.matrixWorld);let h=this.showAtmosphere,u=h&&!h.localVolume&&!h.sample?.["mixed-density"]?0:this.amount;l.extinctionPerM.value=this.extinctionPerM,l.mixedDensity.value=h?.sample?.["mixed-density"]||0,l.amount.value=u,l.time.value=this.field.time,l.lightCount.value=this.lights.length,l.lightShadow.value.fill(-1);let d=0;for(let f=0;f<4;f++)l[`shadow${f}`].value=this.dummyShadow;this.lights.forEach((f,m)=>{l.lightPosition.value[m].copy(f.virtualPosition||f.position),l.lightDirection.value[m].copy(f.direction).normalize(),l.lightColor.value[m].set(f.color.r,f.color.g,f.color.b),l.lightShape.value[m].set(Math.cos(f.angle),Math.max(0,f.intensity??1),Math.max(0,f.distance??0));let x=Math.max(0,Math.min(1,f.penumbra??.55)),p=Math.max(f.angle,Math.min(1.54,f.angle+.23)),g=Math.tan(f.angle);if(l.lightProfile.value[m].set(Math.cos(f.angle*(1-x)),g,Math.tan(p)-g,Math.cos(p)),l.lightStart.value[m]=Math.max(0,f.apexDistance??0),f.shadow?.map?.depthTexture&&d<this.budget.shadows){if(f.shadow.map.depthTexture.compareFunction!==nr)throw new Error("Fog requires forward-depth PCF comparison shadow textures");let y=d++;l.lightShadow.value[m]=y,l[`shadow${y}`].value=f.shadow.map.depthTexture,l.shadowMatrix.value[y].copy(f.shadow.matrix),l.shadowTexel.value[y].set(1/f.shadow.map.width,1/f.shadow.map.height),l.shadowBias.value[y]=f.shadow.bias??-2e-4}}),this.shadowCount=d,this.visibleAmount=u,c.sceneColor.value=t.texture,c.sceneDepth.value=t.depthTexture,c.nearPlane.value=e.near,c.farPlane.value=e.far,c.amount.value=u;try{if(r.autoClear=!0,this.mode==="haze"){if(c.beamLayer.value=!1,u>0&&s){let f=r.getClearColor(new bt),m=r.getClearAlpha();r.setClearColor(0,1),r.setRenderTarget(this.target),r.clear(!0,!1,!1),r.setClearColor(f,m),c.beamLayer.value=!!s(this.target)}r.setRenderTarget(i),this.quad.material=this.hazeMaterial,r.render(this.scene,this.camera)}else u>0&&(r.setRenderTarget(this.target),this.quad.material=this.volumeMaterial,r.render(this.scene,this.camera),s?.(this.target)),r.setRenderTarget(i),this.quad.material=this.compositeMaterial,r.render(this.scene,this.camera)}finally{r.setRenderTarget(o),r.autoClear=a}}stats(){return{quality:this.quality,mode:this.mode,targetSize:this.mode==="haze"?[this.sceneWidth,this.sceneHeight]:[this.width,this.height],raySteps:this.mode==="haze"?8:this.budget.steps,lightCount:this.lights.length,shadowCount:this.shadowCount??0,cpuUpdateMs:this.updateMs,uploads:this.uploads,atmosphere:this.showAtmosphere?.stats(),...this.field.stats()}}dispose(){this.disposed||(this.disposed=!0,this.target.dispose(),this.density.dispose(),this.dummyShadow.dispose(),this.volumeMaterial.dispose(),this.compositeMaterial.dispose(),this.hazeMaterial.dispose(),this.quad.geometry.dispose(),this.scene.clear(),this.lights=[])}};var fc=class{constructor(t,e,i=document,s=window){this.focused=i.hasFocus(),this.visibleDocument=!i.hidden,this.external=!1,this.externalOwners=new Set,this.inViewport=!1,this.pageActive=!0,this.active=!1,this.abort=new AbortController;let r=this.abort.signal,o=()=>{let l=this.pageActive&&(this.external||this.inViewport&&this.visibleDocument&&this.focused);l!==this.active&&(this.active=l,t.dispatchEvent?.(new CustomEvent("tolglow-preview-activity",{detail:l})),e(l))};t.addEventListener?.("tolglow-preview-external",l=>{if(l.detail&&typeof l.detail=="object"){let{owner:c,active:h}=l.detail;h?this.externalOwners.add(c):this.externalOwners.delete(c)}else l.detail===!0?this.externalOwners.add("pip"):this.externalOwners.delete("pip");this.external=this.externalOwners.size>0,o()},{signal:r});let a=l=>{this.focused=l,o()};s.addEventListener("focus",()=>a(!0),{signal:r}),s.addEventListener("blur",()=>a(!1),{signal:r}),i.addEventListener("visibilitychange",()=>{this.visibleDocument=!i.hidden,o()},{signal:r}),s.addEventListener("pagehide",()=>{this.pageActive=!1,o()},{signal:r}),s.addEventListener("pageshow",()=>{this.pageActive=!0,this.visibleDocument=!i.hidden,this.focused=i.hasFocus(),o()},{signal:r}),this.observer=new s.IntersectionObserver(l=>{let c=l.at(-1);!c||r.aborted||(this.inViewport=c.isIntersecting&&c.intersectionRatio>0,o())},{threshold:[0,Number.EPSILON]}),this.observer.observe(t)}dispose(){this.abort.abort(),this.observer.disconnect(),this.active=!1}};function mu(n,t){let e=new Set(t),i=new Set(n),s=t.filter(o=>!i.has(o)),r=0;return n.map(o=>e.has(o)?o:s[r++])}function Rp(n,t,e,i,s,r){let o=new Set(s),a=[];for(let[m,x]of Object.entries(t)){let p=n.get(m),g=x?.["emitted-color"];if(!p||!Array.isArray(g)||g.length!==3||!g.every(Number.isFinite))continue;let y=Math.max(...g)*(p["relative-brightness"]??1);if(y<=0)continue;let v=p.channels?.pan!=null&&p.channels?.tilt!=null;a.push({id:m,level:y,moving:v})}a.sort((m,x)=>+(x.id===e)-+(m.id===e)||Number(x.moving)-Number(m.moving)||x.level-m.level||Number(o.has(x.id))-Number(o.has(m.id))||(m.id<x.id?-1:1));let l=Array.from(new Set([a[0],a.find(m=>!m.moving),...a].filter(Boolean).map(m=>m.id))),c=new Set;n.has(e)&&c.add(e);for(let m of l)c.add(m);for(let m of i)c.add(m);let h=mu(i,Array.from(c).slice(0,i.length)),u=new Set(h),d=new Set(l.filter(m=>u.has(m)));for(let m of s)u.has(m)&&d.add(m);for(let m of h)d.add(m);let f=Array.from(d).slice(0,s.length);return{meshes:h,lights:[...mu(s.slice(0,r),f.slice(0,r)),...mu(s.slice(r),f.slice(r))]}}var jy=Object.freeze({}),Cp=n=>Array.isArray(n)&&n.length===3&&n.every(Number.isFinite),pc=(n,t,e)=>n+(t-n)*e;function Qy(n,t,e,i){if(!n||!t||t["cut?"]||n.strobe>0||t.strobe>0||n.channels?.strobe>0||t.channels?.strobe>0||n.shutter!==t.shutter||n.channels?.shutter!==t.channels?.shutter)return t;let s={...t};for(let l of["intensity","dimmer"])Number.isFinite(n[l])&&Number.isFinite(t[l])&&(s[l]=pc(n[l],t[l],i));let r=n["emitted-color"],o=t["emitted-color"];Cp(r)&&Cp(o)&&(s["emitted-color"]=o.map((l,c)=>pc(r[c],l,i)));let a=["pan","tilt"];if(a.every(l=>Hn(n,l)!==void 0&&Hn(t,l)!==void 0&&Math.abs(yn(e,t,l)-yn(e,n,l))<180)){for(let l of a)s[l]=pc(Hn(n,l),Hn(t,l),i),s[l+"-deg"]=pc(yn(e,n,l),yn(e,t,l),i);s.pose={"pan-deg":s["pan-deg"],"tilt-deg":s["tilt-deg"]},delete s.direction,delete s.aim}return s}var mc=class{constructor(){this.mode="hold"}setMode(t){if(!["hold","interpolate"].includes(t))throw new RangeError("Unknown preview lighting mode");this.mode!==t&&(this.mode=t,this.previous=null)}accept(t,e,i){let s=this.latest,r=t["time-ms"]-s?.["time-ms"],o=this.mode==="interpolate"&&s&&this.rigKey===e&&En(t)===En(s)&&t.revision===s.revision&&!t["blackout?"]&&!s["blackout?"]&&t.master!==0&&s.master!==0&&!t["cut?"]&&t["frame-number"]===s["frame-number"]+1&&r>0&&r<=50&&i>=this.receivedAt&&i-this.receivedAt<75;this.latest=t,this.previous=o?s:null,this.rigKey=e,this.period=r,this.receivedAt=i}values(t,e){let i=this.latest?.output?.values||jy;if(!this.previous)return i;let s=Math.max(0,(e-this.receivedAt)/this.period);if(s>=1)return this.previous=null,i;let r=this.previous.output?.values||{},o=Object.create(null);for(let{id:a,fixture:l}of t)o[a]=Qy(r[a],i[a],l,s);return o}};var tv=(n,t)=>n[0]*t[0]+n[1]*t[1]+n[2]*t[2],Pp=(n,t)=>[n[0]-t[0],n[1]-t[1],n[2]-t[2]],Ip=n=>Math.hypot(n[0],n[1],n[2]),Qi=Object.freeze({margin:1.35,hold:.6,facing:1.5,minimumLevel:1e-4});function ev(n,t){let e=Math.max(.05,n.length??6),i=e/2,s=n.position.map((l,c)=>l+n.direction[c]*i),r=Math.hypot(i,e*Math.tan(Math.min(n.angle,1.4)));for(let l of t.planes||[])if(l[0]*s[0]+l[1]*s[1]+l[2]*s[2]+l[3]<-r)return 0;let o=Math.max(r*.5,Ip(Pp(s,t.eye)),.001),a=Math.min(1,r/(o*Math.max(.001,t.tanHalfFov)));return a*a}function iv(n,t,e=Qi){let i=Math.max(0,n.level||0);if(i<=(e.minimumLevel??Qi.minimumLevel))return 0;let s=ev(n,t);if(s<=0)return 0;let r=Pp(t.eye,n.position),o=Ip(r),a=o>1e-6?Math.max(0,tv(n.direction,r)/o):0,l=1+(e.facing??Qi.facing)*a**4,c=i*Math.sqrt(s)*l;return Number.isFinite(c)?c:0}function Dp(n,t,e,i=null,s=0,r=Qi){let o=r.margin??Qi.margin,a=r.hold??Qi.hold,l=i?.since||new Map,c=new Map,h=[];for(let A of n){let R=iv(A,t,r);c.set(A.id,R),(A.level||0)>(r.minimumLevel??Qi.minimumLevel)&&h.push(A)}let u=Math.max(0,Math.floor(e)),d=A=>{let R=c.get(A.id);return!l.has(A.id)||R<=0?R:s-l.get(A.id)<a?1/0:R*o},m=h.filter(A=>c.get(A.id)>0||A.pinned).map(A=>({id:A.id,pinned:!!A.pinned,key:d(A),score:c.get(A.id)})).sort((A,R)=>Number(R.pinned)-Number(A.pinned)||R.key-A.key||R.score-A.score||(A.id<R.id?-1:A.id>R.id?1:0)).slice(0,u).map(A=>A.id);if(m.length<u){let A=new Set(m),R=h.filter(U=>!A.has(U.id)).sort((U,L)=>Number(l.has(L.id))-Number(l.has(U.id))||L.level-U.level||(U.id<L.id?-1:U.id>L.id?1:0));for(let U of R.slice(0,u-m.length))m.push(U.id)}let x=new Set(m),p=new Map;for(let A of m)p.set(A,l.has(A)?l.get(A):s);let g=h.map(A=>A.id).filter(A=>!x.has(A)),y=i?.shadowSince||new Map,v=Math.min(m.length,Math.max(0,Math.floor(r.shadowSlots??0))),S=new Set(n.filter(A=>A.pinned).map(A=>A.id)),T=(A,R)=>c.get(R)-c.get(A)||(A<R?-1:A>R?1:0),b=A=>{let R=c.get(A);return y.has(A)?s-y.get(A)<a?1/0:R*o:R},_=[...m].sort((A,R)=>Number(S.has(R))-Number(S.has(A))||b(R)-b(A)||T(A,R)).slice(0,v).sort(T),M=new Set(_),D=new Map(_.map(A=>[A,y.has(A)?y.get(A):s]));return{full:[..._,...m.filter(A=>!M.has(A)).sort(T)],faked:g,scores:c,state:{since:p,shadowSince:D}}}function Lp(n,t,e,i){let s=Array.from({length:e},(u,d)=>n?.[d]??null),r=t.slice(0,e),o=r.slice(0,i),a=r.slice(i),l=(u,d,f)=>{let m=new Set(f);for(let g=u;g<d;g++)s[g]!=null&&!m.has(s[g])&&(s[g]=null);let x=new Set(s.slice(u,d)),p=f.filter(g=>!x.has(g));for(let g=u;g<d&&p.length;g++)s[g]==null&&(s[g]=p.shift());return p},c=new Set(o),h=new Set(a);for(let u=0;u<e;u++)(u<i?h.has(s[u]):c.has(s[u]))&&(s[u]=null);return l(0,Math.min(i,e),o),l(Math.min(i,e),e,a),s}function Np(n,t,e,i=Qi.hold){let s=new Map;for(let[r,o]of n||[])e-o.lastLit<i&&s.set(r,{peak:o.peak,lastLit:o.lastLit});for(let[r,o]of t){if(!(o>0))continue;let a=n?.get(r),l=a?a.peak*Math.exp(-Math.max(0,e-a.lastLit)/i):0;s.set(r,{peak:Math.max(o,l),lastLit:e})}return s}function Up(n,t,e,i,s=.3,r=null){let o=s>0?Math.max(0,Math.min(1,i/s)):1,a=t.map((l,c)=>{let h=n?.[c];return h&&h.id!=null&&e.has(h.id)?{...h}:{id:null,weight:0}});for(let l=0;l<a.length;l++){let c=a[l],h=e.has(t[l])?t[l]:null;if(c.id===h){h!=null&&(c.weight=Math.min(1,c.weight+o));continue}if(c.id!=null&&c.weight>o){c.weight-=o;continue}let u=h!=null&&a.some((d,f)=>f!==l&&d.id===h);a[l]=u||h==null?{id:null,weight:0}:{id:h,weight:r?.has(h)?0:1}}return a}function Fp(n,t,e){let i=n,s=[],r=[0,1,2,3].map(o=>[i[o],i[o+4],i[o+8],i[o+12]]);for(let[o,a]of[[0,1],[0,-1],[1,1],[1,-1],[2,1],[2,-1]]){let l=r[3].map((h,u)=>h+a*r[o][u]),c=Math.hypot(l[0],l[1],l[2])||1;s.push(l.map(h=>h/c))}return{eye:t,planes:s,tanHalfFov:e}}var nv=`precision highp float;
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
}`,sv=`precision highp float;
precision highp sampler3D;
${Ao}
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
}`;function rv(n=256){let t=new Uint8Array(n*n),e=16,i=new Float32Array(e*e),s=2654435769;for(let a=0;a<i.length;a++)s=s*1664525+1013904223>>>0,i[a]=s/2**32;let r=(a,l)=>i[l%e*e+a%e];for(let a=0;a<n;a++)for(let l=0;l<n;l++){let c=l*e/n,h=a*e/n,u=Math.floor(c),d=Math.floor(h),f=(c-u)**2*(3-2*(c-u)),m=(h-d)**2*(3-2*(h-d)),x=r(u,d)*(1-f)+r(u+1,d)*f,p=r(u,d+1)*(1-f)+r(u+1,d+1)*f,g=(l*7919+a*104729)%251/251;t[a*n+l]=Math.round(255*(.85*(x*(1-m)+p*m)+.15*g))}let o=new Fi(t,n,n,zn,ei);return o.wrapS=o.wrapT=un,o.minFilter=zi,o.magFilter=ce,o.generateMipmaps=!0,o.needsUpdate=!0,o}function gu(n,t,e){let i=[e[0]/2,e[1],e[2]/2],s=[-i[0],0,-i[2]],r=1/0;for(let o=0;o<3;o++){let a=t[o];if(Math.abs(a)<1e-9)continue;let c=((a>0?i[o]:s[o])-n[o])/a;c>=0&&(r=Math.min(r,c))}return Number.isFinite(r)?r:Math.max(...e)*2}var gc=class{constructor(t=128){this.capacity=t,this.count=0;let e=new mi(1,1,1,28,10,!0),i=new Jr;i.index=e.index,i.setAttribute("position",e.getAttribute("position")),i.setAttribute("uv",e.getAttribute("uv")),e.dispose();let s=(r,o)=>{let a=new Qn(new Float32Array(t*o),o);return a.setUsage(Zi),i.setAttribute(r,a),a};this.attributes={position:s("beamPosition",3),direction:s("beamDirection",3),color:s("beamColor",4),shape:s("beamShape",4)},i.instanceCount=0,this.noise=rv(),this.uniforms={sceneDepth:{value:null},noiseTexture:{value:this.noise},densityTexture:{value:null},room:{value:new C(12,5,10)},resolution:{value:new et(1,1)},nearPlane:{value:.08},farPlane:{value:100},amount:{value:0},extinctionPerM:{value:1.25},time:{value:0}},this.material=new Ln({vertexShader:nv,fragmentShader:sv,uniforms:this.uniforms,glslVersion:_n,transparent:!0,depthTest:!1,depthWrite:!1,side:Ue,blending:Za,blendEquation:gn,blendSrc:tr,blendDst:tr,blendSrcAlpha:Ka,blendDstAlpha:tr,toneMapped:!1}),this.mesh=new Ut(i,this.material),this.mesh.frustumCulled=!1,this.scene=new Ye,this.scene.add(this.mesh)}update(t){let e=Math.min(this.capacity,t.length),i=this.attributes;for(let s=0;s<e;s++){let r=t[s];i.position.setXYZ(s,r.position[0],r.position[1],r.position[2]),i.direction.setXYZ(s,r.direction[0],r.direction[1],r.direction[2]),i.color.setXYZW(s,r.color[0],r.color[1],r.color[2],r.penumbra??.5),i.shape.setXYZW(s,r.radius,r.tangent,r.length,r.apexDistance)}for(let s of Object.values(i))s.clearUpdateRanges(),s.addUpdateRange(0,e*s.itemSize),s.needsUpdate=!0;this.mesh.geometry.instanceCount=this.count=e}render(t,e,i,s,{room:r,amount:o,extinctionPerM:a,time:l,densityTexture:c}){if(!this.count||!(o>0))return!1;let h=this.uniforms;h.sceneDepth.value=s.depthTexture,h.densityTexture.value=c,h.room.value.fromArray(r),h.resolution.value.set(i.width,i.height),h.nearPlane.value=e.near,h.farPlane.value=e.far,h.amount.value=o,h.extinctionPerM.value=a,h.time.value=l;let u=t.getRenderTarget(),d=t.autoClear;try{t.autoClear=!1,t.setRenderTarget(i),t.render(this.scene,e)}finally{t.setRenderTarget(u),t.autoClear=d}return!0}stats(){return{count:this.count,capacity:this.capacity}}dispose(){this.mesh.geometry.dispose(),this.material.dispose(),this.noise.dispose(),this.scene.clear(),this.count=0}};var mE=Object.freeze(["floor","back","left","right","stage-top","stage-front"]),xu=4;function _u(n){let{width:t,height:e,z:i,depth:s}=lu(n);return[t/2,e,i-s/2,i+s/2]}var vu=20,Po=6,xc=0,_c=3,yc=6,vc=9,Mu=12,Su=13,Mc=14,Bp=17;function Op([n,t,e],[i,s,r,o]){let a=[[[-n/2,0,-e/2],[n,0,0],[0,0,e],[0,1,0]],[[-n/2,0,-e/2],[n,0,0],[0,t,0],[0,0,1]],[[-n/2,0,-e/2],[0,0,e],[0,t,0],[1,0,0]],[[n/2,0,-e/2],[0,0,e],[0,t,0],[-1,0,0]],[[-i,s,r],[2*i,0,0],[0,0,o-r],[0,1,0]],[[-i,0,o],[2*i,0,0],[0,s,0],[0,0,1]]],l=new Float64Array(Po*vu);return a.forEach(([c,h,u,d],f)=>{let m=f*vu;l.set(c,m+xc),l.set(h,m+_c),l.set(u,m+yc),l.set(d,m+vc),l[m+Mu]=h[0]*h[0]+h[1]*h[1]+h[2]*h[2],l[m+Su]=u[0]*u[0]+u[1]*u[1]+u[2]*u[2];for(let x=0;x<3;x++)l[m+Mc+x]=c[x]+h[x]/2+u[x]/2;l[m+Bp]=Math.sqrt(l[m+Mu]+l[m+Su])/2}),l}var yu=(n,t,e)=>Math.sqrt(n*n+t*t+e*e),vn=new Float64Array(27),ov=1e-7,av=1e-7;function lv(n,t,e,i,s,r=i.length/Po){s.fill(0);for(let o=0;o<t;o++){let a=n[o],l=a.direction,c=a.virtualPosition,h=l[0],u=l[1],d=l[2],f=a.range,m=a.outerCos,x=!1,p=-1,g=0;for(let y=0;y<Po;y++){let v=y*vu,S=c[0]-e[v+xc],T=c[1]-e[v+xc+1],b=c[2]-e[v+xc+2],_=e[v+vc],M=e[v+vc+1],D=e[v+vc+2],P=S*_+T*M+b*D;if(P<=0)continue;let A=e[v+_c],R=e[v+_c+1],U=e[v+_c+2],L=e[v+Mu],B=e[v+yc],H=e[v+yc+1],z=e[v+yc+2],Q=e[v+Su],W=h*_+u*M+d*D;if(W<-1e-6){let ot=-P/W;if(!(f>0&&ot>f)){let Ct=S+h*ot,Bt=T+u*ot,Nt=b+d*ot,Kt=(Ct*A+Bt*R+Nt*U)/L,Yt=(Ct*B+Bt*H+Nt*z)/Q;if(Kt>=0&&Kt<=1&&Yt>=0&&Yt<=1){i[y*r+s[y]++]=o;continue}}}if(!x){x=!0;let ot=Math.abs(u)<.95?0:1,Ct=Math.abs(u)<.95?1:0,Bt=Ct*d-0*u,Nt=0*h-ot*d,Kt=ot*u-Ct*h,Yt=yu(Bt,Nt,Kt),Se=Bt/Yt,ie=Nt/Yt,Re=Kt/Yt,_e=u*Re-d*ie,Ee=d*Se-h*Re,fi=h*ie-u*Se,ri=Math.sqrt(Math.max(0,1-m**2));vn[0]=h,vn[1]=u,vn[2]=d;for(let O=0;O<8;O++){let oi=O*Math.PI/4,ue=Math.cos(oi)*ri,de=Math.sin(oi)*ri,dt=3+3*O;vn[dt]=h*m+Se*ue+_e*de,vn[dt+1]=u*m+ie*ue+Ee*de,vn[dt+2]=d*m+Re*ue+fi*de}}let Z=1/0,$=1/0,xt=-1/0,Mt=-1/0,q=0;for(let ot=0;ot<27;ot+=3){let Ct=vn[ot],Bt=vn[ot+1],Nt=vn[ot+2],Kt=Ct*_+Bt*M+Nt*D;if(Kt>=-1e-6)continue;let Yt=-P/Kt;if(f>0&&Yt>f)continue;let Se=S+Ct*Yt,ie=T+Bt*Yt,Re=b+Nt*Yt,_e=(Se*A+ie*R+Re*U)/L,Ee=(Se*B+ie*H+Re*z)/Q;Z=Math.min(Z,_e),$=Math.min($,Ee),xt=Math.max(xt,_e),Mt=Math.max(Mt,Ee),q++}let st=.05+.1*Math.max(xt-Z,Mt-$,0);if(q&&Z-st<=1&&xt+st>=0&&$-st<=1&&Mt+st>=0){i[y*r+s[y]++]=o;continue}p<0&&(p=Math.min(Math.PI/2,Math.acos(Math.min(1,m))+.05),g=Math.cos(p));let _t=e[v+Mc]-c[0],wt=e[v+Mc+1]-c[1],rt=e[v+Mc+2]-c[2],Pt=yu(_t,wt,rt),Qt=e[v+Bp];if(!(Pt>Qt&&(f>0&&Pt-Qt>f+av||Math.acos(Math.max(-1,Math.min(1,(_t*h+wt*u+rt*d)/Pt)))-Math.asin(Qt/Pt)>p+ov)))t:for(let ot=0;ot<=6;ot++)for(let Ct=0;Ct<=6;Ct++){let Bt=-S+A*Ct/6+B*ot/6,Nt=-T+R*Ct/6+H*ot/6,Kt=-b+U*Ct/6+z*ot/6,Yt=yu(Bt,Nt,Kt);if(!(Yt<1e-6||f>0&&Yt>f)&&(Bt*h+Nt*u+Kt*d)/Yt>=g){i[y*r+s[y]++]=o;break t}}}}return s}function cv(n){return Math.max(...n)>12.8?512:256}var hv=`out vec2 vUv;
void main(){vUv=uv;gl_Position=vec4(position.xy,0.,1.);}`,uv=`precision highp float;
in vec2 vUv;
uniform sampler2D lightTable;
uniform int tile, tileCount;
uniform int tileLights[128];
uniform vec3 room;
uniform vec4 stage;
out vec4 irradiance;
// 3 x 2 tiles: floor, back, left / right, stage top, stage front. Each draw
// covers one tile's viewport and only the lights whose cones can reach it.
void surface(vec2 local,out vec3 position,out vec3 normal){
  float w=room.x,h=room.y,d=room.z;
  int index=tile;
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
  for(int n=0;n<128;n++){
    if(n>=tileCount)break;
    int i=tileLights[n];
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
}`;function ni(n,t,e){let i=Math.fround(e);return n[t]===i?!1:(n[t]=i,!0)}var Sc=class{constructor(t,{capacity:e=128}={}){this.renderer=t,this.capacity=e,this.count=0,this.dirty=!0,this.redraws=0,this.lights=[],this.room=[12,5,10],this.tileLit=new Array(6).fill(!0),this.lastRender=-1/0,this.tileDraws=0,this.planes=Op(this.room,_u(this.room)),this.tileIndices=new Int32Array(Po*e),this.tileCounts=new Int32Array(Po),this.tile=256,this.target=new me(3*this.tile,2*this.tile,{type:xe,depthBuffer:!1,minFilter:ce,magFilter:ce,generateMipmaps:!1}),this.table=new Fi(new Float32Array(xu*e*4),xu,e,ui,gi),this.table.minFilter=this.table.magFilter=De,this.table.needsUpdate=!0,this.uniforms={lightTable:{value:this.table},tile:{value:0},tileCount:{value:0},tileLights:{value:new Int32Array(e)},room:{value:new C(12,5,10)},stage:{value:new oe(..._u([12,5,10]))}},this.material=new fe({vertexShader:hv,fragmentShader:uv,uniforms:this.uniforms,glslVersion:_n,depthTest:!1,depthWrite:!1,blending:wi,toneMapped:!1}),this.quad=new Ut(new Si(2,2),this.material),this.quad.frustumCulled=!1,this.scene=new Ye,this.scene.add(this.quad),this.camera=new mn,this.materialUniforms={tolglowFootprints:{value:this.target.texture},tolglowRoom:{value:new C(12,5,10)},tolglowStage:{value:this.uniforms.stage.value},tolglowFootprintStrength:{value:0}}}setRoom(t){this.room=[...t],this.tileLit.fill(!0),this.uniforms.room.value.fromArray(t),this.materialUniforms.tolglowRoom.value.fromArray(t);let e=_u(t);this.uniforms.stage.value.set(...e),this.planes=Op(this.room,e);let i=cv(t);i!==this.tile&&(this.tile=i,this.target.setSize(3*i,2*i)),this.dirty=this.force=!0}update(t){let e=Math.min(this.capacity,t.length),i=this.table.image.data,s=e!==this.count;for(let r=0;r<e;r++){let o=t[r],a=r*xu*4,{virtualPosition:l,direction:c,color:h}=o;s=ni(i,a,l[0])|ni(i,a+1,l[1])|ni(i,a+2,l[2])|ni(i,a+3,o.apexDistance)|ni(i,a+4,c[0])|ni(i,a+5,c[1])|ni(i,a+6,c[2])|ni(i,a+7,o.outerCos)|ni(i,a+8,h[0])|ni(i,a+9,h[1])|ni(i,a+10,h[2])|ni(i,a+11,o.innerCos)|ni(i,a+12,o.range)|ni(i,a+13,0)|ni(i,a+14,0)|ni(i,a+15,0)|s}this.count=e,this.lights=t,s&&(this.table.needsUpdate=!0,this.dirty=!0),this.materialUniforms.tolglowFootprintStrength.value=e&&!this.disabled?1:0}render(t=globalThis.performance?.now?.()??0,e=30){if(this.disabled||!this.dirty||!this.force&&t-this.lastRender<1e3/e)return!1;this.dirty=this.force=!1,this.redraws++,this.lastRender=t;let i=this.renderer,s=i.getRenderTarget(),r=this.uniforms,o=lv(this.lights,this.count,this.planes,this.tileIndices,this.tileCounts,this.capacity);try{for(let a=0;a<6;a++){let l=o[a];if(!(!l&&!this.tileLit[a])){this.tileLit[a]=l>0,r.tile.value=a,r.tileCount.value=l;for(let c=0,h=a*this.capacity;c<l;c++)r.tileLights.value[c]=this.tileIndices[h+c];this.target.viewport.set(a%3*this.tile,Math.floor(a/3)*this.tile,this.tile,this.tile),this.target.scissor.copy(this.target.viewport),this.target.scissorTest=!0,i.setRenderTarget(this.target),i.render(this.scene,this.camera),this.tileDraws++}}}finally{this.target.viewport.set(0,0,3*this.tile,2*this.tile),this.target.scissor.copy(this.target.viewport),this.target.scissorTest=!1,i.setRenderTarget(s)}return!0}dispose(){this.target.dispose(),this.table.dispose(),this.material.dispose(),this.quad.geometry.dispose(),this.scene.clear()}};function zp(n,t){let e=n.onBeforeCompile,i=n.customProgramCacheKey;n.onBeforeCompile=(s,r)=>{e?.call(n,s,r),Object.assign(s.uniforms,t),s.vertexShader=`varying vec3 tolglowWorld; varying vec3 tolglowWorldNormal;
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
  reflectedLight.directDiffuse += tolglowFootprint(tolglowWorld, normalize(tolglowWorldNormal)) * BRDF_Lambert(material.diffuseColor);`)},n.customProgramCacheKey=()=>(i?i.call(n):"")+"+tolglow-footprints-v2"}var fv=new C(0,-1,0),bc=16,bu=128,pv=.3;function kp(n,t){let e=t.position||[0,0,0],i=(t["rotation-deg"]||[0,0,0]).map(s=>s*Math.PI/180);return!e.some((s,r)=>s!==n.position.getComponent(r))&&n.rotation.order==="ZYX"&&n.rotation.x===i[0]&&n.rotation.y===i[1]&&n.rotation.z===i[2]?!1:(n.position.fromArray(e),n.rotation.set(...i,"ZYX"),!0)}function Hp(n,t=[]){for(let e of n)e.castShadow&&(e.shadow.needsUpdate=!0);for(let e of t)e.castShadow&&(e.shadow.needsUpdate=!0)}var wc=class{constructor(t,e,i={}){if(this.displayHz=i.displayHz??(i.worker?null:60),this.platform=i,this.displayClock=new Oo,this.lighting=new mc,this.canvas=t,t.tolglowPreview=this,this.status=e,this.ready=!1,this.disposed=!1,this.room=[12,5,10],this.snapshot=null,this.key=null,this.fixtures=[],this.filmSettings={bloom:.28,exposure:.85,grain:.0015},this.quality=this.autoQuality(),this.options=ji[this.quality],this.qualityChoice="auto",this.fogOverride=null,this.workLight=.12,this.visible=!!i.worker,this.lost=!1,this.lastFrame=0,this.renderScale=1,this.scaleChangedAt=0,this.clock=0,this.fogAnimationUntil=0,this.draws=0,this.lastCPU=0,this.abort=new AbortController,this.reducedMotion=i.worker?{matches:!!i.reducedMotion}:matchMedia("(prefers-reduced-motion: reduce)"),this.whenReady=new Promise(r=>{this.resolveReady=r}),this.setStatus("Building venue and lighting shaders\u2026"),this.camera=new Xe(49,1,.08,100),this.controls=i.worker?{target:new C,update:()=>this.camera.lookAt(this.controls.target),dispose(){},touches:{}}:Yf(this.camera,this.canvas),this.setCamera("audience"),i.worker)try{this.initialize()}catch(r){this.fail(r);return}i.worker||(this.refresh=new Bo(r=>this.setDisplayHz(r)),this.resizeObserver=new ResizeObserver(()=>this.resize()),this.resizeObserver.observe(t),this.activity=new fc(t,r=>this.changeActivity(r)));let s=this.abort.signal;t.addEventListener("webglcontextlost",r=>{r.preventDefault(),this.lost=!0,this.ready=!1,this.setStatus("Graphics context lost \xB7 waiting for recovery")},{signal:s}),t.addEventListener("webglcontextrestored",()=>{try{this.restorePending=!0,this.visible&&(this.restoreContext(),this.scheduleFrame(0))}catch(r){this.fail(r)}},{signal:s}),i.worker||(this.navigation=new Jl({camera:this.camera,controls:this.controls,canvas:t,room:()=>this.room,snapshot:()=>this.snapshot,visibleLights:()=>this.litIds,active:()=>this.visible&&!this.paused&&!this.disposed,reducedMotion:()=>this.reducedMotion.matches}),this.reducedMotion.addEventListener("change",()=>this.navigation.notify(),{signal:s})),i.autoStart!==!1&&this.scheduleFrame(0)}get targetHz(){return sp(this.quality,this.qualityChoice,this.softwareRenderer,this.displayHz)}setDisplayHz(t){!t||t===this.displayHz||(this.displayHz=t,this.overBudget=this.underBudget=0)}autoQuality(t=this.platform.viewport?.width??this.canvas.getBoundingClientRect().width){return this.softwareRenderer?"low":(this.platform.worker?this.platform.coarsePointer:matchMedia("(pointer: coarse)").matches)||t<(this.autoLayoutTier==="mobile"?740:660)?"mobile":"desktop"}setStatus(t){this.status&&this.status.textContent!==t&&(this.status.textContent=t)}initialize(){this.hdrRequested=!this.platform.worker&&cp(),this.renderer=new Bl({canvas:this.canvas,alpha:this.hdrRequested,antialias:!1,powerPreference:"high-performance",preserveDrawingBuffer:!1});let t=this.renderer.getContext(),e=t.getExtension("WEBGL_debug_renderer_info");this.maxDimension=Math.min(t.getParameter(t.MAX_RENDERBUFFER_SIZE),t.getParameter(t.MAX_TEXTURE_SIZE)),this.samples=op(t),this.gpuTimer=this.platform.worker?null:new Eo(t),this.deviceRenderer=e?t.getParameter(e.UNMASKED_RENDERER_WEBGL):t.getParameter(t.RENDERER),this.softwareRenderer=/swiftshader|llvmpipe|softpipe|software rasterizer/i.test(this.deviceRenderer||""),this.quality=this.autoLayoutTier=this.autoQuality(),this.options=ji[this.quality],this.frameInterval=1e3/this.targetHz,this.renderer.outputColorSpace=Ie,this.renderer.toneMapping=cs,this.renderer.toneMappingExposure=.85,this.renderer.shadowMap.enabled=!0,this.renderer.shadowMap.type=ls,this.renderer.debug.onShaderError=(i,s,r,o)=>{let a=i.getProgramInfoLog(s)||i.getShaderInfoLog(o)||i.getShaderInfoLog(r);throw new Error(`Preview shader rejected: ${String(a).slice(0,500)}`)},this.scene=new Ye,this.scene.background=new bt(.0015,.002,.003),this.materials=fp(),this.apertureUniform={value:new Float32Array(bc+2)},this.patchRadii={value:new Float32Array(bc+2)};for(let i of Object.values(this.materials))ru(i,this.apertureUniform,this.patchRadii);this.footprints=new Sc(this.renderer,{capacity:bu});for(let i of["floor","wall","stage","curtain"])zp(this.materials[i],this.footprints.materialUniforms);this.beams=new gc(bu),this.refreshEnvironment(),this.hemisphere=new Zs(12966384,3812384,0),this.scene.add(this.hemisphere),this.diffuseBounce=new Zs(0,0,1),this.scene.add(this.diffuseBounce),this.workLamp=new Kr(16769988,0),this.workLamp.position.set(2,6,4),this.scene.add(this.workLamp),this.setWorkLight(this.workLight),this.sceneTarget=new me(1,1,{type:xe,minFilter:ce,magFilter:ce,depthBuffer:!0}),this.sceneTarget.texture.colorSpace=In,this.sceneTarget.depthTexture=new Oi(1,1,hi),this.compositeTarget=new me(1,1,{type:xe,depthBuffer:!1}),this.compositeTarget.texture.colorSpace=In,this.fog=new dc(this.renderer,{quality:this.options.fog||this.quality}),this.fog.setRoom(this.room),this.fog.setAmount(this.fogAmount),this.composer=new Ql(this.renderer,new me(1,1,{type:xe,depthBuffer:!1})),this.inputPass=new tc(this.compositeTarget.texture),this.composer.addPass(this.inputPass),this.glass=new hc(this),this.composer.addPass(this.glass),this.bloom=new dr(new et(1,1),.28,.4,1.1),this.composer.addPass(this.bloom),this.outputPass=ip(),this.composer.addPass(this.outputPass),this.edges=new ic,this.composer.addPass(this.edges),this.film=new nc,this.composer.addPass(this.film),this.setFilm(this.filmSettings),this.rebuild({room:{width:12,height:5,depth:10},fixtures:[],trusses:[]}),this.renderer.compile(this.beams.scene,this.camera),this.resize()}refreshEnvironment(){this.environmentTarget?.dispose();let t=new $l,e=new ar(this.renderer);this.environmentTarget=e.fromScene(t,.04),this.scene.environment=this.environmentTarget.texture,t.dispose(),e.dispose()}setFilm(t){for(let[e,i]of[["bloom",2],["exposure",3],["grain",.08]])Number.isFinite(t?.[e])&&(this.filmSettings[e]=Ke(t[e],0,i));this.film&&(this.renderer.toneMappingExposure=this.filmSettings.exposure,this.bloom.strength=this.filmSettings.bloom,this.bloom.enabled=this.bloom.strength>0,this.film.uniforms.grain.value=this.filmSettings.grain)}setWorkLight(t){this.workLight=Ke(Number(t)||0,0,1),this.hemisphere&&(this.hemisphere.intensity=this.workLight*1.8),this.workLamp&&(this.workLamp.intensity=this.workLight*3.6),this.scene&&(this.scene.environmentIntensity=this.workLight*1.25),this.ready&&this.updateStatus()}get fogAmount(){return this.fogOverride??(this.snapshot?.output?.atmosphere?1:.3)}setFog(t){this.fogOverride=t==null?null:Ke(Number(t)||0,0,1),this.fog?.setAmount(this.fogAmount)}burst(t){return!this.visible||!this.fog?.burst(t)?!1:(this.fogAnimationUntil=this.clock+8,!0)}setQuality(t="auto"){if(!["auto",...Object.keys(ji)].includes(t))throw new Error("Unknown preview quality");let e=this.qualityChoice!==t;if(e&&(this.renderScale=1,this.overBudget=this.underBudget=0,this.scaleChangedAt=performance.now()),this.qualityChoice=t,!this.visible||!this.renderer)return;t==="auto"&&e&&(this.autoLayoutTier=this.autoQuality());let i=t==="auto"?e?this.autoLayoutTier:this.quality:t;if(i===this.quality){e&&this.resize();return}this.applyTier(i)}applyTier(t){this.fog.setQuality(ji[t].fog||t),this.quality=t,this.options=ji[t],this.frameInterval=1e3/this.targetHz,this.width&&this.fog.setSize(this.width,this.height),this.snapshot&&this.update(this.snapshot,!0),this.resize()}setLightingMode(t){t!==this.lighting.mode&&(this.lighting.setMode(t),this.visible&&this.renderer&&this.snapshot&&this.applyLighting(this.snapshot.output?.values||{}))}setLightBudget(t=null){let e=t==null?null:Math.max(0,Math.min(bc,Math.floor(Number(t)||0)));e!==this.lightBudgetOverride&&(this.lightBudgetOverride=e,this.visible&&this.renderer&&this.snapshot?this.update(this.snapshot,!0):this.lightBudgetPending=!0)}get lightBudget(){return np(this.options,this.lightBudgetOverride)}update(t,e=!1,i=performance.now()){if(!t||this.disposed)return;if(this.platform.worker||(this.editorInput||=new Vl(this),this.editorInput.update(t)),!this.visible||!this.renderer){this.snapshot=t;return}let s=En(t);if(this.opticalShow!==s&&this.glass?.dispose(),this.opticalShow=s,this.glass){this.glass.update(t["glass-meshes"],t["glass-volumes"]);for(let l of[this.composer.readBuffer,this.composer.writeBuffer])l.depthBuffer!==this.glass.enabled&&(l.dispose(),l.depthBuffer=this.glass.enabled)}this.editorOverlay||=new ql,this.editorOverlay.update(t),this.snapshot=t;let r=t.rig||{},o=r===this.lastRig?this.key:Hf(r);r!==this.lastRig&&(this.fixtureById=new Map((r.fixtures||[]).map(l=>[String(l.id),l])),this.beamById=new Map([...this.fixtureById].filter(([,l])=>!(l["pixel-layout-id"]&&l["diffuser-radius-m"])))),this.lastRig=r,(e||this.key!==o)&&(this.ready=!1,this.rebuild(r),this.key=o);let a=!1;for(let l of this.trusses||[]){let c=(r.trusses||[]).find(h=>String(h.id)===l.id);c&&kp(l.root,c)&&(a=!0)}this.updateEmittedOutput(t,i,a)}updateEmittedOutput(t,e,i=!1){this.snapshot=t,this.fog?.setAtmosphere(this.opticalShow,t.output?.atmosphere),this.fog?.setAmount(this.fogAmount),this.lighting.accept(t,this.lastRig,e);let s=t.editor?.selection,r=s?.kind==="fixtures"?String(s.id):null,o=Rp(this.beamById,t.output?.values||{},r,this.fixtures.map(c=>c.id),[],0);this.selectedFixture=r,this.fixtures.forEach((c,h)=>{let u=o.meshes[h],d=this.fixtureById.get(u);if(c.fixture!==d){let f=c.lensRadius;c.setAperture(d),c.lensRadius!==f&&(i=!0),c.fixture=d,c.id=u,c.root.name="fixture:"+u}kp(c.root,d)&&(i=!0)});let a=new Set(this.fixtures.map(c=>c.id)),l=t.output?.values||{};this.beamOnly=[];for(let c of Object.keys(l)){let h=this.beamById?.get(c);h&&!a.has(c)&&this.beamOnly.push({id:c,fixture:h})}this.lightingFixtures=this.fixtureSamples.concat(this.beamOnly),this.applyLighting(this.lighting.values(this.lightingFixtures,e),i)}assignLights(t){let e=performance.now()/1e3,i=this.lodTime==null?0:Math.max(0,Math.min(.1,e-this.lodTime));this.lodTime=e;for(let p of this.fixtures)p.light=null,p.fullWeight=0;if(!this.spots.length){this.fading=!1,this.lod={budget:0,full:0,faked:t.length};return}let s=this.camera;s.updateMatrixWorld();let r=(this.lodMatrix||=new Xt).multiplyMatrices(s.projectionMatrix,s.matrixWorldInverse),o=new C().setFromMatrixPosition(s.matrixWorld).toArray(),a=Fp(r.elements,o,Math.tan(s.fov*Math.PI/360));for(let p of t)p.lastBeam=p.beam;this.emission=Np(this.emission,new Map(t.map(p=>[p.id,p.beam.level])),e);let l=this.fixtures.filter(p=>p.lastBeam&&this.emission.has(p.id)),c=l.map(p=>({id:p.id,level:this.emission.get(p.id).peak,pinned:p.id===this.selectedFixture,position:p.lastBeam.optics.optical.toArray(),direction:p.lastBeam.optics.axis.toArray(),angle:p.lastBeam.angle,length:p.lastBeam.length})),h=this.spots.filter(p=>p.castShadow).length,u=Dp(c,a,this.spots.length,this.lodState,e,{...Qi,shadowSlots:h});this.lodState=u.state,this.desiredSlots=Lp(this.desiredSlots,u.full,this.spots.length,h);let d=new Set(l.map(p=>p.id));this.slotStates=Up(this.slotStates,this.desiredSlots,d,i,pv,this.fakedIds);let f=this.slotStates.map(p=>p.id);(f.length!==this.lightIds.length||f.some((p,g)=>p!==this.lightIds[g]))&&(this.lightIds=f);let m=new Map(this.fixtures.map(p=>[p.id,p]));this.slotStates.forEach((p,g)=>{let y=p.id!=null&&m.get(p.id);y&&(y.light=this.spots[g],y.fullWeight=p.weight)});let x=new Set(t.map(p=>p.id));this.fading=this.slotStates.some(p=>p.id!=null&&(p.weight<1||!x.has(p.id))),this.fakedIds=new Set(t.filter(p=>!(p.fullWeight>=1)).map(p=>p.id)),this.lod={budget:this.spots.length,full:u.full.length,faked:u.faked.length,fading:this.fading}}meshlessBeams(t,e){let i=[];for(let{id:s,fixture:r}of this.beamOnly||[]){if(e.has(s))continue;let o=t[s];if(!o)continue;let a=fs(o),l=r["relative-brightness"]??1;if(!(l>0&&Math.max(...a)>0))continue;let c=Kh(r,o),h=Jh(r,o),u=su(r.position||[0,0,0],c,h,ms(r));if(i.push({id:s,fixture:r,beam:{rgb:a,brightness:l,angle:h,optics:u,direction:c,resolved:o,level:Math.max(...a)*l,penumbra:Ke(.72-(Number(o.focus)||0)*.52,.2,.8),length:gu(u.optical.toArray(),u.axis.toArray(),this.room)}}),i.length>=bu)break}return i}applyLighting(t,e=!1){this.displayedValues=t;for(let x of this.diffuserTubes)x.update(t);this.lightEntries=[];let i=[],s=[],r=new ti,o=new ti,a=new C;for(let x of this.fixtures){let p=t[x.id]||{},g=Kh(x.fixture,p),y=fs(p);r.setFromUnitVectors(fv,a.fromArray(g)).premultiply(o.copy(x.root.quaternion).invert());let v=yn(x.fixture,p,"pan")*Math.PI/180;(!x.head.quaternion.equals(r)||x.yoke.rotation.y!==v)&&(e=!0,x.head.quaternion.copy(r),x.yoke.rotation.y=v);let S=x.fixture["relative-brightness"]??1;if(x.lensMaterial.emissive.setRGB(...y).multiplyScalar(8*S),x.beam=null,!(S>0&&Math.max(...y)>0))continue;let T=Jh(x.fixture,p),b=su(x.fixture.position,g,T,x.lensRadius);x.beam={rgb:y,brightness:S,angle:T,optics:b,direction:g,resolved:p,level:Math.max(...y)*S,penumbra:Ke(.72-(Number(p.focus)||0)*.52,.2,.8),length:gu(b.optical.toArray(),b.axis.toArray(),this.room)},s.push(x)}this.assignLights(s);let l=this.meshlessBeams(t,new Set(this.fixtures.map(x=>x.id)));this.activeLights=s.length+l.length;let c=[...s,...l].map(x=>x.id);(c.length!==this.litIds?.length||c.some((x,p)=>x!==this.litIds[p]))&&(this.litIds=c);let h=this.fog?.mode==="haze",u=[],d=[];for(let x of this.spots)this.fixtures.some(p=>p.light===x)||x.intensity!==0&&(x.intensity=0,x.color.setRGB(0,0,0));for(let x of[...this.fixtures,...l]){let p=x.light,g=x.beam;if(p&&!g){p.intensity=0,p.color.setRGB(0,0,0);continue}if(!g)continue;let{angle:y,optics:v,rgb:S,brightness:T}=g,b=Math.max(...this.room)*2.5+v.apexDistance,_=145*T,M=p?x.fullWeight:0,D=h?1:1-M;if(D>0&&u.push({position:v.optical.toArray(),direction:v.axis.toArray(),color:S.map(U=>U*_*D),penumbra:g.penumbra,radius:v.radius,tangent:Math.tan(y),length:g.length,apexDistance:v.apexDistance}),M<1&&d.push({virtualPosition:v.virtual.toArray(),apexDistance:v.apexDistance,direction:v.axis.toArray(),outerCos:Math.cos(y),innerCos:Math.cos(y*(1-g.penumbra)),color:S.map(U=>U*_*(1-M)),range:b}),!p)continue;let P=v.apexDistance+.006,A=a.fromArray(g.direction).add(v.virtual);p.castShadow&&(p.angle!==y||p.distance!==b||p.shadow.camera.near!==P||!p.position.equals(v.virtual)||!p.target.position.equals(A))&&(p.shadow.needsUpdate=!0),p.color.setRGB(...S),p.intensity=_*M,p.angle=y,p.penumbra=g.penumbra,p.position.copy(v.virtual),p.distance=b,(p.shadow.camera.near!==P||p.shadow.camera.far!==b)&&(p.shadow.camera.near=P,p.shadow.camera.far=b,p.shadow.camera.updateProjectionMatrix()),p.userData.apexDistance=v.apexDistance,p.target.position.copy(A),p.target.updateMatrixWorld();let R={position:v.optical,virtualPosition:v.virtual,apexDistance:v.apexDistance,apertureRadius:v.radius,direction:v.axis,color:p.color,angle:p.angle,penumbra:p.penumbra,intensity:p.intensity,distance:p.distance,shadow:p.castShadow?p.shadow:null,pseudo:x.fixture["pseudo-fixture"]};this.lightEntries.push(R),this.reflections.addCandidate(i,x,R,g.resolved["beam-footprint"])}this.beams?.update(u),this.footprints?.update(d),this.fakedEntries=u;let f=new bt(0,0,0),m=[...s,...l].filter(x=>!x.fixture["pseudo-fixture"]&&x.beam.optics.axis.y<0).sort((x,p)=>p.beam.level-x.beam.level||(x.id<p.id?-1:1)).slice(0,bc);for(let x of m){let p=x.beam,g=p.optics.optical,y=p.optics.axis,v=-g.y/y.y,S=g.x+v*y.x,T=g.z+v*y.z;if(v<=0||Math.abs(S)>this.room[0]/2||Math.abs(T)>this.room[2]/2)continue;let b=-y.y*.045*p.brightness/(1+v*v/24);f.r+=p.rgb[0]*b,f.g+=p.rgb[1]*b,f.b+=p.rgb[2]*b}this.diffuseBounce.color.copy(f).multiplyScalar(.14),this.diffuseBounce.groundColor.copy(f).multiplyScalar(.55),this.bounceColor=f,e&&Hp(this.spots,this.reflections?.lights),this.spots.length&&(i.length&&this.content.updateMatrixWorld(!0),this.reflections.resolve(i),this.reflections.illuminate(this.fog.field,this.fogAmount,this.fog.extinctionPerM),this.spotUniformOrder.forEach((x,p)=>{this.apertureUniform.value[p]=x.userData.apexDistance||0,this.patchRadii.value[p]=x.userData.patchRadius||0})),this.ready&&this.updateStatus()}rebuild(t){if(this.content){this.scene.remove(this.content),au(this.content);for(let r of this.fixtures)r.dispose();for(let r of this.diffuserTubes||[])r.dispose();for(let r of this.spots||[])r.dispose();this.reflections?.dispose()}this.room=kf(t.room),this.camera.far=Math.max(100,3*Math.hypot(...this.room)),this.camera.updateProjectionMatrix(),this.fog?.setRoom(this.room),this.content=new ve,this.scene.add(this.content),this.content.add(xp(this.room,this.materials,t.surfaces||[])),this.trusses=[];for(let r of(t.trusses||[]).slice(0,24))if(Array.isArray(r.position)&&r.position.every(Number.isFinite)&&Number.isFinite(r.length)&&r.length>0){let o=mp(r,this.materials);this.content.add(o),this.trusses.push({id:String(r.id),root:o})}this.spots=[],this.lightIds=[];let e=t.fixtures||[];this.diffuserTubes=_p(e.filter(r=>r["pixel-layout-id"]&&r["diffuser-radius-m"]),this.content),this.fixtures=e.filter(r=>!(r["pixel-layout-id"]&&r["diffuser-radius-m"])).slice(0,this.options.fixtures).map((r,o)=>{let a=gp(r,this.materials);ru(a.lensMaterial,this.apertureUniform,this.patchRadii),a.id=String(r.id),a.fixture=r;let l=(r["rotation-deg"]||[0,0,0]).map(c=>(Number(c)||0)*Math.PI/180);return a.root.rotation.set(...l,"ZYX"),this.content.add(a.root),a.light=null,a});for(let r=0;r<Math.min(this.lightBudget,this.fixtures.length);r++){let o=new os(0,0,Math.max(...this.room)*2.5,Math.PI/20,.15,2);o.position.set(0,this.room[1],0),o.shadow.mapSize.set(this.options.shadowSize,this.options.shadowSize),o.shadow.camera.near=.06,o.shadow.camera.far=o.distance,o.shadow.bias=-8e-5,o.shadow.normalBias=.012,o.castShadow=r<this.options.shadows,o.shadow.autoUpdate=!1,o.shadow.needsUpdate=!0,this.spots.push(o),this.lightIds.push(null),this.content.add(o,o.target)}this.lodState=null,this.desiredSlots=null,this.slotStates=null,this.fakedIds=new Set,this.lodTime=null,this.emission=null,this.lightBudgetPending=!1,this.footprints?.setRoom(this.room),this.fixtureSamples=this.fixtures.concat(this.diffuserTubes.flatMap(r=>r.rows)),this.lightingFixtures=this.fixtureSamples,this.beamOnly=[];let i=e.filter(r=>r["pseudo-fixture"]).length,s=this.quality==="mobile"||this.quality==="low"?1:2;this.reflections=new oc(this.content,Math.min(s,i),Math.min(512,this.options.shadowSize),this.fixtures),this.spotUniformOrder=[...this.spots.filter(r=>r.castShadow),...this.reflections.lights,...this.spots.filter(r=>!r.castShadow)],this.totalFixtureCount=(t.fixtures||[]).length,this.lightEntries=[]}measureLoad(t,e){if(this.qualityChoice!=="auto"||this.draws<2||!this.visible)return;this.measuredFrameMs=t;let i=1e3/this.targetHz;if(this.overBudget=t>i*1.35?(this.overBudget||0)+1:0,this.underBudget=t<i*.65?(this.underBudget||0)+1:0,e-this.scaleChangedAt<1e3)return;let s=this.renderScale;if(this.overBudget>=4?s=Math.max(.5,Math.min(s*.85,s*Math.sqrt(i/t))):this.underBudget>=60&&(s=Math.min(1,s+.1)),s===this.renderScale&&!this.softwareRenderer){let r=this.overBudget>=8&&s===.5?-1:this.underBudget>=60&&s===1?1:0,o=lp(this.quality,r);if(o!==this.quality){let a=sc(o,this.qualityChoice,this.softwareRenderer)===sc(this.quality,this.qualityChoice,this.softwareRenderer);this.renderScale=r<0?a?s:.65:.9,this.scaleChangedAt=e,this.overBudget=this.underBudget=0,this.applyTier(o);return}}s!==this.renderScale&&(this.renderScale=s,this.scaleChangedAt=e,this.overBudget=this.underBudget=0,this.resizePending=!0)}resize(){if(!this.visible||!this.renderer||this.disposed)return;this.resizePending=!1;let t=this.platform.worker?this.platform.viewport:this.canvas.getBoundingClientRect();if(!t.width||!t.height)return;if(this.qualityChoice==="auto"){let l=this.autoQuality(t.width);if(l!==this.autoLayoutTier&&(this.autoLayoutTier=l,this.renderScale=1,this.overBudget=this.underBudget=0,this.scaleChangedAt=performance.now(),l!==this.quality)){this.applyTier(l);return}}let e=this.platform.worker?t.pixelRatio:devicePixelRatio,{scene:[i,s],display:[r,o]}=rp(t.width,t.height,e,sc(this.quality,this.qualityChoice,this.softwareRenderer),this.renderScale,this.maxDimension);this.camera.aspect=t.width/t.height,this.camera.updateProjectionMatrix(),(r!==this.displayWidth||o!==this.displayHeight)&&(this.renderer.setSize(r,o,!1),this.displayWidth=r,this.displayHeight=o,this.displayRange=hp(this.renderer.getContext(),r,o,this.hdrRequested),this.outputPass.uniforms.extendedDisplay.value=this.displayRange==="extended");let a=ap(this.quality,i,s,this.samples);i===this.width&&s===this.height&&this.sceneTarget.samples===a||(a!==this.sceneTarget.samples&&(this.sceneTarget.dispose(),this.sceneTarget.samples=a),this.sceneTarget.setSize(i,s),this.compositeTarget.setSize(i,s),this.composer.setSize(i,s),this.fog.setSize(i,s),this.width=i,this.height=s)}setCamera(t){this.camera&&(this.navigation?.setMode("orbit"),Qh(this.camera,this.controls,this.room,t),this.cameraName=t)}resetCamera(){this.setCamera("audience")}setNavigationMode(t){this.navigation?.setMode(t)}setNavigationSpeed(t){this.navigation?.setSpeed(t)}setInteractionActive(t){this.navigation?.setInteractionActive(t)}moveCamera(t,e){this.navigation?.move(t,e)}getNavigationState(){return this.navigation?.state()||null}onNavigationChange(t){return this.navigation?.subscribe(t)||(()=>{})}focusFixture(t){let e=this.fixtureById?.get(String(t));return e?(this.navigation?.setMode("orbit"),this.controls.target.fromArray(e.position||[0,0,0]),this.camera.position.copy(this.controls.target).add(new C(1.3,.5,2)),this.controls.update(),this.cameraName="fixture",!0):!1}changeActivity(t){if(this.refresh?.setActive(t&&!this.paused),this.visible=t,t||this.fog?.showAtmosphere?.suspend(),this.lastFrame=0,this.lighting.latest=this.lighting.previous=null,this.displayClock.cancel(),t||this.gpuTimer?.dispose(),this.navigation?.refreshActivity(),!(!t||this.disposed||this.error))try{this.renderer||this.initialize(),this.restorePending&&this.restoreContext(),this.setQuality(this.qualityChoice),this.snapshot&&this.update(this.snapshot,!!this.lightBudgetPending),this.resize(),this.scheduleFrame(0)}catch(e){this.fail(e)}}restoreContext(){this.restorePending=!1,this.displayWidth=this.displayHeight=null,this.gpuTimer?.dispose(),this.platform.worker||(this.gpuTimer=new Eo(this.renderer.getContext())),this.resize(),this.refreshEnvironment(),this.fog?.upload(),this.footprints&&(this.footprints.dirty=this.footprints.force=!0,this.footprints.tileLit.fill(!0)),Hp(this.spots,this.reflections?.lights),this.lost=!1,this.ready=!1,this.lastFrame=0,this.setStatus("Restoring venue shaders\u2026")}scheduleFrame(t){this.disposed||this.paused||!this.visible||this.lost||this.platform.autoStart===!1||(this.nextFrameAt=performance.now()+t,this.displayClock.at(this.nextFrameAt,e=>this.animate(e)))}pause(){this.paused=!0,this.refresh?.setActive(!1),this.gpuTimer?.dispose(),this.navigation?.refreshActivity(),this.lighting.previous=null,this.displayClock.cancel()}resume(){this.disposed||(this.paused=!1,this.refresh?.setActive(this.visible),this.navigation?.refreshActivity(),this.lastFrame=0,this.scheduleFrame(0))}animate(t){if(this.disposed||this.paused||!this.visible||this.lost)return;let e=this.lastFrame?Math.min(.1,(t-this.lastFrame)/1e3):0;this.lastFrame=t,this.clock+=e;try{let i=this.gpuTimer?.sample(),s=this.gpuTimer?.begin();try{this.draw(e)}finally{s&&this.gpuTimer.end()}i!=null&&(this.lastGpuMs=i,this.measureLoad(Math.max(this.lastCPU,i||0),performance.now())),this.scheduleFrame(Math.max(0,t+this.frameInterval-performance.now()))}catch(i){this.fail(i)}}draw(t=0){if(this.disposed||this.lost||!this.visible||!this.renderer)return!1;this.resizePending&&this.resize();let e=performance.now();this.platform.worker&&this.controls.update();let i=!1;if(this.lighting.latest){let d=this.lighting.values(this.lightingFixtures||this.fixtureSamples,e);d!==this.displayedValues&&(this.applyLighting(d),i=!0)}this.camera.updateMatrixWorld();let s=this.camera.matrixWorld.elements,r=this.lodView||=new Float64Array(16).fill(NaN),o=!s.every((d,f)=>d===r[f]);o&&r.set(s),(o||this.fading)&&!i&&this.displayedValues&&(this.applyLighting(this.displayedValues),this.relights=(this.relights||0)+1);let a=!this.reducedMotion.matches||this.clock<this.fogAnimationUntil,l=this.hemisphere.color.clone().multiplyScalar(this.workLight*.8).add(this.workLamp.color.clone().multiplyScalar(this.workLight*.65));this.bounceColor&&l.add(this.bounceColor),this.fog.setAmbient?.({color:l,intensity:1,groundColor:l.clone().multiplyScalar(.5)}),this.fog.update(a?t:0,this.lightEntries||[]),this.reflections.illuminate(this.fog.field,this.fogAmount,this.fog.extinctionPerM),this.footprints.render(),this.renderer.setRenderTarget(this.sceneTarget),this.renderer.clear(),this.renderer.render(this.scene,this.camera),this.renderedShadows=this.spots.filter(d=>d.castShadow&&d.shadow.map).length,this.inputPass.enabled=this.glass.enabled;let c=this.glass.enabled?this.compositeTarget:this.composer.readBuffer;this.fog.render(this.sceneTarget,this.camera,c,d=>this.beams.render(this.renderer,this.camera,d,this.sceneTarget,{room:this.room,amount:this.fog.visibleAmount,extinctionPerM:this.fog.extinctionPerM,time:this.fog.field.time,densityTexture:this.fog.density})),this.film.uniforms.phase.value=this.reducedMotion.matches?0:Math.floor(this.clock*24)%1024,this.composer.render(t),this.editorOverlay?.render(this.renderer,this.camera),this.lastCPU=performance.now()-e,this.draws++,this.canvas.tolglowCaptureVideo?.(this.canvas),this.observedFrame?.(this.canvas,this.snapshot?.["show-id"],this.snapshot?.epoch,this.snapshot?.["frame-number"]??0);let h=1e3/this.targetHz,u=this.frameInterval||h;this.platform.worker||(this.frameInterval=Math.max(h,this.lastCPU*(this.softwareRenderer?4:1.5),u*.95)),this.ready||(this.ready=!0,this.resolveReady?.(!0)),this.updateStatus()}updateStatus(){let t=this.activeLights>this.spots.length?` \xB7 ${this.activeLights} lit \xB7 ${this.spots.length} full lights`:"",e=this.qualityChoice==="auto"&&this.softwareRenderer?"low \xB7 software GPU":this.quality;this.setStatus(`${e} \xB7 ${this.renderedShadows||0} shadows \xB7 ${this.workLight?"work light":"dark show"}${t}`)}getTelemetry(){return{lightingMode:this.lighting.mode,targetRefreshHz:this.targetHz,tierMaxHz:this.options.fps,displayHz:this.displayHz,ready:this.ready,failed:this.error||null,renderer:`Three.js r${"187"}`,quality:this.quality,deviceRenderer:this.deviceRenderer,softwareRenderer:this.softwareRenderer,pixels:[this.width,this.height],displayPixels:[this.displayWidth,this.displayHeight],sceneSamples:this.sceneTarget?.samples,edgeFilter:"FXAA",displayRange:this.displayRange,gpuTimeMs:this.lastGpuMs,film:{...this.filmSettings},measuredFrameMs:this.measuredFrameMs,renderScale:this.renderScale,frameCPUms:this.lastCPU,frameIntervalMs:this.frameInterval,idleBudgetMs:Math.max(0,this.frameInterval-this.lastCPU),nextFrameAt:this.nextFrameAt,frames:this.draws,geometryCount:this.renderer?.info.memory.geometries||0,textureCount:this.renderer?.info.memory.textures||0,programCount:this.renderer?.info.programs?.length||0,activeLights:this.activeLights||0,reflectedPatches:this.reflections?.patches.map(t=>({fixture:t.id,surface:t.surface,center:t.position.toArray(),majorRadius:t.major.length(),minorRadius:t.minor.length()}))||[],lightIds:this.litIds||[],slotIds:this.lightIds,fixtureCount:this.totalFixtureCount,visibleFixtures:this.fixtures.length,lightCap:this.spots?.length??0,lightLod:this.lod?{...this.lod,fakedBeams:this.beams?.count||0,footprints:this.footprints?.count||0}:null,shadowCount:this.renderedShadows||0,workLight:this.workLight,fogAmount:this.fogAmount,camera:this.camera?.position.toArray(),target:this.controls?.target.toArray(),fog:this.fog?.stats(),visible:this.visible,hidden:this.platform.worker?this.platform.hidden:document.hidden,lost:this.lost,epoch:this.snapshot?.epoch,revision:this.snapshot?.revision,frameNumber:this.snapshot?.["frame-number"]}}fail(t){this.error=String(t?.message||t),this.ready=!1,this.setStatus(`Preview unavailable: ${this.error}`),this.pause(),this.resolveReady?.(!1),console.error("Tolglow preview",t)}dispose(){if(!this.disposed){this.disposed=!0,this.ready=!1,this.pause(),this.abort.abort(),this.resizeObserver?.disconnect(),this.activity?.dispose(),this.editorInput?.dispose(),this.editorOverlay?.dispose(),this.navigation?.dispose(),this.controls?.dispose(),this.fog?.dispose(),this.beams?.dispose(),this.footprints?.dispose();for(let t of this.fixtures)t.dispose();for(let t of this.diffuserTubes||[])t.dispose();for(let t of this.spots||[])t.dispose();this.reflections?.dispose(),this.content&&au(this.content),this.glass?.dispose(),this.inputPass?.dispose(),this.bloom?.dispose(),this.outputPass?.dispose(),this.edges?.dispose(),this.film?.dispose(),this.gpuTimer?.dispose(),this.composer?.dispose(),this.sceneTarget?.dispose(),this.compositeTarget?.dispose(),this.environmentTarget?.dispose(),this.materials&&pp(this.materials),this.renderer?.dispose(),this.resolveReady?.(!1),this.canvas.tolglowPreview===this&&delete this.canvas.tolglowPreview}}};var pt,Io,xs=0,mr=0,mv=0,he,No=!1,Lo=0,xr=null,Tc=!1,tn=!1,Mn=0,Vi=null,gr=null,_s=null,Do=null,ys=null,wu=!1,si=[],Sn=n=>postMessage(n),Vp=()=>tn&&he?.generation===Mn&&(!he.paused||Tc),Gp=()=>si.some(n=>n.fence);function Wp(){if(No||!pt||pt.lost||Vi||!Vp())return!1;let n=pt.softwareRenderer||he.paused?1:2;return si.length+ +(gr!==null)<n&&!(pt.resizePending&&Gp())}function Eu(){Wp()&&!pt.displayClock.pending&&pt.displayClock.at(xr??performance.now(),_v,pt.softwareRenderer?.5:Math.min(2.5,250/pt.targetHz))}function Xp(n){n.fence&&n.gl.deleteSync(n.fence),n.bitmap?.close()}function Tu(){clearTimeout(ys),ys=null;for(let n of si)Xp(n);si.length&&(si.length=0,Sn({type:"render-retired",rendering:!1}))}function qp(){No=!0,Vi=_s=null,pt?.displayClock.cancel(),Tu(),pt?.dispose()}function Ac(n){qp(),Sn({type:"error",message:String(n?.message||n)})}function Yp(n){let t=performance.now();n.bitmap=Io.transferToImageBitmap(),n.bitmapMs=performance.now()-t}function gv(n){ys!==null||wu||(!pt.softwareRenderer&&globalThis.scheduler?.yield&&n.yields<16&&performance.now()-n.submitted<Math.min(4,500/pt.targetHz)?(n.yields++,wu=!0,scheduler.yield().then(()=>{wu=!1,_r()},Ac)):ys=setTimeout(()=>{ys=null,_r()},2))}function xv(){let n=si[0];if(!n||n.fence||gr)return;si.shift(),gr=n;let{bitmap:t,metadata:e,telemetry:i,time:s,started:r,duration:o,gpuWait:a,bitmapMs:l}=n;try{postMessage({type:"frame",...e,bitmap:t,rendering:si.length>0,telemetry:{...i,frameIntervalMs:pt.frameInterval,workerFrameMs:o,rafDeliveryMs:r-s,readyForFrameMs:pt.lastReadyMs,gpuCompletionWaitMs:a,idleBudgetMs:Math.max(0,pt.frameInterval-o),bitmapTransferMs:l}},[t]),n.bitmap=null}catch(c){throw t.close(),c}}function _r(){if(!No)try{if(pt.lost){Tu();return}for(let e of si){if(!e.fence)continue;let{gl:i,fence:s}=e,r=i.clientWaitSync(s,0,0);if(r===i.WAIT_FAILED)throw new Error("Preview GPU frame wait failed");if(r!==i.ALREADY_SIGNALED&&r!==i.CONDITION_SATISFIED){if(performance.now()-e.submitted>3e4)throw new Error("Preview GPU frame did not complete");break}if(i.deleteSync(s),e.fence=null,e.gpuWait=performance.now()-e.submitted,tn&&e.metadata.generation===Mn&&!e.bitmap&&Yp(e),e.duration=performance.now()-e.started,pt.softwareRenderer){let o=1e3/pt.targetHz,a=pt.frameInterval;pt.frameInterval=Math.max(o,Math.min(e.duration*2,a*1.5),a*.5)}}let n=!1;for(;si[0]&&!si[0].fence&&(!tn||si[0].metadata.generation!==Mn);)Xp(si.shift()),n=!0;n&&Sn({type:"render-retired",rendering:si.length>0}),xv();let t=si.find(e=>e.fence);if(t)gv(t);else if(clearTimeout(ys),ys=null,_s&&(Vp()&&pt.measureLoad(_s.elapsed,_s.now),_s=null),Vi){let e=Vi;Vi=null,Ec(e)}Eu()}catch(n){Ac(n)}}function _v(n){if(Wp())try{let t=performance.now(),e=++mv;Sn({type:"render-start",frameId:e,stateId:xs});let i=Lo?Math.min(.1,(n-Lo)/1e3):0,s=1e3/pt.targetHz;xr=Bu(xr,n,s),pt.softwareRenderer||(pt.frameInterval=s),Lo=n,pt.clock+=i,pt.draw(i),Tc=!1;let r=performance.now(),o=pt.renderer.getContext(),a=o.fenceSync(o.SYNC_GPU_COMMANDS_COMPLETE,0);if(!a)throw new Error("Could not fence the preview GPU frame");let l=pt.snapshot,c={time:n,started:t,submitted:r,gl:o,fence:a,bitmap:null,yields:0,metadata:{frameId:e,stateId:xs,actionThrough:mr,generation:Mn,runtimeId:l?.["runtime-id"],showId:l?.["show-id"],epoch:l?.epoch,showFrame:l?.["frame-number"]??0}};si.push(c),o.flush(),pt.softwareRenderer||Yp(c),c.telemetry=pt.getTelemetry(),_r()}catch(t){Ac(t)}}function Ec(n,t=!1){if(!(n.id<=xs||No)){if(xs=n.id,!tn||n.state.generation!==Mn){Sn({type:"state-ack",id:xs,actionThrough:mr});return}if(he=n.state,t){pt.updateEmittedOutput({...pt.snapshot,...he.snapshot},he.receivedAt-performance.timeOrigin),Sn({type:"state-ack",id:xs,actionThrough:mr}),Eu();return}if(!pt){Io=new OffscreenCanvas(1,1);let e="",i={set textContent(r){e=r,Sn({type:"status",text:r})},get textContent(){return e}};if(pt=new wc(Io,i,{worker:!0,autoStart:!1,viewport:he.viewport,displayHz:he.displayHz,coarsePointer:he.coarsePointer,reducedMotion:he.reducedMotion}),pt.error)throw new Error(pt.error);let s=pt.abort.signal;Io.addEventListener("webglcontextlost",()=>{pt.displayClock.cancel(),Tu()},{signal:s}),Io.addEventListener("webglcontextrestored",()=>_r(),{signal:s})}if(pt.setDisplayHz(he.displayHz),pt.platform.viewport=he.viewport,pt.visible=!0,pt.restorePending&&pt.restoreContext(),pt.reducedMotion.matches=he.reducedMotion,pt.setQuality(he.quality),pt.setLightBudget(he.lightBudget??null),pt.setFilm(he.film),pt.setWorkLight(he.workLight),pt.setFog(he.fog),pt.setLightingMode(he.lightingMode),he.snapshot){let e=he.snapshot;for(let i of["sculpt-guide","spatial-volumes","volume-guides","glass-meshes","glass-volumes","video-surfaces"])Object.hasOwn(e,i)||(e[i]=pt.snapshot?.[i]);!e.rig&&pt.snapshot&&(e.rig=pt.snapshot.rig),Object.hasOwn(e,"editor")||(e.editor=pt.snapshot?.editor),pt.update(e,!1,he.receivedAt-performance.timeOrigin)}pt.camera.position.fromArray(he.camera.position),pt.controls.target.fromArray(he.camera.target),pt.camera.up.fromArray(he.camera.up),pt.camera.fov=he.camera.fov,pt.cameraName=he.camera.name,pt.controls.update(),pt.resize();for(let e of he.actions)e.id>mr&&(e.type==="burst"?pt.burst(e.options):e.type==="draw"&&(Tc=!0),mr=e.id);Sn({type:"state-ack",id:xs,actionThrough:mr}),he.paused&&(Lo=0,xr=null,Do=null,pt.lighting.previous=null,pt.displayClock.cancel()),Eu()}}self.onmessage=n=>{if(!No)try{let t=n.data;if(t.type==="activity"){if(tn=t.active,Mn=t.generation,Tc=!1,Lo=0,xr=Do=_s=null,Vi&&Vi.state.generation!==Mn){let e=Vi;Vi=null,Ec(e)}pt&&(pt.visible=tn,pt.platform.hidden=!tn,tn||pt.fog?.showAtmosphere?.suspend(),pt.displayClock.cancel(),pt.lighting.latest=pt.lighting.previous=null,_r())}else if(t.type==="state"){let e=tn&&t.state.generation===Mn;e&&t.outputOnly&&!Vi&&pt?.snapshot&&!pt.lost&&!pt.softwareRenderer&&En(t.state.snapshot)===En(pt.snapshot)?Ec(t,!0):e&&(Gp()||pt?.lost&&!pt.restorePending)?Vi=t:Ec(t)}else if(t.type==="frame-ack"&&t.frameId===gr?.metadata.frameId){let e=gr;if(gr=null,tn&&e.metadata.generation===Mn){let i=performance.now();pt.lastReadyMs=i-e.time,_s={elapsed:Math.min(pt.lastReadyMs,Do===null?1/0:i-Do),now:i},Do=i,pt.softwareRenderer&&(xr=i+pt.frameInterval)}_r()}else t.type==="dispose"&&(qp(),close())}catch(t){Ac(t)}};Sn({type:"ready"});
/*! Bundled license information:

three/build/three.core.js:
three/build/three.module.js:
  (**
   * @license
   * Copyright 2010-2026 Three.js Authors
   * SPDX-License-Identifier: MIT
   *)
*/
