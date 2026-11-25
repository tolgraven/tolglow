var Bi={low:{dpr:.75,maxPixels:18e4,shadows:1,shadowSize:256,fps:10,fixtures:32,fog:"phone"},mobile:{dpr:2,maxPixels:15e5,shadows:2,shadowSize:512,fps:120,fixtures:32},desktop:{dpr:2,maxPixels:32e5,shadows:4,shadowSize:1024,fps:120,fixtures:64},high:{dpr:4,maxPixels:8294400,shadows:4,shadowSize:2048,fps:120,fixtures:128}};function qh(n,t,e,i){let s=n==="low"&&t==="auto"&&!e&&i?Bi.mobile.fps:Bi[n].fps;return Math.min(s,i||s)}function ga(n,t,e){return Bi[n==="low"&&t==="auto"&&!e?"mobile":n]}function Yh(n,t,e,i,s,r){let a=Math.min(e||1,Math.sqrt(8294400/(n*t)),r/Math.max(n,t)),o=Math.min(a,i.dpr,Math.sqrt(i.maxPixels/(n*t)))*s,l=c=>[n,t].map(h=>Math.max(1,Math.round(h*c)));return{scene:l(o),display:l(a)}}function Zh(n){let t=i=>Array.from(n.getInternalformatParameter(n.RENDERBUFFER,i,n.SAMPLES)),e=t(n.DEPTH_COMPONENT24);return Math.max(0,...t(n.RGBA16F).filter(i=>i<=4&&e.includes(i)))}function Kh(n,t,e,i){return n==="low"||t*e*i>16e6?0:i}function Jh(n,t){let e=Object.keys(Bi);return e[Math.max(0,Math.min(e.length-1,e.indexOf(n)+t))]}function xa(){let n=globalThis.WebGL2RenderingContext?.prototype;return!!(globalThis.matchMedia?.("(dynamic-range: high)").matches&&typeof n?.drawingBufferStorage=="function"&&typeof n?.drawingBufferToneMapping=="function")}function $h(n,t,e,i){if(!i||!n.getExtension("EXT_color_buffer_float"))return"sdr";try{if(n.drawingBufferStorage(n.RGBA16F,t,e),n.drawingBufferFormat===n.RGBA16F&&n.drawingBufferToneMapping({mode:"extended"})?.mode==="extended")return"extended"}catch{}if(n.drawingBufferFormat===n.RGBA16F){n.drawingBufferStorage(n.RGBA8,t,e);try{n.drawingBufferToneMapping({mode:"standard"})}catch{}}return"sdr"}var nr=class{constructor(t){this.gl=t,this.extension=t.getExtension("EXT_disjoint_timer_query_webgl2")}sample(){let t=this.gl;if(!this.extension){let s=this.elapsed;return this.elapsed=null,s}if(!this.query)return null;let e=t.getParameter(this.extension.GPU_DISJOINT_EXT);if(!e&&!t.getQueryParameter(this.query,t.QUERY_RESULT_AVAILABLE))return null;let i=e?null:t.getQueryParameter(this.query,t.QUERY_RESULT)/1e6;return this.dispose(),i}begin(){return this.extension?this.query||(this.query=this.gl.createQuery(),!this.query)?!1:(this.gl.beginQuery(this.extension.TIME_ELAPSED_EXT,this.query),!0):this.fence?!1:(this.started=performance.now(),!0)}end(){let t=this.gl;if(this.extension){t.endQuery(this.extension.TIME_ELAPSED_EXT);return}if(this.fence=t.fenceSync(t.SYNC_GPU_COMMANDS_COMPLETE,0),!this.fence)return;t.flush();let e=()=>{let i=t.clientWaitSync(this.fence,0,0),s=performance.now()-this.started;if(i===t.TIMEOUT_EXPIRED&&s<3e4){this.timeout=setTimeout(e,2);return}this.elapsed=i===t.CONDITION_SATISFIED||i===t.ALREADY_SIGNALED?s:null,t.deleteSync(this.fence),this.fence=null};this.timeout=setTimeout(e,0)}dispose(){this.query&&this.gl.deleteQuery(this.query),this.fence&&this.gl.deleteSync(this.fence),clearTimeout(this.timeout),this.query=this.fence=this.elapsed=null}};var Xi=n=>JSON.stringify([n?.["runtime-id"]??null,n?.["show-id"]??null,n?.epoch??null]);function jh(n,t,e,i){try{return n.frameId<=e||Xi({"runtime-id":n.runtimeId,"show-id":n.showId,epoch:n.epoch})!==t?!1:(i(n.bitmap),!0)}finally{n.bitmap.close()}}var yn={LEFT:0,MIDDLE:1,RIGHT:2,ROTATE:0,DOLLY:1,PAN:2},Li={ROTATE:0,PAN:1,DOLLY_PAN:2,DOLLY_ROTATE:3},Fu=0,Dc=1,Ou=2;var Yn=1,Bu=2,Ls=3,Mn=0,ke=1,je=2,Ni=0,Ns=1,zr=2,Lc=3,Nc=4,zu=5;var Zn=100,ku=101,Hu=102,Vu=103,Gu=104,Wu=200,Xu=201,qu=202,Yu=203,Uc=204,Fc=205,Zu=206,Ku=207,Ju=208,$u=209,ju=210,Qu=211,td=212,ed=213,id=214,Za=0,Ka=1,Ja=2,xs=3,$a=4,ja=5,Qa=6,to=7,Ro=0,nd=1,sd=2,Hi=0,kr=1,Hr=2,Vr=3,Kn=4,Gr=5,Wr=6,Xr=7;var Oc=300,Jn=301,Us=302,Co=303,Po=304,Fn=1e3,ki=1001,eo=1002,Se=1003,rd=1004;var qr=1005;var ae=1006,Io=1007;var sn=1008;var ii=1009,Bc=1010,zc=1011,Fs=1012,Do=1013,Qe=1014,yi=1015,fe=1016,Lo=1017,No=1018,$n=1020,kc=35902,Hc=35899,Vc=1021,Gc=1022,ci=1023,Ai=1026,rn=1027,Os=1028,Uo=1029,Sn=1030,Fo=1031;var Oo=1033,Yr=33776,Zr=33777,Kr=33778,Jr=33779,Bo=35840,zo=35841,ko=35842,Ho=35843,Vo=36196,Go=37492,Wo=37496,Xo=37488,qo=37489,$r=37490,Yo=37491,Zo=37808,Ko=37809,Jo=37810,$o=37811,jo=37812,Qo=37813,tl=37814,el=37815,il=37816,nl=37817,sl=37818,rl=37819,al=37820,ol=37821,ll=36492,cl=36494,hl=36495,ul=36283,dl=36284,jr=36285,fl=36286;var pr=2300,io=2301,qa=2302,yc=2303,Mc=2400,Sc=2401,bc=2402;var ad=3200;var Qr=0,od=1,Ui="",Fe="srgb",pn="srgb-linear",mr="linear",Jt="srgb";var Ya=7680;var ld=519,cd=512,hd=513,ud=514,Bs=515,dd=516,fd=517,Wc=518,Xc=519,pd=35044,bn=35048;var ta="300 es",mi=2e3,_s=2001;function Zf(n){for(let t=n.length-1;t>=0;--t)if(n[t]>=65535)return!0;return!1}function Kf(n){return ArrayBuffer.isView(n)&&!(n instanceof DataView)}function gr(n){return document.createElementNS("http://www.w3.org/1999/xhtml",n)}function md(){let n=gr("canvas");return n.style.display="block",n}var Qh={},vs=null;function qc(...n){let t="THREE."+n.shift();vs?vs("log",t,...n):console.log(t,...n)}function gd(n){let t=n[0];if(typeof t=="string"&&t.startsWith("TSL:")){let e=n[1];e&&e.isStackTrace?n[0]+=" "+e.getLocation():n[1]='Stack trace not available. Enable "THREE.Node.captureStackTrace" to capture stack traces.'}return n}function Pt(...n){n=gd(n);let t="THREE."+n.shift();if(vs)vs("warn",t,...n);else{let e=n[0];e&&e.isStackTrace?console.warn(e.getError(t)):console.warn(t,...n)}}function Lt(...n){n=gd(n);let t="THREE."+n.shift();if(vs)vs("error",t,...n);else{let e=n[0];e&&e.isStackTrace?console.error(e.getError(t)):console.error(t,...n)}}function Un(...n){let t=n.join(" ");t in Qh||(Qh[t]=!0,Pt(...n))}function xd(n,t,e){return new Promise(function(i,s){function r(){switch(n.clientWaitSync(t,n.SYNC_FLUSH_COMMANDS_BIT,0)){case n.WAIT_FAILED:s();break;case n.TIMEOUT_EXPIRED:setTimeout(r,e);break;default:i()}}setTimeout(r,e)})}var _d={[Za]:Za,[Ja]:Qa,[$a]:$a,[xs]:ja,[Ka]:Ka,[Qa]:Ja,[to]:to,[ja]:xs},Ri=class{addEventListener(t,e){this._listeners===void 0&&(this._listeners={});let i=this._listeners;i[t]===void 0&&(i[t]=[]),i[t].indexOf(e)===-1&&i[t].push(e)}hasEventListener(t,e){let i=this._listeners;return i===void 0?!1:i[t]!==void 0&&i[t].indexOf(e)!==-1}removeEventListener(t,e){let i=this._listeners;if(i===void 0)return;let s=i[t];if(s!==void 0){let r=s.indexOf(e);r!==-1&&s.splice(r,1)}}dispatchEvent(t){let e=this._listeners;if(e===void 0)return;let i=e[t.type];if(i!==void 0){t.target=this;let s=i.slice(0);for(let r=0,a=s.length;r<a;r++)s[r].call(this,t);t.target=null}}},We=["00","01","02","03","04","05","06","07","08","09","0a","0b","0c","0d","0e","0f","10","11","12","13","14","15","16","17","18","19","1a","1b","1c","1d","1e","1f","20","21","22","23","24","25","26","27","28","29","2a","2b","2c","2d","2e","2f","30","31","32","33","34","35","36","37","38","39","3a","3b","3c","3d","3e","3f","40","41","42","43","44","45","46","47","48","49","4a","4b","4c","4d","4e","4f","50","51","52","53","54","55","56","57","58","59","5a","5b","5c","5d","5e","5f","60","61","62","63","64","65","66","67","68","69","6a","6b","6c","6d","6e","6f","70","71","72","73","74","75","76","77","78","79","7a","7b","7c","7d","7e","7f","80","81","82","83","84","85","86","87","88","89","8a","8b","8c","8d","8e","8f","90","91","92","93","94","95","96","97","98","99","9a","9b","9c","9d","9e","9f","a0","a1","a2","a3","a4","a5","a6","a7","a8","a9","aa","ab","ac","ad","ae","af","b0","b1","b2","b3","b4","b5","b6","b7","b8","b9","ba","bb","bc","bd","be","bf","c0","c1","c2","c3","c4","c5","c6","c7","c8","c9","ca","cb","cc","cd","ce","cf","d0","d1","d2","d3","d4","d5","d6","d7","d8","d9","da","db","dc","dd","de","df","e0","e1","e2","e3","e4","e5","e6","e7","e8","e9","ea","eb","ec","ed","ee","ef","f0","f1","f2","f3","f4","f5","f6","f7","f8","f9","fa","fb","fc","fd","fe","ff"],tu=1234567,ms=Math.PI/180,On=180/Math.PI;function zs(){let n=Math.random()*4294967295|0,t=Math.random()*4294967295|0,e=Math.random()*4294967295|0,i=Math.random()*4294967295|0;return(We[n&255]+We[n>>8&255]+We[n>>16&255]+We[n>>24&255]+"-"+We[t&255]+We[t>>8&255]+"-"+We[t>>16&15|64]+We[t>>24&255]+"-"+We[e&63|128]+We[e>>8&255]+"-"+We[e>>16&255]+We[e>>24&255]+We[i&255]+We[i>>8&255]+We[i>>16&255]+We[i>>24&255]).toLowerCase()}function Bt(n,t,e){return Math.max(t,Math.min(e,n))}function Yc(n,t){return(n%t+t)%t}function Jf(n,t,e,i,s){return i+(n-t)*(s-i)/(e-t)}function $f(n,t,e){return n!==t?(e-n)/(t-n):0}function ur(n,t,e){return(1-e)*n+e*t}function jf(n,t,e,i){return ur(n,t,1-Math.exp(-e*i))}function Qf(n,t=1){return t-Math.abs(Yc(n,t*2)-t)}function tp(n,t,e){return n<=t?0:n>=e?1:(n=(n-t)/(e-t),n*n*(3-2*n))}function ep(n,t,e){return n<=t?0:n>=e?1:(n=(n-t)/(e-t),n*n*n*(n*(n*6-15)+10))}function ip(n,t){return n+Math.floor(Math.random()*(t-n+1))}function np(n,t){return n+Math.random()*(t-n)}function sp(n){return n*(.5-Math.random())}function rp(n){n!==void 0&&(tu=n);let t=tu+=1831565813;return t=Math.imul(t^t>>>15,t|1),t^=t+Math.imul(t^t>>>7,t|61),((t^t>>>14)>>>0)/4294967296}function ap(n){return n*ms}function op(n){return n*On}function lp(n){return n>0&&Number.isInteger(n)&&2**Math.round(Math.log2(n))===n}function cp(n){return Math.pow(2,Math.ceil(Math.log(n)/Math.LN2))}function Zc(n){return Math.pow(2,Math.floor(Math.log(n)/Math.LN2))}function hp(n,t,e,i,s){let r=Math.cos,a=Math.sin,o=r(e/2),l=a(e/2),c=r((t+i)/2),h=a((t+i)/2),u=r((t-i)/2),d=a((t-i)/2),f=r((i-t)/2),p=a((i-t)/2);switch(s){case"XYX":n.set(o*h,l*u,l*d,o*c);break;case"YZY":n.set(l*d,o*h,l*u,o*c);break;case"ZXZ":n.set(l*u,l*d,o*h,o*c);break;case"XZX":n.set(o*h,l*p,l*f,o*c);break;case"YXY":n.set(l*f,o*h,l*p,o*c);break;case"ZYZ":n.set(l*p,l*f,o*h,o*c);break;default:Pt("MathUtils: .setQuaternionFromProperEuler() encountered an unknown order: "+s)}}function Kc(n){switch(n.constructor){case Uint32Array:return 4294967295;case Uint16Array:return 65535;case Uint8Array:case Uint8ClampedArray:return 255;case Int32Array:return 2147483647;case Int16Array:return 32767;case Int8Array:return 127;default:return}}function vd(n,t){if(t.constructor===Float32Array)return n;let e=Kc(t);if(e===void 0)throw new Error("THREE.MathUtils: Invalid component type.");return Math.max(n/e,-1)}function yd(n,t){if(t.constructor===Float32Array)return n;let e=Kc(t);if(e===void 0)throw new Error("THREE.MathUtils: Invalid component type.");return Math.round(n*e)}var ks={DEG2RAD:ms,RAD2DEG:On,generateUUID:zs,clamp:Bt,euclideanModulo:Yc,mapLinear:Jf,inverseLerp:$f,lerp:ur,damp:jf,pingpong:Qf,smoothstep:tp,smootherstep:ep,randInt:ip,randFloat:np,randFloatSpread:sp,seededRandom:rp,degToRad:ap,radToDeg:op,isPowerOfTwo:lp,ceilPowerOfTwo:cp,floorPowerOfTwo:Zc,setQuaternionFromProperEuler:hp,normalize:yd,denormalize:vd},et=class{static{this.prototype.isVector2=!0}constructor(t=0,e=0){this.x=t,this.y=e}get width(){return this.x}set width(t){this.x=t}get height(){return this.y}set height(t){this.y=t}set(t,e){return this.x=t,this.y=e,this}setScalar(t){return this.x=t,this.y=t,this}setX(t){return this.x=t,this}setY(t){return this.y=t,this}setComponent(t,e){switch(t){case 0:this.x=e;break;case 1:this.y=e;break;default:throw new Error("THREE.Vector2: index is out of range: "+t)}return this}getComponent(t){switch(t){case 0:return this.x;case 1:return this.y;default:throw new Error("THREE.Vector2: index is out of range: "+t)}}clone(){return new this.constructor(this.x,this.y)}copy(t){return this.x=t.x,this.y=t.y,this}add(t){return this.x+=t.x,this.y+=t.y,this}addScalar(t){return this.x+=t,this.y+=t,this}addVectors(t,e){return this.x=t.x+e.x,this.y=t.y+e.y,this}addScaledVector(t,e){return this.x+=t.x*e,this.y+=t.y*e,this}sub(t){return this.x-=t.x,this.y-=t.y,this}subScalar(t){return this.x-=t,this.y-=t,this}subVectors(t,e){return this.x=t.x-e.x,this.y=t.y-e.y,this}multiply(t){return this.x*=t.x,this.y*=t.y,this}multiplyScalar(t){return this.x*=t,this.y*=t,this}divide(t){return this.x/=t.x,this.y/=t.y,this}divideScalar(t){return this.multiplyScalar(1/t)}applyMatrix3(t){let e=this.x,i=this.y,s=t.elements;return this.x=s[0]*e+s[3]*i+s[6],this.y=s[1]*e+s[4]*i+s[7],this}min(t){return this.x=Math.min(this.x,t.x),this.y=Math.min(this.y,t.y),this}max(t){return this.x=Math.max(this.x,t.x),this.y=Math.max(this.y,t.y),this}clamp(t,e){return this.x=Bt(this.x,t.x,e.x),this.y=Bt(this.y,t.y,e.y),this}clampScalar(t,e){return this.x=Bt(this.x,t,e),this.y=Bt(this.y,t,e),this}clampLength(t,e){let i=this.length();return this.divideScalar(i||1).multiplyScalar(Bt(i,t,e))}floor(){return this.x=Math.floor(this.x),this.y=Math.floor(this.y),this}ceil(){return this.x=Math.ceil(this.x),this.y=Math.ceil(this.y),this}round(){return this.x=Math.round(this.x),this.y=Math.round(this.y),this}roundToZero(){return this.x=Math.trunc(this.x),this.y=Math.trunc(this.y),this}negate(){return this.x=-this.x,this.y=-this.y,this}dot(t){return this.x*t.x+this.y*t.y}cross(t){return this.x*t.y-this.y*t.x}lengthSq(){return this.x*this.x+this.y*this.y}length(){return Math.sqrt(this.x*this.x+this.y*this.y)}manhattanLength(){return Math.abs(this.x)+Math.abs(this.y)}normalize(){return this.divideScalar(this.length()||1)}angle(){return Math.atan2(-this.y,-this.x)+Math.PI}angleTo(t){let e=Math.sqrt(this.lengthSq()*t.lengthSq());if(e===0)return Math.PI/2;let i=this.dot(t)/e;return Math.acos(Bt(i,-1,1))}distanceTo(t){return Math.sqrt(this.distanceToSquared(t))}distanceToSquared(t){let e=this.x-t.x,i=this.y-t.y;return e*e+i*i}manhattanDistanceTo(t){return Math.abs(this.x-t.x)+Math.abs(this.y-t.y)}setLength(t){return this.normalize().multiplyScalar(t)}lerp(t,e){return this.x+=(t.x-this.x)*e,this.y+=(t.y-this.y)*e,this}lerpVectors(t,e,i){return this.x=t.x+(e.x-t.x)*i,this.y=t.y+(e.y-t.y)*i,this}equals(t){return t.x===this.x&&t.y===this.y}fromArray(t,e=0){return this.x=t[e],this.y=t[e+1],this}toArray(t=[],e=0){return t[e]=this.x,t[e+1]=this.y,t}fromBufferAttribute(t,e){return this.x=t.getX(e),this.y=t.getY(e),this}rotateAround(t,e){let i=Math.cos(e),s=Math.sin(e),r=this.x-t.x,a=this.y-t.y;return this.x=r*i-a*s+t.x,this.y=r*s+a*i+t.y,this}random(){return this.x=Math.random(),this.y=Math.random(),this}*[Symbol.iterator](){yield this.x,yield this.y}},Je=class{constructor(t=0,e=0,i=0,s=1){this.isQuaternion=!0,this._x=t,this._y=e,this._z=i,this._w=s}static slerpFlat(t,e,i,s,r,a,o){let l=i[s+0],c=i[s+1],h=i[s+2],u=i[s+3],d=r[a+0],f=r[a+1],p=r[a+2],v=r[a+3];if(u!==v||l!==d||c!==f||h!==p){let g=l*d+c*f+h*p+u*v;g<0&&(d=-d,f=-f,p=-p,v=-v,g=-g);let m=1-o;if(g<.9995){let _=Math.acos(g),S=Math.sin(_);m=Math.sin(m*_)/S,o=Math.sin(o*_)/S,l=l*m+d*o,c=c*m+f*o,h=h*m+p*o,u=u*m+v*o}else{l=l*m+d*o,c=c*m+f*o,h=h*m+p*o,u=u*m+v*o;let _=1/Math.sqrt(l*l+c*c+h*h+u*u);l*=_,c*=_,h*=_,u*=_}}t[e]=l,t[e+1]=c,t[e+2]=h,t[e+3]=u}static multiplyQuaternionsFlat(t,e,i,s,r,a){let o=i[s],l=i[s+1],c=i[s+2],h=i[s+3],u=r[a],d=r[a+1],f=r[a+2],p=r[a+3];return t[e]=o*p+h*u+l*f-c*d,t[e+1]=l*p+h*d+c*u-o*f,t[e+2]=c*p+h*f+o*d-l*u,t[e+3]=h*p-o*u-l*d-c*f,t}get x(){return this._x}set x(t){this._x=t,this._onChangeCallback()}get y(){return this._y}set y(t){this._y=t,this._onChangeCallback()}get z(){return this._z}set z(t){this._z=t,this._onChangeCallback()}get w(){return this._w}set w(t){this._w=t,this._onChangeCallback()}set(t,e,i,s){return this._x=t,this._y=e,this._z=i,this._w=s,this._onChangeCallback(),this}clone(){return new this.constructor(this._x,this._y,this._z,this._w)}copy(t){return this._x=t.x,this._y=t.y,this._z=t.z,this._w=t.w,this._onChangeCallback(),this}setFromEuler(t,e=!0){let i=t._x,s=t._y,r=t._z,a=t._order,o=Math.cos,l=Math.sin,c=o(i/2),h=o(s/2),u=o(r/2),d=l(i/2),f=l(s/2),p=l(r/2);switch(a){case"XYZ":this._x=d*h*u+c*f*p,this._y=c*f*u-d*h*p,this._z=c*h*p+d*f*u,this._w=c*h*u-d*f*p;break;case"YXZ":this._x=d*h*u+c*f*p,this._y=c*f*u-d*h*p,this._z=c*h*p-d*f*u,this._w=c*h*u+d*f*p;break;case"ZXY":this._x=d*h*u-c*f*p,this._y=c*f*u+d*h*p,this._z=c*h*p+d*f*u,this._w=c*h*u-d*f*p;break;case"ZYX":this._x=d*h*u-c*f*p,this._y=c*f*u+d*h*p,this._z=c*h*p-d*f*u,this._w=c*h*u+d*f*p;break;case"YZX":this._x=d*h*u+c*f*p,this._y=c*f*u+d*h*p,this._z=c*h*p-d*f*u,this._w=c*h*u-d*f*p;break;case"XZY":this._x=d*h*u-c*f*p,this._y=c*f*u-d*h*p,this._z=c*h*p+d*f*u,this._w=c*h*u+d*f*p;break;default:Pt("Quaternion: .setFromEuler() encountered an unknown order: "+a)}return e===!0&&this._onChangeCallback(),this}setFromAxisAngle(t,e){let i=e/2,s=Math.sin(i);return this._x=t.x*s,this._y=t.y*s,this._z=t.z*s,this._w=Math.cos(i),this._onChangeCallback(),this}setFromRotationMatrix(t){let e=t.elements,i=e[0],s=e[4],r=e[8],a=e[1],o=e[5],l=e[9],c=e[2],h=e[6],u=e[10],d=i+o+u;if(d>0){let f=.5/Math.sqrt(d+1);this._w=.25/f,this._x=(h-l)*f,this._y=(r-c)*f,this._z=(a-s)*f}else if(i>o&&i>u){let f=2*Math.sqrt(1+i-o-u);this._w=(h-l)/f,this._x=.25*f,this._y=(s+a)/f,this._z=(r+c)/f}else if(o>u){let f=2*Math.sqrt(1+o-i-u);this._w=(r-c)/f,this._x=(s+a)/f,this._y=.25*f,this._z=(l+h)/f}else{let f=2*Math.sqrt(1+u-i-o);this._w=(a-s)/f,this._x=(r+c)/f,this._y=(l+h)/f,this._z=.25*f}return this._onChangeCallback(),this}setFromUnitVectors(t,e){let i=t.dot(e)+1;return i<1e-8?(i=0,Math.abs(t.x)>Math.abs(t.z)?(this._x=-t.y,this._y=t.x,this._z=0,this._w=i):(this._x=0,this._y=-t.z,this._z=t.y,this._w=i)):(this._x=t.y*e.z-t.z*e.y,this._y=t.z*e.x-t.x*e.z,this._z=t.x*e.y-t.y*e.x,this._w=i),this.normalize()}angleTo(t){return 2*Math.acos(Math.abs(Bt(this.dot(t),-1,1)))}rotateTowards(t,e){let i=this.angleTo(t);if(i===0)return this;let s=Math.min(1,e/i);return this.slerp(t,s),this}identity(){return this.set(0,0,0,1)}invert(){return this.conjugate()}conjugate(){return this._x*=-1,this._y*=-1,this._z*=-1,this._onChangeCallback(),this}dot(t){return this._x*t._x+this._y*t._y+this._z*t._z+this._w*t._w}lengthSq(){return this._x*this._x+this._y*this._y+this._z*this._z+this._w*this._w}length(){return Math.sqrt(this._x*this._x+this._y*this._y+this._z*this._z+this._w*this._w)}normalize(){let t=this.length();return t===0?(this._x=0,this._y=0,this._z=0,this._w=1):(t=1/t,this._x=this._x*t,this._y=this._y*t,this._z=this._z*t,this._w=this._w*t),this._onChangeCallback(),this}multiply(t){return this.multiplyQuaternions(this,t)}premultiply(t){return this.multiplyQuaternions(t,this)}multiplyQuaternions(t,e){let i=t._x,s=t._y,r=t._z,a=t._w,o=e._x,l=e._y,c=e._z,h=e._w;return this._x=i*h+a*o+s*c-r*l,this._y=s*h+a*l+r*o-i*c,this._z=r*h+a*c+i*l-s*o,this._w=a*h-i*o-s*l-r*c,this._onChangeCallback(),this}slerp(t,e){let i=t._x,s=t._y,r=t._z,a=t._w,o=this.dot(t);o<0&&(i=-i,s=-s,r=-r,a=-a,o=-o);let l=1-e;if(o<.9995){let c=Math.acos(o),h=Math.sin(c);l=Math.sin(l*c)/h,e=Math.sin(e*c)/h,this._x=this._x*l+i*e,this._y=this._y*l+s*e,this._z=this._z*l+r*e,this._w=this._w*l+a*e,this._onChangeCallback()}else this._x=this._x*l+i*e,this._y=this._y*l+s*e,this._z=this._z*l+r*e,this._w=this._w*l+a*e,this.normalize();return this}slerpQuaternions(t,e,i){return this.copy(t).slerp(e,i)}random(){let t=2*Math.PI*Math.random(),e=2*Math.PI*Math.random(),i=Math.random(),s=Math.sqrt(1-i),r=Math.sqrt(i);return this.set(s*Math.sin(t),s*Math.cos(t),r*Math.sin(e),r*Math.cos(e))}equals(t){return t._x===this._x&&t._y===this._y&&t._z===this._z&&t._w===this._w}fromArray(t,e=0){return this._x=t[e],this._y=t[e+1],this._z=t[e+2],this._w=t[e+3],this._onChangeCallback(),this}toArray(t=[],e=0){return t[e]=this._x,t[e+1]=this._y,t[e+2]=this._z,t[e+3]=this._w,t}fromBufferAttribute(t,e){return this._x=t.getX(e),this._y=t.getY(e),this._z=t.getZ(e),this._w=t.getW(e),this._onChangeCallback(),this}toJSON(){return this.toArray()}_onChange(t){return this._onChangeCallback=t,this}_onChangeCallback(){}*[Symbol.iterator](){yield this._x,yield this._y,yield this._z,yield this._w}},A=class{static{this.prototype.isVector3=!0}constructor(t=0,e=0,i=0){this.x=t,this.y=e,this.z=i}set(t,e,i){return i===void 0&&(i=this.z),this.x=t,this.y=e,this.z=i,this}setScalar(t){return this.x=t,this.y=t,this.z=t,this}setX(t){return this.x=t,this}setY(t){return this.y=t,this}setZ(t){return this.z=t,this}setComponent(t,e){switch(t){case 0:this.x=e;break;case 1:this.y=e;break;case 2:this.z=e;break;default:throw new Error("THREE.Vector3: index is out of range: "+t)}return this}getComponent(t){switch(t){case 0:return this.x;case 1:return this.y;case 2:return this.z;default:throw new Error("THREE.Vector3: index is out of range: "+t)}}clone(){return new this.constructor(this.x,this.y,this.z)}copy(t){return this.x=t.x,this.y=t.y,this.z=t.z,this}add(t){return this.x+=t.x,this.y+=t.y,this.z+=t.z,this}addScalar(t){return this.x+=t,this.y+=t,this.z+=t,this}addVectors(t,e){return this.x=t.x+e.x,this.y=t.y+e.y,this.z=t.z+e.z,this}addScaledVector(t,e){return this.x+=t.x*e,this.y+=t.y*e,this.z+=t.z*e,this}sub(t){return this.x-=t.x,this.y-=t.y,this.z-=t.z,this}subScalar(t){return this.x-=t,this.y-=t,this.z-=t,this}subVectors(t,e){return this.x=t.x-e.x,this.y=t.y-e.y,this.z=t.z-e.z,this}multiply(t){return this.x*=t.x,this.y*=t.y,this.z*=t.z,this}multiplyScalar(t){return this.x*=t,this.y*=t,this.z*=t,this}multiplyVectors(t,e){return this.x=t.x*e.x,this.y=t.y*e.y,this.z=t.z*e.z,this}applyEuler(t){return this.applyQuaternion(eu.setFromEuler(t))}applyAxisAngle(t,e){return this.applyQuaternion(eu.setFromAxisAngle(t,e))}applyMatrix3(t){let e=this.x,i=this.y,s=this.z,r=t.elements;return this.x=r[0]*e+r[3]*i+r[6]*s,this.y=r[1]*e+r[4]*i+r[7]*s,this.z=r[2]*e+r[5]*i+r[8]*s,this}applyNormalMatrix(t){return this.applyMatrix3(t).normalize()}applyMatrix4(t){let e=this.x,i=this.y,s=this.z,r=t.elements,a=1/(r[3]*e+r[7]*i+r[11]*s+r[15]);return this.x=(r[0]*e+r[4]*i+r[8]*s+r[12])*a,this.y=(r[1]*e+r[5]*i+r[9]*s+r[13])*a,this.z=(r[2]*e+r[6]*i+r[10]*s+r[14])*a,this}applyQuaternion(t){let e=this.x,i=this.y,s=this.z,r=t.x,a=t.y,o=t.z,l=t.w,c=2*(a*s-o*i),h=2*(o*e-r*s),u=2*(r*i-a*e);return this.x=e+l*c+a*u-o*h,this.y=i+l*h+o*c-r*u,this.z=s+l*u+r*h-a*c,this}project(t){return this.applyMatrix4(t.matrixWorldInverse).applyMatrix4(t.projectionMatrix)}unproject(t){return this.applyMatrix4(t.projectionMatrixInverse).applyMatrix4(t.matrixWorld)}transformDirection(t){let e=this.x,i=this.y,s=this.z,r=t.elements;return this.x=r[0]*e+r[4]*i+r[8]*s,this.y=r[1]*e+r[5]*i+r[9]*s,this.z=r[2]*e+r[6]*i+r[10]*s,this.normalize()}divide(t){return this.x/=t.x,this.y/=t.y,this.z/=t.z,this}divideScalar(t){return this.multiplyScalar(1/t)}min(t){return this.x=Math.min(this.x,t.x),this.y=Math.min(this.y,t.y),this.z=Math.min(this.z,t.z),this}max(t){return this.x=Math.max(this.x,t.x),this.y=Math.max(this.y,t.y),this.z=Math.max(this.z,t.z),this}clamp(t,e){return this.x=Bt(this.x,t.x,e.x),this.y=Bt(this.y,t.y,e.y),this.z=Bt(this.z,t.z,e.z),this}clampScalar(t,e){return this.x=Bt(this.x,t,e),this.y=Bt(this.y,t,e),this.z=Bt(this.z,t,e),this}clampLength(t,e){let i=this.length();return this.divideScalar(i||1).multiplyScalar(Bt(i,t,e))}floor(){return this.x=Math.floor(this.x),this.y=Math.floor(this.y),this.z=Math.floor(this.z),this}ceil(){return this.x=Math.ceil(this.x),this.y=Math.ceil(this.y),this.z=Math.ceil(this.z),this}round(){return this.x=Math.round(this.x),this.y=Math.round(this.y),this.z=Math.round(this.z),this}roundToZero(){return this.x=Math.trunc(this.x),this.y=Math.trunc(this.y),this.z=Math.trunc(this.z),this}negate(){return this.x=-this.x,this.y=-this.y,this.z=-this.z,this}dot(t){return this.x*t.x+this.y*t.y+this.z*t.z}lengthSq(){return this.x*this.x+this.y*this.y+this.z*this.z}length(){return Math.sqrt(this.x*this.x+this.y*this.y+this.z*this.z)}manhattanLength(){return Math.abs(this.x)+Math.abs(this.y)+Math.abs(this.z)}normalize(){return this.divideScalar(this.length()||1)}setLength(t){return this.normalize().multiplyScalar(t)}lerp(t,e){return this.x+=(t.x-this.x)*e,this.y+=(t.y-this.y)*e,this.z+=(t.z-this.z)*e,this}lerpVectors(t,e,i){return this.x=t.x+(e.x-t.x)*i,this.y=t.y+(e.y-t.y)*i,this.z=t.z+(e.z-t.z)*i,this}cross(t){return this.crossVectors(this,t)}crossVectors(t,e){let i=t.x,s=t.y,r=t.z,a=e.x,o=e.y,l=e.z;return this.x=s*l-r*o,this.y=r*a-i*l,this.z=i*o-s*a,this}projectOnVector(t){let e=t.lengthSq();if(e===0)return this.set(0,0,0);let i=t.dot(this)/e;return this.copy(t).multiplyScalar(i)}projectOnPlane(t){return Kl.copy(this).projectOnVector(t),this.sub(Kl)}reflect(t){return this.sub(Kl.copy(t).multiplyScalar(2*this.dot(t)))}angleTo(t){let e=Math.sqrt(this.lengthSq()*t.lengthSq());if(e===0)return Math.PI/2;let i=this.dot(t)/e;return Math.acos(Bt(i,-1,1))}distanceTo(t){return Math.sqrt(this.distanceToSquared(t))}distanceToSquared(t){let e=this.x-t.x,i=this.y-t.y,s=this.z-t.z;return e*e+i*i+s*s}manhattanDistanceTo(t){return Math.abs(this.x-t.x)+Math.abs(this.y-t.y)+Math.abs(this.z-t.z)}setFromSpherical(t){return this.setFromSphericalCoords(t.radius,t.phi,t.theta)}setFromSphericalCoords(t,e,i){let s=Math.sin(e)*t;return this.x=s*Math.sin(i),this.y=Math.cos(e)*t,this.z=s*Math.cos(i),this}setFromCylindrical(t){return this.setFromCylindricalCoords(t.radius,t.theta,t.y)}setFromCylindricalCoords(t,e,i){return this.x=t*Math.sin(e),this.y=i,this.z=t*Math.cos(e),this}setFromMatrixPosition(t){let e=t.elements;return this.x=e[12],this.y=e[13],this.z=e[14],this}setFromMatrixScale(t){let e=this.setFromMatrixColumn(t,0).length(),i=this.setFromMatrixColumn(t,1).length(),s=this.setFromMatrixColumn(t,2).length();return this.x=e,this.y=i,this.z=s,this}setFromMatrixColumn(t,e){return this.fromArray(t.elements,e*4)}setFromMatrix3Column(t,e){return this.fromArray(t.elements,e*3)}setFromEuler(t){return this.x=t._x,this.y=t._y,this.z=t._z,this}setFromColor(t){return this.x=t.r,this.y=t.g,this.z=t.b,this}equals(t){return t.x===this.x&&t.y===this.y&&t.z===this.z}fromArray(t,e=0){return this.x=t[e],this.y=t[e+1],this.z=t[e+2],this}toArray(t=[],e=0){return t[e]=this.x,t[e+1]=this.y,t[e+2]=this.z,t}fromBufferAttribute(t,e){return this.x=t.getX(e),this.y=t.getY(e),this.z=t.getZ(e),this}random(){return this.x=Math.random(),this.y=Math.random(),this.z=Math.random(),this}randomDirection(){let t=Math.random()*Math.PI*2,e=Math.random()*2-1,i=Math.sqrt(1-e*e);return this.x=i*Math.cos(t),this.y=e,this.z=i*Math.sin(t),this}*[Symbol.iterator](){yield this.x,yield this.y,yield this.z}},Kl=new A,eu=new Je,Nt=class{static{this.prototype.isMatrix3=!0}constructor(t,e,i,s,r,a,o,l,c){this.elements=[1,0,0,0,1,0,0,0,1],t!==void 0&&this.set(t,e,i,s,r,a,o,l,c)}set(t,e,i,s,r,a,o,l,c){let h=this.elements;return h[0]=t,h[1]=s,h[2]=o,h[3]=e,h[4]=r,h[5]=l,h[6]=i,h[7]=a,h[8]=c,this}identity(){return this.set(1,0,0,0,1,0,0,0,1),this}copy(t){let e=this.elements,i=t.elements;return e[0]=i[0],e[1]=i[1],e[2]=i[2],e[3]=i[3],e[4]=i[4],e[5]=i[5],e[6]=i[6],e[7]=i[7],e[8]=i[8],this}extractBasis(t,e,i){return t.setFromMatrix3Column(this,0),e.setFromMatrix3Column(this,1),i.setFromMatrix3Column(this,2),this}setFromMatrix4(t){let e=t.elements;return this.set(e[0],e[4],e[8],e[1],e[5],e[9],e[2],e[6],e[10]),this}multiply(t){return this.multiplyMatrices(this,t)}premultiply(t){return this.multiplyMatrices(t,this)}multiplyMatrices(t,e){let i=t.elements,s=e.elements,r=this.elements,a=i[0],o=i[3],l=i[6],c=i[1],h=i[4],u=i[7],d=i[2],f=i[5],p=i[8],v=s[0],g=s[3],m=s[6],_=s[1],S=s[4],M=s[7],T=s[2],w=s[5],x=s[8];return r[0]=a*v+o*_+l*T,r[3]=a*g+o*S+l*w,r[6]=a*m+o*M+l*x,r[1]=c*v+h*_+u*T,r[4]=c*g+h*S+u*w,r[7]=c*m+h*M+u*x,r[2]=d*v+f*_+p*T,r[5]=d*g+f*S+p*w,r[8]=d*m+f*M+p*x,this}multiplyScalar(t){let e=this.elements;return e[0]*=t,e[3]*=t,e[6]*=t,e[1]*=t,e[4]*=t,e[7]*=t,e[2]*=t,e[5]*=t,e[8]*=t,this}determinant(){let t=this.elements,e=t[0],i=t[1],s=t[2],r=t[3],a=t[4],o=t[5],l=t[6],c=t[7],h=t[8];return e*a*h-e*o*c-i*r*h+i*o*l+s*r*c-s*a*l}invert(){let t=this.elements,e=t[0],i=t[1],s=t[2],r=t[3],a=t[4],o=t[5],l=t[6],c=t[7],h=t[8],u=h*a-o*c,d=o*l-h*r,f=c*r-a*l,p=e*u+i*d+s*f;if(p===0)return this.set(0,0,0,0,0,0,0,0,0);let v=1/p;return t[0]=u*v,t[1]=(s*c-h*i)*v,t[2]=(o*i-s*a)*v,t[3]=d*v,t[4]=(h*e-s*l)*v,t[5]=(s*r-o*e)*v,t[6]=f*v,t[7]=(i*l-c*e)*v,t[8]=(a*e-i*r)*v,this}transpose(){let t,e=this.elements;return t=e[1],e[1]=e[3],e[3]=t,t=e[2],e[2]=e[6],e[6]=t,t=e[5],e[5]=e[7],e[7]=t,this}getNormalMatrix(t){return this.setFromMatrix4(t).invert().transpose()}transposeIntoArray(t){let e=this.elements;return t[0]=e[0],t[1]=e[3],t[2]=e[6],t[3]=e[1],t[4]=e[4],t[5]=e[7],t[6]=e[2],t[7]=e[5],t[8]=e[8],this}setUvTransform(t,e,i,s,r,a,o){let l=Math.cos(r),c=Math.sin(r);return this.set(i*l,i*c,-i*(l*a+c*o)+a+t,-s*c,s*l,-s*(-c*a+l*o)+o+e,0,0,1),this}scale(t,e){return Un("Matrix3: .scale() is deprecated. Use .makeScale() instead."),this.premultiply(Jl.makeScale(t,e)),this}rotate(t){return Un("Matrix3: .rotate() is deprecated. Use .makeRotation() instead."),this.premultiply(Jl.makeRotation(-t)),this}translate(t,e){return Un("Matrix3: .translate() is deprecated. Use .makeTranslation() instead."),this.premultiply(Jl.makeTranslation(t,e)),this}makeTranslation(t,e){return t.isVector2?this.set(1,0,t.x,0,1,t.y,0,0,1):this.set(1,0,t,0,1,e,0,0,1),this}makeRotation(t){let e=Math.cos(t),i=Math.sin(t);return this.set(e,-i,0,i,e,0,0,0,1),this}makeScale(t,e){return this.set(t,0,0,0,e,0,0,0,1),this}equals(t){let e=this.elements,i=t.elements;for(let s=0;s<9;s++)if(e[s]!==i[s])return!1;return!0}fromArray(t,e=0){for(let i=0;i<9;i++)this.elements[i]=t[i+e];return this}toArray(t=[],e=0){let i=this.elements;return t[e]=i[0],t[e+1]=i[1],t[e+2]=i[2],t[e+3]=i[3],t[e+4]=i[4],t[e+5]=i[5],t[e+6]=i[6],t[e+7]=i[7],t[e+8]=i[8],t}clone(){return new this.constructor().fromArray(this.elements)}},Jl=new Nt,iu=new Nt().set(.4123908,.3575843,.1804808,.212639,.7151687,.0721923,.0193308,.1191948,.9505322),nu=new Nt().set(3.2409699,-1.5373832,-.4986108,-.9692436,1.8759675,.0415551,.0556301,-.203977,1.0569715);function up(){let n={enabled:!0,workingColorSpace:pn,spaces:{},convert:function(s,r,a){return this.enabled===!1||r===a||!r||!a||(this.spaces[r].transfer===Jt&&(s.r=Qi(s.r),s.g=Qi(s.g),s.b=Qi(s.b)),this.spaces[r].primaries!==this.spaces[a].primaries&&(s.applyMatrix3(this.spaces[r].toXYZ),s.applyMatrix3(this.spaces[a].fromXYZ)),this.spaces[a].transfer===Jt&&(s.r=gs(s.r),s.g=gs(s.g),s.b=gs(s.b))),s},workingToColorSpace:function(s,r){return this.convert(s,this.workingColorSpace,r)},colorSpaceToWorking:function(s,r){return this.convert(s,r,this.workingColorSpace)},getPrimaries:function(s){return this.spaces[s].primaries},getTransfer:function(s){return s===Ui?mr:this.spaces[s].transfer},getToneMappingMode:function(s){return this.spaces[s].outputColorSpaceConfig.toneMappingMode||"standard"},getLuminanceCoefficients:function(s,r=this.workingColorSpace){return s.fromArray(this.spaces[r].luminanceCoefficients)},define:function(s){Object.assign(this.spaces,s)},_getMatrix:function(s,r,a){return s.copy(this.spaces[a].fromXYZ).multiply(this.spaces[r].toXYZ)},_getDrawingBufferColorSpace:function(s){return this.spaces[s].outputColorSpaceConfig.drawingBufferColorSpace},_getUnpackColorSpace:function(s=this.workingColorSpace){return this.spaces[s].workingColorSpaceConfig.unpackColorSpace}},t=[.64,.33,.3,.6,.15,.06],e=[.2126,.7152,.0722],i=[.3127,.329];return n.define({[pn]:{primaries:t,whitePoint:i,transfer:mr,toXYZ:iu,fromXYZ:nu,luminanceCoefficients:e,workingColorSpaceConfig:{unpackColorSpace:Fe},outputColorSpaceConfig:{drawingBufferColorSpace:Fe}},[Fe]:{primaries:t,whitePoint:i,transfer:Jt,toXYZ:iu,fromXYZ:nu,luminanceCoefficients:e,outputColorSpaceConfig:{drawingBufferColorSpace:Fe}}}),n}var Wt=up();function Qi(n){return n<.04045?n*.0773993808:Math.pow(n*.9478672986+.0521327014,2.4)}function gs(n){return n<.0031308?n*12.92:1.055*Math.pow(n,.41666)-.055}var es,no=class{static getDataURL(t,e="image/png"){if(/^data:/i.test(t.src)||typeof HTMLCanvasElement>"u")return t.src;let i;if(t instanceof HTMLCanvasElement)i=t;else{es===void 0&&(es=gr("canvas")),es.width=t.width,es.height=t.height;let s=es.getContext("2d");t instanceof ImageData?s.putImageData(t,0,0):s.drawImage(t,0,0,t.width,t.height),i=es}return i.toDataURL(e)}static sRGBToLinear(t){if(typeof HTMLImageElement<"u"&&t instanceof HTMLImageElement||typeof HTMLCanvasElement<"u"&&t instanceof HTMLCanvasElement||typeof ImageBitmap<"u"&&t instanceof ImageBitmap){let e=gr("canvas");e.width=t.width,e.height=t.height;let i=e.getContext("2d");i.drawImage(t,0,0,t.width,t.height);let s=i.getImageData(0,0,t.width,t.height),r=s.data;for(let a=0;a<r.length;a++)r[a]=Qi(r[a]/255)*255;return i.putImageData(s,0,0),e}else if(t.data){let e=t.data.slice(0);for(let i=0;i<e.length;i++)e instanceof Uint8Array||e instanceof Uint8ClampedArray?e[i]=Math.floor(Qi(e[i]/255)*255):e[i]=Qi(e[i]);return{data:e,width:t.width,height:t.height}}else return Pt("ImageUtils.sRGBToLinear(): Unsupported image type. No color space conversion applied."),t}},dp=0,Bn=class{constructor(t=null){this.isTextureSource=!0,Object.defineProperty(this,"id",{value:dp++}),this.uuid=zs(),this.data=t,this.dataReady=!0,this.version=0}getSize(t){let e=this.data;return typeof HTMLVideoElement<"u"&&e instanceof HTMLVideoElement?t.set(e.videoWidth,e.videoHeight,0):typeof VideoFrame<"u"&&e instanceof VideoFrame?t.set(e.displayWidth,e.displayHeight,0):e!==null?t.set(e.width||0,e.height||0,e.depth||0):t.set(0,0,0),t}set needsUpdate(t){t===!0&&this.version++}toJSON(t){let e=t===void 0||typeof t=="string";if(!e&&t.images[this.uuid]!==void 0)return t.images[this.uuid];let i={uuid:this.uuid,url:""},s=this.data;if(s!==null){let r;if(Array.isArray(s)){r=[];for(let a=0,o=s.length;a<o;a++)s[a].isDataTexture?r.push($l(s[a].image)):r.push($l(s[a]))}else r=$l(s);i.url=r}return e||(t.images[this.uuid]=i),i}};function $l(n){return typeof HTMLImageElement<"u"&&n instanceof HTMLImageElement||typeof HTMLCanvasElement<"u"&&n instanceof HTMLCanvasElement||typeof ImageBitmap<"u"&&n instanceof ImageBitmap?no.getDataURL(n):n.data?{data:Array.from(n.data),width:n.width,height:n.height,type:n.data.constructor.name}:(Pt("Texture: Unable to serialize Texture."),{})}var fp=0,jl=new A,qe=class n extends Ri{constructor(t=n.DEFAULT_IMAGE,e=n.DEFAULT_MAPPING,i=ki,s=ki,r=ae,a=sn,o=ci,l=ii,c=n.DEFAULT_ANISOTROPY,h=Ui){super(),this.isTexture=!0,Object.defineProperty(this,"id",{value:fp++}),this.uuid=zs(),this.name="",this.source=new Bn(t),this.mipmaps=[],this.mapping=e,this.channel=0,this.wrapS=i,this.wrapT=s,this.magFilter=r,this.minFilter=a,this.anisotropy=c,this.format=o,this.internalFormat=null,this.type=l,this.offset=new et(0,0),this.repeat=new et(1,1),this.center=new et(0,0),this.rotation=0,this.matrixAutoUpdate=!0,this.matrix=new Nt,this.generateMipmaps=!0,this.mipmapsAutoUpdate=!0,this.premultiplyAlpha=!1,this.flipY=!0,this.unpackAlignment=4,this.colorSpace=h,this.userData={},this.updateRanges=[],this.version=0,this.onUpdate=null,this.renderTarget=null,this.isRenderTargetTexture=!1,this.isArrayTexture=!!(t&&t.depth&&t.depth>1),this.pmremVersion=0,this.isPMREMTexture=!1,this.normalized=!1}get width(){return this.source.getSize(jl).x}get height(){return this.source.getSize(jl).y}get depth(){return this.source.getSize(jl).z}get image(){return this.source.data}set image(t){this.source.data=t}updateMatrix(){this.matrix.setUvTransform(this.offset.x,this.offset.y,this.repeat.x,this.repeat.y,this.rotation,this.center.x,this.center.y)}addUpdateRange(t,e){this.updateRanges.push({start:t,count:e})}clearUpdateRanges(){this.updateRanges.length=0}clone(){return new this.constructor().copy(this)}copy(t){return this.name=t.name,this.source=t.source,this.mipmaps=t.mipmaps.slice(0),this.mapping=t.mapping,this.channel=t.channel,this.wrapS=t.wrapS,this.wrapT=t.wrapT,this.magFilter=t.magFilter,this.minFilter=t.minFilter,this.anisotropy=t.anisotropy,this.format=t.format,this.internalFormat=t.internalFormat,this.type=t.type,this.normalized=t.normalized,this.offset.copy(t.offset),this.repeat.copy(t.repeat),this.center.copy(t.center),this.rotation=t.rotation,this.matrixAutoUpdate=t.matrixAutoUpdate,this.matrix.copy(t.matrix),this.generateMipmaps=t.generateMipmaps,this.mipmapsAutoUpdate=t.mipmapsAutoUpdate,this.premultiplyAlpha=t.premultiplyAlpha,this.flipY=t.flipY,this.unpackAlignment=t.unpackAlignment,this.colorSpace=t.colorSpace,this.renderTarget=t.renderTarget,this.isRenderTargetTexture=t.isRenderTargetTexture,this.isPMREMTexture=t.isPMREMTexture,this.isArrayTexture=t.isArrayTexture,this.userData=JSON.parse(JSON.stringify(t.userData)),this.needsUpdate=!0,this}setValues(t){for(let e in t){let i=t[e];if(i===void 0){Pt(`Texture.setValues(): parameter '${e}' has value of undefined.`);continue}let s=this[e];if(s===void 0){Pt(`Texture.setValues(): property '${e}' does not exist.`);continue}s&&i&&s.isVector2&&i.isVector2||s&&i&&s.isVector3&&i.isVector3||s&&i&&s.isMatrix3&&i.isMatrix3?s.copy(i):this[e]=i}}toJSON(t){let e=t===void 0||typeof t=="string";if(!e&&t.textures[this.uuid]!==void 0)return t.textures[this.uuid];let i={metadata:{version:4.7,type:"Texture",generator:"Texture.toJSON"},uuid:this.uuid,name:this.name,image:this.source.toJSON(t).uuid,mapping:this.mapping,channel:this.channel,repeat:[this.repeat.x,this.repeat.y],offset:[this.offset.x,this.offset.y],center:[this.center.x,this.center.y],rotation:this.rotation,wrap:[this.wrapS,this.wrapT],format:this.format,internalFormat:this.internalFormat,type:this.type,normalized:this.normalized,colorSpace:this.colorSpace,minFilter:this.minFilter,magFilter:this.magFilter,anisotropy:this.anisotropy,flipY:this.flipY,generateMipmaps:this.generateMipmaps,mipmapsAutoUpdate:this.mipmapsAutoUpdate,premultiplyAlpha:this.premultiplyAlpha,unpackAlignment:this.unpackAlignment};return Object.keys(this.userData).length>0&&(i.userData=this.userData),e||(t.textures[this.uuid]=i),i}dispose(){this.dispatchEvent({type:"dispose"})}transformUv(t){if(this.mapping!==Oc)return t;if(t.applyMatrix3(this.matrix),t.x<0||t.x>1)switch(this.wrapS){case Fn:t.x=t.x-Math.floor(t.x);break;case ki:t.x=t.x<0?0:1;break;case eo:Math.abs(Math.floor(t.x)%2)===1?t.x=Math.ceil(t.x)-t.x:t.x=t.x-Math.floor(t.x);break}if(t.y<0||t.y>1)switch(this.wrapT){case Fn:t.y=t.y-Math.floor(t.y);break;case ki:t.y=t.y<0?0:1;break;case eo:Math.abs(Math.floor(t.y)%2)===1?t.y=Math.ceil(t.y)-t.y:t.y=t.y-Math.floor(t.y);break}return this.flipY&&(t.y=1-t.y),t}set needsUpdate(t){t===!0&&(this.version++,this.source.needsUpdate=!0)}set needsPMREMUpdate(t){t===!0&&this.pmremVersion++}};qe.DEFAULT_IMAGE=null;qe.DEFAULT_MAPPING=Oc;qe.DEFAULT_ANISOTROPY=1;var ie=class{static{this.prototype.isVector4=!0}constructor(t=0,e=0,i=0,s=1){this.x=t,this.y=e,this.z=i,this.w=s}get width(){return this.z}set width(t){this.z=t}get height(){return this.w}set height(t){this.w=t}set(t,e,i,s){return this.x=t,this.y=e,this.z=i,this.w=s,this}setScalar(t){return this.x=t,this.y=t,this.z=t,this.w=t,this}setX(t){return this.x=t,this}setY(t){return this.y=t,this}setZ(t){return this.z=t,this}setW(t){return this.w=t,this}setComponent(t,e){switch(t){case 0:this.x=e;break;case 1:this.y=e;break;case 2:this.z=e;break;case 3:this.w=e;break;default:throw new Error("THREE.Vector4: index is out of range: "+t)}return this}getComponent(t){switch(t){case 0:return this.x;case 1:return this.y;case 2:return this.z;case 3:return this.w;default:throw new Error("THREE.Vector4: index is out of range: "+t)}}clone(){return new this.constructor(this.x,this.y,this.z,this.w)}copy(t){return this.x=t.x,this.y=t.y,this.z=t.z,this.w=t.w!==void 0?t.w:1,this}add(t){return this.x+=t.x,this.y+=t.y,this.z+=t.z,this.w+=t.w,this}addScalar(t){return this.x+=t,this.y+=t,this.z+=t,this.w+=t,this}addVectors(t,e){return this.x=t.x+e.x,this.y=t.y+e.y,this.z=t.z+e.z,this.w=t.w+e.w,this}addScaledVector(t,e){return this.x+=t.x*e,this.y+=t.y*e,this.z+=t.z*e,this.w+=t.w*e,this}sub(t){return this.x-=t.x,this.y-=t.y,this.z-=t.z,this.w-=t.w,this}subScalar(t){return this.x-=t,this.y-=t,this.z-=t,this.w-=t,this}subVectors(t,e){return this.x=t.x-e.x,this.y=t.y-e.y,this.z=t.z-e.z,this.w=t.w-e.w,this}multiply(t){return this.x*=t.x,this.y*=t.y,this.z*=t.z,this.w*=t.w,this}multiplyScalar(t){return this.x*=t,this.y*=t,this.z*=t,this.w*=t,this}applyMatrix4(t){let e=this.x,i=this.y,s=this.z,r=this.w,a=t.elements;return this.x=a[0]*e+a[4]*i+a[8]*s+a[12]*r,this.y=a[1]*e+a[5]*i+a[9]*s+a[13]*r,this.z=a[2]*e+a[6]*i+a[10]*s+a[14]*r,this.w=a[3]*e+a[7]*i+a[11]*s+a[15]*r,this}divide(t){return this.x/=t.x,this.y/=t.y,this.z/=t.z,this.w/=t.w,this}divideScalar(t){return this.multiplyScalar(1/t)}setAxisAngleFromQuaternion(t){this.w=2*Math.acos(t.w);let e=Math.sqrt(1-t.w*t.w);return e<1e-4?(this.x=1,this.y=0,this.z=0):(this.x=t.x/e,this.y=t.y/e,this.z=t.z/e),this}setAxisAngleFromRotationMatrix(t){let e,i,s,r,l=t.elements,c=l[0],h=l[4],u=l[8],d=l[1],f=l[5],p=l[9],v=l[2],g=l[6],m=l[10];if(Math.abs(h-d)<.01&&Math.abs(u-v)<.01&&Math.abs(p-g)<.01){if(Math.abs(h+d)<.1&&Math.abs(u+v)<.1&&Math.abs(p+g)<.1&&Math.abs(c+f+m-3)<.1)return this.set(1,0,0,0),this;e=Math.PI;let S=(c+1)/2,M=(f+1)/2,T=(m+1)/2,w=(h+d)/4,x=(u+v)/4,y=(p+g)/4;return S>M&&S>T?S<.01?(i=0,s=.707106781,r=.707106781):(i=Math.sqrt(S),s=w/i,r=x/i):M>T?M<.01?(i=.707106781,s=0,r=.707106781):(s=Math.sqrt(M),i=w/s,r=y/s):T<.01?(i=.707106781,s=.707106781,r=0):(r=Math.sqrt(T),i=x/r,s=y/r),this.set(i,s,r,e),this}let _=Math.sqrt((g-p)*(g-p)+(u-v)*(u-v)+(d-h)*(d-h));return Math.abs(_)<.001&&(_=1),this.x=(g-p)/_,this.y=(u-v)/_,this.z=(d-h)/_,this.w=Math.acos((c+f+m-1)/2),this}setFromMatrixPosition(t){let e=t.elements;return this.x=e[12],this.y=e[13],this.z=e[14],this.w=e[15],this}min(t){return this.x=Math.min(this.x,t.x),this.y=Math.min(this.y,t.y),this.z=Math.min(this.z,t.z),this.w=Math.min(this.w,t.w),this}max(t){return this.x=Math.max(this.x,t.x),this.y=Math.max(this.y,t.y),this.z=Math.max(this.z,t.z),this.w=Math.max(this.w,t.w),this}clamp(t,e){return this.x=Bt(this.x,t.x,e.x),this.y=Bt(this.y,t.y,e.y),this.z=Bt(this.z,t.z,e.z),this.w=Bt(this.w,t.w,e.w),this}clampScalar(t,e){return this.x=Bt(this.x,t,e),this.y=Bt(this.y,t,e),this.z=Bt(this.z,t,e),this.w=Bt(this.w,t,e),this}clampLength(t,e){let i=this.length();return this.divideScalar(i||1).multiplyScalar(Bt(i,t,e))}floor(){return this.x=Math.floor(this.x),this.y=Math.floor(this.y),this.z=Math.floor(this.z),this.w=Math.floor(this.w),this}ceil(){return this.x=Math.ceil(this.x),this.y=Math.ceil(this.y),this.z=Math.ceil(this.z),this.w=Math.ceil(this.w),this}round(){return this.x=Math.round(this.x),this.y=Math.round(this.y),this.z=Math.round(this.z),this.w=Math.round(this.w),this}roundToZero(){return this.x=Math.trunc(this.x),this.y=Math.trunc(this.y),this.z=Math.trunc(this.z),this.w=Math.trunc(this.w),this}negate(){return this.x=-this.x,this.y=-this.y,this.z=-this.z,this.w=-this.w,this}dot(t){return this.x*t.x+this.y*t.y+this.z*t.z+this.w*t.w}lengthSq(){return this.x*this.x+this.y*this.y+this.z*this.z+this.w*this.w}length(){return Math.sqrt(this.x*this.x+this.y*this.y+this.z*this.z+this.w*this.w)}manhattanLength(){return Math.abs(this.x)+Math.abs(this.y)+Math.abs(this.z)+Math.abs(this.w)}normalize(){return this.divideScalar(this.length()||1)}setLength(t){return this.normalize().multiplyScalar(t)}lerp(t,e){return this.x+=(t.x-this.x)*e,this.y+=(t.y-this.y)*e,this.z+=(t.z-this.z)*e,this.w+=(t.w-this.w)*e,this}lerpVectors(t,e,i){return this.x=t.x+(e.x-t.x)*i,this.y=t.y+(e.y-t.y)*i,this.z=t.z+(e.z-t.z)*i,this.w=t.w+(e.w-t.w)*i,this}equals(t){return t.x===this.x&&t.y===this.y&&t.z===this.z&&t.w===this.w}fromArray(t,e=0){return this.x=t[e],this.y=t[e+1],this.z=t[e+2],this.w=t[e+3],this}toArray(t=[],e=0){return t[e]=this.x,t[e+1]=this.y,t[e+2]=this.z,t[e+3]=this.w,t}fromBufferAttribute(t,e){return this.x=t.getX(e),this.y=t.getY(e),this.z=t.getZ(e),this.w=t.getW(e),this}random(){return this.x=Math.random(),this.y=Math.random(),this.z=Math.random(),this.w=Math.random(),this}*[Symbol.iterator](){yield this.x,yield this.y,yield this.z,yield this.w}},so=class extends Ri{constructor(t=1,e=1,i={}){super(),i=Object.assign({generateMipmaps:!1,internalFormat:null,minFilter:ae,depthBuffer:!0,stencilBuffer:!1,resolveColorBuffer:!0,resolveDepthBuffer:!0,resolveStencilBuffer:!0,storeMultisampledColorBuffer:!0,storeMultisampledDepthBuffer:!0,storeMultisampledStencilBuffer:!0,depthTexture:null,samples:0,count:1,depth:1,multiview:!1,useArrayDepthTexture:!1},i),this.isRenderTarget=!0,this.width=t,this.height=e,this.depth=i.depth,this.scissor=new ie(0,0,t,e),this.scissorTest=!1,this.viewport=new ie(0,0,t,e),this.textures=[];let s=i.count;for(let r=0;r<s;r++)this.textures[r]=new qe({width:t,height:e,depth:i.depth}),this.textures[r].isRenderTargetTexture=!0,this.textures[r].renderTarget=this;this._setTextureOptions(i),this.depthBuffer=i.depthBuffer,this.stencilBuffer=i.stencilBuffer,this.resolveColorBuffer=i.resolveColorBuffer,this.resolveDepthBuffer=i.resolveDepthBuffer,this.resolveStencilBuffer=i.resolveStencilBuffer,this.storeMultisampledColorBuffer=i.storeMultisampledColorBuffer,this.storeMultisampledDepthBuffer=i.storeMultisampledDepthBuffer,this.storeMultisampledStencilBuffer=i.storeMultisampledStencilBuffer,this._depthTexture=null,this.depthTexture=i.depthTexture,this.samples=i.samples,this.multiview=i.multiview,this.useArrayDepthTexture=i.useArrayDepthTexture}_setTextureOptions(t={}){let e={minFilter:ae,generateMipmaps:!1,flipY:!1,internalFormat:null};t.mapping!==void 0&&(e.mapping=t.mapping),t.wrapS!==void 0&&(e.wrapS=t.wrapS),t.wrapT!==void 0&&(e.wrapT=t.wrapT),t.wrapR!==void 0&&(e.wrapR=t.wrapR),t.magFilter!==void 0&&(e.magFilter=t.magFilter),t.minFilter!==void 0&&(e.minFilter=t.minFilter),t.format!==void 0&&(e.format=t.format),t.type!==void 0&&(e.type=t.type),t.anisotropy!==void 0&&(e.anisotropy=t.anisotropy),t.colorSpace!==void 0&&(e.colorSpace=t.colorSpace),t.flipY!==void 0&&(e.flipY=t.flipY),t.generateMipmaps!==void 0&&(e.generateMipmaps=t.generateMipmaps),t.mipmapsAutoUpdate!==void 0&&(e.mipmapsAutoUpdate=t.mipmapsAutoUpdate),t.internalFormat!==void 0&&(e.internalFormat=t.internalFormat);for(let i=0;i<this.textures.length;i++)this.textures[i].setValues(e)}get texture(){return this.textures[0]}set texture(t){this.textures[0]=t}set depthTexture(t){this._depthTexture!==null&&this._depthTexture.renderTarget===this&&(this._depthTexture.renderTarget=null),t!==null&&t.renderTarget===null&&(t.renderTarget=this),this._depthTexture=t}get depthTexture(){return this._depthTexture}setSize(t,e,i=1){if(this.width!==t||this.height!==e||this.depth!==i){this.width=t,this.height=e,this.depth=i;for(let s=0,r=this.textures.length;s<r;s++)this.textures[s].image.width=t,this.textures[s].image.height=e,this.textures[s].image.depth=i,this.textures[s].isData3DTexture!==!0&&(this.textures[s].isArrayTexture=this.textures[s].image.depth>1);this.dispose()}this.viewport.set(0,0,t,e),this.scissor.set(0,0,t,e)}clone(){return new this.constructor().copy(this)}copy(t){this.width=t.width,this.height=t.height,this.depth=t.depth,this.scissor.copy(t.scissor),this.scissorTest=t.scissorTest,this.viewport.copy(t.viewport),this.textures.length=0;for(let e=0,i=t.textures.length;e<i;e++){this.textures[e]=t.textures[e].clone(),this.textures[e].isRenderTargetTexture=!0,this.textures[e].renderTarget=this;let s=Object.assign({},t.textures[e].image);this.textures[e].source=new Bn(s)}if(this.depthBuffer=t.depthBuffer,this.stencilBuffer=t.stencilBuffer,this.resolveColorBuffer=t.resolveColorBuffer,this.resolveDepthBuffer=t.resolveDepthBuffer,this.resolveStencilBuffer=t.resolveStencilBuffer,this.storeMultisampledColorBuffer=t.storeMultisampledColorBuffer,this.storeMultisampledDepthBuffer=t.storeMultisampledDepthBuffer,this.storeMultisampledStencilBuffer=t.storeMultisampledStencilBuffer,t.depthTexture!==null)if(t.depthTexture.renderTarget===t){let e=t.depthTexture.clone();e.renderTarget=null,this.depthTexture=e}else this.depthTexture=t.depthTexture;else this.depthTexture=null;return this.samples=t.samples,this.multiview=t.multiview,this.useArrayDepthTexture=t.useArrayDepthTexture,this}dispose(){this.dispatchEvent({type:"dispose"})}},ce=class extends so{constructor(t=1,e=1,i={}){super(t,e,i),this.isWebGLRenderTarget=!0}},xr=class extends qe{constructor(t=null,e=1,i=1,s=1){super(null),this.isDataArrayTexture=!0,this.image={data:t,width:e,height:i,depth:s},this.magFilter=Se,this.minFilter=Se,this.wrapR=ki,this.generateMipmaps=!1,this.flipY=!1,this.unpackAlignment=1,this.layerUpdates=new Set}copy(t){return super.copy(t),this.wrapR=t.wrapR,this}addLayerUpdate(t){this.layerUpdates.add(t)}clearLayerUpdates(){this.layerUpdates.clear()}};var ys=class extends qe{constructor(t=null,e=1,i=1,s=1){super(null),this.isData3DTexture=!0,this.image={data:t,width:e,height:i,depth:s},this.magFilter=Se,this.minFilter=Se,this.wrapR=ki,this.generateMipmaps=!1,this.flipY=!1,this.unpackAlignment=1}copy(t){return super.copy(t),this.wrapR=t.wrapR,this}};var Gt=class n{static{this.prototype.isMatrix4=!0}constructor(t,e,i,s,r,a,o,l,c,h,u,d,f,p,v,g){this.elements=[1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1],t!==void 0&&this.set(t,e,i,s,r,a,o,l,c,h,u,d,f,p,v,g)}set(t,e,i,s,r,a,o,l,c,h,u,d,f,p,v,g){let m=this.elements;return m[0]=t,m[4]=e,m[8]=i,m[12]=s,m[1]=r,m[5]=a,m[9]=o,m[13]=l,m[2]=c,m[6]=h,m[10]=u,m[14]=d,m[3]=f,m[7]=p,m[11]=v,m[15]=g,this}identity(){return this.set(1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1),this}clone(){return new n().fromArray(this.elements)}copy(t){let e=this.elements,i=t.elements;return e[0]=i[0],e[1]=i[1],e[2]=i[2],e[3]=i[3],e[4]=i[4],e[5]=i[5],e[6]=i[6],e[7]=i[7],e[8]=i[8],e[9]=i[9],e[10]=i[10],e[11]=i[11],e[12]=i[12],e[13]=i[13],e[14]=i[14],e[15]=i[15],this}copyPosition(t){let e=this.elements,i=t.elements;return e[12]=i[12],e[13]=i[13],e[14]=i[14],this}setFromMatrix3(t){let e=t.elements;return this.set(e[0],e[3],e[6],0,e[1],e[4],e[7],0,e[2],e[5],e[8],0,0,0,0,1),this}extractBasis(t,e,i){return this.determinantAffine()===0?(t.set(1,0,0),e.set(0,1,0),i.set(0,0,1),this):(t.setFromMatrixColumn(this,0),e.setFromMatrixColumn(this,1),i.setFromMatrixColumn(this,2),this)}makeBasis(t,e,i){return this.set(t.x,e.x,i.x,0,t.y,e.y,i.y,0,t.z,e.z,i.z,0,0,0,0,1),this}extractRotation(t){if(t.determinantAffine()===0)return this.identity();let e=this.elements,i=t.elements,s=1/is.setFromMatrixColumn(t,0).length(),r=1/is.setFromMatrixColumn(t,1).length(),a=1/is.setFromMatrixColumn(t,2).length();return e[0]=i[0]*s,e[1]=i[1]*s,e[2]=i[2]*s,e[3]=0,e[4]=i[4]*r,e[5]=i[5]*r,e[6]=i[6]*r,e[7]=0,e[8]=i[8]*a,e[9]=i[9]*a,e[10]=i[10]*a,e[11]=0,e[12]=0,e[13]=0,e[14]=0,e[15]=1,this}makeRotationFromEuler(t){let e=this.elements,i=t.x,s=t.y,r=t.z,a=Math.cos(i),o=Math.sin(i),l=Math.cos(s),c=Math.sin(s),h=Math.cos(r),u=Math.sin(r);if(t.order==="XYZ"){let d=a*h,f=a*u,p=o*h,v=o*u;e[0]=l*h,e[4]=-l*u,e[8]=c,e[1]=f+p*c,e[5]=d-v*c,e[9]=-o*l,e[2]=v-d*c,e[6]=p+f*c,e[10]=a*l}else if(t.order==="YXZ"){let d=l*h,f=l*u,p=c*h,v=c*u;e[0]=d+v*o,e[4]=p*o-f,e[8]=a*c,e[1]=a*u,e[5]=a*h,e[9]=-o,e[2]=f*o-p,e[6]=v+d*o,e[10]=a*l}else if(t.order==="ZXY"){let d=l*h,f=l*u,p=c*h,v=c*u;e[0]=d-v*o,e[4]=-a*u,e[8]=p+f*o,e[1]=f+p*o,e[5]=a*h,e[9]=v-d*o,e[2]=-a*c,e[6]=o,e[10]=a*l}else if(t.order==="ZYX"){let d=a*h,f=a*u,p=o*h,v=o*u;e[0]=l*h,e[4]=p*c-f,e[8]=d*c+v,e[1]=l*u,e[5]=v*c+d,e[9]=f*c-p,e[2]=-c,e[6]=o*l,e[10]=a*l}else if(t.order==="YZX"){let d=a*l,f=a*c,p=o*l,v=o*c;e[0]=l*h,e[4]=v-d*u,e[8]=p*u+f,e[1]=u,e[5]=a*h,e[9]=-o*h,e[2]=-c*h,e[6]=f*u+p,e[10]=d-v*u}else if(t.order==="XZY"){let d=a*l,f=a*c,p=o*l,v=o*c;e[0]=l*h,e[4]=-u,e[8]=c*h,e[1]=d*u+v,e[5]=a*h,e[9]=f*u-p,e[2]=p*u-f,e[6]=o*h,e[10]=v*u+d}return e[3]=0,e[7]=0,e[11]=0,e[12]=0,e[13]=0,e[14]=0,e[15]=1,this}makeRotationFromQuaternion(t){return this.compose(pp,t,mp)}lookAt(t,e,i){let s=this.elements;return si.subVectors(t,e),si.lengthSq()===0&&(si.z=1),si.normalize(),cn.crossVectors(i,si),cn.lengthSq()===0&&(Math.abs(i.z)===1?si.x+=1e-4:si.z+=1e-4,si.normalize(),cn.crossVectors(i,si)),cn.normalize(),_a.crossVectors(si,cn),s[0]=cn.x,s[4]=_a.x,s[8]=si.x,s[1]=cn.y,s[5]=_a.y,s[9]=si.y,s[2]=cn.z,s[6]=_a.z,s[10]=si.z,this}multiply(t){return this.multiplyMatrices(this,t)}premultiply(t){return this.multiplyMatrices(t,this)}multiplyMatrices(t,e){let i=t.elements,s=e.elements,r=this.elements,a=i[0],o=i[4],l=i[8],c=i[12],h=i[1],u=i[5],d=i[9],f=i[13],p=i[2],v=i[6],g=i[10],m=i[14],_=i[3],S=i[7],M=i[11],T=i[15],w=s[0],x=s[4],y=s[8],P=s[12],C=s[1],I=s[5],L=s[9],F=s[13],D=s[2],B=s[6],G=s[10],k=s[14],j=s[3],W=s[7],K=s[11],Q=s[15];return r[0]=a*w+o*C+l*D+c*j,r[4]=a*x+o*I+l*B+c*W,r[8]=a*y+o*L+l*G+c*K,r[12]=a*P+o*F+l*k+c*Q,r[1]=h*w+u*C+d*D+f*j,r[5]=h*x+u*I+d*B+f*W,r[9]=h*y+u*L+d*G+f*K,r[13]=h*P+u*F+d*k+f*Q,r[2]=p*w+v*C+g*D+m*j,r[6]=p*x+v*I+g*B+m*W,r[10]=p*y+v*L+g*G+m*K,r[14]=p*P+v*F+g*k+m*Q,r[3]=_*w+S*C+M*D+T*j,r[7]=_*x+S*I+M*B+T*W,r[11]=_*y+S*L+M*G+T*K,r[15]=_*P+S*F+M*k+T*Q,this}multiplyScalar(t){let e=this.elements;return e[0]*=t,e[4]*=t,e[8]*=t,e[12]*=t,e[1]*=t,e[5]*=t,e[9]*=t,e[13]*=t,e[2]*=t,e[6]*=t,e[10]*=t,e[14]*=t,e[3]*=t,e[7]*=t,e[11]*=t,e[15]*=t,this}determinant(){let t=this.elements,e=t[0],i=t[4],s=t[8],r=t[12],a=t[1],o=t[5],l=t[9],c=t[13],h=t[2],u=t[6],d=t[10],f=t[14],p=t[3],v=t[7],g=t[11],m=t[15],_=l*f-c*d,S=o*f-c*u,M=o*d-l*u,T=a*f-c*h,w=a*d-l*h,x=a*u-o*h;return e*(v*_-g*S+m*M)-i*(p*_-g*T+m*w)+s*(p*S-v*T+m*x)-r*(p*M-v*w+g*x)}determinantAffine(){let t=this.elements,e=t[0],i=t[4],s=t[8],r=t[1],a=t[5],o=t[9],l=t[2],c=t[6],h=t[10];return e*(a*h-o*c)-i*(r*h-o*l)+s*(r*c-a*l)}transpose(){let t=this.elements,e;return e=t[1],t[1]=t[4],t[4]=e,e=t[2],t[2]=t[8],t[8]=e,e=t[6],t[6]=t[9],t[9]=e,e=t[3],t[3]=t[12],t[12]=e,e=t[7],t[7]=t[13],t[13]=e,e=t[11],t[11]=t[14],t[14]=e,this}setPosition(t,e,i){let s=this.elements;return t.isVector3?(s[12]=t.x,s[13]=t.y,s[14]=t.z):(s[12]=t,s[13]=e,s[14]=i),this}invert(){let t=this.elements,e=t[0],i=t[1],s=t[2],r=t[3],a=t[4],o=t[5],l=t[6],c=t[7],h=t[8],u=t[9],d=t[10],f=t[11],p=t[12],v=t[13],g=t[14],m=t[15],_=e*o-i*a,S=e*l-s*a,M=e*c-r*a,T=i*l-s*o,w=i*c-r*o,x=s*c-r*l,y=h*v-u*p,P=h*g-d*p,C=h*m-f*p,I=u*g-d*v,L=u*m-f*v,F=d*m-f*g,D=_*F-S*L+M*I+T*C-w*P+x*y;if(D===0)return this.set(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0);let B=1/D;return t[0]=(o*F-l*L+c*I)*B,t[1]=(s*L-i*F-r*I)*B,t[2]=(v*x-g*w+m*T)*B,t[3]=(d*w-u*x-f*T)*B,t[4]=(l*C-a*F-c*P)*B,t[5]=(e*F-s*C+r*P)*B,t[6]=(g*M-p*x-m*S)*B,t[7]=(h*x-d*M+f*S)*B,t[8]=(a*L-o*C+c*y)*B,t[9]=(i*C-e*L-r*y)*B,t[10]=(p*w-v*M+m*_)*B,t[11]=(u*M-h*w-f*_)*B,t[12]=(o*P-a*I-l*y)*B,t[13]=(e*I-i*P+s*y)*B,t[14]=(v*S-p*T-g*_)*B,t[15]=(h*T-u*S+d*_)*B,this}scale(t){let e=this.elements,i=t.x,s=t.y,r=t.z;return e[0]*=i,e[4]*=s,e[8]*=r,e[1]*=i,e[5]*=s,e[9]*=r,e[2]*=i,e[6]*=s,e[10]*=r,e[3]*=i,e[7]*=s,e[11]*=r,this}getMaxScaleOnAxis(){let t=this.elements,e=t[0]*t[0]+t[1]*t[1]+t[2]*t[2],i=t[4]*t[4]+t[5]*t[5]+t[6]*t[6],s=t[8]*t[8]+t[9]*t[9]+t[10]*t[10];return Math.sqrt(Math.max(e,i,s))}makeTranslation(t,e,i){return t.isVector3?this.set(1,0,0,t.x,0,1,0,t.y,0,0,1,t.z,0,0,0,1):this.set(1,0,0,t,0,1,0,e,0,0,1,i,0,0,0,1),this}makeRotationX(t){let e=Math.cos(t),i=Math.sin(t);return this.set(1,0,0,0,0,e,-i,0,0,i,e,0,0,0,0,1),this}makeRotationY(t){let e=Math.cos(t),i=Math.sin(t);return this.set(e,0,i,0,0,1,0,0,-i,0,e,0,0,0,0,1),this}makeRotationZ(t){let e=Math.cos(t),i=Math.sin(t);return this.set(e,-i,0,0,i,e,0,0,0,0,1,0,0,0,0,1),this}makeRotationAxis(t,e){let i=Math.cos(e),s=Math.sin(e),r=1-i,a=t.x,o=t.y,l=t.z,c=r*a,h=r*o;return this.set(c*a+i,c*o-s*l,c*l+s*o,0,c*o+s*l,h*o+i,h*l-s*a,0,c*l-s*o,h*l+s*a,r*l*l+i,0,0,0,0,1),this}makeScale(t,e,i){return this.set(t,0,0,0,0,e,0,0,0,0,i,0,0,0,0,1),this}makeShear(t,e,i,s,r,a){return this.set(1,i,r,0,t,1,a,0,e,s,1,0,0,0,0,1),this}compose(t,e,i){let s=this.elements,r=e._x,a=e._y,o=e._z,l=e._w,c=r+r,h=a+a,u=o+o,d=r*c,f=r*h,p=r*u,v=a*h,g=a*u,m=o*u,_=l*c,S=l*h,M=l*u,T=i.x,w=i.y,x=i.z;return s[0]=(1-(v+m))*T,s[1]=(f+M)*T,s[2]=(p-S)*T,s[3]=0,s[4]=(f-M)*w,s[5]=(1-(d+m))*w,s[6]=(g+_)*w,s[7]=0,s[8]=(p+S)*x,s[9]=(g-_)*x,s[10]=(1-(d+v))*x,s[11]=0,s[12]=t.x,s[13]=t.y,s[14]=t.z,s[15]=1,this}decompose(t,e,i){let s=this.elements;t.x=s[12],t.y=s[13],t.z=s[14];let r=this.determinantAffine();if(r===0)return i.set(1,1,1),e.identity(),this;let a=is.set(s[0],s[1],s[2]).length(),o=is.set(s[4],s[5],s[6]).length(),l=is.set(s[8],s[9],s[10]).length();r<0&&(a=-a),wi.copy(this);let c=1/a,h=1/o,u=1/l;return wi.elements[0]*=c,wi.elements[1]*=c,wi.elements[2]*=c,wi.elements[4]*=h,wi.elements[5]*=h,wi.elements[6]*=h,wi.elements[8]*=u,wi.elements[9]*=u,wi.elements[10]*=u,e.setFromRotationMatrix(wi),i.x=a,i.y=o,i.z=l,this}makePerspective(t,e,i,s,r,a,o=mi,l=!1){let c=this.elements,h=2*r/(e-t),u=2*r/(i-s),d=(e+t)/(e-t),f=(i+s)/(i-s),p,v;if(l)p=r/(a-r),v=a*r/(a-r);else if(o===mi)p=-(a+r)/(a-r),v=-2*a*r/(a-r);else if(o===_s)p=-a/(a-r),v=-a*r/(a-r);else throw new Error("THREE.Matrix4.makePerspective(): Invalid coordinate system: "+o);return c[0]=h,c[4]=0,c[8]=d,c[12]=0,c[1]=0,c[5]=u,c[9]=f,c[13]=0,c[2]=0,c[6]=0,c[10]=p,c[14]=v,c[3]=0,c[7]=0,c[11]=-1,c[15]=0,this}makeOrthographic(t,e,i,s,r,a,o=mi,l=!1){let c=this.elements,h=2/(e-t),u=2/(i-s),d=-(e+t)/(e-t),f=-(i+s)/(i-s),p,v;if(l)p=1/(a-r),v=a/(a-r);else if(o===mi)p=-2/(a-r),v=-(a+r)/(a-r);else if(o===_s)p=-1/(a-r),v=-r/(a-r);else throw new Error("THREE.Matrix4.makeOrthographic(): Invalid coordinate system: "+o);return c[0]=h,c[4]=0,c[8]=0,c[12]=d,c[1]=0,c[5]=u,c[9]=0,c[13]=f,c[2]=0,c[6]=0,c[10]=p,c[14]=v,c[3]=0,c[7]=0,c[11]=0,c[15]=1,this}equals(t){let e=this.elements,i=t.elements;for(let s=0;s<16;s++)if(e[s]!==i[s])return!1;return!0}fromArray(t,e=0){for(let i=0;i<16;i++)this.elements[i]=t[i+e];return this}toArray(t=[],e=0){let i=this.elements;return t[e]=i[0],t[e+1]=i[1],t[e+2]=i[2],t[e+3]=i[3],t[e+4]=i[4],t[e+5]=i[5],t[e+6]=i[6],t[e+7]=i[7],t[e+8]=i[8],t[e+9]=i[9],t[e+10]=i[10],t[e+11]=i[11],t[e+12]=i[12],t[e+13]=i[13],t[e+14]=i[14],t[e+15]=i[15],t}},is=new A,wi=new Gt,pp=new A(0,0,0),mp=new A(1,1,1),cn=new A,_a=new A,si=new A,su=new Gt,ru=new Je,gi=class n{constructor(t=0,e=0,i=0,s=n.DEFAULT_ORDER){this.isEuler=!0,this._x=t,this._y=e,this._z=i,this._order=s}get x(){return this._x}set x(t){this._x=t,this._onChangeCallback()}get y(){return this._y}set y(t){this._y=t,this._onChangeCallback()}get z(){return this._z}set z(t){this._z=t,this._onChangeCallback()}get order(){return this._order}set order(t){this._order=t,this._onChangeCallback()}set(t,e,i,s=this._order){return this._x=t,this._y=e,this._z=i,this._order=s,this._onChangeCallback(),this}clone(){return new this.constructor(this._x,this._y,this._z,this._order)}copy(t){return this._x=t._x,this._y=t._y,this._z=t._z,this._order=t._order,this._onChangeCallback(),this}setFromRotationMatrix(t,e=this._order,i=!0){let s=t.elements,r=s[0],a=s[4],o=s[8],l=s[1],c=s[5],h=s[9],u=s[2],d=s[6],f=s[10];switch(e){case"XYZ":this._y=Math.asin(Bt(o,-1,1)),Math.abs(o)<.9999999?(this._x=Math.atan2(-h,f),this._z=Math.atan2(-a,r)):(this._x=Math.atan2(d,c),this._z=0);break;case"YXZ":this._x=Math.asin(-Bt(h,-1,1)),Math.abs(h)<.9999999?(this._y=Math.atan2(o,f),this._z=Math.atan2(l,c)):(this._y=Math.atan2(-u,r),this._z=0);break;case"ZXY":this._x=Math.asin(Bt(d,-1,1)),Math.abs(d)<.9999999?(this._y=Math.atan2(-u,f),this._z=Math.atan2(-a,c)):(this._y=0,this._z=Math.atan2(l,r));break;case"ZYX":this._y=Math.asin(-Bt(u,-1,1)),Math.abs(u)<.9999999?(this._x=Math.atan2(d,f),this._z=Math.atan2(l,r)):(this._x=0,this._z=Math.atan2(-a,c));break;case"YZX":this._z=Math.asin(Bt(l,-1,1)),Math.abs(l)<.9999999?(this._x=Math.atan2(-h,c),this._y=Math.atan2(-u,r)):(this._x=0,this._y=Math.atan2(o,f));break;case"XZY":this._z=Math.asin(-Bt(a,-1,1)),Math.abs(a)<.9999999?(this._x=Math.atan2(d,c),this._y=Math.atan2(o,r)):(this._x=Math.atan2(-h,f),this._y=0);break;default:Pt("Euler: .setFromRotationMatrix() encountered an unknown order: "+e)}return this._order=e,i===!0&&this._onChangeCallback(),this}setFromQuaternion(t,e,i){return su.makeRotationFromQuaternion(t),this.setFromRotationMatrix(su,e,i)}setFromVector3(t,e=this._order){return this.set(t.x,t.y,t.z,e)}reorder(t){return ru.setFromEuler(this),this.setFromQuaternion(ru,t)}equals(t){return t._x===this._x&&t._y===this._y&&t._z===this._z&&t._order===this._order}fromArray(t){return this._x=t[0],this._y=t[1],this._z=t[2],t[3]!==void 0&&(this._order=t[3]),this._onChangeCallback(),this}toArray(t=[],e=0){return t[e]=this._x,t[e+1]=this._y,t[e+2]=this._z,t[e+3]=this._order,t}_onChange(t){return this._onChangeCallback=t,this}_onChangeCallback(){}*[Symbol.iterator](){yield this._x,yield this._y,yield this._z,yield this._order}};gi.DEFAULT_ORDER="XYZ";var Ms=class{constructor(){this.mask=1}set(t){this.mask=(1<<t|0)>>>0}enable(t){this.mask|=1<<t|0}enableAll(){this.mask=-1}toggle(t){this.mask^=1<<t|0}disable(t){this.mask&=~(1<<t|0)}disableAll(){this.mask=0}test(t){return(this.mask&t.mask)!==0}isEnabled(t){return(this.mask&(1<<t|0))!==0}},gp=0,au=new A,ns=new Je,qi=new Gt,va=new A,sr=new A,xp=new A,_p=new Je,ou=new A(1,0,0),lu=new A(0,1,0),cu=new A(0,0,1),hu={type:"added"},vp={type:"removed"},ss={type:"childadded",child:null},Ql={type:"childremoved",child:null},de=class n extends Ri{constructor(){super(),this.isObject3D=!0,Object.defineProperty(this,"id",{value:gp++}),this.uuid=zs(),this.name="",this.type="Object3D",this.parent=null,this.children=[],this.up=n.DEFAULT_UP.clone();let t=new A,e=new gi,i=new Je,s=new A(1,1,1);function r(){i.setFromEuler(e,!1)}function a(){e.setFromQuaternion(i,void 0,!1)}e._onChange(r),i._onChange(a),Object.defineProperties(this,{position:{configurable:!0,enumerable:!0,value:t},rotation:{configurable:!0,enumerable:!0,value:e},quaternion:{configurable:!0,enumerable:!0,value:i},scale:{configurable:!0,enumerable:!0,value:s},modelViewMatrix:{value:new Gt},normalMatrix:{value:new Nt}}),this.matrix=new Gt,this.matrixWorld=new Gt,this.matrixAutoUpdate=n.DEFAULT_MATRIX_AUTO_UPDATE,this.matrixWorldAutoUpdate=n.DEFAULT_MATRIX_WORLD_AUTO_UPDATE,this.matrixWorldNeedsUpdate=!1,this.layers=new Ms,this.visible=!0,this.castShadow=!1,this.receiveShadow=!1,this.frustumCulled=!0,this.renderOrder=0,this.animations=[],this.customDepthMaterial=void 0,this.customDistanceMaterial=void 0,this.static=!1,this.userData={},this.pivot=null}onBeforeShadow(){}onAfterShadow(){}onBeforeRender(){}onAfterRender(){}applyMatrix4(t){this.matrixAutoUpdate&&this.updateMatrix(),this.matrix.premultiply(t),this.matrix.decompose(this.position,this.quaternion,this.scale),this.pivot!==null&&this.position.copy(this.pivot).applyMatrix4(this.matrix).sub(this.pivot)}applyQuaternion(t){return this.quaternion.premultiply(t),this}setRotationFromAxisAngle(t,e){this.quaternion.setFromAxisAngle(t,e)}setRotationFromEuler(t){this.quaternion.setFromEuler(t,!0)}setRotationFromMatrix(t){this.quaternion.setFromRotationMatrix(t)}setRotationFromQuaternion(t){this.quaternion.copy(t)}rotateOnAxis(t,e){return ns.setFromAxisAngle(t,e),this.quaternion.multiply(ns),this}rotateOnWorldAxis(t,e){return ns.setFromAxisAngle(t,e),this.quaternion.premultiply(ns),this}rotateX(t){return this.rotateOnAxis(ou,t)}rotateY(t){return this.rotateOnAxis(lu,t)}rotateZ(t){return this.rotateOnAxis(cu,t)}translateOnAxis(t,e){return au.copy(t).applyQuaternion(this.quaternion),this.position.add(au.multiplyScalar(e)),this}translateX(t){return this.translateOnAxis(ou,t)}translateY(t){return this.translateOnAxis(lu,t)}translateZ(t){return this.translateOnAxis(cu,t)}localToWorld(t){return this.updateWorldMatrix(!0,!1),t.applyMatrix4(this.matrixWorld)}worldToLocal(t){return this.updateWorldMatrix(!0,!1),t.applyMatrix4(qi.copy(this.matrixWorld).invert())}lookAt(t,e,i){t.isVector3?va.copy(t):va.set(t,e,i);let s=this.parent;this.updateWorldMatrix(!0,!1),sr.setFromMatrixPosition(this.matrixWorld),this.isCamera||this.isLight?qi.lookAt(sr,va,this.up):qi.lookAt(va,sr,this.up),this.quaternion.setFromRotationMatrix(qi),s&&(qi.extractRotation(s.matrixWorld),ns.setFromRotationMatrix(qi),this.quaternion.premultiply(ns.invert()))}add(t){if(arguments.length>1){for(let e=0;e<arguments.length;e++)this.add(arguments[e]);return this}return t===this?(Lt("Object3D.add: object can't be added as a child of itself.",t),this):(t&&t.isObject3D?(t.removeFromParent(),t.parent=this,this.children.push(t),t.dispatchEvent(hu),ss.child=t,this.dispatchEvent(ss),ss.child=null):Lt("Object3D.add: object not an instance of THREE.Object3D.",t),this)}remove(t){if(arguments.length>1){for(let i=0;i<arguments.length;i++)this.remove(arguments[i]);return this}let e=this.children.indexOf(t);return e!==-1&&(t.parent=null,this.children.splice(e,1),t.dispatchEvent(vp),Ql.child=t,this.dispatchEvent(Ql),Ql.child=null),this}removeFromParent(){let t=this.parent;return t!==null&&t.remove(this),this}clear(){return this.remove(...this.children)}attach(t){return this.updateWorldMatrix(!0,!1),qi.copy(this.matrixWorld).invert(),t.parent!==null&&(t.parent.updateWorldMatrix(!0,!1),qi.multiply(t.parent.matrixWorld)),t.applyMatrix4(qi),t.removeFromParent(),t.parent=this,this.children.push(t),t.updateWorldMatrix(!1,!0),t.dispatchEvent(hu),ss.child=t,this.dispatchEvent(ss),ss.child=null,this}getObjectById(t){return this.getObjectByProperty("id",t)}getObjectByName(t){return this.getObjectByProperty("name",t)}getObjectByProperty(t,e){if(this[t]===e)return this;for(let i=0,s=this.children.length;i<s;i++){let a=this.children[i].getObjectByProperty(t,e);if(a!==void 0)return a}}getObjectsByProperty(t,e,i=[]){this[t]===e&&i.push(this);let s=this.children;for(let r=0,a=s.length;r<a;r++)s[r].getObjectsByProperty(t,e,i);return i}getWorldPosition(t){return this.updateWorldMatrix(!0,!1),t.setFromMatrixPosition(this.matrixWorld)}getWorldQuaternion(t){return this.updateWorldMatrix(!0,!1),this.matrixWorld.decompose(sr,t,xp),t}getWorldScale(t){return this.updateWorldMatrix(!0,!1),this.matrixWorld.decompose(sr,_p,t),t}getWorldDirection(t){this.updateWorldMatrix(!0,!1);let e=this.matrixWorld.elements;return t.set(e[8],e[9],e[10]).normalize()}raycast(){}intersectsFrustum(){return!0}traverse(t){t(this);let e=this.children;for(let i=0,s=e.length;i<s;i++)e[i].traverse(t)}traverseVisible(t){if(this.visible===!1)return;t(this);let e=this.children;for(let i=0,s=e.length;i<s;i++)e[i].traverseVisible(t)}traverseAncestors(t){let e=this.parent;e!==null&&(t(e),e.traverseAncestors(t))}updateMatrix(){this.matrix.compose(this.position,this.quaternion,this.scale);let t=this.pivot;if(t!==null){let e=t.x,i=t.y,s=t.z,r=this.matrix.elements;r[12]+=e-r[0]*e-r[4]*i-r[8]*s,r[13]+=i-r[1]*e-r[5]*i-r[9]*s,r[14]+=s-r[2]*e-r[6]*i-r[10]*s}this.matrixWorldNeedsUpdate=!0}updateMatrixWorld(t){this.matrixAutoUpdate&&this.updateMatrix(),(this.matrixWorldNeedsUpdate||t)&&(this.matrixWorldAutoUpdate===!0&&(this.parent===null?this.matrixWorld.copy(this.matrix):this.matrixWorld.multiplyMatrices(this.parent.matrixWorld,this.matrix)),this.matrixWorldNeedsUpdate=!1,t=!0);let e=this.children;for(let i=0,s=e.length;i<s;i++)e[i].updateMatrixWorld(t)}updateWorldMatrix(t,e,i=!1){let s=this.parent;if(t===!0&&s!==null&&s.updateWorldMatrix(!0,!1),this.matrixAutoUpdate&&this.updateMatrix(),(this.matrixWorldNeedsUpdate||i)&&(this.matrixWorldAutoUpdate===!0&&(this.parent===null?this.matrixWorld.copy(this.matrix):this.matrixWorld.multiplyMatrices(this.parent.matrixWorld,this.matrix)),this.matrixWorldNeedsUpdate=!1,i=!0),e===!0){let r=this.children;for(let a=0,o=r.length;a<o;a++)r[a].updateWorldMatrix(!1,!0,i)}}toJSON(t){let e=t===void 0||typeof t=="string",i={};e&&(t={geometries:{},materials:{},textures:{},images:{},shapes:{},skeletons:{},animations:{},nodes:{}},i.metadata={version:4.7,type:"Object",generator:"Object3D.toJSON"});let s={};s.uuid=this.uuid,s.type=this.type,s.name=this.name,s.castShadow=this.castShadow,s.receiveShadow=this.receiveShadow,s.visible=this.visible,s.frustumCulled=this.frustumCulled,s.renderOrder=this.renderOrder,s.static=this.static,s.matrixAutoUpdate=this.matrixAutoUpdate,Object.keys(this.userData).length>0&&(s.userData=this.userData),s.layers=this.layers.mask,s.matrix=this.matrix.toArray(),s.up=this.up.toArray(),this.pivot!==null&&(s.pivot=this.pivot.toArray()),this.morphTargetDictionary!==void 0&&(s.morphTargetDictionary=Object.assign({},this.morphTargetDictionary)),this.morphTargetInfluences!==void 0&&(s.morphTargetInfluences=this.morphTargetInfluences.slice()),this.isInstancedMesh&&(s.type="InstancedMesh",s.count=this.count,s.instanceMatrix=this.instanceMatrix.toJSON(),this.instanceColor!==null&&(s.instanceColor=this.instanceColor.toJSON())),this.isBatchedMesh&&(s.type="BatchedMesh",s.perObjectFrustumCulled=this.perObjectFrustumCulled,s.sortObjects=this.sortObjects,s.drawRanges=this._drawRanges,s.reservedRanges=this._reservedRanges,s.geometryInfo=this._geometryInfo.map(o=>({...o,boundingBox:o.boundingBox?o.boundingBox.toJSON():void 0,boundingSphere:o.boundingSphere?o.boundingSphere.toJSON():void 0})),s.instanceInfo=this._instanceInfo.map(o=>({...o})),s.availableInstanceIds=this._availableInstanceIds.slice(),s.availableGeometryIds=this._availableGeometryIds.slice(),s.nextIndexStart=this._nextIndexStart,s.nextVertexStart=this._nextVertexStart,s.geometryCount=this._geometryCount,s.maxInstanceCount=this._maxInstanceCount,s.maxVertexCount=this._maxVertexCount,s.maxIndexCount=this._maxIndexCount,s.geometryInitialized=this._geometryInitialized,s.matricesTexture=this._matricesTexture.toJSON(t),s.indirectTexture=this._indirectTexture.toJSON(t),this._colorsTexture!==null&&(s.colorsTexture=this._colorsTexture.toJSON(t)),this.boundingSphere!==null&&(s.boundingSphere=this.boundingSphere.toJSON()),this.boundingBox!==null&&(s.boundingBox=this.boundingBox.toJSON()));function r(o,l){return o[l.uuid]===void 0&&(o[l.uuid]=l.toJSON(t)),l.uuid}if(this.isScene)this.background&&(this.background.isColor?s.background=this.background.toJSON():this.background.isTexture&&(s.background=this.background.toJSON(t).uuid)),this.environment&&this.environment.isTexture&&this.environment.isRenderTargetTexture!==!0&&(s.environment=this.environment.toJSON(t).uuid);else if(this.isMesh||this.isLine||this.isPoints){s.geometry=r(t.geometries,this.geometry);let o=this.geometry.parameters;if(o!==void 0&&o.shapes!==void 0){let l=o.shapes;if(Array.isArray(l))for(let c=0,h=l.length;c<h;c++){let u=l[c];r(t.shapes,u)}else r(t.shapes,l)}}if(this.isSkinnedMesh&&(s.bindMode=this.bindMode,s.bindMatrix=this.bindMatrix.toArray(),this.skeleton!==void 0&&(r(t.skeletons,this.skeleton),s.skeleton=this.skeleton.uuid)),this.material!==void 0)if(Array.isArray(this.material)){let o=[];for(let l=0,c=this.material.length;l<c;l++)o.push(r(t.materials,this.material[l]));s.material=o}else s.material=r(t.materials,this.material);if(this.children.length>0){s.children=[];for(let o=0;o<this.children.length;o++)s.children.push(this.children[o].toJSON(t).object)}if(this.animations.length>0){s.animations=[];for(let o=0;o<this.animations.length;o++){let l=this.animations[o];s.animations.push(r(t.animations,l))}}if(e){let o=a(t.geometries),l=a(t.materials),c=a(t.textures),h=a(t.images),u=a(t.shapes),d=a(t.skeletons),f=a(t.animations),p=a(t.nodes);o.length>0&&(i.geometries=o),l.length>0&&(i.materials=l),c.length>0&&(i.textures=c),h.length>0&&(i.images=h),u.length>0&&(i.shapes=u),d.length>0&&(i.skeletons=d),f.length>0&&(i.animations=f),p.length>0&&(i.nodes=p)}return i.object=s,i;function a(o){let l=[];for(let c in o){let h=o[c];delete h.metadata,l.push(h)}return l}}clone(t){return new this.constructor().copy(this,t)}copy(t,e=!0){if(this.name=t.name,this.up.copy(t.up),this.position.copy(t.position),this.rotation.order=t.rotation.order,this.quaternion.copy(t.quaternion),this.scale.copy(t.scale),this.pivot=t.pivot!==null?t.pivot.clone():null,this.matrix.copy(t.matrix),this.matrixWorld.copy(t.matrixWorld),this.matrixAutoUpdate=t.matrixAutoUpdate,this.matrixWorldAutoUpdate=t.matrixWorldAutoUpdate,this.matrixWorldNeedsUpdate=t.matrixWorldNeedsUpdate,this.layers.mask=t.layers.mask,this.visible=t.visible,this.castShadow=t.castShadow,this.receiveShadow=t.receiveShadow,this.frustumCulled=t.frustumCulled,this.renderOrder=t.renderOrder,this.static=t.static,this.animations=t.animations.slice(),this.userData=JSON.parse(JSON.stringify(t.userData)),e===!0)for(let i=0;i<t.children.length;i++){let s=t.children[i];this.add(s.clone())}return this}dispose(){this.dispatchEvent({type:"dispose"})}};de.DEFAULT_UP=new A(0,1,0);de.DEFAULT_MATRIX_AUTO_UPDATE=!0;de.DEFAULT_MATRIX_WORLD_AUTO_UPDATE=!0;var xe=class extends de{constructor(){super(),this.isGroup=!0,this.type="Group"}},yp={type:"move"},Ss=class{constructor(){this._targetRay=null,this._grip=null,this._hand=null}getHandSpace(){return this._hand===null&&(this._hand=new xe,this._hand.matrixAutoUpdate=!1,this._hand.visible=!1,this._hand.joints={},this._hand.inputState={pinching:!1}),this._hand}getTargetRaySpace(){return this._targetRay===null&&(this._targetRay=new xe,this._targetRay.matrixAutoUpdate=!1,this._targetRay.visible=!1,this._targetRay.hasLinearVelocity=!1,this._targetRay.linearVelocity=new A,this._targetRay.hasAngularVelocity=!1,this._targetRay.angularVelocity=new A),this._targetRay}getGripSpace(){return this._grip===null&&(this._grip=new xe,this._grip.matrixAutoUpdate=!1,this._grip.visible=!1,this._grip.hasLinearVelocity=!1,this._grip.linearVelocity=new A,this._grip.hasAngularVelocity=!1,this._grip.angularVelocity=new A,this._grip.eventsEnabled=!1),this._grip}dispatchEvent(t){return this._targetRay!==null&&this._targetRay.dispatchEvent(t),this._grip!==null&&this._grip.dispatchEvent(t),this._hand!==null&&this._hand.dispatchEvent(t),this}connect(t){if(t&&t.hand){let e=this._hand;if(e)for(let i of t.hand.values())this._getHandJoint(e,i)}return this.dispatchEvent({type:"connected",data:t}),this}disconnect(t){return this.dispatchEvent({type:"disconnected",data:t}),this._targetRay!==null&&(this._targetRay.visible=!1),this._grip!==null&&(this._grip.visible=!1),this._hand!==null&&(this._hand.visible=!1),this}update(t,e,i){let s=null,r=null,a=null,o=this._targetRay,l=this._grip,c=this._hand;if(t&&e.session.visibilityState!=="visible-blurred"){if(c&&t.hand){a=!0;for(let v of t.hand.values()){let g=e.getJointPose(v,i),m=this._getHandJoint(c,v);g!==null&&(m.matrix.fromArray(g.transform.matrix),m.matrix.decompose(m.position,m.quaternion,m.scale),m.matrixWorldNeedsUpdate=!0,m.jointRadius=g.radius),m.visible=g!==null}let h=c.joints["index-finger-tip"],u=c.joints["thumb-tip"],d=h.position.distanceTo(u.position),f=.02,p=.005;c.inputState.pinching&&d>f+p?(c.inputState.pinching=!1,this.dispatchEvent({type:"pinchend",handedness:t.handedness,target:this})):!c.inputState.pinching&&d<=f-p&&(c.inputState.pinching=!0,this.dispatchEvent({type:"pinchstart",handedness:t.handedness,target:this}))}else l!==null&&t.gripSpace&&(r=e.getPose(t.gripSpace,i),r!==null&&(l.matrix.fromArray(r.transform.matrix),l.matrix.decompose(l.position,l.quaternion,l.scale),l.matrixWorldNeedsUpdate=!0,r.linearVelocity?(l.hasLinearVelocity=!0,l.linearVelocity.copy(r.linearVelocity)):l.hasLinearVelocity=!1,r.angularVelocity?(l.hasAngularVelocity=!0,l.angularVelocity.copy(r.angularVelocity)):l.hasAngularVelocity=!1,l.eventsEnabled&&l.dispatchEvent({type:"gripUpdated",data:t,target:this})));o!==null&&(s=e.getPose(t.targetRaySpace,i),s===null&&r!==null&&(s=r),s!==null&&(o.matrix.fromArray(s.transform.matrix),o.matrix.decompose(o.position,o.quaternion,o.scale),o.matrixWorldNeedsUpdate=!0,s.linearVelocity?(o.hasLinearVelocity=!0,o.linearVelocity.copy(s.linearVelocity)):o.hasLinearVelocity=!1,s.angularVelocity?(o.hasAngularVelocity=!0,o.angularVelocity.copy(s.angularVelocity)):o.hasAngularVelocity=!1,this.dispatchEvent(yp)))}return o!==null&&(o.visible=s!==null),l!==null&&(l.visible=r!==null),c!==null&&(c.visible=a!==null),this}_getHandJoint(t,e){if(t.joints[e.jointName]===void 0){let i=new xe;i.matrixAutoUpdate=!1,i.visible=!1,t.joints[e.jointName]=i,t.add(i)}return t.joints[e.jointName]}},Md={aliceblue:15792383,antiquewhite:16444375,aqua:65535,aquamarine:8388564,azure:15794175,beige:16119260,bisque:16770244,black:0,blanchedalmond:16772045,blue:255,blueviolet:9055202,brown:10824234,burlywood:14596231,cadetblue:6266528,chartreuse:8388352,chocolate:13789470,coral:16744272,cornflowerblue:6591981,cornsilk:16775388,crimson:14423100,cyan:65535,darkblue:139,darkcyan:35723,darkgoldenrod:12092939,darkgray:11119017,darkgreen:25600,darkgrey:11119017,darkkhaki:12433259,darkmagenta:9109643,darkolivegreen:5597999,darkorange:16747520,darkorchid:10040012,darkred:9109504,darksalmon:15308410,darkseagreen:9419919,darkslateblue:4734347,darkslategray:3100495,darkslategrey:3100495,darkturquoise:52945,darkviolet:9699539,deeppink:16716947,deepskyblue:49151,dimgray:6908265,dimgrey:6908265,dodgerblue:2003199,firebrick:11674146,floralwhite:16775920,forestgreen:2263842,fuchsia:16711935,gainsboro:14474460,ghostwhite:16316671,gold:16766720,goldenrod:14329120,gray:8421504,green:32768,greenyellow:11403055,grey:8421504,honeydew:15794160,hotpink:16738740,indianred:13458524,indigo:4915330,ivory:16777200,khaki:15787660,lavender:15132410,lavenderblush:16773365,lawngreen:8190976,lemonchiffon:16775885,lightblue:11393254,lightcoral:15761536,lightcyan:14745599,lightgoldenrodyellow:16448210,lightgray:13882323,lightgreen:9498256,lightgrey:13882323,lightpink:16758465,lightsalmon:16752762,lightseagreen:2142890,lightskyblue:8900346,lightslategray:7833753,lightslategrey:7833753,lightsteelblue:11584734,lightyellow:16777184,lime:65280,limegreen:3329330,linen:16445670,magenta:16711935,maroon:8388608,mediumaquamarine:6737322,mediumblue:205,mediumorchid:12211667,mediumpurple:9662683,mediumseagreen:3978097,mediumslateblue:8087790,mediumspringgreen:64154,mediumturquoise:4772300,mediumvioletred:13047173,midnightblue:1644912,mintcream:16121850,mistyrose:16770273,moccasin:16770229,navajowhite:16768685,navy:128,oldlace:16643558,olive:8421376,olivedrab:7048739,orange:16753920,orangered:16729344,orchid:14315734,palegoldenrod:15657130,palegreen:10025880,paleturquoise:11529966,palevioletred:14381203,papayawhip:16773077,peachpuff:16767673,peru:13468991,pink:16761035,plum:14524637,powderblue:11591910,purple:8388736,rebeccapurple:6697881,red:16711680,rosybrown:12357519,royalblue:4286945,saddlebrown:9127187,salmon:16416882,sandybrown:16032864,seagreen:3050327,seashell:16774638,sienna:10506797,silver:12632256,skyblue:8900331,slateblue:6970061,slategray:7372944,slategrey:7372944,snow:16775930,springgreen:65407,steelblue:4620980,tan:13808780,teal:32896,thistle:14204888,tomato:16737095,turquoise:4251856,violet:15631086,wheat:16113331,white:16777215,whitesmoke:16119285,yellow:16776960,yellowgreen:10145074},hn={h:0,s:0,l:0},ya={h:0,s:0,l:0};function tc(n,t,e){return e<0&&(e+=1),e>1&&(e-=1),e<1/6?n+(t-n)*6*e:e<1/2?t:e<2/3?n+(t-n)*6*(2/3-e):n}var Mt=class{constructor(t,e,i){return this.isColor=!0,this.r=1,this.g=1,this.b=1,this.set(t,e,i)}set(t,e,i){if(e===void 0&&i===void 0){let s=t;s&&s.isColor?this.copy(s):typeof s=="number"?this.setHex(s):typeof s=="string"&&this.setStyle(s)}else this.setRGB(t,e,i);return this}setScalar(t){return this.r=t,this.g=t,this.b=t,this}setHex(t,e=Fe){return t=Math.floor(t),this.r=(t>>16&255)/255,this.g=(t>>8&255)/255,this.b=(t&255)/255,Wt.colorSpaceToWorking(this,e),this}setRGB(t,e,i,s=Wt.workingColorSpace){return this.r=t,this.g=e,this.b=i,Wt.colorSpaceToWorking(this,s),this}setHSL(t,e,i,s=Wt.workingColorSpace){if(t=Yc(t,1),e=Bt(e,0,1),i=Bt(i,0,1),e===0)this.r=this.g=this.b=i;else{let r=i<=.5?i*(1+e):i+e-i*e,a=2*i-r;this.r=tc(a,r,t+1/3),this.g=tc(a,r,t),this.b=tc(a,r,t-1/3)}return Wt.colorSpaceToWorking(this,s),this}setStyle(t,e=Fe){function i(r){r!==void 0&&parseFloat(r)<1&&Pt("Color: Alpha component of "+t+" will be ignored.")}let s;if(s=/^(\w+)\(([^\)]*)\)/.exec(t)){let r,a=s[1],o=s[2];switch(a){case"rgb":case"rgba":if(r=/^\s*(\d+)\s*,\s*(\d+)\s*,\s*(\d+)\s*(?:,\s*(\d+(?:\.\d+)?|\.\d+)\s*)?$/.exec(o))return i(r[4]),this.setRGB(Math.min(255,parseInt(r[1],10))/255,Math.min(255,parseInt(r[2],10))/255,Math.min(255,parseInt(r[3],10))/255,e);if(r=/^\s*(\d+(?:\.\d+)?|\.\d+)\%\s*,\s*(\d+(?:\.\d+)?|\.\d+)\%\s*,\s*(\d+(?:\.\d+)?|\.\d+)\%\s*(?:,\s*(\d+(?:\.\d+)?|\.\d+)\s*)?$/.exec(o))return i(r[4]),this.setRGB(Math.min(100,parseFloat(r[1]))/100,Math.min(100,parseFloat(r[2]))/100,Math.min(100,parseFloat(r[3]))/100,e);break;case"hsl":case"hsla":if(r=/^\s*([+-]?(?:\d+(?:\.\d+)?|\.\d+))\s*,\s*(\d+(?:\.\d+)?|\.\d+)\%\s*,\s*(\d+(?:\.\d+)?|\.\d+)\%\s*(?:,\s*(\d+(?:\.\d+)?|\.\d+)\s*)?$/.exec(o))return i(r[4]),this.setHSL(parseFloat(r[1])/360,parseFloat(r[2])/100,parseFloat(r[3])/100,e);break;default:Pt("Color: Unknown color model "+t)}}else if(s=/^\#([A-Fa-f\d]+)$/.exec(t)){let r=s[1],a=r.length;if(a===3)return this.setRGB(parseInt(r.charAt(0),16)/15,parseInt(r.charAt(1),16)/15,parseInt(r.charAt(2),16)/15,e);if(a===6)return this.setHex(parseInt(r,16),e);Pt("Color: Invalid hex color "+t)}else if(t&&t.length>0)return this.setColorName(t,e);return this}setColorName(t,e=Fe){let i=Md[t.toLowerCase()];return i!==void 0?this.setHex(i,e):Pt("Color: Unknown color "+t),this}clone(){return new this.constructor(this.r,this.g,this.b)}copy(t){return this.r=t.r,this.g=t.g,this.b=t.b,this}copySRGBToLinear(t){return this.r=Qi(t.r),this.g=Qi(t.g),this.b=Qi(t.b),this}copyLinearToSRGB(t){return this.r=gs(t.r),this.g=gs(t.g),this.b=gs(t.b),this}convertSRGBToLinear(){return this.copySRGBToLinear(this),this}convertLinearToSRGB(){return this.copyLinearToSRGB(this),this}getHex(t=Fe){return Wt.workingToColorSpace(Xe.copy(this),t),Math.round(Bt(Xe.r*255,0,255))*65536+Math.round(Bt(Xe.g*255,0,255))*256+Math.round(Bt(Xe.b*255,0,255))}getHexString(t=Fe){return("000000"+this.getHex(t).toString(16)).slice(-6)}getHSL(t,e=Wt.workingColorSpace){Wt.workingToColorSpace(Xe.copy(this),e);let i=Xe.r,s=Xe.g,r=Xe.b,a=Math.max(i,s,r),o=Math.min(i,s,r),l,c,h=(o+a)/2;if(o===a)l=0,c=0;else{let u=a-o;switch(c=h<=.5?u/(a+o):u/(2-a-o),a){case i:l=(s-r)/u+(s<r?6:0);break;case s:l=(r-i)/u+2;break;case r:l=(i-s)/u+4;break}l/=6}return t.h=l,t.s=c,t.l=h,t}getRGB(t,e=Wt.workingColorSpace){return Wt.workingToColorSpace(Xe.copy(this),e),t.r=Xe.r,t.g=Xe.g,t.b=Xe.b,t}getStyle(t=Fe){Wt.workingToColorSpace(Xe.copy(this),t);let e=Xe.r,i=Xe.g,s=Xe.b;return t!==Fe?`color(${t} ${e.toFixed(3)} ${i.toFixed(3)} ${s.toFixed(3)})`:`rgb(${Math.round(e*255)},${Math.round(i*255)},${Math.round(s*255)})`}offsetHSL(t,e,i){return this.getHSL(hn),this.setHSL(hn.h+t,hn.s+e,hn.l+i)}add(t){return this.r+=t.r,this.g+=t.g,this.b+=t.b,this}addColors(t,e){return this.r=t.r+e.r,this.g=t.g+e.g,this.b=t.b+e.b,this}addScalar(t){return this.r+=t,this.g+=t,this.b+=t,this}sub(t){return this.r=Math.max(0,this.r-t.r),this.g=Math.max(0,this.g-t.g),this.b=Math.max(0,this.b-t.b),this}multiply(t){return this.r*=t.r,this.g*=t.g,this.b*=t.b,this}multiplyScalar(t){return this.r*=t,this.g*=t,this.b*=t,this}lerp(t,e){return this.r+=(t.r-this.r)*e,this.g+=(t.g-this.g)*e,this.b+=(t.b-this.b)*e,this}lerpColors(t,e,i){return this.r=t.r+(e.r-t.r)*i,this.g=t.g+(e.g-t.g)*i,this.b=t.b+(e.b-t.b)*i,this}lerpHSL(t,e){this.getHSL(hn),t.getHSL(ya);let i=ur(hn.h,ya.h,e),s=ur(hn.s,ya.s,e),r=ur(hn.l,ya.l,e);return this.setHSL(i,s,r),this}setFromVector3(t){return this.r=t.x,this.g=t.y,this.b=t.z,this}applyMatrix3(t){let e=this.r,i=this.g,s=this.b,r=t.elements;return this.r=r[0]*e+r[3]*i+r[6]*s,this.g=r[1]*e+r[4]*i+r[7]*s,this.b=r[2]*e+r[5]*i+r[8]*s,this}equals(t){return t.r===this.r&&t.g===this.g&&t.b===this.b}fromArray(t,e=0){return this.r=t[e],this.g=t[e+1],this.b=t[e+2],this}toArray(t=[],e=0){return t[e]=this.r,t[e+1]=this.g,t[e+2]=this.b,t}fromBufferAttribute(t,e){return this.r=t.getX(e),this.g=t.getY(e),this.b=t.getZ(e),this}toJSON(){return this.getHex()}*[Symbol.iterator](){yield this.r,yield this.g,yield this.b}},Xe=new Mt;Mt.NAMES=Md;var ai=class extends de{constructor(){super(),this.isScene=!0,this.type="Scene",this.background=null,this.environment=null,this.fog=null,this.backgroundBlurriness=0,this.backgroundIntensity=1,this.backgroundRotation=new gi,this.environmentIntensity=1,this.environmentRotation=new gi,this.overrideMaterial=null,typeof __THREE_DEVTOOLS__<"u"&&__THREE_DEVTOOLS__.dispatchEvent(new CustomEvent("observe",{detail:this}))}copy(t,e){return super.copy(t,e),this.background=t.background!==null?t.background.clone():null,this.environment=t.environment!==null?t.environment.clone():null,this.fog=t.fog!==null?t.fog.clone():null,this.backgroundBlurriness=t.backgroundBlurriness,this.backgroundIntensity=t.backgroundIntensity,this.backgroundRotation.copy(t.backgroundRotation),this.environmentIntensity=t.environmentIntensity,this.environmentRotation.copy(t.environmentRotation),this.overrideMaterial=t.overrideMaterial!==null?t.overrideMaterial.clone():null,this.matrixAutoUpdate=t.matrixAutoUpdate,this}toJSON(t){let e=super.toJSON(t);return this.fog!==null&&(e.object.fog=this.fog.toJSON()),e.object.backgroundBlurriness=this.backgroundBlurriness,e.object.backgroundIntensity=this.backgroundIntensity,e.object.backgroundRotation=this.backgroundRotation.toArray(),e.object.environmentIntensity=this.environmentIntensity,e.object.environmentRotation=this.environmentRotation.toArray(),e}},Ei=new A,Yi=new A,ec=new A,Zi=new A,rs=new A,as=new A,uu=new A,ic=new A,nc=new A,sc=new A,rc=new ie,ac=new ie,oc=new ie,$i=class n{constructor(t=new A,e=new A,i=new A){this.a=t,this.b=e,this.c=i}static getNormal(t,e,i,s){s.subVectors(i,e),Ei.subVectors(t,e),s.cross(Ei);let r=s.lengthSq();return r>0?s.multiplyScalar(1/Math.sqrt(r)):s.set(0,0,0)}static getBarycoord(t,e,i,s,r){Ei.subVectors(s,e),Yi.subVectors(i,e),ec.subVectors(t,e);let a=Ei.dot(Ei),o=Ei.dot(Yi),l=Ei.dot(ec),c=Yi.dot(Yi),h=Yi.dot(ec),u=a*c-o*o;if(u===0)return r.set(0,0,0),null;let d=1/u,f=(c*l-o*h)*d,p=(a*h-o*l)*d;return r.set(1-f-p,p,f)}static containsPoint(t,e,i,s){return this.getBarycoord(t,e,i,s,Zi)===null?!1:Zi.x>=0&&Zi.y>=0&&Zi.x+Zi.y<=1}static getInterpolation(t,e,i,s,r,a,o,l){return this.getBarycoord(t,e,i,s,Zi)===null?(l.x=0,l.y=0,"z"in l&&(l.z=0),"w"in l&&(l.w=0),null):(l.setScalar(0),l.addScaledVector(r,Zi.x),l.addScaledVector(a,Zi.y),l.addScaledVector(o,Zi.z),l)}static getInterpolatedAttribute(t,e,i,s,r,a){return rc.setScalar(0),ac.setScalar(0),oc.setScalar(0),rc.fromBufferAttribute(t,e),ac.fromBufferAttribute(t,i),oc.fromBufferAttribute(t,s),a.setScalar(0),a.addScaledVector(rc,r.x),a.addScaledVector(ac,r.y),a.addScaledVector(oc,r.z),a}static isFrontFacing(t,e,i,s){return Ei.subVectors(i,e),Yi.subVectors(t,e),Ei.cross(Yi).dot(s)<0}set(t,e,i){return this.a.copy(t),this.b.copy(e),this.c.copy(i),this}setFromPointsAndIndices(t,e,i,s){return this.a.copy(t[e]),this.b.copy(t[i]),this.c.copy(t[s]),this}setFromAttributeAndIndices(t,e,i,s){return this.a.fromBufferAttribute(t,e),this.b.fromBufferAttribute(t,i),this.c.fromBufferAttribute(t,s),this}clone(){return new this.constructor().copy(this)}copy(t){return this.a.copy(t.a),this.b.copy(t.b),this.c.copy(t.c),this}getArea(){return Ei.subVectors(this.c,this.b),Yi.subVectors(this.a,this.b),Ei.cross(Yi).length()*.5}getMidpoint(t){return t.addVectors(this.a,this.b).add(this.c).multiplyScalar(1/3)}getNormal(t){return n.getNormal(this.a,this.b,this.c,t)}getPlane(t){return t.setFromCoplanarPoints(this.a,this.b,this.c)}getBarycoord(t,e){return n.getBarycoord(t,this.a,this.b,this.c,e)}getInterpolation(t,e,i,s,r){return n.getInterpolation(t,this.a,this.b,this.c,e,i,s,r)}containsPoint(t){return n.containsPoint(t,this.a,this.b,this.c)}isFrontFacing(t){return n.isFrontFacing(this.a,this.b,this.c,t)}intersectsBox(t){return t.intersectsTriangle(this)}closestPointToPoint(t,e){let i=this.a,s=this.b,r=this.c,a,o;rs.subVectors(s,i),as.subVectors(r,i),ic.subVectors(t,i);let l=rs.dot(ic),c=as.dot(ic);if(l<=0&&c<=0)return e.copy(i);nc.subVectors(t,s);let h=rs.dot(nc),u=as.dot(nc);if(h>=0&&u<=h)return e.copy(s);let d=l*u-h*c;if(d<=0&&l>=0&&h<=0&&l-h>0)return a=l/(l-h),e.copy(i).addScaledVector(rs,a);sc.subVectors(t,r);let f=rs.dot(sc),p=as.dot(sc);if(p>=0&&f<=p)return e.copy(r);let v=f*c-l*p;if(v<=0&&c>=0&&p<=0)return o=c/(c-p),e.copy(i).addScaledVector(as,o);let g=h*p-f*u;if(g<=0&&u-h>=0&&f-p>=0)return uu.subVectors(r,s),o=(u-h)/(u-h+(f-p)),e.copy(s).addScaledVector(uu,o);let m=1/(g+v+d);return a=v*m,o=d*m,e.copy(i).addScaledVector(rs,a).addScaledVector(as,o)}equals(t){return t.a.equals(this.a)&&t.b.equals(this.b)&&t.c.equals(this.c)}},xi=class{constructor(t=new A(1/0,1/0,1/0),e=new A(-1/0,-1/0,-1/0)){this.isBox3=!0,this.min=t,this.max=e}set(t,e){return this.min.copy(t),this.max.copy(e),this}setFromArray(t){this.makeEmpty();for(let e=0,i=t.length;e<i;e+=3)this.expandByPoint(Ti.fromArray(t,e));return this}setFromBufferAttribute(t){this.makeEmpty();for(let e=0,i=t.count;e<i;e++)this.expandByPoint(Ti.fromBufferAttribute(t,e));return this}setFromPoints(t){this.makeEmpty();for(let e=0,i=t.length;e<i;e++)this.expandByPoint(t[e]);return this}setFromCenterAndSize(t,e){let i=Ti.copy(e).multiplyScalar(.5);return this.min.copy(t).sub(i),this.max.copy(t).add(i),this}setFromObject(t,e=!1){return this.makeEmpty(),this.expandByObject(t,e)}clone(){return new this.constructor().copy(this)}copy(t){return this.min.copy(t.min),this.max.copy(t.max),this}makeEmpty(){return this.min.x=this.min.y=this.min.z=1/0,this.max.x=this.max.y=this.max.z=-1/0,this}isEmpty(){return this.max.x<this.min.x||this.max.y<this.min.y||this.max.z<this.min.z}getCenter(t){return this.isEmpty()?t.set(0,0,0):t.addVectors(this.min,this.max).multiplyScalar(.5)}getSize(t){return this.isEmpty()?t.set(0,0,0):t.subVectors(this.max,this.min)}expandByPoint(t){return this.min.min(t),this.max.max(t),this}expandByVector(t){return this.min.sub(t),this.max.add(t),this}expandByScalar(t){return this.min.addScalar(-t),this.max.addScalar(t),this}expandByObject(t,e=!1){t.updateWorldMatrix(!1,!1);let i=t.geometry;if(i!==void 0){let r=i.getAttribute("position");if(e===!0&&r!==void 0&&t.isInstancedMesh!==!0&&t.isBatchedMesh!==!0)for(let a=0,o=r.count;a<o;a++)t.isMesh===!0?t.getVertexPosition(a,Ti):Ti.fromBufferAttribute(r,a),Ti.applyMatrix4(t.matrixWorld),this.expandByPoint(Ti);else t.boundingBox!==void 0?(t.boundingBox===null&&t.computeBoundingBox(),Ma.copy(t.boundingBox)):(i.boundingBox===null&&i.computeBoundingBox(),Ma.copy(i.boundingBox)),Ma.applyMatrix4(t.matrixWorld),this.union(Ma)}let s=t.children;for(let r=0,a=s.length;r<a;r++)this.expandByObject(s[r],e);return this}containsPoint(t){return t.x>=this.min.x&&t.x<=this.max.x&&t.y>=this.min.y&&t.y<=this.max.y&&t.z>=this.min.z&&t.z<=this.max.z}containsBox(t){return this.min.x<=t.min.x&&t.max.x<=this.max.x&&this.min.y<=t.min.y&&t.max.y<=this.max.y&&this.min.z<=t.min.z&&t.max.z<=this.max.z}getParameter(t,e){return e.set((t.x-this.min.x)/(this.max.x-this.min.x),(t.y-this.min.y)/(this.max.y-this.min.y),(t.z-this.min.z)/(this.max.z-this.min.z))}intersectsBox(t){return t.max.x>=this.min.x&&t.min.x<=this.max.x&&t.max.y>=this.min.y&&t.min.y<=this.max.y&&t.max.z>=this.min.z&&t.min.z<=this.max.z}intersectsSphere(t){return this.clampPoint(t.center,Ti),Ti.distanceToSquared(t.center)<=t.radius*t.radius}intersectsPlane(t){let e,i;return t.normal.x>0?(e=t.normal.x*this.min.x,i=t.normal.x*this.max.x):(e=t.normal.x*this.max.x,i=t.normal.x*this.min.x),t.normal.y>0?(e+=t.normal.y*this.min.y,i+=t.normal.y*this.max.y):(e+=t.normal.y*this.max.y,i+=t.normal.y*this.min.y),t.normal.z>0?(e+=t.normal.z*this.min.z,i+=t.normal.z*this.max.z):(e+=t.normal.z*this.max.z,i+=t.normal.z*this.min.z),e<=-t.constant&&i>=-t.constant}intersectsTriangle(t){if(this.isEmpty())return!1;this.getCenter(rr),Sa.subVectors(this.max,rr),os.subVectors(t.a,rr),ls.subVectors(t.b,rr),cs.subVectors(t.c,rr),un.subVectors(ls,os),dn.subVectors(cs,ls),In.subVectors(os,cs);let e=[0,-un.z,un.y,0,-dn.z,dn.y,0,-In.z,In.y,un.z,0,-un.x,dn.z,0,-dn.x,In.z,0,-In.x,-un.y,un.x,0,-dn.y,dn.x,0,-In.y,In.x,0];return!lc(e,os,ls,cs,Sa)||(e=[1,0,0,0,1,0,0,0,1],!lc(e,os,ls,cs,Sa))?!1:(ba.crossVectors(un,dn),e=[ba.x,ba.y,ba.z],lc(e,os,ls,cs,Sa))}clampPoint(t,e){return e.copy(t).clamp(this.min,this.max)}distanceToPoint(t){return this.clampPoint(t,Ti).distanceTo(t)}getBoundingSphere(t){return this.isEmpty()?t.makeEmpty():(this.getCenter(t.center),t.radius=this.getSize(Ti).length()*.5),t}intersect(t){return this.min.max(t.min),this.max.min(t.max),this.isEmpty()&&this.makeEmpty(),this}union(t){return this.min.min(t.min),this.max.max(t.max),this}applyMatrix4(t){return this.isEmpty()?this:(Ki[0].set(this.min.x,this.min.y,this.min.z).applyMatrix4(t),Ki[1].set(this.min.x,this.min.y,this.max.z).applyMatrix4(t),Ki[2].set(this.min.x,this.max.y,this.min.z).applyMatrix4(t),Ki[3].set(this.min.x,this.max.y,this.max.z).applyMatrix4(t),Ki[4].set(this.max.x,this.min.y,this.min.z).applyMatrix4(t),Ki[5].set(this.max.x,this.min.y,this.max.z).applyMatrix4(t),Ki[6].set(this.max.x,this.max.y,this.min.z).applyMatrix4(t),Ki[7].set(this.max.x,this.max.y,this.max.z).applyMatrix4(t),this.setFromPoints(Ki),this)}translate(t){return this.min.add(t),this.max.add(t),this}equals(t){return t.min.equals(this.min)&&t.max.equals(this.max)}toJSON(){return{min:this.min.toArray(),max:this.max.toArray()}}fromJSON(t){return this.min.fromArray(t.min),this.max.fromArray(t.max),this}},Ki=[new A,new A,new A,new A,new A,new A,new A,new A],Ti=new A,Ma=new xi,os=new A,ls=new A,cs=new A,un=new A,dn=new A,In=new A,rr=new A,Sa=new A,ba=new A,Dn=new A;function lc(n,t,e,i,s){for(let r=0,a=n.length-3;r<=a;r+=3){Dn.fromArray(n,r);let o=s.x*Math.abs(Dn.x)+s.y*Math.abs(Dn.y)+s.z*Math.abs(Dn.z),l=t.dot(Dn),c=e.dot(Dn),h=i.dot(Dn);if(Math.max(-Math.max(l,c,h),Math.min(l,c,h))>o)return!1}return!0}var ji=Mp();function Mp(){let n=new ArrayBuffer(4),t=new Float32Array(n),e=new Uint32Array(n),i=new Uint32Array(512),s=new Uint32Array(512);for(let l=0;l<256;++l){let c=l-127;c<-27?(i[l]=0,i[l|256]=32768,s[l]=24,s[l|256]=24):c<-14?(i[l]=1024>>-c-14,i[l|256]=1024>>-c-14|32768,s[l]=-c-1,s[l|256]=-c-1):c<=15?(i[l]=c+15<<10,i[l|256]=c+15<<10|32768,s[l]=13,s[l|256]=13):c<128?(i[l]=31744,i[l|256]=64512,s[l]=24,s[l|256]=24):(i[l]=31744,i[l|256]=64512,s[l]=13,s[l|256]=13)}let r=new Uint32Array(2048),a=new Uint32Array(64),o=new Uint32Array(64);for(let l=1;l<1024;++l){let c=l<<13,h=0;for(;(c&8388608)===0;)c<<=1,h-=8388608;c&=-8388609,h+=947912704,r[l]=c|h}for(let l=1024;l<2048;++l)r[l]=939524096+(l-1024<<13);for(let l=1;l<31;++l)a[l]=l<<23;a[31]=1199570944,a[32]=2147483648;for(let l=33;l<63;++l)a[l]=2147483648+(l-32<<23);a[63]=3347054592;for(let l=1;l<64;++l)l!==32&&(o[l]=1024);return{floatView:t,uint32View:e,baseTable:i,shiftTable:s,mantissaTable:r,exponentTable:a,offsetTable:o}}function Sp(n){Math.abs(n)>65504&&Pt("DataUtils.toHalfFloat(): Value out of range."),n=Bt(n,-65504,65504),ji.floatView[0]=n;let t=ji.uint32View[0],e=t>>23&511;return ji.baseTable[e]+((t&8388607)>>ji.shiftTable[e])}function bp(n){let t=n>>10;return ji.uint32View[0]=ji.mantissaTable[ji.offsetTable[t]+(n&1023)]+ji.exponentTable[t],ji.floatView[0]}var _r=class{static toHalfFloat(t){return Sp(t)}static fromHalfFloat(t){return bp(t)}},Me=new A,wa=new et,wp=0,_e=class extends Ri{constructor(t,e,i=!1){if(super(),Array.isArray(t))throw new TypeError("THREE.BufferAttribute: array should be a Typed Array.");this.isBufferAttribute=!0,Object.defineProperty(this,"id",{value:wp++}),this.name="",this.array=t,this.itemSize=e,this.count=t!==void 0?t.length/e:0,this.normalized=i,this.usage=pd,this.updateRanges=[],this.gpuType=yi,this.version=0,this._normalizationScale=t!==void 0?Kc(t):void 0}onUploadCallback(){}set needsUpdate(t){t===!0&&this.version++}setUsage(t){return this.usage=t,this}addUpdateRange(t,e){this.updateRanges.push({start:t,count:e})}clearUpdateRanges(){this.updateRanges.length=0}copy(t){return this.name=t.name,this.array=new t.array.constructor(t.array),this.itemSize=t.itemSize,this.count=t.count,this.normalized=t.normalized,this.usage=t.usage,this.gpuType=t.gpuType,this._normalizationScale=t._normalizationScale,this}copyAt(t,e,i){t*=this.itemSize,i*=e.itemSize;for(let s=0,r=this.itemSize;s<r;s++)this.array[t+s]=e.array[i+s];return this}copyArray(t){return this.array.set(t),this}applyMatrix3(t){if(this.itemSize===2)for(let e=0,i=this.count;e<i;e++)wa.fromBufferAttribute(this,e),wa.applyMatrix3(t),this.setXY(e,wa.x,wa.y);else if(this.itemSize===3)for(let e=0,i=this.count;e<i;e++)Me.fromBufferAttribute(this,e),Me.applyMatrix3(t),this.setXYZ(e,Me.x,Me.y,Me.z);return this}applyMatrix4(t){for(let e=0,i=this.count;e<i;e++)Me.fromBufferAttribute(this,e),Me.applyMatrix4(t),this.setXYZ(e,Me.x,Me.y,Me.z);return this}applyNormalMatrix(t){for(let e=0,i=this.count;e<i;e++)Me.fromBufferAttribute(this,e),Me.applyNormalMatrix(t),this.setXYZ(e,Me.x,Me.y,Me.z);return this}transformDirection(t){for(let e=0,i=this.count;e<i;e++)Me.fromBufferAttribute(this,e),Me.transformDirection(t),this.setXYZ(e,Me.x,Me.y,Me.z);return this}set(t,e=0){return this.array.set(t,e),this}getComponent(t,e){let i=this.array[t*this.itemSize+e];return this.normalized&&(i=this._denormalize(i)),i}setComponent(t,e,i){return this.normalized&&(i=this._normalize(i)),this.array[t*this.itemSize+e]=i,this}getX(t){let e=this.array[t*this.itemSize];return this.normalized&&(e=this._denormalize(e)),e}setX(t,e){return this.normalized&&(e=this._normalize(e)),this.array[t*this.itemSize]=e,this}getY(t){let e=this.array[t*this.itemSize+1];return this.normalized&&(e=this._denormalize(e)),e}setY(t,e){return this.normalized&&(e=this._normalize(e)),this.array[t*this.itemSize+1]=e,this}getZ(t){let e=this.array[t*this.itemSize+2];return this.normalized&&(e=this._denormalize(e)),e}setZ(t,e){return this.normalized&&(e=this._normalize(e)),this.array[t*this.itemSize+2]=e,this}getW(t){let e=this.array[t*this.itemSize+3];return this.normalized&&(e=this._denormalize(e)),e}setW(t,e){return this.normalized&&(e=this._normalize(e)),this.array[t*this.itemSize+3]=e,this}setXY(t,e,i){return t*=this.itemSize,this.normalized&&(e=this._normalize(e),i=this._normalize(i)),this.array[t+0]=e,this.array[t+1]=i,this}setXYZ(t,e,i,s){return t*=this.itemSize,this.normalized&&(e=this._normalize(e),i=this._normalize(i),s=this._normalize(s)),this.array[t+0]=e,this.array[t+1]=i,this.array[t+2]=s,this}setXYZW(t,e,i,s,r){return t*=this.itemSize,this.normalized&&(e=this._normalize(e),i=this._normalize(i),s=this._normalize(s),r=this._normalize(r)),this.array[t+0]=e,this.array[t+1]=i,this.array[t+2]=s,this.array[t+3]=r,this}onUpload(t){return this.onUploadCallback=t,this}clone(){return new this.constructor(this.array,this.itemSize).copy(this)}toJSON(){let t={itemSize:this.itemSize,type:this.array.constructor.name,array:Array.from(this.array),normalized:this.normalized};return t.name=this.name,t.usage=this.usage,t.gpuType=this.gpuType,t}dispose(){this.dispatchEvent({type:"dispose"})}_denormalize(t){let e=this._normalizationScale;return e!==void 0?Math.max(t/e,-1):vd(t,this.array)}_normalize(t){let e=this._normalizationScale;return e!==void 0?Math.round(t*e):yd(t,this.array)}};var vr=class extends _e{constructor(t,e,i){super(new Uint16Array(t),e,i)}};var yr=class extends _e{constructor(t,e,i){super(new Uint32Array(t),e,i)}};var Kt=class extends _e{constructor(t,e,i){super(new Float32Array(t),e,i)}},Ep=new xi,ar=new A,cc=new A,$e=class{constructor(t=new A,e=-1){this.isSphere=!0,this.center=t,this.radius=e}set(t,e){return this.center.copy(t),this.radius=e,this}setFromPoints(t,e){let i=this.center;e!==void 0?i.copy(e):Ep.setFromPoints(t).getCenter(i);let s=0;for(let r=0,a=t.length;r<a;r++)s=Math.max(s,i.distanceToSquared(t[r]));return this.radius=Math.sqrt(s),this}copy(t){return this.center.copy(t.center),this.radius=t.radius,this}isEmpty(){return this.radius<0}makeEmpty(){return this.center.set(0,0,0),this.radius=-1,this}containsPoint(t){return t.distanceToSquared(this.center)<=this.radius*this.radius}distanceToPoint(t){return t.distanceTo(this.center)-this.radius}intersectsSphere(t){let e=this.radius+t.radius;return t.center.distanceToSquared(this.center)<=e*e}intersectsBox(t){return t.intersectsSphere(this)}intersectsPlane(t){return Math.abs(t.distanceToPoint(this.center))<=this.radius}clampPoint(t,e){let i=this.center.distanceToSquared(t);return e.copy(t),i>this.radius*this.radius&&(e.sub(this.center).normalize(),e.multiplyScalar(this.radius).add(this.center)),e}getBoundingBox(t){return this.isEmpty()?(t.makeEmpty(),t):(t.set(this.center,this.center),t.expandByScalar(this.radius),t)}applyMatrix4(t){return this.center.applyMatrix4(t),this.radius=this.radius*t.getMaxScaleOnAxis(),this}translate(t){return this.center.add(t),this}expandByPoint(t){if(this.isEmpty())return this.center.copy(t),this.radius=0,this;ar.subVectors(t,this.center);let e=ar.lengthSq();if(e>this.radius*this.radius){let i=Math.sqrt(e),s=(i-this.radius)*.5;this.center.addScaledVector(ar,s/i),this.radius+=s}return this}union(t){return t.isEmpty()?this:this.isEmpty()?(this.copy(t),this):(this.center.equals(t.center)===!0?this.radius=Math.max(this.radius,t.radius):(cc.subVectors(t.center,this.center).setLength(t.radius),this.expandByPoint(ar.copy(t.center).add(cc)),this.expandByPoint(ar.copy(t.center).sub(cc))),this)}equals(t){return t.center.equals(this.center)&&t.radius===this.radius}clone(){return new this.constructor().copy(this)}toJSON(){return{radius:this.radius,center:this.center.toArray()}}fromJSON(t){return this.radius=t.radius,this.center.fromArray(t.center),this}},Tp=0,pi=new Gt,hc=new de,hs=new A,ri=new xi,or=new xi,Ue=new A,ee=class n extends Ri{constructor(){super(),this.isBufferGeometry=!0,Object.defineProperty(this,"id",{value:Tp++}),this.uuid=zs(),this.name="",this.type="BufferGeometry",this.index=null,this.indirect=null,this.indirectOffset=0,this.attributes={},this.morphAttributes={},this.morphTargetsRelative=!1,this.groups=[],this.boundingBox=null,this.boundingSphere=null,this.drawRange={start:0,count:1/0},this.userData={},this._transformed=!1}getIndex(){return this.index}setIndex(t){return Array.isArray(t)?this.index=new(Zf(t)?yr:vr)(t,1):this.index=t,this}setIndirect(t,e=0){return this.indirect=t,this.indirectOffset=e,this}getIndirect(){return this.indirect}getAttribute(t){return this.attributes[t]}setAttribute(t,e){return this.attributes[t]=e,this}deleteAttribute(t){return delete this.attributes[t],this}hasAttribute(t){return this.attributes[t]!==void 0}addGroup(t,e,i=0){this.groups.push({start:t,count:e,materialIndex:i})}clearGroups(){this.groups=[]}setDrawRange(t,e){this.drawRange.start=t,this.drawRange.count=e}applyMatrix4(t){let e=this.attributes.position;e!==void 0&&(e.applyMatrix4(t),e.needsUpdate=!0);let i=this.attributes.normal;if(i!==void 0){let r=new Nt().getNormalMatrix(t);i.applyNormalMatrix(r),i.needsUpdate=!0}let s=this.attributes.tangent;return s!==void 0&&(s.transformDirection(t),s.needsUpdate=!0),this.boundingBox!==null&&this.computeBoundingBox(),this.boundingSphere!==null&&this.computeBoundingSphere(),this._transformed=!0,this}applyQuaternion(t){return pi.makeRotationFromQuaternion(t),this.applyMatrix4(pi),this}rotateX(t){return pi.makeRotationX(t),this.applyMatrix4(pi),this}rotateY(t){return pi.makeRotationY(t),this.applyMatrix4(pi),this}rotateZ(t){return pi.makeRotationZ(t),this.applyMatrix4(pi),this}translate(t,e,i){return pi.makeTranslation(t,e,i),this.applyMatrix4(pi),this}scale(t,e,i){return pi.makeScale(t,e,i),this.applyMatrix4(pi),this}lookAt(t){return hc.lookAt(t),hc.updateMatrix(),this.applyMatrix4(hc.matrix),this}center(){return this.computeBoundingBox(),this.boundingBox.getCenter(hs).negate(),this.translate(hs.x,hs.y,hs.z),this}setFromPoints(t){let e=this.getAttribute("position");if(e===void 0){let i=[];for(let s=0,r=t.length;s<r;s++){let a=t[s];i.push(a.x,a.y,a.z||0)}this.setAttribute("position",new Kt(i,3))}else{let i=Math.min(t.length,e.count);for(let s=0;s<i;s++){let r=t[s];e.setXYZ(s,r.x,r.y,r.z||0)}t.length>e.count&&Pt("BufferGeometry: Buffer size too small for points data. Use .dispose() and create a new geometry."),e.needsUpdate=!0}return this}computeBoundingBox(){this.boundingBox===null&&(this.boundingBox=new xi);let t=this.attributes.position,e=this.morphAttributes.position;if(t&&t.isGLBufferAttribute){Lt("BufferGeometry.computeBoundingBox(): GLBufferAttribute requires a manual bounding box.",this),this.boundingBox.set(new A(-1/0,-1/0,-1/0),new A(1/0,1/0,1/0));return}if(t!==void 0){if(this.boundingBox.setFromBufferAttribute(t),e)for(let i=0,s=e.length;i<s;i++){let r=e[i];ri.setFromBufferAttribute(r),this.morphTargetsRelative?(Ue.addVectors(this.boundingBox.min,ri.min),this.boundingBox.expandByPoint(Ue),Ue.addVectors(this.boundingBox.max,ri.max),this.boundingBox.expandByPoint(Ue)):(this.boundingBox.expandByPoint(ri.min),this.boundingBox.expandByPoint(ri.max))}}else this.boundingBox.makeEmpty();(isNaN(this.boundingBox.min.x)||isNaN(this.boundingBox.min.y)||isNaN(this.boundingBox.min.z))&&Lt('BufferGeometry.computeBoundingBox(): Computed min/max have NaN values. The "position" attribute is likely to have NaN values.',this)}computeBoundingSphere(){this.boundingSphere===null&&(this.boundingSphere=new $e);let t=this.attributes.position,e=this.morphAttributes.position;if(t&&t.isGLBufferAttribute){Lt("BufferGeometry.computeBoundingSphere(): GLBufferAttribute requires a manual bounding sphere.",this),this.boundingSphere.set(new A,1/0);return}if(t){let i=this.boundingSphere.center;if(ri.setFromBufferAttribute(t),e)for(let r=0,a=e.length;r<a;r++){let o=e[r];or.setFromBufferAttribute(o),this.morphTargetsRelative?(Ue.addVectors(ri.min,or.min),ri.expandByPoint(Ue),Ue.addVectors(ri.max,or.max),ri.expandByPoint(Ue)):(ri.expandByPoint(or.min),ri.expandByPoint(or.max))}ri.getCenter(i);let s=0;for(let r=0,a=t.count;r<a;r++)Ue.fromBufferAttribute(t,r),s=Math.max(s,i.distanceToSquared(Ue));if(e)for(let r=0,a=e.length;r<a;r++){let o=e[r],l=this.morphTargetsRelative;for(let c=0,h=o.count;c<h;c++)Ue.fromBufferAttribute(o,c),l&&(hs.fromBufferAttribute(t,c),Ue.add(hs)),s=Math.max(s,i.distanceToSquared(Ue))}this.boundingSphere.radius=Math.sqrt(s),isNaN(this.boundingSphere.radius)&&Lt('BufferGeometry.computeBoundingSphere(): Computed radius is NaN. The "position" attribute is likely to have NaN values.',this)}}computeTangents(){let t=this.index,e=this.attributes;if(t===null||e.position===void 0||e.normal===void 0||e.uv===void 0){Lt("BufferGeometry: .computeTangents() failed. Missing required attributes (index, position, normal or uv)");return}let i=e.position,s=e.normal,r=e.uv,a=this.getAttribute("tangent");(a===void 0||a.count!==i.count)&&(a=new _e(new Float32Array(4*i.count),4),this.setAttribute("tangent",a));let o=[],l=[],c=new Uint8Array(i.count);for(let P=0;P<i.count;P++)o[P]=new A,l[P]=new A;let h=new A,u=new A,d=new A,f=new et,p=new et,v=new et,g=new A,m=new A;function _(P,C,I){c[P]=c[C]=c[I]=1,h.fromBufferAttribute(i,P),u.fromBufferAttribute(i,C),d.fromBufferAttribute(i,I),f.fromBufferAttribute(r,P),p.fromBufferAttribute(r,C),v.fromBufferAttribute(r,I),u.sub(h),d.sub(h),p.sub(f),v.sub(f);let L=1/(p.x*v.y-v.x*p.y);isFinite(L)&&(g.copy(u).multiplyScalar(v.y).addScaledVector(d,-p.y).multiplyScalar(L),m.copy(d).multiplyScalar(p.x).addScaledVector(u,-v.x).multiplyScalar(L),o[P].add(g),o[C].add(g),o[I].add(g),l[P].add(m),l[C].add(m),l[I].add(m))}let S=this.groups;S.length===0&&(S=[{start:0,count:t.count}]);for(let P=0,C=S.length;P<C;++P){let I=S[P],L=I.start,F=I.count;for(let D=L,B=L+F;D<B;D+=3)_(t.getX(D+0),t.getX(D+1),t.getX(D+2))}let M=new A,T=new A,w=new A,x=new A;function y(P){w.fromBufferAttribute(s,P),x.copy(w);let C=o[P];M.copy(C),M.sub(w.multiplyScalar(w.dot(C))).normalize(),T.crossVectors(x,C);let L=T.dot(l[P])<0?-1:1;a.setXYZW(P,M.x,M.y,M.z,L)}for(let P=0,C=i.count;P<C;++P)c[P]&&y(P);this._transformed=!0}computeVertexNormals(){let t=this.index,e=this.getAttribute("position");if(e!==void 0){let i=this.getAttribute("normal");if(i===void 0||i.count!==e.count)i=new _e(new Float32Array(e.count*3),3),this.setAttribute("normal",i);else for(let d=0,f=i.count;d<f;d++)i.setXYZ(d,0,0,0);let s=new A,r=new A,a=new A,o=new A,l=new A,c=new A,h=new A,u=new A;if(t)for(let d=0,f=t.count;d<f;d+=3){let p=t.getX(d+0),v=t.getX(d+1),g=t.getX(d+2);s.fromBufferAttribute(e,p),r.fromBufferAttribute(e,v),a.fromBufferAttribute(e,g),h.subVectors(a,r),u.subVectors(s,r),h.cross(u),o.fromBufferAttribute(i,p),l.fromBufferAttribute(i,v),c.fromBufferAttribute(i,g),o.add(h),l.add(h),c.add(h),i.setXYZ(p,o.x,o.y,o.z),i.setXYZ(v,l.x,l.y,l.z),i.setXYZ(g,c.x,c.y,c.z)}else for(let d=0,f=e.count;d<f;d+=3)s.fromBufferAttribute(e,d+0),r.fromBufferAttribute(e,d+1),a.fromBufferAttribute(e,d+2),h.subVectors(a,r),u.subVectors(s,r),h.cross(u),i.setXYZ(d+0,h.x,h.y,h.z),i.setXYZ(d+1,h.x,h.y,h.z),i.setXYZ(d+2,h.x,h.y,h.z);this.normalizeNormals(),i.needsUpdate=!0}}normalizeNormals(){let t=this.attributes.normal;for(let e=0,i=t.count;e<i;e++)Ue.fromBufferAttribute(t,e),Ue.normalize(),t.setXYZ(e,Ue.x,Ue.y,Ue.z)}toNonIndexed(){function t(o,l){let c=o.array,h=o.itemSize,u=o.normalized,d=new c.constructor(l.length*h),f=0,p=0;for(let v=0,g=l.length;v<g;v++){o.isInterleavedBufferAttribute?f=l[v]*o.data.stride+o.offset:f=l[v]*h;for(let m=0;m<h;m++)d[p++]=c[f++]}return new _e(d,h,u)}if(this.index===null)return Pt("BufferGeometry.toNonIndexed(): BufferGeometry is already non-indexed."),this;let e=new n,i=this.index.array,s=this.attributes;for(let o in s){let l=s[o],c=t(l,i);e.setAttribute(o,c)}let r=this.morphAttributes;for(let o in r){let l=[],c=r[o];for(let h=0,u=c.length;h<u;h++){let d=c[h],f=t(d,i);l.push(f)}e.morphAttributes[o]=l}e.morphTargetsRelative=this.morphTargetsRelative;let a=this.groups;for(let o=0,l=a.length;o<l;o++){let c=a[o];e.addGroup(c.start,c.count,c.materialIndex)}return e}toJSON(){let t={metadata:{version:4.7,type:"BufferGeometry",generator:"BufferGeometry.toJSON"}};if(t.uuid=this.uuid,t.type=this.parameters!==void 0&&this._transformed===!0?"BufferGeometry":this.type,t.name=this.name,Object.keys(this.userData).length>0&&(t.userData=this.userData),this.parameters!==void 0&&this._transformed!==!0){let l=this.parameters;for(let c in l)l[c]!==void 0&&(t[c]=l[c]);return t}t.data={attributes:{}};let e=this.index;e!==null&&(t.data.index={type:e.array.constructor.name,array:Array.prototype.slice.call(e.array)});let i=this.attributes;for(let l in i){let c=i[l];t.data.attributes[l]=c.toJSON(t.data)}let s={},r=!1;for(let l in this.morphAttributes){let c=this.morphAttributes[l],h=[];for(let u=0,d=c.length;u<d;u++){let f=c[u];h.push(f.toJSON(t.data))}h.length>0&&(s[l]=h,r=!0)}r&&(t.data.morphAttributes=s,t.data.morphTargetsRelative=this.morphTargetsRelative);let a=this.groups;a.length>0&&(t.data.groups=JSON.parse(JSON.stringify(a)));let o=this.boundingSphere;return o!==null&&(t.data.boundingSphere=o.toJSON()),t}clone(){return new this.constructor().copy(this)}copy(t){this.index=null,this.attributes={},this.morphAttributes={},this.groups=[],this.boundingBox=null,this.boundingSphere=null;let e={};this.name=t.name;let i=t.index;i!==null&&this.setIndex(i.clone());let s=t.attributes;for(let c in s){let h=s[c];this.setAttribute(c,h.clone(e))}let r=t.morphAttributes;for(let c in r){let h=[],u=r[c];for(let d=0,f=u.length;d<f;d++)h.push(u[d].clone(e));this.morphAttributes[c]=h}this.morphTargetsRelative=t.morphTargetsRelative;let a=t.groups;for(let c=0,h=a.length;c<h;c++){let u=a[c];this.addGroup(u.start,u.count,u.materialIndex)}let o=t.boundingBox;o!==null&&(this.boundingBox=o.clone());let l=t.boundingSphere;return l!==null&&(this.boundingSphere=l.clone()),this.drawRange.start=t.drawRange.start,this.drawRange.count=t.drawRange.count,this.userData=t.userData,this._transformed=t._transformed,this}dispose(){this.dispatchEvent({type:"dispose"})}};var uc=new A,Ap=new A,Rp=new Nt,Ke=class{constructor(t=new A(1,0,0),e=0){this.isPlane=!0,this.normal=t,this.constant=e}set(t,e){return this.normal.copy(t),this.constant=e,this}setComponents(t,e,i,s){return this.normal.set(t,e,i),this.constant=s,this}setFromNormalAndCoplanarPoint(t,e){return this.normal.copy(t),this.constant=-e.dot(this.normal),this}setFromCoplanarPoints(t,e,i){let s=uc.subVectors(i,e).cross(Ap.subVectors(t,e)).normalize();return this.setFromNormalAndCoplanarPoint(s,t),this}copy(t){return this.normal.copy(t.normal),this.constant=t.constant,this}normalize(){let t=1/this.normal.length();return this.normal.multiplyScalar(t),this.constant*=t,this}negate(){return this.constant*=-1,this.normal.negate(),this}distanceToPoint(t){return this.normal.dot(t)+this.constant}distanceToSphere(t){return this.distanceToPoint(t.center)-t.radius}projectPoint(t,e){return e.copy(t).addScaledVector(this.normal,-this.distanceToPoint(t))}intersectLine(t,e,i=!0){let s=t.delta(uc),r=this.normal.dot(s);if(r===0)return this.distanceToPoint(t.start)===0?e.copy(t.start):null;let a=-(t.start.dot(this.normal)+this.constant)/r;return i===!0&&(a<0||a>1)?null:e.copy(t.start).addScaledVector(s,a)}intersectsLine(t){let e=this.distanceToPoint(t.start),i=this.distanceToPoint(t.end);return e<0&&i>0||i<0&&e>0}intersectsBox(t){return t.intersectsPlane(this)}intersectsSphere(t){return t.intersectsPlane(this)}coplanarPoint(t){return t.copy(this.normal).multiplyScalar(-this.constant)}applyMatrix4(t,e){let i=e||Rp.getNormalMatrix(t),s=this.coplanarPoint(uc).applyMatrix4(t),r=this.normal.applyMatrix3(i).normalize();return this.constant=-s.dot(r),this}translate(t){return this.constant-=t.dot(this.normal),this}equals(t){return t.normal.equals(this.normal)&&t.constant===this.constant}clone(){return new this.constructor().copy(this)}toJSON(){return{normal:this.normal.toArray(),constant:this.constant}}fromJSON(t){return this.normal.fromArray(t.normal),this.constant=t.constant,this}},Cp=0,Ci=class extends Ri{constructor(){super(),this.isMaterial=!0,Object.defineProperty(this,"id",{value:Cp++}),this.uuid=zs(),this.name="",this.type="Material",this.blending=Ns,this.side=Mn,this.vertexColors=!1,this.opacity=1,this.transparent=!1,this.alphaHash=!1,this.blendSrc=Uc,this.blendDst=Fc,this.blendEquation=Zn,this.blendSrcAlpha=null,this.blendDstAlpha=null,this.blendEquationAlpha=null,this.blendColor=new Mt(0,0,0),this.blendAlpha=0,this.depthFunc=xs,this.depthTest=!0,this.depthWrite=!0,this.stencilWriteMask=255,this.stencilFunc=ld,this.stencilRef=0,this.stencilFuncMask=255,this.stencilFail=Ya,this.stencilZFail=Ya,this.stencilZPass=Ya,this.stencilWrite=!1,this.clippingPlanes=null,this.clipIntersection=!1,this.clipShadows=!1,this.shadowSide=null,this.colorWrite=!0,this.precision=null,this.polygonOffset=!1,this.polygonOffsetFactor=0,this.polygonOffsetUnits=0,this.dithering=!1,this.alphaToCoverage=!1,this.premultipliedAlpha=!1,this.forceSinglePass=!1,this.allowOverride=!0,this.visible=!0,this.toneMapped=!0,this.userData={},this.version=0,this._alphaTest=0}get alphaTest(){return this._alphaTest}set alphaTest(t){this._alphaTest>0!=t>0&&this.version++,this._alphaTest=t}onBeforeRender(){}onBeforeCompile(){}customProgramCacheKey(){return this.onBeforeCompile.toString()}setValues(t){if(t!==void 0)for(let e in t){let i=t[e];if(i===void 0){Pt(`Material: parameter '${e}' has value of undefined.`);continue}let s=this[e];if(s===void 0){Pt(`Material: '${e}' is not a property of THREE.${this.type}.`);continue}s&&s.isColor?s.set(i):s&&s.isVector2&&i&&i.isVector2||s&&s.isEuler&&i&&i.isEuler||s&&s.isVector3&&i&&i.isVector3?s.copy(i):this[e]=i}}toJSON(t){let e=t===void 0||typeof t=="string";e&&(t={textures:{},images:{}});let i={metadata:{version:4.7,type:"Material",generator:"Material.toJSON"}};i.uuid=this.uuid,i.type=this.type,i.blending=this.blending,i.side=this.side,i.shadowSide=this.shadowSide,i.vertexColors=this.vertexColors,i.opacity=this.opacity,i.transparent=this.transparent,i.blendSrc=this.blendSrc,i.blendDst=this.blendDst,i.blendEquation=this.blendEquation,i.blendSrcAlpha=this.blendSrcAlpha,i.blendDstAlpha=this.blendDstAlpha,i.blendEquationAlpha=this.blendEquationAlpha,i.blendColor=this.blendColor.getHex(),i.blendAlpha=this.blendAlpha,i.depthFunc=this.depthFunc,i.depthTest=this.depthTest,i.depthWrite=this.depthWrite,i.colorWrite=this.colorWrite,i.clipIntersection=this.clipIntersection,i.clipShadows=this.clipShadows,i.stencilWriteMask=this.stencilWriteMask,i.stencilFunc=this.stencilFunc,i.stencilRef=this.stencilRef,i.stencilFuncMask=this.stencilFuncMask,i.stencilFail=this.stencilFail,i.stencilZFail=this.stencilZFail,i.stencilZPass=this.stencilZPass,i.stencilWrite=this.stencilWrite,i.polygonOffset=this.polygonOffset,i.polygonOffsetFactor=this.polygonOffsetFactor,i.polygonOffsetUnits=this.polygonOffsetUnits,i.dithering=this.dithering,i.alphaTest=this.alphaTest,i.alphaHash=this.alphaHash,i.alphaToCoverage=this.alphaToCoverage,i.premultipliedAlpha=this.premultipliedAlpha,i.forceSinglePass=this.forceSinglePass,i.allowOverride=this.allowOverride,i.visible=this.visible,i.toneMapped=this.toneMapped,i.name=this.name,this.color&&this.color.isColor&&(i.color=this.color.getHex()),this.roughness!==void 0&&(i.roughness=this.roughness),this.metalness!==void 0&&(i.metalness=this.metalness),this.sheen!==void 0&&(i.sheen=this.sheen),this.sheenColor&&this.sheenColor.isColor&&(i.sheenColor=this.sheenColor.getHex()),this.sheenRoughness!==void 0&&(i.sheenRoughness=this.sheenRoughness),this.emissive&&this.emissive.isColor&&(i.emissive=this.emissive.getHex()),this.emissiveIntensity!==void 0&&(i.emissiveIntensity=this.emissiveIntensity),this.specular&&this.specular.isColor&&(i.specular=this.specular.getHex()),this.specularIntensity!==void 0&&(i.specularIntensity=this.specularIntensity),this.specularColor&&this.specularColor.isColor&&(i.specularColor=this.specularColor.getHex()),this.shininess!==void 0&&(i.shininess=this.shininess),this.clearcoat!==void 0&&(i.clearcoat=this.clearcoat),this.clearcoatRoughness!==void 0&&(i.clearcoatRoughness=this.clearcoatRoughness),this.diffuseRoughness!==void 0&&(i.diffuseRoughness=this.diffuseRoughness),this.diffuseRoughnessMap&&this.diffuseRoughnessMap.isTexture&&(i.diffuseRoughnessMap=this.diffuseRoughnessMap.toJSON(t).uuid),this.clearcoatMap&&this.clearcoatMap.isTexture&&(i.clearcoatMap=this.clearcoatMap.toJSON(t).uuid),this.clearcoatRoughnessMap&&this.clearcoatRoughnessMap.isTexture&&(i.clearcoatRoughnessMap=this.clearcoatRoughnessMap.toJSON(t).uuid),this.clearcoatNormalMap&&this.clearcoatNormalMap.isTexture&&(i.clearcoatNormalMap=this.clearcoatNormalMap.toJSON(t).uuid,i.clearcoatNormalScale=this.clearcoatNormalScale.toArray()),this.sheenColorMap&&this.sheenColorMap.isTexture&&(i.sheenColorMap=this.sheenColorMap.toJSON(t).uuid),this.sheenRoughnessMap&&this.sheenRoughnessMap.isTexture&&(i.sheenRoughnessMap=this.sheenRoughnessMap.toJSON(t).uuid),this.dispersion!==void 0&&(i.dispersion=this.dispersion),this.retroreflectivity!==void 0&&(i.retroreflectivity=this.retroreflectivity),this.iridescence!==void 0&&(i.iridescence=this.iridescence),this.iridescenceIOR!==void 0&&(i.iridescenceIOR=this.iridescenceIOR),this.iridescenceThicknessMinimum!==void 0&&(i.iridescenceThicknessMinimum=this.iridescenceThicknessMinimum),this.iridescenceThicknessMaximum!==void 0&&(i.iridescenceThicknessMaximum=this.iridescenceThicknessMaximum),this.iridescenceMap&&this.iridescenceMap.isTexture&&(i.iridescenceMap=this.iridescenceMap.toJSON(t).uuid),this.iridescenceThicknessMap&&this.iridescenceThicknessMap.isTexture&&(i.iridescenceThicknessMap=this.iridescenceThicknessMap.toJSON(t).uuid),this.anisotropy!==void 0&&(i.anisotropy=this.anisotropy),this.anisotropyRotation!==void 0&&(i.anisotropyRotation=this.anisotropyRotation),this.anisotropyMap&&this.anisotropyMap.isTexture&&(i.anisotropyMap=this.anisotropyMap.toJSON(t).uuid),this.map&&this.map.isTexture&&(i.map=this.map.toJSON(t).uuid),this.matcap&&this.matcap.isTexture&&(i.matcap=this.matcap.toJSON(t).uuid),this.alphaMap&&this.alphaMap.isTexture&&(i.alphaMap=this.alphaMap.toJSON(t).uuid),this.lightMap&&this.lightMap.isTexture&&(i.lightMap=this.lightMap.toJSON(t).uuid,i.lightMapIntensity=this.lightMapIntensity),this.aoMap&&this.aoMap.isTexture&&(i.aoMap=this.aoMap.toJSON(t).uuid,i.aoMapIntensity=this.aoMapIntensity),this.bumpMap&&this.bumpMap.isTexture&&(i.bumpMap=this.bumpMap.toJSON(t).uuid,i.bumpScale=this.bumpScale),this.normalMap&&this.normalMap.isTexture&&(i.normalMap=this.normalMap.toJSON(t).uuid,i.normalMapType=this.normalMapType,i.normalScale=this.normalScale.toArray()),this.displacementMap&&this.displacementMap.isTexture&&(i.displacementMap=this.displacementMap.toJSON(t).uuid,i.displacementScale=this.displacementScale,i.displacementBias=this.displacementBias),this.roughnessMap&&this.roughnessMap.isTexture&&(i.roughnessMap=this.roughnessMap.toJSON(t).uuid),this.metalnessMap&&this.metalnessMap.isTexture&&(i.metalnessMap=this.metalnessMap.toJSON(t).uuid),this.emissiveMap&&this.emissiveMap.isTexture&&(i.emissiveMap=this.emissiveMap.toJSON(t).uuid),this.specularMap&&this.specularMap.isTexture&&(i.specularMap=this.specularMap.toJSON(t).uuid),this.specularIntensityMap&&this.specularIntensityMap.isTexture&&(i.specularIntensityMap=this.specularIntensityMap.toJSON(t).uuid),this.specularColorMap&&this.specularColorMap.isTexture&&(i.specularColorMap=this.specularColorMap.toJSON(t).uuid),this.envMap&&this.envMap.isTexture&&(i.envMap=this.envMap.toJSON(t).uuid,this.combine!==void 0&&(i.combine=this.combine)),this.envMapRotation!==void 0&&(i.envMapRotation=this.envMapRotation.toArray()),this.envMapIntensity!==void 0&&(i.envMapIntensity=this.envMapIntensity),this.reflectivity!==void 0&&(i.reflectivity=this.reflectivity),this.refractionRatio!==void 0&&(i.refractionRatio=this.refractionRatio),this.gradientMap&&this.gradientMap.isTexture&&(i.gradientMap=this.gradientMap.toJSON(t).uuid),this.transmission!==void 0&&(i.transmission=this.transmission),this.transmissionMap&&this.transmissionMap.isTexture&&(i.transmissionMap=this.transmissionMap.toJSON(t).uuid),this.thickness!==void 0&&(i.thickness=this.thickness),this.thicknessMap&&this.thicknessMap.isTexture&&(i.thicknessMap=this.thicknessMap.toJSON(t).uuid),this.attenuationDistance!==void 0&&(i.attenuationDistance=this.attenuationDistance),this.attenuationColor!==void 0&&(i.attenuationColor=this.attenuationColor.getHex()),this.size!==void 0&&(i.size=this.size),this.sizeAttenuation!==void 0&&(i.sizeAttenuation=this.sizeAttenuation),Array.isArray(this.clippingPlanes)&&this.clippingPlanes.length>0&&(i.clippingPlanes=this.clippingPlanes.map(r=>r.toJSON())),this.rotation!==void 0&&(i.rotation=this.rotation),this.depthPacking!==void 0&&(i.depthPacking=this.depthPacking),this.linewidth!==void 0&&(i.linewidth=this.linewidth),this.linecap!==void 0&&(i.linecap=this.linecap),this.linejoin!==void 0&&(i.linejoin=this.linejoin),this.dashSize!==void 0&&(i.dashSize=this.dashSize),this.gapSize!==void 0&&(i.gapSize=this.gapSize),this.scale!==void 0&&(i.scale=this.scale),this.wireframe!==void 0&&(i.wireframe=this.wireframe),this.wireframeLinewidth!==void 0&&(i.wireframeLinewidth=this.wireframeLinewidth),this.wireframeLinecap!==void 0&&(i.wireframeLinecap=this.wireframeLinecap),this.wireframeLinejoin!==void 0&&(i.wireframeLinejoin=this.wireframeLinejoin),this.flatShading!==void 0&&(i.flatShading=this.flatShading),this.fog!==void 0&&(i.fog=this.fog),Object.keys(this.userData).length>0&&(i.userData=this.userData);function s(r){let a=[];for(let o in r){let l=r[o];delete l.metadata,a.push(l)}return a}if(e){let r=s(t.textures),a=s(t.images);r.length>0&&(i.textures=r),a.length>0&&(i.images=a)}return i}fromJSON(t,e){if(t.uuid!==void 0&&(this.uuid=t.uuid),t.name!==void 0&&(this.name=t.name),t.color!==void 0&&this.color!==void 0&&this.color.setHex(t.color),t.roughness!==void 0&&(this.roughness=t.roughness),t.metalness!==void 0&&(this.metalness=t.metalness),t.sheen!==void 0&&(this.sheen=t.sheen),t.sheenColor!==void 0&&(this.sheenColor=new Mt().setHex(t.sheenColor)),t.sheenRoughness!==void 0&&(this.sheenRoughness=t.sheenRoughness),t.emissive!==void 0&&this.emissive!==void 0&&this.emissive.setHex(t.emissive),t.specular!==void 0&&this.specular!==void 0&&this.specular.setHex(t.specular),t.specularIntensity!==void 0&&(this.specularIntensity=t.specularIntensity),t.specularColor!==void 0&&this.specularColor!==void 0&&this.specularColor.setHex(t.specularColor),t.shininess!==void 0&&(this.shininess=t.shininess),t.clearcoat!==void 0&&(this.clearcoat=t.clearcoat),t.clearcoatRoughness!==void 0&&(this.clearcoatRoughness=t.clearcoatRoughness),t.diffuseRoughness!==void 0&&(this.diffuseRoughness=t.diffuseRoughness),t.dispersion!==void 0&&(this.dispersion=t.dispersion),t.retroreflectivity!==void 0&&(this.retroreflectivity=t.retroreflectivity),t.iridescence!==void 0&&(this.iridescence=t.iridescence),t.iridescenceIOR!==void 0&&(this.iridescenceIOR=t.iridescenceIOR),t.iridescenceThicknessMinimum!==void 0&&(this.iridescenceThicknessMinimum=t.iridescenceThicknessMinimum),t.iridescenceThicknessMaximum!==void 0&&(this.iridescenceThicknessMaximum=t.iridescenceThicknessMaximum),t.iridescenceThicknessRange!==void 0&&(this.iridescenceThicknessMinimum=t.iridescenceThicknessRange[0],this.iridescenceThicknessMaximum=t.iridescenceThicknessRange[1]),t.transmission!==void 0&&(this.transmission=t.transmission),t.thickness!==void 0&&(this.thickness=t.thickness),t.attenuationDistance!==void 0&&(this.attenuationDistance=t.attenuationDistance),t.attenuationColor!==void 0&&this.attenuationColor!==void 0&&this.attenuationColor.setHex(t.attenuationColor),t.anisotropy!==void 0&&(this.anisotropy=t.anisotropy),t.anisotropyRotation!==void 0&&(this.anisotropyRotation=t.anisotropyRotation),t.fog!==void 0&&(this.fog=t.fog),t.flatShading!==void 0&&(this.flatShading=t.flatShading),t.blending!==void 0&&(this.blending=t.blending),t.combine!==void 0&&(this.combine=t.combine),t.side!==void 0&&(this.side=t.side),t.shadowSide!==void 0&&(this.shadowSide=t.shadowSide),t.opacity!==void 0&&(this.opacity=t.opacity),t.transparent!==void 0&&(this.transparent=t.transparent),t.alphaTest!==void 0&&(this.alphaTest=t.alphaTest),t.alphaHash!==void 0&&(this.alphaHash=t.alphaHash),t.depthFunc!==void 0&&(this.depthFunc=t.depthFunc),t.depthTest!==void 0&&(this.depthTest=t.depthTest),t.depthWrite!==void 0&&(this.depthWrite=t.depthWrite),t.colorWrite!==void 0&&(this.colorWrite=t.colorWrite),t.clippingPlanes!==void 0&&(this.clippingPlanes=t.clippingPlanes.map(i=>new Ke().fromJSON(i))),t.clipIntersection!==void 0&&(this.clipIntersection=t.clipIntersection),t.clipShadows!==void 0&&(this.clipShadows=t.clipShadows),t.depthPacking!==void 0&&(this.depthPacking=t.depthPacking),t.blendSrc!==void 0&&(this.blendSrc=t.blendSrc),t.blendDst!==void 0&&(this.blendDst=t.blendDst),t.blendEquation!==void 0&&(this.blendEquation=t.blendEquation),t.blendSrcAlpha!==void 0&&(this.blendSrcAlpha=t.blendSrcAlpha),t.blendDstAlpha!==void 0&&(this.blendDstAlpha=t.blendDstAlpha),t.blendEquationAlpha!==void 0&&(this.blendEquationAlpha=t.blendEquationAlpha),t.blendColor!==void 0&&this.blendColor!==void 0&&this.blendColor.setHex(t.blendColor),t.blendAlpha!==void 0&&(this.blendAlpha=t.blendAlpha),t.stencilWriteMask!==void 0&&(this.stencilWriteMask=t.stencilWriteMask),t.stencilFunc!==void 0&&(this.stencilFunc=t.stencilFunc),t.stencilRef!==void 0&&(this.stencilRef=t.stencilRef),t.stencilFuncMask!==void 0&&(this.stencilFuncMask=t.stencilFuncMask),t.stencilFail!==void 0&&(this.stencilFail=t.stencilFail),t.stencilZFail!==void 0&&(this.stencilZFail=t.stencilZFail),t.stencilZPass!==void 0&&(this.stencilZPass=t.stencilZPass),t.stencilWrite!==void 0&&(this.stencilWrite=t.stencilWrite),t.wireframe!==void 0&&(this.wireframe=t.wireframe),t.wireframeLinewidth!==void 0&&(this.wireframeLinewidth=t.wireframeLinewidth),t.wireframeLinecap!==void 0&&(this.wireframeLinecap=t.wireframeLinecap),t.wireframeLinejoin!==void 0&&(this.wireframeLinejoin=t.wireframeLinejoin),t.rotation!==void 0&&(this.rotation=t.rotation),t.linewidth!==void 0&&(this.linewidth=t.linewidth),t.linecap!==void 0&&(this.linecap=t.linecap),t.linejoin!==void 0&&(this.linejoin=t.linejoin),t.dashSize!==void 0&&(this.dashSize=t.dashSize),t.gapSize!==void 0&&(this.gapSize=t.gapSize),t.scale!==void 0&&(this.scale=t.scale),t.polygonOffset!==void 0&&(this.polygonOffset=t.polygonOffset),t.polygonOffsetFactor!==void 0&&(this.polygonOffsetFactor=t.polygonOffsetFactor),t.polygonOffsetUnits!==void 0&&(this.polygonOffsetUnits=t.polygonOffsetUnits),t.dithering!==void 0&&(this.dithering=t.dithering),t.alphaToCoverage!==void 0&&(this.alphaToCoverage=t.alphaToCoverage),t.premultipliedAlpha!==void 0&&(this.premultipliedAlpha=t.premultipliedAlpha),t.forceSinglePass!==void 0&&(this.forceSinglePass=t.forceSinglePass),t.allowOverride!==void 0&&(this.allowOverride=t.allowOverride),t.visible!==void 0&&(this.visible=t.visible),t.toneMapped!==void 0&&(this.toneMapped=t.toneMapped),t.userData!==void 0&&(this.userData=t.userData),t.vertexColors!==void 0&&(typeof t.vertexColors=="number"?this.vertexColors=t.vertexColors>0:this.vertexColors=t.vertexColors),t.size!==void 0&&(this.size=t.size),t.sizeAttenuation!==void 0&&(this.sizeAttenuation=t.sizeAttenuation),t.map!==void 0&&(this.map=e[t.map]||null),t.matcap!==void 0&&(this.matcap=e[t.matcap]||null),t.alphaMap!==void 0&&(this.alphaMap=e[t.alphaMap]||null),t.bumpMap!==void 0&&(this.bumpMap=e[t.bumpMap]||null),t.bumpScale!==void 0&&(this.bumpScale=t.bumpScale),t.normalMap!==void 0&&(this.normalMap=e[t.normalMap]||null),t.normalMapType!==void 0&&(this.normalMapType=t.normalMapType),t.normalScale!==void 0){let i=t.normalScale;Array.isArray(i)===!1&&(i=[i,i]),this.normalScale=new et().fromArray(i)}return t.displacementMap!==void 0&&(this.displacementMap=e[t.displacementMap]||null),t.displacementScale!==void 0&&(this.displacementScale=t.displacementScale),t.displacementBias!==void 0&&(this.displacementBias=t.displacementBias),t.roughnessMap!==void 0&&(this.roughnessMap=e[t.roughnessMap]||null),t.metalnessMap!==void 0&&(this.metalnessMap=e[t.metalnessMap]||null),t.emissiveMap!==void 0&&(this.emissiveMap=e[t.emissiveMap]||null),t.emissiveIntensity!==void 0&&(this.emissiveIntensity=t.emissiveIntensity),t.specularMap!==void 0&&(this.specularMap=e[t.specularMap]||null),t.specularIntensityMap!==void 0&&(this.specularIntensityMap=e[t.specularIntensityMap]||null),t.specularColorMap!==void 0&&(this.specularColorMap=e[t.specularColorMap]||null),t.envMap!==void 0&&(this.envMap=e[t.envMap]||null),t.envMapRotation!==void 0&&this.envMapRotation.fromArray(t.envMapRotation),t.envMapIntensity!==void 0&&(this.envMapIntensity=t.envMapIntensity),t.reflectivity!==void 0&&(this.reflectivity=t.reflectivity),t.refractionRatio!==void 0&&(this.refractionRatio=t.refractionRatio),t.lightMap!==void 0&&(this.lightMap=e[t.lightMap]||null),t.lightMapIntensity!==void 0&&(this.lightMapIntensity=t.lightMapIntensity),t.aoMap!==void 0&&(this.aoMap=e[t.aoMap]||null),t.aoMapIntensity!==void 0&&(this.aoMapIntensity=t.aoMapIntensity),t.gradientMap!==void 0&&(this.gradientMap=e[t.gradientMap]||null),t.clearcoatMap!==void 0&&(this.clearcoatMap=e[t.clearcoatMap]||null),t.clearcoatRoughnessMap!==void 0&&(this.clearcoatRoughnessMap=e[t.clearcoatRoughnessMap]||null),t.clearcoatNormalMap!==void 0&&(this.clearcoatNormalMap=e[t.clearcoatNormalMap]||null),t.clearcoatNormalScale!==void 0&&(this.clearcoatNormalScale=new et().fromArray(t.clearcoatNormalScale)),t.diffuseRoughnessMap!==void 0&&(this.diffuseRoughnessMap=e[t.diffuseRoughnessMap]||null),t.iridescenceMap!==void 0&&(this.iridescenceMap=e[t.iridescenceMap]||null),t.iridescenceThicknessMap!==void 0&&(this.iridescenceThicknessMap=e[t.iridescenceThicknessMap]||null),t.transmissionMap!==void 0&&(this.transmissionMap=e[t.transmissionMap]||null),t.thicknessMap!==void 0&&(this.thicknessMap=e[t.thicknessMap]||null),t.anisotropyMap!==void 0&&(this.anisotropyMap=e[t.anisotropyMap]||null),t.sheenColorMap!==void 0&&(this.sheenColorMap=e[t.sheenColorMap]||null),t.sheenRoughnessMap!==void 0&&(this.sheenRoughnessMap=e[t.sheenRoughnessMap]||null),this}clone(){return new this.constructor().copy(this)}copy(t){this.name=t.name,this.blending=t.blending,this.side=t.side,this.vertexColors=t.vertexColors,this.opacity=t.opacity,this.transparent=t.transparent,this.blendSrc=t.blendSrc,this.blendDst=t.blendDst,this.blendEquation=t.blendEquation,this.blendSrcAlpha=t.blendSrcAlpha,this.blendDstAlpha=t.blendDstAlpha,this.blendEquationAlpha=t.blendEquationAlpha,this.blendColor.copy(t.blendColor),this.blendAlpha=t.blendAlpha,this.depthFunc=t.depthFunc,this.depthTest=t.depthTest,this.depthWrite=t.depthWrite,this.stencilWriteMask=t.stencilWriteMask,this.stencilFunc=t.stencilFunc,this.stencilRef=t.stencilRef,this.stencilFuncMask=t.stencilFuncMask,this.stencilFail=t.stencilFail,this.stencilZFail=t.stencilZFail,this.stencilZPass=t.stencilZPass,this.stencilWrite=t.stencilWrite;let e=t.clippingPlanes,i=null;if(e!==null){let s=e.length;i=new Array(s);for(let r=0;r!==s;++r)i[r]=e[r].clone()}return this.clippingPlanes=i,this.clipIntersection=t.clipIntersection,this.clipShadows=t.clipShadows,this.shadowSide=t.shadowSide,this.colorWrite=t.colorWrite,this.precision=t.precision,this.polygonOffset=t.polygonOffset,this.polygonOffsetFactor=t.polygonOffsetFactor,this.polygonOffsetUnits=t.polygonOffsetUnits,this.dithering=t.dithering,this.alphaTest=t.alphaTest,this.alphaHash=t.alphaHash,this.alphaToCoverage=t.alphaToCoverage,this.premultipliedAlpha=t.premultipliedAlpha,this.forceSinglePass=t.forceSinglePass,this.allowOverride=t.allowOverride,this.visible=t.visible,this.toneMapped=t.toneMapped,this.userData=JSON.parse(JSON.stringify(t.userData)),this}dispose(){this.dispatchEvent({type:"dispose"})}set needsUpdate(t){t===!0&&this.version++}};var Ji=new A,dc=new A,Ea=new A,Ta=new A,tn=class{constructor(t=new A,e=new A(0,0,-1)){this.origin=t,this.direction=e}set(t,e){return this.origin.copy(t),this.direction.copy(e),this}copy(t){return this.origin.copy(t.origin),this.direction.copy(t.direction),this}at(t,e){return e.copy(this.origin).addScaledVector(this.direction,t)}lookAt(t){return this.direction.copy(t).sub(this.origin).normalize(),this}recast(t){return this.origin.copy(this.at(t,Ji)),this}closestPointToPoint(t,e){e.subVectors(t,this.origin);let i=e.dot(this.direction);return i<0?e.copy(this.origin):e.copy(this.origin).addScaledVector(this.direction,i)}distanceToPoint(t){return Math.sqrt(this.distanceSqToPoint(t))}distanceSqToPoint(t){let e=Ji.subVectors(t,this.origin).dot(this.direction);return e<0?this.origin.distanceToSquared(t):(Ji.copy(this.origin).addScaledVector(this.direction,e),Ji.distanceToSquared(t))}distanceSqToSegment(t,e,i,s){dc.copy(t).add(e).multiplyScalar(.5),Ea.copy(e).sub(t).normalize(),Ta.copy(this.origin).sub(dc);let r=t.distanceTo(e)*.5,a=-this.direction.dot(Ea),o=Ta.dot(this.direction),l=-Ta.dot(Ea),c=Ta.lengthSq(),h=Math.abs(1-a*a),u,d,f,p;if(h>0)if(u=a*l-o,d=a*o-l,p=r*h,u>=0)if(d>=-p)if(d<=p){let v=1/h;u*=v,d*=v,f=u*(u+a*d+2*o)+d*(a*u+d+2*l)+c}else d=r,u=Math.max(0,-(a*d+o)),f=-u*u+d*(d+2*l)+c;else d=-r,u=Math.max(0,-(a*d+o)),f=-u*u+d*(d+2*l)+c;else d<=-p?(u=Math.max(0,-(-a*r+o)),d=u>0?-r:Math.min(Math.max(-r,-l),r),f=-u*u+d*(d+2*l)+c):d<=p?(u=0,d=Math.min(Math.max(-r,-l),r),f=d*(d+2*l)+c):(u=Math.max(0,-(a*r+o)),d=u>0?r:Math.min(Math.max(-r,-l),r),f=-u*u+d*(d+2*l)+c);else d=a>0?-r:r,u=Math.max(0,-(a*d+o)),f=-u*u+d*(d+2*l)+c;return i&&i.copy(this.origin).addScaledVector(this.direction,u),s&&s.copy(dc).addScaledVector(Ea,d),f}intersectSphere(t,e){if(t.radius<0)return null;Ji.subVectors(t.center,this.origin);let i=Ji.dot(this.direction),s=Ji.dot(Ji)-i*i,r=t.radius*t.radius;if(s>r)return null;let a=Math.sqrt(r-s),o=i-a,l=i+a;return l<0?null:o<0?this.at(l,e):this.at(o,e)}intersectsSphere(t){return t.radius<0?!1:this.distanceSqToPoint(t.center)<=t.radius*t.radius}distanceToPlane(t){let e=t.normal.dot(this.direction);if(e===0)return t.distanceToPoint(this.origin)===0?0:null;let i=-(this.origin.dot(t.normal)+t.constant)/e;return i>=0?i:null}intersectPlane(t,e){let i=this.distanceToPlane(t);return i===null?null:this.at(i,e)}intersectsPlane(t){let e=t.distanceToPoint(this.origin);return e===0||t.normal.dot(this.direction)*e<0}intersectBox(t,e){let i,s,r,a,o,l,c=1/this.direction.x,h=1/this.direction.y,u=1/this.direction.z,d=this.origin;return c>=0?(i=(t.min.x-d.x)*c,s=(t.max.x-d.x)*c):(i=(t.max.x-d.x)*c,s=(t.min.x-d.x)*c),h>=0?(r=(t.min.y-d.y)*h,a=(t.max.y-d.y)*h):(r=(t.max.y-d.y)*h,a=(t.min.y-d.y)*h),i>a||r>s||((r>i||isNaN(i))&&(i=r),(a<s||isNaN(s))&&(s=a),u>=0?(o=(t.min.z-d.z)*u,l=(t.max.z-d.z)*u):(o=(t.max.z-d.z)*u,l=(t.min.z-d.z)*u),i>l||o>s)||((o>i||i!==i)&&(i=o),(l<s||s!==s)&&(s=l),s<0)?null:this.at(i>=0?i:s,e)}intersectsBox(t){return this.intersectBox(t,Ji)!==null}intersectTriangle(t,e,i,s,r){let a=this.origin,o=this.direction,l=o.x,c=o.y,h=o.z,u=t.x-a.x,d=t.y-a.y,f=t.z-a.z,p=e.x-a.x,v=e.y-a.y,g=e.z-a.z,m=i.x-a.x,_=i.y-a.y,S=i.z-a.z,M=Math.abs(l),T=Math.abs(c),w=Math.abs(h),x,y,P,C,I,L,F,D,B,G,k,j;if(M>=T&&M>=w?(P=l,L=u,B=p,j=m,l>=0?(x=c,y=h,C=d,I=f,F=v,D=g,G=_,k=S):(x=h,y=c,C=f,I=d,F=g,D=v,G=S,k=_)):T>=w?(P=c,L=d,B=v,j=_,c>=0?(x=h,y=l,C=f,I=u,F=g,D=p,G=S,k=m):(x=l,y=h,C=u,I=f,F=p,D=g,G=m,k=S)):(P=h,L=f,B=g,j=S,h>=0?(x=l,y=c,C=u,I=d,F=p,D=v,G=m,k=_):(x=c,y=l,C=d,I=u,F=v,D=p,G=_,k=m)),P===0)return null;let W=x/P,K=y/P,Q=1/P,dt=C-W*L,yt=I-K*L,q=F-W*B,at=D-K*B,gt=G-W*j,Et=k-K*j,rt=gt*at-Et*q,Dt=dt*Et-yt*gt,se=q*yt-at*dt;if(s){if(rt<0||Dt<0||se<0)return null}else if((rt<0||Dt<0||se<0)&&(rt>0||Dt>0||se>0))return null;let Rt=rt+Dt+se;if(Rt===0)return null;let Ht=Q*(rt*L+Dt*B+se*j);return(Rt>0?Ht<0:Ht>0)?null:this.at(Ht/Rt,r)}applyMatrix4(t){return this.origin.applyMatrix4(t),this.direction.transformDirection(t),this}equals(t){return t.origin.equals(this.origin)&&t.direction.equals(this.direction)}clone(){return new this.constructor().copy(this)}},_i=class extends Ci{constructor(t){super(),this.isMeshBasicMaterial=!0,this.type="MeshBasicMaterial",this.color=new Mt(16777215),this.map=null,this.lightMap=null,this.lightMapIntensity=1,this.aoMap=null,this.aoMapIntensity=1,this.specularMap=null,this.alphaMap=null,this.envMap=null,this.envMapRotation=new gi,this.combine=Ro,this.reflectivity=1,this.refractionRatio=.98,this.wireframe=!1,this.wireframeLinewidth=1,this.wireframeLinecap="round",this.wireframeLinejoin="round",this.fog=!0,this.setValues(t)}copy(t){return super.copy(t),this.color.copy(t.color),this.map=t.map,this.lightMap=t.lightMap,this.lightMapIntensity=t.lightMapIntensity,this.aoMap=t.aoMap,this.aoMapIntensity=t.aoMapIntensity,this.specularMap=t.specularMap,this.alphaMap=t.alphaMap,this.envMap=t.envMap,this.envMapRotation.copy(t.envMapRotation),this.combine=t.combine,this.reflectivity=t.reflectivity,this.refractionRatio=t.refractionRatio,this.wireframe=t.wireframe,this.wireframeLinewidth=t.wireframeLinewidth,this.wireframeLinecap=t.wireframeLinecap,this.wireframeLinejoin=t.wireframeLinejoin,this.fog=t.fog,this}},du=new Gt,Ln=new tn,Aa=new $e,fu=new A,Ra=new A,Ca=new A,Pa=new A,fc=new A,Ia=new A,pu=new A,Da=new A,Ut=class extends de{constructor(t=new ee,e=new _i){super(),this.isMesh=!0,this.type="Mesh",this.geometry=t,this.material=e,this.morphTargetDictionary=void 0,this.morphTargetInfluences=void 0,this.count=1,this.updateMorphTargets()}copy(t,e){return super.copy(t,e),t.morphTargetInfluences!==void 0?this.morphTargetInfluences=t.morphTargetInfluences.slice():this.morphTargetInfluences=void 0,t.morphTargetDictionary!==void 0?this.morphTargetDictionary=Object.assign({},t.morphTargetDictionary):this.morphTargetDictionary=void 0,this.material=Array.isArray(t.material)?t.material.slice():t.material,this.geometry=t.geometry,this}updateMorphTargets(){let e=this.geometry.morphAttributes,i=Object.keys(e);if(i.length>0){let s=e[i[0]];if(s!==void 0){this.morphTargetInfluences=[],this.morphTargetDictionary={};for(let r=0,a=s.length;r<a;r++){let o=s[r].name||String(r);this.morphTargetInfluences.push(0),this.morphTargetDictionary[o]=r}}}}getVertexPosition(t,e){let i=this.geometry,s=i.attributes.position,r=i.morphAttributes.position,a=i.morphTargetsRelative;e.fromBufferAttribute(s,t);let o=this.morphTargetInfluences;if(r&&o){Ia.set(0,0,0);for(let l=0,c=r.length;l<c;l++){let h=o[l],u=r[l];h!==0&&(fc.fromBufferAttribute(u,t),a?Ia.addScaledVector(fc,h):Ia.addScaledVector(fc.sub(e),h))}e.add(Ia)}return e}intersectsFrustum(t){return t.intersectsObject(this)}raycast(t,e){let i=this.geometry,s=this.material,r=this.matrixWorld;s!==void 0&&(i.boundingSphere===null&&i.computeBoundingSphere(),Aa.copy(i.boundingSphere),Aa.applyMatrix4(r),Ln.copy(t.ray).recast(t.near),!(Aa.containsPoint(Ln.origin)===!1&&(Ln.intersectSphere(Aa,fu)===null||Ln.origin.distanceToSquared(fu)>(t.far-t.near)**2))&&(du.copy(r).invert(),Ln.copy(t.ray).applyMatrix4(du),!(i.boundingBox!==null&&Ln.intersectsBox(i.boundingBox)===!1)&&this._computeIntersections(t,e,Ln)))}_computeIntersections(t,e,i){let s,r=this.geometry,a=this.material,o=r.index,l=r.attributes.position,c=r.attributes.uv,h=r.attributes.uv1,u=r.attributes.normal,d=r.groups,f=r.drawRange;if(o!==null)if(Array.isArray(a))for(let p=0,v=d.length;p<v;p++){let g=d[p],m=a[g.materialIndex],_=Math.max(g.start,f.start),S=Math.min(o.count,Math.min(g.start+g.count,f.start+f.count));for(let M=_,T=S;M<T;M+=3){let w=o.getX(M),x=o.getX(M+1),y=o.getX(M+2);s=La(this,m,t,i,c,h,u,w,x,y),s&&(s.faceIndex=Math.floor(M/3),s.face.materialIndex=g.materialIndex,e.push(s))}}else{let p=Math.max(0,f.start),v=Math.min(o.count,f.start+f.count);for(let g=p,m=v;g<m;g+=3){let _=o.getX(g),S=o.getX(g+1),M=o.getX(g+2);s=La(this,a,t,i,c,h,u,_,S,M),s&&(s.faceIndex=Math.floor(g/3),e.push(s))}}else if(l!==void 0)if(Array.isArray(a))for(let p=0,v=d.length;p<v;p++){let g=d[p],m=a[g.materialIndex],_=Math.max(g.start,f.start),S=Math.min(l.count,Math.min(g.start+g.count,f.start+f.count));for(let M=_,T=S;M<T;M+=3){let w=M,x=M+1,y=M+2;s=La(this,m,t,i,c,h,u,w,x,y),s&&(s.faceIndex=Math.floor(M/3),s.face.materialIndex=g.materialIndex,e.push(s))}}else{let p=Math.max(0,f.start),v=Math.min(l.count,f.start+f.count);for(let g=p,m=v;g<m;g+=3){let _=g,S=g+1,M=g+2;s=La(this,a,t,i,c,h,u,_,S,M),s&&(s.faceIndex=Math.floor(g/3),e.push(s))}}}};function Pp(n,t,e,i,s,r,a,o){let l;if(t.side===ke?l=i.intersectTriangle(a,r,s,!0,o):l=i.intersectTriangle(s,r,a,t.side===Mn,o),l===null)return null;Da.copy(o),Da.applyMatrix4(n.matrixWorld);let c=e.ray.origin.distanceTo(Da);return c<e.near||c>e.far?null:{distance:c,point:Da.clone(),object:n}}function La(n,t,e,i,s,r,a,o,l,c){n.getVertexPosition(o,Ra),n.getVertexPosition(l,Ca),n.getVertexPosition(c,Pa);let h=Pp(n,t,e,i,Ra,Ca,Pa,pu);if(h){let u=new A;$i.getBarycoord(pu,Ra,Ca,Pa,u),s&&(h.uv=$i.getInterpolatedAttribute(s,o,l,c,u,new et)),r&&(h.uv1=$i.getInterpolatedAttribute(r,o,l,c,u,new et)),a&&(h.normal=$i.getInterpolatedAttribute(a,o,l,c,u,new A),h.normal.dot(i.direction)>0&&h.normal.multiplyScalar(-1));let d={a:o,b:l,c,normal:new A,materialIndex:0};$i.getNormal(Ra,Ca,Pa,d.normal),h.face=d,h.barycoord=u}return h}var zn=class extends qe{constructor(t=null,e=1,i=1,s,r,a,o,l,c=Se,h=Se,u,d){super(null,a,o,l,c,h,s,r,u,d),this.isDataTexture=!0,this.image={data:t,width:e,height:i},this.generateMipmaps=!1,this.flipY=!1,this.unpackAlignment=1}};var Mr=class extends _e{constructor(t,e,i,s=1){super(t,e,i),this.isInstancedBufferAttribute=!0,this.meshPerAttribute=s}copy(t){return super.copy(t),this.meshPerAttribute=t.meshPerAttribute,this}toJSON(){let t=super.toJSON();return t.meshPerAttribute=this.meshPerAttribute,t.isInstancedBufferAttribute=!0,t}},us=new Gt,mu=new Gt,Na=[],gu=new xi,Ip=new Gt,lr=new Ut,cr=new $e,kn=class extends Ut{constructor(t,e,i){super(t,e),this.isInstancedMesh=!0,this.instanceMatrix=new Mr(new Float32Array(i*16),16),this.instanceColor=null,this.morphTexture=null,this.count=i,this.boundingBox=null,this.boundingSphere=null;for(let s=0;s<i;s++)this.setMatrixAt(s,Ip)}computeBoundingBox(){let t=this.geometry,e=this.count;this.boundingBox===null&&(this.boundingBox=new xi),t.boundingBox===null&&t.computeBoundingBox(),this.boundingBox.makeEmpty();for(let i=0;i<e;i++)this.getMatrixAt(i,us),gu.copy(t.boundingBox).applyMatrix4(us),this.boundingBox.union(gu)}computeBoundingSphere(){let t=this.geometry,e=this.count;this.boundingSphere===null&&(this.boundingSphere=new $e),t.boundingSphere===null&&t.computeBoundingSphere(),this.boundingSphere.makeEmpty();for(let i=0;i<e;i++)this.getMatrixAt(i,us),cr.copy(t.boundingSphere).applyMatrix4(us),this.boundingSphere.union(cr)}copy(t,e){return super.copy(t,e),this.instanceMatrix.copy(t.instanceMatrix),this.morphTexture=t.morphTexture!==null?t.morphTexture.clone():null,this.instanceColor=t.instanceColor!==null?t.instanceColor.clone():null,this.count=t.count,this.boundingBox=t.boundingBox!==null?t.boundingBox.clone():null,this.boundingSphere=t.boundingSphere!==null?t.boundingSphere.clone():null,this}getColorAt(t,e){return this.instanceColor===null?e.setRGB(1,1,1):e.fromArray(this.instanceColor.array,t*3)}getMatrixAt(t,e){return e.fromArray(this.instanceMatrix.array,t*16)}getMorphAt(t,e){let i=e.morphTargetInfluences,s=this.morphTexture.source.data.data,r=i.length+1,a=t*r+1;for(let o=0;o<i.length;o++)i[o]=s[a+o]}raycast(t,e){let i=this.matrixWorld,s=this.count;if(lr.geometry=this.geometry,lr.material=this.material,lr.material!==void 0&&(this.boundingSphere===null&&this.computeBoundingSphere(),cr.copy(this.boundingSphere),cr.applyMatrix4(i),t.ray.intersectsSphere(cr)!==!1))for(let r=0;r<s;r++){this.getMatrixAt(r,us),mu.multiplyMatrices(i,us),lr.matrixWorld=mu,lr.raycast(t,Na);for(let a=0,o=Na.length;a<o;a++){let l=Na[a];l.instanceId=r,l.object=this,e.push(l)}Na.length=0}}setColorAt(t,e){return this.instanceColor===null&&(this.instanceColor=new Mr(new Float32Array(this.instanceMatrix.count*3).fill(1),3)),e.toArray(this.instanceColor.array,t*3),this}setMatrixAt(t,e){return e.toArray(this.instanceMatrix.array,t*16),this}setMorphAt(t,e){let i=e.morphTargetInfluences,s=i.length+1;this.morphTexture===null&&(this.morphTexture=new zn(new Float32Array(s*this.count),s,this.count,Os,yi));let r=this.morphTexture.source.data.data,a=0;for(let c=0;c<i.length;c++)a+=i[c];let o=this.geometry.morphTargetsRelative?1:1-a,l=s*t;return r[l]=o,r.set(i,l+1),this}updateMorphTargets(){}dispose(){super.dispose(),this.instanceMatrix.dispose(),this.instanceColor!==null&&this.instanceColor.dispose(),this.morphTexture!==null&&(this.morphTexture.dispose(),this.morphTexture=null)}},Nn=new $e,Dp=new et(.5,.5),Ua=new A,mn=class{constructor(t=new Ke,e=new Ke,i=new Ke,s=new Ke,r=new Ke,a=new Ke){this.planes=[t,e,i,s,r,a]}set(t,e,i,s,r,a){let o=this.planes;return o[0].copy(t),o[1].copy(e),o[2].copy(i),o[3].copy(s),o[4].copy(r),o[5].copy(a),this}copy(t){let e=this.planes;for(let i=0;i<6;i++)e[i].copy(t.planes[i]);return this}setFromProjectionMatrix(t,e=mi,i=!1){let s=this.planes,r=t.elements,a=r[0],o=r[1],l=r[2],c=r[3],h=r[4],u=r[5],d=r[6],f=r[7],p=r[8],v=r[9],g=r[10],m=r[11],_=r[12],S=r[13],M=r[14],T=r[15];if(s[0].setComponents(c-a,f-h,m-p,T-_).normalize(),s[1].setComponents(c+a,f+h,m+p,T+_).normalize(),s[2].setComponents(c+o,f+u,m+v,T+S).normalize(),s[3].setComponents(c-o,f-u,m-v,T-S).normalize(),i)s[4].setComponents(l,d,g,M).normalize(),s[5].setComponents(c-l,f-d,m-g,T-M).normalize();else if(s[4].setComponents(c-l,f-d,m-g,T-M).normalize(),e===mi)s[5].setComponents(c+l,f+d,m+g,T+M).normalize();else if(e===_s)s[5].setComponents(l,d,g,M).normalize();else throw new Error("THREE.Frustum.setFromProjectionMatrix(): Invalid coordinate system: "+e);return this}intersectsObject(t){if(t.boundingSphere!==void 0)t.boundingSphere===null&&t.computeBoundingSphere(),Nn.copy(t.boundingSphere).applyMatrix4(t.matrixWorld);else{let e=t.geometry;e.boundingSphere===null&&e.computeBoundingSphere(),Nn.copy(e.boundingSphere).applyMatrix4(t.matrixWorld)}return this.intersectsSphere(Nn)}intersectsSprite(t){Nn.center.set(0,0,0);let e=Dp.distanceTo(t.center);return Nn.radius=.7071067811865476+e,Nn.applyMatrix4(t.matrixWorld),this.intersectsSphere(Nn)}intersectsSphere(t){let e=this.planes,i=t.center,s=-t.radius;for(let r=0;r<6;r++)if(e[r].distanceToPoint(i)<s)return!1;return!0}intersectsBox(t){let e=this.planes;for(let i=0;i<6;i++){let s=e[i];if(Ua.x=s.normal.x>0?t.max.x:t.min.x,Ua.y=s.normal.y>0?t.max.y:t.min.y,Ua.z=s.normal.z>0?t.max.z:t.min.z,s.distanceToPoint(Ua)<0)return!1}return!0}containsPoint(t){let e=this.planes;for(let i=0;i<6;i++)if(e[i].distanceToPoint(t)<0)return!1;return!0}clone(){return new this.constructor().copy(this)}},xu=new Gt,ro=class n{constructor(){this.coordinateSystem=mi,this._frustums=[],this._count=0}setFromArrayCamera(t){let e=t.cameras,i=this._frustums;for(let s=0;s<e.length;s++){let r=e[s];xu.multiplyMatrices(r.projectionMatrix,r.matrixWorldInverse),i[s]===void 0&&(i[s]=new mn),i[s].setFromProjectionMatrix(xu,r.coordinateSystem,r.reversedDepth)}return this._count=e.length,this}getFrustum(t){return this._frustums[t]}intersectsObject(t){let e=this._frustums;for(let i=0;i<this._count;i++)if(e[i].intersectsObject(t))return!0;return!1}intersectsSprite(t){let e=this._frustums;for(let i=0;i<this._count;i++)if(e[i].intersectsSprite(t))return!0;return!1}intersectsSphere(t){let e=this._frustums;for(let i=0;i<this._count;i++)if(e[i].intersectsSphere(t))return!0;return!1}intersectsBox(t){let e=this._frustums;for(let i=0;i<this._count;i++)if(e[i].intersectsBox(t))return!0;return!1}containsPoint(t){let e=this._frustums;for(let i=0;i<this._count;i++)if(e[i].containsPoint(t))return!0;return!1}copy(t){this.coordinateSystem=t.coordinateSystem;let e=this._frustums,i=t._frustums;for(let s=0;s<t._count;s++)e[s]===void 0&&(e[s]=new mn),e[s].copy(i[s]);return this._count=t._count,this}clone(){return new n().copy(this)}};var Pi=class extends Ci{constructor(t){super(),this.isLineBasicMaterial=!0,this.type="LineBasicMaterial",this.color=new Mt(16777215),this.map=null,this.linewidth=1,this.linecap="round",this.linejoin="round",this.fog=!0,this.setValues(t)}copy(t){return super.copy(t),this.color.copy(t.color),this.map=t.map,this.linewidth=t.linewidth,this.linecap=t.linecap,this.linejoin=t.linejoin,this.fog=t.fog,this}},ao=new A,oo=new A,_u=new Gt,hr=new tn,Fa=new $e,pc=new A,vu=new A,bs=class extends de{constructor(t=new ee,e=new Pi){super(),this.isLine=!0,this.type="Line",this.geometry=t,this.material=e,this.morphTargetDictionary=void 0,this.morphTargetInfluences=void 0,this.updateMorphTargets()}copy(t,e){return super.copy(t,e),t.morphTargetInfluences!==void 0?this.morphTargetInfluences=t.morphTargetInfluences.slice():this.morphTargetInfluences=void 0,t.morphTargetDictionary!==void 0?this.morphTargetDictionary=Object.assign({},t.morphTargetDictionary):this.morphTargetDictionary=void 0,this.material=Array.isArray(t.material)?t.material.slice():t.material,this.geometry=t.geometry,this}computeLineDistances(){let t=this.geometry;if(t.index===null){let e=t.attributes.position,i=[0];for(let s=1,r=e.count;s<r;s++)ao.fromBufferAttribute(e,s-1),oo.fromBufferAttribute(e,s),i[s]=i[s-1],i[s]+=ao.distanceTo(oo);t.setAttribute("lineDistance",new Kt(i,1))}else Pt("Line.computeLineDistances(): Computation only possible with non-indexed BufferGeometry.");return this}intersectsFrustum(t){return t.intersectsObject(this)}raycast(t,e){let i=this.geometry,s=this.matrixWorld,r=t.params.Line.threshold,a=i.drawRange;if(i.boundingSphere===null&&i.computeBoundingSphere(),Fa.copy(i.boundingSphere),Fa.applyMatrix4(s),Fa.radius+=r,t.ray.intersectsSphere(Fa)===!1)return;_u.copy(s).invert(),hr.copy(t.ray).applyMatrix4(_u);let o=r/((this.scale.x+this.scale.y+this.scale.z)/3),l=o*o,c=this.isLineSegments?2:1,h=i.index,d=i.attributes.position;if(h!==null){let f=Math.max(0,a.start),p=Math.min(h.count,a.start+a.count);for(let v=f,g=p-1;v<g;v+=c){let m=h.getX(v),_=h.getX(v+1),S=Oa(this,t,hr,l,m,_,v);S&&e.push(S)}if(this.isLineLoop){let v=h.getX(p-1),g=h.getX(f),m=Oa(this,t,hr,l,v,g,p-1);m&&e.push(m)}}else{let f=Math.max(0,a.start),p=Math.min(d.count,a.start+a.count);for(let v=f,g=p-1;v<g;v+=c){let m=Oa(this,t,hr,l,v,v+1,v);m&&e.push(m)}if(this.isLineLoop){let v=Oa(this,t,hr,l,p-1,f,p-1);v&&e.push(v)}}}updateMorphTargets(){let e=this.geometry.morphAttributes,i=Object.keys(e);if(i.length>0){let s=e[i[0]];if(s!==void 0){this.morphTargetInfluences=[],this.morphTargetDictionary={};for(let r=0,a=s.length;r<a;r++){let o=s[r].name||String(r);this.morphTargetInfluences.push(0),this.morphTargetDictionary[o]=r}}}}};function Oa(n,t,e,i,s,r,a){let o=n.geometry.attributes.position;if(ao.fromBufferAttribute(o,s),oo.fromBufferAttribute(o,r),e.distanceSqToSegment(ao,oo,pc,vu)>i)return;pc.applyMatrix4(n.matrixWorld);let c=t.ray.origin.distanceTo(pc);if(!(c<t.near||c>t.far))return{distance:c,point:vu.clone().applyMatrix4(n.matrixWorld),index:a,face:null,faceIndex:null,barycoord:null,object:n}}var yu=new A,Mu=new A,en=class extends bs{constructor(t,e){super(t,e),this.isLineSegments=!0,this.type="LineSegments"}computeLineDistances(){let t=this.geometry;if(t.index===null){let e=t.attributes.position,i=[];for(let s=0,r=e.count;s<r;s+=2)yu.fromBufferAttribute(e,s),Mu.fromBufferAttribute(e,s+1),i[s]=s===0?0:i[s-1],i[s+1]=i[s]+yu.distanceTo(Mu);t.setAttribute("lineDistance",new Kt(i,1))}else Pt("LineSegments.computeLineDistances(): Computation only possible with non-indexed BufferGeometry.");return this}};var ws=class extends Ci{constructor(t){super(),this.isPointsMaterial=!0,this.type="PointsMaterial",this.color=new Mt(16777215),this.map=null,this.alphaMap=null,this.size=1,this.sizeAttenuation=!0,this.fog=!0,this.setValues(t)}copy(t){return super.copy(t),this.color.copy(t.color),this.map=t.map,this.alphaMap=t.alphaMap,this.size=t.size,this.sizeAttenuation=t.sizeAttenuation,this.fog=t.fog,this}},Su=new Gt,wc=new tn,Ba=new $e,za=new A,Sr=class extends de{constructor(t=new ee,e=new ws){super(),this.isPoints=!0,this.type="Points",this.geometry=t,this.material=e,this.morphTargetDictionary=void 0,this.morphTargetInfluences=void 0,this.updateMorphTargets()}copy(t,e){return super.copy(t,e),t.morphTargetInfluences!==void 0?this.morphTargetInfluences=t.morphTargetInfluences.slice():this.morphTargetInfluences=void 0,t.morphTargetDictionary!==void 0?this.morphTargetDictionary=Object.assign({},t.morphTargetDictionary):this.morphTargetDictionary=void 0,this.material=Array.isArray(t.material)?t.material.slice():t.material,this.geometry=t.geometry,this}intersectsFrustum(t){return t.intersectsObject(this)}raycast(t,e){let i=this.geometry,s=this.matrixWorld,r=t.params.Points.threshold,a=i.drawRange;if(i.boundingSphere===null&&i.computeBoundingSphere(),Ba.copy(i.boundingSphere),Ba.applyMatrix4(s),Ba.radius+=r,t.ray.intersectsSphere(Ba)===!1)return;Su.copy(s).invert(),wc.copy(t.ray).applyMatrix4(Su);let o=r/((this.scale.x+this.scale.y+this.scale.z)/3),l=o*o,c=i.index,u=i.attributes.position;if(c!==null){let d=Math.max(0,a.start),f=Math.min(c.count,a.start+a.count);for(let p=d,v=f;p<v;p++){let g=c.getX(p);za.fromBufferAttribute(u,g),bu(za,g,l,s,t,e,this)}}else{let d=Math.max(0,a.start),f=Math.min(u.count,a.start+a.count);for(let p=d,v=f;p<v;p++)za.fromBufferAttribute(u,p),bu(za,p,l,s,t,e,this)}}updateMorphTargets(){let e=this.geometry.morphAttributes,i=Object.keys(e);if(i.length>0){let s=e[i[0]];if(s!==void 0){this.morphTargetInfluences=[],this.morphTargetDictionary={};for(let r=0,a=s.length;r<a;r++){let o=s[r].name||String(r);this.morphTargetInfluences.push(0),this.morphTargetDictionary[o]=r}}}}};function bu(n,t,e,i,s,r,a){let o=wc.distanceSqToPoint(n);if(o<e){let l=new A;wc.closestPointToPoint(n,l),l.applyMatrix4(i);let c=s.ray.origin.distanceTo(l);if(c<s.near||c>s.far)return;r.push({distance:c,distanceToRay:Math.sqrt(o),point:l,index:t,face:null,faceIndex:null,barycoord:null,object:a})}}var br=class extends qe{constructor(t,e){super({width:t,height:e}),this.isFramebufferTexture=!0,this.magFilter=Se,this.minFilter=Se,this.generateMipmaps=!1,this.needsUpdate=!0}copy(t){return super.copy(t),this.source=new Bn(Object.assign({},t.image)),this}};var wr=class extends qe{constructor(t=[],e=Jn,i,s,r,a,o,l,c,h){super(t,e,i,s,r,a,o,l,c,h),this.isCubeTexture=!0,this.flipY=!1}get images(){return this.image}set images(t){this.image=t}},Es=class extends qe{constructor(t,e,i,s,r,a,o,l,c){super(t,e,i,s,r,a,o,l,c),this.isCanvasTexture=!0,this.needsUpdate=!0}};var Ii=class extends qe{constructor(t,e,i=Qe,s,r,a,o=Se,l=Se,c,h=Ai,u=1){if(h!==Ai&&h!==rn)throw new Error("THREE.DepthTexture: format must be either THREE.DepthFormat or THREE.DepthStencilFormat");let d={width:t,height:e,depth:u};super(d,s,r,a,o,l,h,i,c),this.isDepthTexture=!0,this.flipY=!1,this.generateMipmaps=!1,this.compareFunction=null}copy(t){return super.copy(t),this.source=new Bn(Object.assign({},t.image)),this.compareFunction=t.compareFunction,this}toJSON(t){let e=super.toJSON(t);return e.compareFunction=this.compareFunction,e}},Er=class extends Ii{constructor(t,e=Qe,i=Jn,s,r,a=Se,o=Se,l,c=Ai){let h={width:t,height:t,depth:1},u=[h,h,h,h,h,h];super(t,t,e,i,s,r,a,o,l,c),this.image=u,this.isCubeDepthTexture=!0,this.isCubeTexture=!0}get images(){return this.image}set images(t){this.image=t}},Tr=class extends qe{constructor(t=null){super(),this.sourceTexture=t,this.isExternalTexture=!0}copy(t){return super.copy(t),this.sourceTexture=t.sourceTexture,this}},ve=class n extends ee{constructor(t=1,e=1,i=1,s=1,r=1,a=1){super(),this.type="BoxGeometry",this.parameters={width:t,height:e,depth:i,widthSegments:s,heightSegments:r,depthSegments:a};let o=this;s=Math.floor(s),r=Math.floor(r),a=Math.floor(a);let l=[],c=[],h=[],u=[],d=0,f=0;p("z","y","x",-1,-1,i,e,t,a,r,0),p("z","y","x",1,-1,i,e,-t,a,r,1),p("x","z","y",1,1,t,i,e,s,a,2),p("x","z","y",1,-1,t,i,-e,s,a,3),p("x","y","z",1,-1,t,e,i,s,r,4),p("x","y","z",-1,-1,t,e,-i,s,r,5),this.setIndex(l),this.setAttribute("position",new Kt(c,3)),this.setAttribute("normal",new Kt(h,3)),this.setAttribute("uv",new Kt(u,2));function p(v,g,m,_,S,M,T,w,x,y,P){let C=M/x,I=T/y,L=M/2,F=T/2,D=w/2,B=x+1,G=y+1,k=0,j=0,W=new A;for(let K=0;K<G;K++){let Q=K*I-F;for(let dt=0;dt<B;dt++){let yt=dt*C-L;W[v]=yt*_,W[g]=Q*S,W[m]=D,c.push(W.x,W.y,W.z),W[v]=0,W[g]=0,W[m]=w>0?1:-1,h.push(W.x,W.y,W.z),u.push(dt/x),u.push(1-K/y),k+=1}}for(let K=0;K<y;K++)for(let Q=0;Q<x;Q++){let dt=d+Q+B*K,yt=d+Q+B*(K+1),q=d+(Q+1)+B*(K+1),at=d+(Q+1)+B*K;l.push(dt,yt,at),l.push(yt,q,at),j+=6}o.addGroup(f,j,P),f+=j,d+=k}}copy(t){return super.copy(t),this.parameters=Object.assign({},t.parameters),this}static fromJSON(t){return new n(t.width,t.height,t.depth,t.widthSegments,t.heightSegments,t.depthSegments)}};var Ar=class n extends ee{constructor(t=1,e=32,i=0,s=Math.PI*2){super(),this.type="CircleGeometry",this.parameters={radius:t,segments:e,thetaStart:i,thetaLength:s},e=Math.max(3,e);let r=[],a=[],o=[],l=[],c=new A,h=new et;a.push(0,0,0),o.push(0,0,1),l.push(.5,.5);for(let u=0,d=3;u<=e;u++,d+=3){let f=i+u/e*s;c.x=t*Math.cos(f),c.y=t*Math.sin(f),a.push(c.x,c.y,c.z),o.push(0,0,1),h.x=(a[d]/t+1)/2,h.y=(a[d+1]/t+1)/2,l.push(h.x,h.y)}for(let u=1;u<=e;u++)r.push(u,u+1,0);this.setIndex(r),this.setAttribute("position",new Kt(a,3)),this.setAttribute("normal",new Kt(o,3)),this.setAttribute("uv",new Kt(l,2))}copy(t){return super.copy(t),this.parameters=Object.assign({},t.parameters),this}static fromJSON(t){return new n(t.radius,t.segments,t.thetaStart,t.thetaLength)}},vi=class n extends ee{constructor(t=1,e=1,i=1,s=32,r=1,a=!1,o=0,l=Math.PI*2){super(),this.type="CylinderGeometry",this.parameters={radiusTop:t,radiusBottom:e,height:i,radialSegments:s,heightSegments:r,openEnded:a,thetaStart:o,thetaLength:l};let c=this;s=Math.floor(s),r=Math.floor(r);let h=[],u=[],d=[],f=[],p=0,v=[],g=i/2,m=0;_(),a===!1&&(t>0&&S(!0),e>0&&S(!1)),this.setIndex(h),this.setAttribute("position",new Kt(u,3)),this.setAttribute("normal",new Kt(d,3)),this.setAttribute("uv",new Kt(f,2));function _(){let M=new A,T=new A,w=0,x=(e-t)/i;for(let y=0;y<=r;y++){let P=[],C=y/r,I=C*(e-t)+t;for(let L=0;L<=s;L++){let F=L/s,D=F*l+o,B=Math.sin(D),G=Math.cos(D);T.x=I*B,T.y=-C*i+g,T.z=I*G,u.push(T.x,T.y,T.z),M.set(B,x,G).normalize(),d.push(M.x,M.y,M.z),f.push(F,1-C),P.push(p++)}v.push(P)}for(let y=0;y<s;y++)for(let P=0;P<r;P++){let C=v[P][y],I=v[P+1][y],L=v[P+1][y+1],F=v[P][y+1];(t>0||P!==0)&&(h.push(C,I,F),w+=3),(e>0||P!==r-1)&&(h.push(I,L,F),w+=3)}c.addGroup(m,w,0),m+=w}function S(M){let T=p,w=new et,x=new A,y=0,P=M===!0?t:e,C=M===!0?1:-1;for(let L=1;L<=s;L++)u.push(0,g*C,0),d.push(0,C,0),f.push(.5,.5),p++;let I=p;for(let L=0;L<=s;L++){let D=L/s*l+o,B=Math.cos(D),G=Math.sin(D);x.x=P*G,x.y=g*C,x.z=P*B,u.push(x.x,x.y,x.z),d.push(0,C,0),w.x=B*.5+.5,w.y=G*.5*C+.5,f.push(w.x,w.y),p++}for(let L=0;L<s;L++){let F=T+L,D=I+L;M===!0?h.push(D,D+1,F):h.push(D+1,D,F),y+=3}c.addGroup(m,y,M===!0?1:2),m+=y}}copy(t){return super.copy(t),this.parameters=Object.assign({},t.parameters),this}static fromJSON(t){return new n(t.radiusTop,t.radiusBottom,t.height,t.radialSegments,t.heightSegments,t.openEnded,t.thetaStart,t.thetaLength)}};var ka=new A,Ha=new A,mc=new A,Va=new $i,Hn=class extends ee{constructor(t=null,e=1){if(super(),this.type="EdgesGeometry",this.parameters={geometry:t,thresholdAngle:e},t!==null){let s=Math.pow(10,4),r=Math.cos(ms*e),a=t.getIndex(),o=t.getAttribute("position"),l=a?a.count:o.count,c=[0,0,0],h=["a","b","c"],u=new Array(3),d=new Map,f=[];for(let p=0;p<l;p+=3){a?(c[0]=a.getX(p),c[1]=a.getX(p+1),c[2]=a.getX(p+2)):(c[0]=p,c[1]=p+1,c[2]=p+2);let{a:v,b:g,c:m}=Va;if(v.fromBufferAttribute(o,c[0]),g.fromBufferAttribute(o,c[1]),m.fromBufferAttribute(o,c[2]),Va.getNormal(mc),u[0]=`${Math.round(v.x*s)},${Math.round(v.y*s)},${Math.round(v.z*s)}`,u[1]=`${Math.round(g.x*s)},${Math.round(g.y*s)},${Math.round(g.z*s)}`,u[2]=`${Math.round(m.x*s)},${Math.round(m.y*s)},${Math.round(m.z*s)}`,!(u[0]===u[1]||u[1]===u[2]||u[2]===u[0]))for(let _=0;_<3;_++){let S=(_+1)%3,M=u[_],T=u[S],w=Va[h[_]],x=Va[h[S]],y=`${M}_${T}`,P=`${T}_${M}`,C=d.get(P);C?(mc.dot(C.normal)<=r&&(f.push(w.x,w.y,w.z),f.push(x.x,x.y,x.z)),d.set(P,null)):d.has(y)||d.set(y,{index0:c[_],index1:c[S],normal:mc.clone()})}}for(let p of d.values())if(p){let{index0:v,index1:g}=p;ka.fromBufferAttribute(o,v),Ha.fromBufferAttribute(o,g),f.push(ka.x,ka.y,ka.z),f.push(Ha.x,Ha.y,Ha.z)}this.setAttribute("position",new Kt(f,3))}}copy(t){return super.copy(t),this.parameters=Object.assign({},t.parameters),this}},oi=class{constructor(){this.type="Curve",this.arcLengthDivisions=200,this.needsUpdate=!1,this.cacheArcLengths=null}getPoint(){Pt("Curve: .getPoint() not implemented.")}getPointAt(t,e){let i=this.getUtoTmapping(t);return this.getPoint(i,e)}getPoints(t=5){let e=[];for(let i=0;i<=t;i++)e.push(this.getPoint(i/t));return e}getSpacedPoints(t=5){let e=[];for(let i=0;i<=t;i++)e.push(this.getPointAt(i/t));return e}getLength(){let t=this.getLengths();return t[t.length-1]}getLengths(t=this.arcLengthDivisions){if(this.cacheArcLengths&&this.cacheArcLengths.length===t+1&&!this.needsUpdate)return this.cacheArcLengths;this.needsUpdate=!1;let e=[],i,s=this.getPoint(0),r=0;e.push(0);for(let a=1;a<=t;a++)i=this.getPoint(a/t),r+=i.distanceTo(s),e.push(r),s=i;return this.cacheArcLengths=e,e}updateArcLengths(){this.needsUpdate=!0,this.getLengths()}getUtoTmapping(t,e=null){let i=this.getLengths(),s=0,r=i.length,a;e?a=e:a=t*i[r-1];let o=0,l=r-1,c;for(;o<=l;)if(s=Math.floor(o+(l-o)/2),c=i[s]-a,c<0)o=s+1;else if(c>0)l=s-1;else{l=s;break}if(s=l,i[s]===a)return s/(r-1);let h=i[s],d=i[s+1]-h,f=(a-h)/d;return(s+f)/(r-1)}getTangent(t,e){let s=t-1e-4,r=t+1e-4;s<0&&(s=0),r>1&&(r=1);let a=this.getPoint(s),o=this.getPoint(r),l=e||(a.isVector2?new et:new A);return l.copy(o).sub(a).normalize(),l}getTangentAt(t,e){let i=this.getUtoTmapping(t);return this.getTangent(i,e)}computeFrenetFrames(t,e=!1){let i=new A,s=[],r=[],a=[],o=new A,l=new Gt;for(let f=0;f<=t;f++){let p=f/t;s[f]=this.getTangentAt(p,new A)}r[0]=new A,a[0]=new A;let c=Number.MAX_VALUE,h=Math.abs(s[0].x),u=Math.abs(s[0].y),d=Math.abs(s[0].z);h<=c&&(c=h,i.set(1,0,0)),u<=c&&(c=u,i.set(0,1,0)),d<=c&&i.set(0,0,1),o.crossVectors(s[0],i).normalize(),r[0].crossVectors(s[0],o),a[0].crossVectors(s[0],r[0]);for(let f=1;f<=t;f++){if(r[f]=r[f-1].clone(),a[f]=a[f-1].clone(),o.crossVectors(s[f-1],s[f]),o.length()>Number.EPSILON){o.normalize();let p=Math.acos(Bt(s[f-1].dot(s[f]),-1,1));r[f].applyMatrix4(l.makeRotationAxis(o,p))}a[f].crossVectors(s[f],r[f])}if(e===!0){let f=Math.acos(Bt(r[0].dot(r[t]),-1,1));f/=t,s[0].dot(o.crossVectors(r[0],r[t]))>0&&(f=-f);for(let p=1;p<=t;p++)r[p].applyMatrix4(l.makeRotationAxis(s[p],f*p)),a[p].crossVectors(s[p],r[p])}return{tangents:s,normals:r,binormals:a}}clone(){return new this.constructor().copy(this)}copy(t){return this.arcLengthDivisions=t.arcLengthDivisions,this.cacheArcLengths=null,this}toJSON(){let t={metadata:{version:4.7,type:"Curve",generator:"Curve.toJSON"}};return t.arcLengthDivisions=this.arcLengthDivisions,t.type=this.type,t}fromJSON(t){return this.arcLengthDivisions=t.arcLengthDivisions,this.cacheArcLengths=null,this}},Rr=class extends oi{constructor(t=0,e=0,i=1,s=1,r=0,a=Math.PI*2,o=!1,l=0){super(),this.isEllipseCurve=!0,this.type="EllipseCurve",this.aX=t,this.aY=e,this.xRadius=i,this.yRadius=s,this.aStartAngle=r,this.aEndAngle=a,this.aClockwise=o,this.aRotation=l}getPoint(t,e=new et){let i=e,s=Math.PI*2,r=this.aEndAngle-this.aStartAngle,a=Math.abs(r)<Number.EPSILON;for(;r<0;)r+=s;for(;r>s;)r-=s;r<Number.EPSILON&&(a?r=0:r=s),this.aClockwise===!0&&!a&&(r===s?r=-s:r=r-s);let o=this.aStartAngle+t*r,l=this.aX+this.xRadius*Math.cos(o),c=this.aY+this.yRadius*Math.sin(o);if(this.aRotation!==0){let h=Math.cos(this.aRotation),u=Math.sin(this.aRotation),d=l-this.aX,f=c-this.aY;l=d*h-f*u+this.aX,c=d*u+f*h+this.aY}return i.set(l,c)}copy(t){return super.copy(t),this.aX=t.aX,this.aY=t.aY,this.xRadius=t.xRadius,this.yRadius=t.yRadius,this.aStartAngle=t.aStartAngle,this.aEndAngle=t.aEndAngle,this.aClockwise=t.aClockwise,this.aRotation=t.aRotation,this}toJSON(){let t=super.toJSON();return t.aX=this.aX,t.aY=this.aY,t.xRadius=this.xRadius,t.yRadius=this.yRadius,t.aStartAngle=this.aStartAngle,t.aEndAngle=this.aEndAngle,t.aClockwise=this.aClockwise,t.aRotation=this.aRotation,t}fromJSON(t){return super.fromJSON(t),this.aX=t.aX,this.aY=t.aY,this.xRadius=t.xRadius,this.yRadius=t.yRadius,this.aStartAngle=t.aStartAngle,this.aEndAngle=t.aEndAngle,this.aClockwise=t.aClockwise,this.aRotation=t.aRotation,this}},lo=class extends Rr{constructor(t,e,i,s,r,a){super(t,e,i,i,s,r,a),this.isArcCurve=!0,this.type="ArcCurve"}};function Jc(){let n=0,t=0,e=0,i=0;function s(r,a,o,l){n=r,t=o,e=-3*r+3*a-2*o-l,i=2*r-2*a+o+l}return{initCatmullRom:function(r,a,o,l,c){s(a,o,c*(o-r),c*(l-a))},initNonuniformCatmullRom:function(r,a,o,l,c,h,u){let d=(a-r)/c-(o-r)/(c+h)+(o-a)/h,f=(o-a)/h-(l-a)/(h+u)+(l-o)/u;d*=h,f*=h,s(a,o,d,f)},calc:function(r){let a=r*r,o=a*r;return n+t*r+e*a+i*o}}}var wu=new A,Eu=new A,gc=new Jc,xc=new Jc,_c=new Jc,Ts=class extends oi{constructor(t=[],e=!1,i="centripetal",s=.5){super(),this.isCatmullRomCurve3=!0,this.type="CatmullRomCurve3",this.points=t,this.closed=e,this.curveType=i,this.tension=s}getPoint(t,e=new A){let i=e,s=this.points,r=s.length,a=(r-(this.closed?0:1))*t,o=Math.floor(a),l=a-o;this.closed?o+=o>0?0:(Math.floor(Math.abs(o)/r)+1)*r:l===0&&o===r-1&&(o=r-2,l=1);let c,h;this.closed||o>0?c=s[(o-1)%r]:(Eu.subVectors(s[0],s[1]).add(s[0]),c=Eu);let u=s[o%r],d=s[(o+1)%r];if(this.closed||o+2<r?h=s[(o+2)%r]:(wu.subVectors(s[r-1],s[r-2]).add(s[r-1]),h=wu),this.curveType==="centripetal"||this.curveType==="chordal"){let f=this.curveType==="chordal"?.5:.25,p=Math.pow(c.distanceToSquared(u),f),v=Math.pow(u.distanceToSquared(d),f),g=Math.pow(d.distanceToSquared(h),f);v<1e-4&&(v=1),p<1e-4&&(p=v),g<1e-4&&(g=v),gc.initNonuniformCatmullRom(c.x,u.x,d.x,h.x,p,v,g),xc.initNonuniformCatmullRom(c.y,u.y,d.y,h.y,p,v,g),_c.initNonuniformCatmullRom(c.z,u.z,d.z,h.z,p,v,g)}else this.curveType==="catmullrom"&&(gc.initCatmullRom(c.x,u.x,d.x,h.x,this.tension),xc.initCatmullRom(c.y,u.y,d.y,h.y,this.tension),_c.initCatmullRom(c.z,u.z,d.z,h.z,this.tension));return i.set(gc.calc(l),xc.calc(l),_c.calc(l)),i}copy(t){super.copy(t),this.points=[];for(let e=0,i=t.points.length;e<i;e++){let s=t.points[e];this.points.push(s.clone())}return this.closed=t.closed,this.curveType=t.curveType,this.tension=t.tension,this}toJSON(){let t=super.toJSON();t.points=[];for(let e=0,i=this.points.length;e<i;e++){let s=this.points[e];t.points.push(s.toArray())}return t.closed=this.closed,t.curveType=this.curveType,t.tension=this.tension,t}fromJSON(t){super.fromJSON(t),this.points=[];for(let e=0,i=t.points.length;e<i;e++){let s=t.points[e];this.points.push(new A().fromArray(s))}return this.closed=t.closed,this.curveType=t.curveType,this.tension=t.tension,this}};function Tu(n,t,e,i,s){let r=(i-t)*.5,a=(s-e)*.5,o=n*n,l=n*o;return(2*e-2*i+r+a)*l+(-3*e+3*i-2*r-a)*o+r*n+e}function Lp(n,t){let e=1-n;return e*e*t}function Np(n,t){return 2*(1-n)*n*t}function Up(n,t){return n*n*t}function dr(n,t,e,i){return Lp(n,t)+Np(n,e)+Up(n,i)}function Fp(n,t){let e=1-n;return e*e*e*t}function Op(n,t){let e=1-n;return 3*e*e*n*t}function Bp(n,t){return 3*(1-n)*n*n*t}function zp(n,t){return n*n*n*t}function fr(n,t,e,i,s){return Fp(n,t)+Op(n,e)+Bp(n,i)+zp(n,s)}var co=class extends oi{constructor(t=new et,e=new et,i=new et,s=new et){super(),this.isCubicBezierCurve=!0,this.type="CubicBezierCurve",this.v0=t,this.v1=e,this.v2=i,this.v3=s}getPoint(t,e=new et){let i=e,s=this.v0,r=this.v1,a=this.v2,o=this.v3;return i.set(fr(t,s.x,r.x,a.x,o.x),fr(t,s.y,r.y,a.y,o.y)),i}copy(t){return super.copy(t),this.v0.copy(t.v0),this.v1.copy(t.v1),this.v2.copy(t.v2),this.v3.copy(t.v3),this}toJSON(){let t=super.toJSON();return t.v0=this.v0.toArray(),t.v1=this.v1.toArray(),t.v2=this.v2.toArray(),t.v3=this.v3.toArray(),t}fromJSON(t){return super.fromJSON(t),this.v0.fromArray(t.v0),this.v1.fromArray(t.v1),this.v2.fromArray(t.v2),this.v3.fromArray(t.v3),this}},ho=class extends oi{constructor(t=new A,e=new A,i=new A,s=new A){super(),this.isCubicBezierCurve3=!0,this.type="CubicBezierCurve3",this.v0=t,this.v1=e,this.v2=i,this.v3=s}getPoint(t,e=new A){let i=e,s=this.v0,r=this.v1,a=this.v2,o=this.v3;return i.set(fr(t,s.x,r.x,a.x,o.x),fr(t,s.y,r.y,a.y,o.y),fr(t,s.z,r.z,a.z,o.z)),i}copy(t){return super.copy(t),this.v0.copy(t.v0),this.v1.copy(t.v1),this.v2.copy(t.v2),this.v3.copy(t.v3),this}toJSON(){let t=super.toJSON();return t.v0=this.v0.toArray(),t.v1=this.v1.toArray(),t.v2=this.v2.toArray(),t.v3=this.v3.toArray(),t}fromJSON(t){return super.fromJSON(t),this.v0.fromArray(t.v0),this.v1.fromArray(t.v1),this.v2.fromArray(t.v2),this.v3.fromArray(t.v3),this}},uo=class extends oi{constructor(t=new et,e=new et){super(),this.isLineCurve=!0,this.type="LineCurve",this.v1=t,this.v2=e}getPoint(t,e=new et){let i=e;return t===1?i.copy(this.v2):(i.copy(this.v2).sub(this.v1),i.multiplyScalar(t).add(this.v1)),i}getPointAt(t,e){return this.getPoint(t,e)}getTangent(t,e=new et){return e.subVectors(this.v2,this.v1).normalize()}getTangentAt(t,e){return this.getTangent(t,e)}copy(t){return super.copy(t),this.v1.copy(t.v1),this.v2.copy(t.v2),this}toJSON(){let t=super.toJSON();return t.v1=this.v1.toArray(),t.v2=this.v2.toArray(),t}fromJSON(t){return super.fromJSON(t),this.v1.fromArray(t.v1),this.v2.fromArray(t.v2),this}},As=class extends oi{constructor(t=new A,e=new A){super(),this.isLineCurve3=!0,this.type="LineCurve3",this.v1=t,this.v2=e}getPoint(t,e=new A){let i=e;return t===1?i.copy(this.v2):(i.copy(this.v2).sub(this.v1),i.multiplyScalar(t).add(this.v1)),i}getPointAt(t,e){return this.getPoint(t,e)}getTangent(t,e=new A){return e.subVectors(this.v2,this.v1).normalize()}getTangentAt(t,e){return this.getTangent(t,e)}copy(t){return super.copy(t),this.v1.copy(t.v1),this.v2.copy(t.v2),this}toJSON(){let t=super.toJSON();return t.v1=this.v1.toArray(),t.v2=this.v2.toArray(),t}fromJSON(t){return super.fromJSON(t),this.v1.fromArray(t.v1),this.v2.fromArray(t.v2),this}},fo=class extends oi{constructor(t=new et,e=new et,i=new et){super(),this.isQuadraticBezierCurve=!0,this.type="QuadraticBezierCurve",this.v0=t,this.v1=e,this.v2=i}getPoint(t,e=new et){let i=e,s=this.v0,r=this.v1,a=this.v2;return i.set(dr(t,s.x,r.x,a.x),dr(t,s.y,r.y,a.y)),i}copy(t){return super.copy(t),this.v0.copy(t.v0),this.v1.copy(t.v1),this.v2.copy(t.v2),this}toJSON(){let t=super.toJSON();return t.v0=this.v0.toArray(),t.v1=this.v1.toArray(),t.v2=this.v2.toArray(),t}fromJSON(t){return super.fromJSON(t),this.v0.fromArray(t.v0),this.v1.fromArray(t.v1),this.v2.fromArray(t.v2),this}},Cr=class extends oi{constructor(t=new A,e=new A,i=new A){super(),this.isQuadraticBezierCurve3=!0,this.type="QuadraticBezierCurve3",this.v0=t,this.v1=e,this.v2=i}getPoint(t,e=new A){let i=e,s=this.v0,r=this.v1,a=this.v2;return i.set(dr(t,s.x,r.x,a.x),dr(t,s.y,r.y,a.y),dr(t,s.z,r.z,a.z)),i}copy(t){return super.copy(t),this.v0.copy(t.v0),this.v1.copy(t.v1),this.v2.copy(t.v2),this}toJSON(){let t=super.toJSON();return t.v0=this.v0.toArray(),t.v1=this.v1.toArray(),t.v2=this.v2.toArray(),t}fromJSON(t){return super.fromJSON(t),this.v0.fromArray(t.v0),this.v1.fromArray(t.v1),this.v2.fromArray(t.v2),this}},po=class extends oi{constructor(t=[]){super(),this.isSplineCurve=!0,this.type="SplineCurve",this.points=t}getPoint(t,e=new et){let i=e,s=this.points,r=(s.length-1)*t,a=Math.floor(r),o=r-a,l=s[a===0?a:a-1],c=s[a],h=s[a>s.length-2?s.length-1:a+1],u=s[a>s.length-3?s.length-1:a+2];return i.set(Tu(o,l.x,c.x,h.x,u.x),Tu(o,l.y,c.y,h.y,u.y)),i}copy(t){super.copy(t),this.points=[];for(let e=0,i=t.points.length;e<i;e++){let s=t.points[e];this.points.push(s.clone())}return this}toJSON(){let t=super.toJSON();t.points=[];for(let e=0,i=this.points.length;e<i;e++){let s=this.points[e];t.points.push(s.toArray())}return t}fromJSON(t){super.fromJSON(t),this.points=[];for(let e=0,i=t.points.length;e<i;e++){let s=t.points[e];this.points.push(new et().fromArray(s))}return this}},Ec=Object.freeze({__proto__:null,ArcCurve:lo,CatmullRomCurve3:Ts,CubicBezierCurve:co,CubicBezierCurve3:ho,EllipseCurve:Rr,LineCurve:uo,LineCurve3:As,QuadraticBezierCurve:fo,QuadraticBezierCurve3:Cr,SplineCurve:po}),Pr=class extends oi{constructor(){super(),this.type="CurvePath",this.curves=[],this.autoClose=!1,this.cacheLengths=null}add(t){this.curves.push(t)}closePath(){let t=this.curves[0].getPoint(0),e=this.curves[this.curves.length-1].getPoint(1);if(!t.equals(e)){let i=t.isVector2===!0?"LineCurve":"LineCurve3";this.curves.push(new Ec[i](e,t))}return this}getPoint(t,e){let i=t*this.getLength(),s=this.getCurveLengths(),r=0;for(;r<s.length;){if(s[r]>=i){let a=s[r]-i,o=this.curves[r],l=o.getLength(),c=l===0?0:1-a/l;return o.getPointAt(c,e)}r++}return null}getLength(){let t=this.getCurveLengths();return t[t.length-1]}updateArcLengths(){this.needsUpdate=!0,this.cacheLengths=null,this.getCurveLengths()}getCurveLengths(){if(this.cacheLengths&&this.cacheLengths.length===this.curves.length)return this.cacheLengths;let t=[],e=0;for(let i=0,s=this.curves.length;i<s;i++)e+=this.curves[i].getLength(),t.push(e);return this.cacheLengths=t,t}getSpacedPoints(t=40){let e=[];for(let i=0;i<=t;i++)e.push(this.getPoint(i/t));return this.autoClose&&e.push(e[0]),e}getPoints(t=12){let e=[],i;for(let s=0,r=this.curves;s<r.length;s++){let a=r[s],o=a.isEllipseCurve?t*2:a.isLineCurve||a.isLineCurve3?1:a.isSplineCurve?t*a.points.length:t,l=a.getPoints(o);for(let c=0;c<l.length;c++){let h=l[c];i&&i.equals(h)||(e.push(h),i=h)}}return this.autoClose&&e.length>1&&!e[e.length-1].equals(e[0])&&e.push(e[0]),e}copy(t){super.copy(t),this.curves=[];for(let e=0,i=t.curves.length;e<i;e++){let s=t.curves[e];this.curves.push(s.clone())}return this.autoClose=t.autoClose,this.cacheLengths=null,this}toJSON(){let t=super.toJSON();t.autoClose=this.autoClose,t.curves=[];for(let e=0,i=this.curves.length;e<i;e++){let s=this.curves[e];t.curves.push(s.toJSON())}return t}fromJSON(t){super.fromJSON(t),this.autoClose=t.autoClose,this.curves=[];for(let e=0,i=t.curves.length;e<i;e++){let s=t.curves[e];this.curves.push(new Ec[s.type]().fromJSON(s))}return this.cacheLengths=null,this}};var Di=class n extends ee{constructor(t=1,e=1,i=1,s=1){super(),this.type="PlaneGeometry",this.parameters={width:t,height:e,widthSegments:i,heightSegments:s};let r=t/2,a=e/2,o=Math.floor(i),l=Math.floor(s),c=o+1,h=l+1,u=t/o,d=e/l,f=[],p=[],v=[],g=[];for(let m=0;m<h;m++){let _=m*d-a;for(let S=0;S<c;S++){let M=S*u-r;p.push(M,-_,0),v.push(0,0,1),g.push(S/o),g.push(1-m/l)}}for(let m=0;m<l;m++)for(let _=0;_<o;_++){let S=_+c*m,M=_+c*(m+1),T=_+1+c*(m+1),w=_+1+c*m;f.push(S,M,w),f.push(M,T,w)}this.setIndex(f),this.setAttribute("position",new Kt(p,3)),this.setAttribute("normal",new Kt(v,3)),this.setAttribute("uv",new Kt(g,2))}copy(t){return super.copy(t),this.parameters=Object.assign({},t.parameters),this}static fromJSON(t){return new n(t.width,t.height,t.widthSegments,t.heightSegments)}};var nn=class n extends ee{constructor(t=1,e=32,i=16,s=0,r=Math.PI*2,a=0,o=Math.PI){super(),this.type="SphereGeometry",this.parameters={radius:t,widthSegments:e,heightSegments:i,phiStart:s,phiLength:r,thetaStart:a,thetaLength:o},e=Math.max(3,Math.floor(e)),i=Math.max(2,Math.floor(i));let l=Math.min(a+o,Math.PI),c=0,h=[],u=new A,d=new A,f=[],p=[],v=[],g=[];for(let m=0;m<=i;m++){let _=[],S=m/i,M=a+S*o,T=t*Math.cos(M),w=Math.sqrt(t*t-T*T),x=0;m===0&&a===0?x=.5/e:m===i&&l===Math.PI&&(x=-.5/e);for(let y=0;y<=e;y++){let P=y/e,C=s+P*r;u.x=-w*Math.cos(C),u.y=T,u.z=w*Math.sin(C),p.push(u.x,u.y,u.z),d.copy(u).normalize(),v.push(d.x,d.y,d.z),g.push(P+x,1-S),_.push(c++)}h.push(_)}for(let m=0;m<i;m++)for(let _=0;_<e;_++){let S=h[m][_+1],M=h[m][_],T=h[m+1][_],w=h[m+1][_+1];(m!==0||a>0)&&f.push(S,M,w),(m!==i-1||l<Math.PI)&&f.push(M,T,w)}this.setIndex(f),this.setAttribute("position",new Kt(p,3)),this.setAttribute("normal",new Kt(v,3)),this.setAttribute("uv",new Kt(g,2))}copy(t){return super.copy(t),this.parameters=Object.assign({},t.parameters),this}static fromJSON(t){return new n(t.radius,t.widthSegments,t.heightSegments,t.phiStart,t.phiLength,t.thetaStart,t.thetaLength)}};var Ir=class n extends ee{constructor(t=1,e=.4,i=12,s=48,r=Math.PI*2,a=0,o=Math.PI*2){super(),this.type="TorusGeometry",this.parameters={radius:t,tube:e,radialSegments:i,tubularSegments:s,arc:r,thetaStart:a,thetaLength:o},i=Math.floor(i),s=Math.floor(s);let l=[],c=[],h=[],u=[],d=new A,f=new A,p=new A;for(let v=0;v<=i;v++){let g=a+v/i*o;for(let m=0;m<=s;m++){let _=m/s*r;f.x=(t+e*Math.cos(g))*Math.cos(_),f.y=(t+e*Math.cos(g))*Math.sin(_),f.z=e*Math.sin(g),c.push(f.x,f.y,f.z),d.x=t*Math.cos(_),d.y=t*Math.sin(_),p.subVectors(f,d).normalize(),h.push(p.x,p.y,p.z),u.push(m/s),u.push(v/i)}}for(let v=1;v<=i;v++)for(let g=1;g<=s;g++){let m=(s+1)*v+g-1,_=(s+1)*(v-1)+g-1,S=(s+1)*(v-1)+g,M=(s+1)*v+g;l.push(m,_,M),l.push(_,S,M)}this.setIndex(l),this.setAttribute("position",new Kt(c,3)),this.setAttribute("normal",new Kt(h,3)),this.setAttribute("uv",new Kt(u,2))}copy(t){return super.copy(t),this.parameters=Object.assign({},t.parameters),this}static fromJSON(t){return new n(t.radius,t.tube,t.radialSegments,t.tubularSegments,t.arc,t.thetaStart,t.thetaLength)}};var Vn=class n extends ee{constructor(t=new Cr(new A(-1,-1,0),new A(-1,1,0),new A(1,1,0)),e=64,i=1,s=8,r=!1){super(),this.type="TubeGeometry",this.parameters={path:t,tubularSegments:e,radius:i,radialSegments:s,closed:r};let a=t.computeFrenetFrames(e,r);this.tangents=a.tangents,this.normals=a.normals,this.binormals=a.binormals;let o=new A,l=new A,c=new et,h=new A,u=[],d=[],f=[],p=[];v(),this.setIndex(p),this.setAttribute("position",new Kt(u,3)),this.setAttribute("normal",new Kt(d,3)),this.setAttribute("uv",new Kt(f,2));function v(){for(let S=0;S<e;S++)g(S);g(r===!1?e:0),_(),m()}function g(S){h=t.getPointAt(S/e,h);let M=a.normals[S],T=a.binormals[S];for(let w=0;w<=s;w++){let x=w/s*Math.PI*2,y=Math.sin(x),P=-Math.cos(x);l.x=P*M.x+y*T.x,l.y=P*M.y+y*T.y,l.z=P*M.z+y*T.z,l.normalize(),d.push(l.x,l.y,l.z),o.x=h.x+i*l.x,o.y=h.y+i*l.y,o.z=h.z+i*l.z,u.push(o.x,o.y,o.z)}}function m(){for(let S=1;S<=e;S++)for(let M=1;M<=s;M++){let T=(s+1)*(S-1)+(M-1),w=(s+1)*S+(M-1),x=(s+1)*S+M,y=(s+1)*(S-1)+M;p.push(T,w,y),p.push(w,x,y)}}function _(){for(let S=0;S<=e;S++)for(let M=0;M<=s;M++)c.x=S/e,c.y=M/s,f.push(c.x,c.y)}}copy(t){return super.copy(t),this.parameters=Object.assign({},t.parameters),this}toJSON(){let t=super.toJSON();return t.path=this.parameters.path.toJSON(),t}static fromJSON(t){return new n(new Ec[t.path.type]().fromJSON(t.path),t.tubularSegments,t.radius,t.radialSegments,t.closed)}};function jn(n){let t={};for(let e in n){t[e]={};for(let i in n[e]){let s=n[e][i];if(Au(s))s.isRenderTargetTexture?(Pt("UniformsUtils: Textures of render targets cannot be cloned via cloneUniforms() or mergeUniforms()."),t[e][i]=null):t[e][i]=s.clone();else if(Array.isArray(s))if(Au(s[0])){let r=[];for(let a=0,o=s.length;a<o;a++)r[a]=s[a].clone();t[e][i]=r}else t[e][i]=s.slice();else t[e][i]=s}}return t}function Ye(n){let t={};for(let e=0;e<n.length;e++){let i=jn(n[e]);for(let s in i)t[s]=i[s]}return t}function Au(n){return n&&(n.isColor||n.isMatrix3||n.isMatrix4||n.isVector2||n.isVector3||n.isVector4||n.isTexture||n.isQuaternion)}function kp(n){let t=[];for(let e=0;e<n.length;e++)t.push(n[e].clone());return t}function $c(n){let t=n.getRenderTarget();return t===null?n.outputColorSpace:t.isXRRenderTarget===!0?t.texture.colorSpace:Wt.workingColorSpace}var Fi={clone:jn,merge:Ye},Hp=`void main() {
	gl_Position = projectionMatrix * modelViewMatrix * vec4( position, 1.0 );
}`,Vp=`void main() {
	gl_FragColor = vec4( 1.0, 0.0, 0.0, 1.0 );
}`,oe=class extends Ci{constructor(t){super(),this.isShaderMaterial=!0,this.type="ShaderMaterial",this.defines={},this.uniforms={},this.uniformsGroups=[],this.vertexShader=Hp,this.fragmentShader=Vp,this.linewidth=1,this.wireframe=!1,this.wireframeLinewidth=1,this.fog=!1,this.lights=!1,this.clipping=!1,this.forceSinglePass=!0,this.extensions={clipCullDistance:!1,multiDraw:!1},this.defaultAttributeValues={color:[1,1,1],uv:[0,0],uv1:[0,0]},this.index0AttributeName=void 0,this.uniformsNeedUpdate=!1,this.glslVersion=null,t!==void 0&&this.setValues(t)}copy(t){return super.copy(t),this.fragmentShader=t.fragmentShader,this.vertexShader=t.vertexShader,this.uniforms=jn(t.uniforms),this.uniformsGroups=kp(t.uniformsGroups),this.defines=Object.assign({},t.defines),this.wireframe=t.wireframe,this.wireframeLinewidth=t.wireframeLinewidth,this.fog=t.fog,this.lights=t.lights,this.clipping=t.clipping,this.extensions=Object.assign({},t.extensions),this.glslVersion=t.glslVersion,this.defaultAttributeValues=Object.assign({},t.defaultAttributeValues),this.index0AttributeName=t.index0AttributeName,this.uniformsNeedUpdate=t.uniformsNeedUpdate,this}toJSON(t){let e=super.toJSON(t);e.glslVersion=this.glslVersion,e.uniforms={};for(let s in this.uniforms){let a=this.uniforms[s].value;a&&a.isTexture?e.uniforms[s]={type:"t",value:a.toJSON(t).uuid}:a&&a.isColor?e.uniforms[s]={type:"c",value:a.getHex()}:a&&a.isVector2?e.uniforms[s]={type:"v2",value:a.toArray()}:a&&a.isVector3?e.uniforms[s]={type:"v3",value:a.toArray()}:a&&a.isVector4?e.uniforms[s]={type:"v4",value:a.toArray()}:a&&a.isMatrix3?e.uniforms[s]={type:"m3",value:a.toArray()}:a&&a.isMatrix4?e.uniforms[s]={type:"m4",value:a.toArray()}:e.uniforms[s]={value:a}}Object.keys(this.defines).length>0&&(e.defines=this.defines),e.vertexShader=this.vertexShader,e.fragmentShader=this.fragmentShader,e.lights=this.lights,e.clipping=this.clipping;let i={};for(let s in this.extensions)this.extensions[s]===!0&&(i[s]=!0);return Object.keys(i).length>0&&(e.extensions=i),e}fromJSON(t,e){if(super.fromJSON(t,e),t.uniforms!==void 0)for(let i in t.uniforms){let s=t.uniforms[i];switch(this.uniforms[i]={},s.type){case"t":this.uniforms[i].value=e[s.value]||null;break;case"c":this.uniforms[i].value=new Mt().setHex(s.value);break;case"v2":this.uniforms[i].value=new et().fromArray(s.value);break;case"v3":this.uniforms[i].value=new A().fromArray(s.value);break;case"v4":this.uniforms[i].value=new ie().fromArray(s.value);break;case"m3":this.uniforms[i].value=new Nt().fromArray(s.value);break;case"m4":this.uniforms[i].value=new Gt().fromArray(s.value);break;default:this.uniforms[i].value=s.value}}if(t.defines!==void 0&&(this.defines=t.defines),t.vertexShader!==void 0&&(this.vertexShader=t.vertexShader),t.fragmentShader!==void 0&&(this.fragmentShader=t.fragmentShader),t.glslVersion!==void 0&&(this.glslVersion=t.glslVersion),t.extensions!==void 0)for(let i in t.extensions)this.extensions[i]=t.extensions[i];return t.lights!==void 0&&(this.lights=t.lights),t.clipping!==void 0&&(this.clipping=t.clipping),this}},Rs=class extends oe{constructor(t){super(t),this.isRawShaderMaterial=!0,this.type="RawShaderMaterial"}},be=class extends Ci{constructor(t){super(),this.isMeshStandardMaterial=!0,this.type="MeshStandardMaterial",this.defines={STANDARD:""},this.color=new Mt(16777215),this.roughness=1,this.metalness=0,this.map=null,this.lightMap=null,this.lightMapIntensity=1,this.aoMap=null,this.aoMapIntensity=1,this.emissive=new Mt(0),this.emissiveIntensity=1,this.emissiveMap=null,this.bumpMap=null,this.bumpScale=1,this.normalMap=null,this.normalMapType=Qr,this.normalScale=new et(1,1),this.displacementMap=null,this.displacementScale=1,this.displacementBias=0,this.roughnessMap=null,this.metalnessMap=null,this.alphaMap=null,this.envMap=null,this.envMapRotation=new gi,this.envMapIntensity=1,this.wireframe=!1,this.wireframeLinewidth=1,this.wireframeLinecap="round",this.wireframeLinejoin="round",this.flatShading=!1,this.fog=!0,this.setValues(t)}copy(t){return super.copy(t),this.defines={STANDARD:""},this.color.copy(t.color),this.roughness=t.roughness,this.metalness=t.metalness,this.map=t.map,this.lightMap=t.lightMap,this.lightMapIntensity=t.lightMapIntensity,this.aoMap=t.aoMap,this.aoMapIntensity=t.aoMapIntensity,this.emissive.copy(t.emissive),this.emissiveMap=t.emissiveMap,this.emissiveIntensity=t.emissiveIntensity,this.bumpMap=t.bumpMap,this.bumpScale=t.bumpScale,this.normalMap=t.normalMap,this.normalMapType=t.normalMapType,this.normalScale.copy(t.normalScale),this.displacementMap=t.displacementMap,this.displacementScale=t.displacementScale,this.displacementBias=t.displacementBias,this.roughnessMap=t.roughnessMap,this.metalnessMap=t.metalnessMap,this.alphaMap=t.alphaMap,this.envMap=t.envMap,this.envMapRotation.copy(t.envMapRotation),this.envMapIntensity=t.envMapIntensity,this.wireframe=t.wireframe,this.wireframeLinewidth=t.wireframeLinewidth,this.wireframeLinecap=t.wireframeLinecap,this.wireframeLinejoin=t.wireframeLinejoin,this.flatShading=t.flatShading,this.fog=t.fog,this}},Dr=class extends be{constructor(t){super(),this.isMeshPhysicalMaterial=!0,this.defines={STANDARD:"",PHYSICAL:""},this.type="MeshPhysicalMaterial",this.anisotropyRotation=0,this.anisotropyMap=null,this.clearcoatMap=null,this.clearcoatRoughness=0,this.clearcoatRoughnessMap=null,this.clearcoatNormalScale=new et(1,1),this.clearcoatNormalMap=null,this.diffuseRoughnessMap=null,this.ior=1.5,Object.defineProperty(this,"reflectivity",{get:function(){return Bt(2.5*(this.ior-1)/(this.ior+1),0,1)},set:function(e){this.ior=(1+.4*e)/(1-.4*e)}}),this.iridescenceMap=null,this.iridescenceIOR=1.3,this.iridescenceThicknessMinimum=100,this.iridescenceThicknessMaximum=400,this.iridescenceThicknessMap=null,this.sheenColor=new Mt(0),this.sheenColorMap=null,this.sheenRoughness=1,this.sheenRoughnessMap=null,this.transmissionMap=null,this.thickness=0,this.thicknessMap=null,this.attenuationDistance=1/0,this.attenuationColor=new Mt(1,1,1),this.specularIntensity=1,this.specularIntensityMap=null,this.specularColor=new Mt(1,1,1),this.specularColorMap=null,this._anisotropy=0,this._clearcoat=0,this._diffuseRoughness=0,this._dispersion=0,this._iridescence=0,this._retroreflectivity=0,this._sheen=0,this._transmission=0,this.setValues(t)}get anisotropy(){return this._anisotropy}set anisotropy(t){this._anisotropy>0!=t>0&&this.version++,this._anisotropy=t}get clearcoat(){return this._clearcoat}set clearcoat(t){this._clearcoat>0!=t>0&&this.version++,this._clearcoat=t}get diffuseRoughness(){return this._diffuseRoughness}set diffuseRoughness(t){this._diffuseRoughness>0!=t>0&&this.version++,this._diffuseRoughness=t}get iridescence(){return this._iridescence}set iridescence(t){this._iridescence>0!=t>0&&this.version++,this._iridescence=t}get dispersion(){return this._dispersion}set dispersion(t){this._dispersion>0!=t>0&&this.version++,this._dispersion=t}get retroreflectivity(){return this._retroreflectivity}set retroreflectivity(t){this._retroreflectivity>0!=t>0&&this.version++,this._retroreflectivity=t}get sheen(){return this._sheen}set sheen(t){this._sheen>0!=t>0&&this.version++,this._sheen=t}get transmission(){return this._transmission}set transmission(t){this._transmission>0!=t>0&&this.version++,this._transmission=t}get iridescenceThicknessRange(){return Un('MeshPhysicalMaterial: The "iridescenceThicknessRange" property has been replaced by "iridescenceThicknessMinimum" and "iridescenceThicknessMaximum".'),[this.iridescenceThicknessMinimum,this.iridescenceThicknessMaximum]}set iridescenceThicknessRange(t){Un('MeshPhysicalMaterial: The "iridescenceThicknessRange" property has been replaced by "iridescenceThicknessMinimum" and "iridescenceThicknessMaximum".'),this.iridescenceThicknessMinimum=t[0],this.iridescenceThicknessMaximum=t[1]}copy(t){return super.copy(t),this.defines={STANDARD:"",PHYSICAL:""},this.anisotropy=t.anisotropy,this.anisotropyRotation=t.anisotropyRotation,this.anisotropyMap=t.anisotropyMap,this.clearcoat=t.clearcoat,this.clearcoatMap=t.clearcoatMap,this.clearcoatRoughness=t.clearcoatRoughness,this.clearcoatRoughnessMap=t.clearcoatRoughnessMap,this.clearcoatNormalMap=t.clearcoatNormalMap,this.clearcoatNormalScale.copy(t.clearcoatNormalScale),this.diffuseRoughness=t.diffuseRoughness,this.diffuseRoughnessMap=t.diffuseRoughnessMap,this.dispersion=t.dispersion,this.ior=t.ior,this.iridescence=t.iridescence,this.iridescenceMap=t.iridescenceMap,this.iridescenceIOR=t.iridescenceIOR,this.iridescenceThicknessMinimum=t.iridescenceThicknessMinimum,this.iridescenceThicknessMaximum=t.iridescenceThicknessMaximum,this.iridescenceThicknessMap=t.iridescenceThicknessMap,this.retroreflectivity=t.retroreflectivity,this.sheen=t.sheen,this.sheenColor.copy(t.sheenColor),this.sheenColorMap=t.sheenColorMap,this.sheenRoughness=t.sheenRoughness,this.sheenRoughnessMap=t.sheenRoughnessMap,this.transmission=t.transmission,this.transmissionMap=t.transmissionMap,this.thickness=t.thickness,this.thicknessMap=t.thicknessMap,this.attenuationDistance=t.attenuationDistance,this.attenuationColor.copy(t.attenuationColor),this.specularIntensity=t.specularIntensity,this.specularIntensityMap=t.specularIntensityMap,this.specularColor.copy(t.specularColor),this.specularColorMap=t.specularColorMap,this}};var Lr=class extends Ci{constructor(t){super(),this.isMeshLambertMaterial=!0,this.type="MeshLambertMaterial",this.color=new Mt(16777215),this.map=null,this.lightMap=null,this.lightMapIntensity=1,this.aoMap=null,this.aoMapIntensity=1,this.emissive=new Mt(0),this.emissiveIntensity=1,this.emissiveMap=null,this.bumpMap=null,this.bumpScale=1,this.normalMap=null,this.normalMapType=Qr,this.normalScale=new et(1,1),this.displacementMap=null,this.displacementScale=1,this.displacementBias=0,this.specularMap=null,this.alphaMap=null,this.envMap=null,this.envMapRotation=new gi,this.combine=Ro,this.reflectivity=1,this.envMapIntensity=1,this.refractionRatio=.98,this.wireframe=!1,this.wireframeLinewidth=1,this.wireframeLinecap="round",this.wireframeLinejoin="round",this.flatShading=!1,this.fog=!0,this.setValues(t)}copy(t){return super.copy(t),this.color.copy(t.color),this.map=t.map,this.lightMap=t.lightMap,this.lightMapIntensity=t.lightMapIntensity,this.aoMap=t.aoMap,this.aoMapIntensity=t.aoMapIntensity,this.emissive.copy(t.emissive),this.emissiveMap=t.emissiveMap,this.emissiveIntensity=t.emissiveIntensity,this.bumpMap=t.bumpMap,this.bumpScale=t.bumpScale,this.normalMap=t.normalMap,this.normalMapType=t.normalMapType,this.normalScale.copy(t.normalScale),this.displacementMap=t.displacementMap,this.displacementScale=t.displacementScale,this.displacementBias=t.displacementBias,this.specularMap=t.specularMap,this.alphaMap=t.alphaMap,this.envMap=t.envMap,this.envMapRotation.copy(t.envMapRotation),this.combine=t.combine,this.reflectivity=t.reflectivity,this.envMapIntensity=t.envMapIntensity,this.refractionRatio=t.refractionRatio,this.wireframe=t.wireframe,this.wireframeLinewidth=t.wireframeLinewidth,this.wireframeLinecap=t.wireframeLinecap,this.wireframeLinejoin=t.wireframeLinejoin,this.flatShading=t.flatShading,this.fog=t.fog,this}},mo=class extends Ci{constructor(t){super(),this.isMeshDepthMaterial=!0,this.type="MeshDepthMaterial",this.depthPacking=ad,this.map=null,this.alphaMap=null,this.displacementMap=null,this.displacementScale=1,this.displacementBias=0,this.wireframe=!1,this.wireframeLinewidth=1,this.setValues(t)}copy(t){return super.copy(t),this.depthPacking=t.depthPacking,this.map=t.map,this.alphaMap=t.alphaMap,this.displacementMap=t.displacementMap,this.displacementScale=t.displacementScale,this.displacementBias=t.displacementBias,this.wireframe=t.wireframe,this.wireframeLinewidth=t.wireframeLinewidth,this}},go=class extends Ci{constructor(t){super(),this.isMeshDistanceMaterial=!0,this.type="MeshDistanceMaterial",this.map=null,this.alphaMap=null,this.displacementMap=null,this.displacementScale=1,this.displacementBias=0,this.setValues(t)}copy(t){return super.copy(t),this.map=t.map,this.alphaMap=t.alphaMap,this.displacementMap=t.displacementMap,this.displacementScale=t.displacementScale,this.displacementBias=t.displacementBias,this}};function ds(n,t){return!n||n.constructor===t?n:typeof t.BYTES_PER_ELEMENT=="number"?new t(n):Array.prototype.slice.call(n)}function Ga(n){return n!==void 0&&n.inTangents!==void 0&&n.outTangents!==void 0}var gn=class{constructor(t,e,i,s){this.parameterPositions=t,this._cachedIndex=0,this.resultBuffer=s!==void 0?s:new e.constructor(i),this.sampleValues=e,this.valueSize=i,this.settings=null,this.DefaultSettings_={}}evaluate(t){let e=this.parameterPositions,i=this._cachedIndex,s=e[i],r=e[i-1];i:{t:{let a;e:{n:if(!(t<s)){for(let o=i+2;;){if(s===void 0){if(t<r)break n;return i=e.length,this._cachedIndex=i,this.copySampleValue_(i-1)}if(i===o)break;if(r=s,s=e[++i],t<s)break t}a=e.length;break e}if(!(t>=r)){let o=e[1];t<o&&(i=2,r=o);for(let l=i-2;;){if(r===void 0)return this._cachedIndex=0,this.copySampleValue_(0);if(i===l)break;if(s=r,r=e[--i-1],t>=r)break t}a=i,i=0;break e}break i}for(;i<a;){let o=i+a>>>1;t<e[o]?a=o:i=o+1}if(s=e[i],r=e[i-1],r===void 0)return this._cachedIndex=0,this.copySampleValue_(0);if(s===void 0)return i=e.length,this._cachedIndex=i,this.copySampleValue_(i-1)}this._cachedIndex=i,this.intervalChanged_(i,r,s)}return this.interpolate_(i,r,t,s)}getSettings_(){return this.settings||this.DefaultSettings_}copySampleValue_(t){let e=this.resultBuffer,i=this.sampleValues,s=this.valueSize,r=t*s;for(let a=0;a!==s;++a)e[a]=i[r+a];return e}interpolate_(){throw new Error("THREE.Interpolant: Call to abstract method.")}intervalChanged_(){}},xo=class extends gn{constructor(t,e,i,s){super(t,e,i,s),this._weightPrev=-0,this._offsetPrev=-0,this._weightNext=-0,this._offsetNext=-0,this.DefaultSettings_={endingStart:Mc,endingEnd:Mc}}intervalChanged_(t,e,i){let s=this.parameterPositions,r=t-2,a=t+1,o=s[r],l=s[a];if(o===void 0)switch(this.getSettings_().endingStart){case Sc:r=t,o=2*e-i;break;case bc:r=s.length-2,o=e+s[r]-s[r+1];break;default:r=t,o=i}if(l===void 0)switch(this.getSettings_().endingEnd){case Sc:a=t,l=2*i-e;break;case bc:a=1,l=i+s[1]-s[0];break;default:a=t-1,l=e}let c=(i-e)*.5,h=this.valueSize;this._weightPrev=c/(e-o),this._weightNext=c/(l-i),this._offsetPrev=r*h,this._offsetNext=a*h}interpolate_(t,e,i,s){let r=this.resultBuffer,a=this.sampleValues,o=this.valueSize,l=t*o,c=l-o,h=this._offsetPrev,u=this._offsetNext,d=this._weightPrev,f=this._weightNext,p=(i-e)/(s-e),v=p*p,g=v*p,m=-d*g+2*d*v-d*p,_=(1+d)*g+(-1.5-2*d)*v+(-.5+d)*p+1,S=(-1-f)*g+(1.5+f)*v+.5*p,M=f*g-f*v;for(let T=0;T!==o;++T)r[T]=m*a[h+T]+_*a[c+T]+S*a[l+T]+M*a[u+T];return r}},_o=class extends gn{constructor(t,e,i,s){super(t,e,i,s)}interpolate_(t,e,i,s){let r=this.resultBuffer,a=this.sampleValues,o=this.valueSize,l=t*o,c=l-o,h=(i-e)/(s-e),u=1-h;for(let d=0;d!==o;++d)r[d]=a[c+d]*u+a[l+d]*h;return r}},vo=class extends gn{constructor(t,e,i,s){super(t,e,i,s)}interpolate_(t){return this.copySampleValue_(t-1)}},yo=class extends gn{interpolate_(t,e,i,s){let r=this.resultBuffer,a=this.sampleValues,o=this.valueSize,l=t*o,c=l-o,h=this.inTangents,u=this.outTangents;if(!h||!u){let p=(i-e)/(s-e),v=1-p;for(let g=0;g!==o;++g)r[g]=a[c+g]*v+a[l+g]*p;return r}let d=o*2,f=t-1;for(let p=0;p!==o;++p){let v=a[c+p],g=a[l+p],m=f*d+p*2,_=u[m],S=u[m+1],M=t*d+p*2,T=h[M],w=h[M+1],x=Wp(i,e,_,T,s);r[p]=Sd(x,v,S,w,g)}return r}};function Sd(n,t,e,i,s){let r=1-n;return r*r*r*t+3*r*r*n*e+3*r*n*n*i+n*n*n*s}function Gp(n,t,e,i,s){let r=1-n;return 3*r*r*(e-t)+6*r*n*(i-e)+3*n*n*(s-i)}function Wp(n,t,e,i,s){let r=(n-t)/(s-t);for(let a=0;a<8;a++){let o=Sd(r,t,e,i,s)-n;if(Math.abs(o)<1e-10)break;let l=Gp(r,t,e,i,s);if(Math.abs(l)<1e-10)break;r=Math.max(0,Math.min(1,r-o/l))}return r}var li=class{constructor(t,e,i,s){if(t===void 0)throw new Error("THREE.KeyframeTrack: track name is undefined");if(e===void 0||e.length===0)throw new Error("THREE.KeyframeTrack: no keyframes in track named "+t);this.name=t,this.times=ds(e,this.TimeBufferType),this.values=ds(i,this.ValueBufferType),this.setInterpolation(s||this.DefaultInterpolation)}static toJSON(t){let e=t.constructor,i;if(e.toJSON!==this.toJSON)i=e.toJSON(t);else{i={name:t.name,times:ds(t.times,Array),values:ds(t.values,Array)};let s=t.getInterpolation();s!==t.DefaultInterpolation&&(i.interpolation=s),Ga(t.settings)&&(i.settings={inTangents:ds(t.settings.inTangents,Array),outTangents:ds(t.settings.outTangents,Array)})}return i.type=t.ValueTypeName,i}InterpolantFactoryMethodDiscrete(t){return new vo(this.times,this.values,this.getValueSize(),t)}InterpolantFactoryMethodLinear(t){return new _o(this.times,this.values,this.getValueSize(),t)}InterpolantFactoryMethodSmooth(t){return new xo(this.times,this.values,this.getValueSize(),t)}InterpolantFactoryMethodBezier(t){let e=new yo(this.times,this.values,this.getValueSize(),t);return this.settings&&(e.inTangents=this.settings.inTangents,e.outTangents=this.settings.outTangents),e}setInterpolation(t){let e;switch(t){case pr:e=this.InterpolantFactoryMethodDiscrete;break;case io:e=this.InterpolantFactoryMethodLinear;break;case qa:e=this.InterpolantFactoryMethodSmooth;break;case yc:e=this.InterpolantFactoryMethodBezier;break}if(e===void 0){let i="unsupported interpolation for "+this.ValueTypeName+" keyframe track named "+this.name;if(this.createInterpolant===void 0)if(t!==this.DefaultInterpolation)this.setInterpolation(this.DefaultInterpolation);else throw new Error(i);return Pt("KeyframeTrack:",i),this}return this.createInterpolant=e,this}getInterpolation(){switch(this.createInterpolant){case this.InterpolantFactoryMethodDiscrete:return pr;case this.InterpolantFactoryMethodLinear:return io;case this.InterpolantFactoryMethodSmooth:return qa;case this.InterpolantFactoryMethodBezier:return yc}}getValueSize(){return this.values.length/this.times.length}shift(t){if(t!==0){let e=this.times;for(let i=0,s=e.length;i!==s;++i)e[i]+=t;Ga(this.settings)&&(Ru(this.settings.inTangents,t),Ru(this.settings.outTangents,t))}return this}scale(t){if(t!==1){let e=this.times;for(let i=0,s=e.length;i!==s;++i)e[i]*=t;Ga(this.settings)&&(Cu(this.settings.inTangents,t),Cu(this.settings.outTangents,t))}return this}trim(t,e){let i=this.times,s=i.length,r=0,a=s-1;for(;r!==s&&i[r]<t;)++r;for(;a!==-1&&i[a]>e;)--a;if(++a,r!==0||a!==s){r>=a&&(a=Math.max(a,1),r=a-1);let o=this.getValueSize();this.times=i.slice(r,a),this.values=this.values.slice(r*o,a*o)}return this}validate(){let t=!0,e=this.getValueSize();e-Math.floor(e)!==0&&(Lt("KeyframeTrack: Invalid value size in track.",this),t=!1);let i=this.times,s=this.values,r=i.length;r===0&&(Lt("KeyframeTrack: Track is empty.",this),t=!1);let a=null;for(let o=0;o!==r;o++){let l=i[o];if(typeof l=="number"&&isNaN(l)){Lt("KeyframeTrack: Time is not a valid number.",this,o,l),t=!1;break}if(a!==null&&a>l){Lt("KeyframeTrack: Out of order keys.",this,o,l,a),t=!1;break}a=l}if(s!==void 0&&Kf(s))for(let o=0,l=s.length;o!==l;++o){let c=s[o];if(isNaN(c)){Lt("KeyframeTrack: Value is not a valid number.",this,o,c),t=!1;break}}return t}optimize(){let t=this.times.slice(),e=this.values.slice(),i=this.getValueSize(),s=this.getInterpolation()===qa,r=t.length-1,a=1;for(let o=1;o<r;++o){let l=!1,c=t[o],h=t[o+1];if(c!==h&&(o!==1||c!==t[0]))if(s)l=!0;else{let u=o*i,d=u-i,f=u+i;for(let p=0;p!==i;++p){let v=e[u+p];if(v!==e[d+p]||v!==e[f+p]){l=!0;break}}}if(l){if(o!==a){t[a]=t[o];let u=o*i,d=a*i;for(let f=0;f!==i;++f)e[d+f]=e[u+f]}++a}}if(r>0){t[a]=t[r];for(let o=r*i,l=a*i,c=0;c!==i;++c)e[l+c]=e[o+c];++a}return a!==t.length?(this.times=t.slice(0,a),this.values=e.slice(0,a*i)):(this.times=t,this.values=e),this}clone(){let t=this.times.slice(),e=this.values.slice(),i=this.constructor,s=new i(this.name,t,e);return s.createInterpolant=this.createInterpolant,Ga(this.settings)&&(s.settings={inTangents:this.settings.inTangents.slice(),outTangents:this.settings.outTangents.slice()}),s}};function Ru(n,t){for(let e=0,i=n.length;e!==i;e+=2)n[e]+=t}function Cu(n,t){for(let e=0,i=n.length;e!==i;e+=2)n[e]*=t}li.prototype.ValueTypeName="";li.prototype.TimeBufferType=Float32Array;li.prototype.ValueBufferType=Float32Array;li.prototype.DefaultInterpolation=io;var xn=class extends li{constructor(t,e,i){super(t,e,i)}};xn.prototype.ValueTypeName="bool";xn.prototype.ValueBufferType=Array;xn.prototype.DefaultInterpolation=pr;xn.prototype.InterpolantFactoryMethodLinear=void 0;xn.prototype.InterpolantFactoryMethodSmooth=void 0;var Mo=class extends li{constructor(t,e,i,s){super(t,e,i,s)}};Mo.prototype.ValueTypeName="color";var So=class extends li{constructor(t,e,i,s){super(t,e,i,s)}};So.prototype.ValueTypeName="number";var bo=class extends gn{constructor(t,e,i,s){super(t,e,i,s)}interpolate_(t,e,i,s){let r=this.resultBuffer,a=this.sampleValues,o=this.valueSize,l=(i-e)/(s-e),c=t*o;for(let h=c+o;c!==h;c+=4)Je.slerpFlat(r,0,a,c-o,a,c,l);return r}},Nr=class extends li{constructor(t,e,i,s){super(t,e,i,s)}InterpolantFactoryMethodLinear(t){return new bo(this.times,this.values,this.getValueSize(),t)}};Nr.prototype.ValueTypeName="quaternion";Nr.prototype.InterpolantFactoryMethodSmooth=void 0;var _n=class extends li{constructor(t,e,i){super(t,e,i)}};_n.prototype.ValueTypeName="string";_n.prototype.ValueBufferType=Array;_n.prototype.DefaultInterpolation=pr;_n.prototype.InterpolantFactoryMethodLinear=void 0;_n.prototype.InterpolantFactoryMethodSmooth=void 0;var wo=class extends li{constructor(t,e,i,s){super(t,e,i,s)}};wo.prototype.ValueTypeName="vector";var Eo=class{constructor(t,e,i){let s=this,r=!1,a=0,o=0,l,c=[];this.onStart=void 0,this.onLoad=t,this.onProgress=e,this.onError=i,this._abortController=null,this.itemStart=function(h){o++,r===!1&&s.onStart!==void 0&&s.onStart(h,a,o),r=!0},this.itemEnd=function(h){a++,s.onProgress!==void 0&&s.onProgress(h,a,o),a===o&&(r=!1,s.onLoad!==void 0&&s.onLoad())},this.itemError=function(h){s.onError!==void 0&&s.onError(h)},this.resolveURL=function(h){return h=h.normalize("NFC"),l?l(h):h},this.setURLModifier=function(h){return l=h,this},this.addHandler=function(h,u){return c.push(h,u),this},this.removeHandler=function(h){let u=c.indexOf(h);return u!==-1&&c.splice(u,2),this},this.getHandler=function(h){for(let u=0,d=c.length;u<d;u+=2){let f=c[u],p=c[u+1];if(f.global&&(f.lastIndex=0),f.test(h))return p}return null},this.abort=function(){return this.abortController.abort(),this._abortController=null,this}}get abortController(){return this._abortController||(this._abortController=new AbortController),this._abortController}},bd=new Eo,To=class{constructor(t){this.manager=t!==void 0?t:bd,this.crossOrigin="anonymous",this.withCredentials=!1,this.path="",this.resourcePath="",this.requestHeader={},this.priority="auto",this.cache="default",typeof __THREE_DEVTOOLS__<"u"&&__THREE_DEVTOOLS__.dispatchEvent(new CustomEvent("observe",{detail:this}))}load(){}loadAsync(t,e){let i=this;return new Promise(function(s,r){i.load(t,s,e,r)})}parse(){}setCrossOrigin(t){return this.crossOrigin=t,this}setWithCredentials(t){return this.withCredentials=t,this}setPath(t){return this.path=t,this}setResourcePath(t){return this.resourcePath=t,this}setRequestHeader(t){return this.requestHeader=t,this}setPriority(t){return this.priority=t,this}setCache(t){return this.cache=t,this}abort(){return this}};To.DEFAULT_MATERIAL_NAME="__DEFAULT";var Gn=class extends de{static registerNode(t){this.prototype._lightNode=t}constructor(t,e=1){super(),this.isLight=!0,this.type="Light",this.color=new Mt(t),this.intensity=e}copy(t,e){return super.copy(t,e),this.color.copy(t.color),this.intensity=t.intensity,this}toJSON(t){let e=super.toJSON(t);return e.object.color=this.color.getHex(),e.object.intensity=this.intensity,e}},Cs=class extends Gn{constructor(t,e,i){super(t,i),this.isHemisphereLight=!0,this.type="HemisphereLight",this.position.copy(de.DEFAULT_UP),this.updateMatrix(),this.groundColor=new Mt(e)}copy(t,e){return super.copy(t,e),this.groundColor.copy(t.groundColor),this}toJSON(t){let e=super.toJSON(t);return e.object.groundColor=this.groundColor.getHex(),e}},vc=new Gt,Pu=new A,Iu=new A,Ps=class{constructor(t){this.camera=t,this.intensity=1,this.bias=0,this.biasNode=null,this.normalBias=0,this.radius=1,this.blurSamples=8,this.mapSize=new et(512,512),this.mapType=ii,this.map=null,this.mapPass=null,this.matrix=new Gt,this.autoUpdate=!0,this.needsUpdate=!1,this._frustum=new mn,this._frameExtents=new et(1,1),this._viewportCount=1,this._viewports=[new ie(0,0,1,1)]}getViewportCount(){return this._viewportCount}getCamera(){return this.camera}getFrustum(){return this._frustum}updateMatrices(t){let e=this.camera;Pu.setFromMatrixPosition(t.matrixWorld),e.position.copy(Pu),Iu.setFromMatrixPosition(t.target.matrixWorld),e.lookAt(Iu),e.updateMatrixWorld(),this._updateMatrix(e,this.matrix,this._frustum)}_updateMatrix(t,e,i,s){vc.multiplyMatrices(t.projectionMatrix,t.matrixWorldInverse),i.setFromProjectionMatrix(vc,t.coordinateSystem,t.reversedDepth);let r=this._frameExtents,a=s?s.z/r.x:1,o=s?s.w/r.y:1,l=s?s.x/r.x:0,c=s?s.y/r.y:0;t.coordinateSystem===_s||t.reversedDepth?e.set(.5*a,0,0,.5*a+l,0,.5*o,0,.5*o+c,0,0,1,0,0,0,0,1):e.set(.5*a,0,0,.5*a+l,0,.5*o,0,.5*o+c,0,0,.5,.5,0,0,0,1),e.multiply(vc)}getViewport(t){return this._viewports[t]}getFrameExtents(){return this._frameExtents}dispose(){this.map&&this.map.dispose(),this.mapPass&&this.mapPass.dispose()}copy(t){return this.camera=t.camera.clone(),this.intensity=t.intensity,this.bias=t.bias,this.radius=t.radius,this.autoUpdate=t.autoUpdate,this.needsUpdate=t.needsUpdate,this.normalBias=t.normalBias,this.blurSamples=t.blurSamples,this.mapSize.copy(t.mapSize),this.mapType=t.mapType,this.biasNode=t.biasNode,this}clone(){return new this.constructor().copy(this)}toJSON(){let t={};return t.intensity=this.intensity,t.bias=this.bias,t.normalBias=this.normalBias,t.radius=this.radius,t.blurSamples=this.blurSamples,t.mapSize=this.mapSize.toArray(),t.mapType=this.mapType,t.camera=this.camera.toJSON(!1).object,delete t.camera.matrix,t}},Wa=new A,Xa=new Je,zi=new A,Wn=class extends de{constructor(){super(),this.isCamera=!0,this.type="Camera",this.matrixWorldInverse=new Gt,this.projectionMatrix=new Gt,this.projectionMatrixInverse=new Gt,this.coordinateSystem=mi,this._reversedDepth=!1}get reversedDepth(){return this._reversedDepth}copy(t,e){return super.copy(t,e),this.matrixWorldInverse.copy(t.matrixWorldInverse),this.projectionMatrix.copy(t.projectionMatrix),this.projectionMatrixInverse.copy(t.projectionMatrixInverse),this.coordinateSystem=t.coordinateSystem,this}getWorldDirection(t){return super.getWorldDirection(t).negate()}updateMatrixWorld(t){super.updateMatrixWorld(t),this.matrixWorld.decompose(Wa,Xa,zi),zi.x===1&&zi.y===1&&zi.z===1?this.matrixWorldInverse.copy(this.matrixWorld).invert():this.matrixWorldInverse.compose(Wa,Xa,zi.set(1,1,1)).invert()}updateWorldMatrix(t,e,i=!1){super.updateWorldMatrix(t,e,i),this.matrixWorld.decompose(Wa,Xa,zi),zi.x===1&&zi.y===1&&zi.z===1?this.matrixWorldInverse.copy(this.matrixWorld).invert():this.matrixWorldInverse.compose(Wa,Xa,zi.set(1,1,1)).invert()}clone(){return new this.constructor().copy(this)}},fn=new A,Du=new et,Lu=new et,Te=class extends Wn{constructor(t=50,e=1,i=.1,s=2e3){super(),this.isPerspectiveCamera=!0,this.type="PerspectiveCamera",this.fov=t,this.zoom=1,this.near=i,this.far=s,this.focus=10,this.aspect=e,this.view=null,this.filmGauge=35,this.filmOffset=0,this.updateProjectionMatrix()}copy(t,e){return super.copy(t,e),this.fov=t.fov,this.zoom=t.zoom,this.near=t.near,this.far=t.far,this.focus=t.focus,this.aspect=t.aspect,this.view=t.view===null?null:Object.assign({},t.view),this.filmGauge=t.filmGauge,this.filmOffset=t.filmOffset,this}setFocalLength(t){let e=.5*this.getFilmHeight()/t;this.fov=On*2*Math.atan(e),this.updateProjectionMatrix()}getFocalLength(){let t=Math.tan(ms*.5*this.fov);return .5*this.getFilmHeight()/t}getEffectiveFOV(){return On*2*Math.atan(Math.tan(ms*.5*this.fov)/this.zoom)}getFilmWidth(){return this.filmGauge*Math.min(this.aspect,1)}getFilmHeight(){return this.filmGauge/Math.max(this.aspect,1)}getViewBounds(t,e,i){fn.set(-1,-1,.5).applyMatrix4(this.projectionMatrixInverse),e.set(fn.x,fn.y).multiplyScalar(-t/fn.z),fn.set(1,1,.5).applyMatrix4(this.projectionMatrixInverse),i.set(fn.x,fn.y).multiplyScalar(-t/fn.z)}getViewSize(t,e){return this.getViewBounds(t,Du,Lu),e.subVectors(Lu,Du)}setViewOffset(t,e,i,s,r,a){this.aspect=t/e,this.view===null&&(this.view={enabled:!0,fullWidth:1,fullHeight:1,offsetX:0,offsetY:0,width:1,height:1}),this.view.enabled=!0,this.view.fullWidth=t,this.view.fullHeight=e,this.view.offsetX=i,this.view.offsetY=s,this.view.width=r,this.view.height=a,this.updateProjectionMatrix()}clearViewOffset(){this.view!==null&&(this.view.enabled=!1),this.updateProjectionMatrix()}updateProjectionMatrix(){let t=this.near,e=t*Math.tan(ms*.5*this.fov)/this.zoom,i=2*e,s=this.aspect*i,r=-.5*s,a=this.view;if(this.view!==null&&this.view.enabled){let l=a.fullWidth,c=a.fullHeight;r+=a.offsetX*s/l,e-=a.offsetY*i/c,s*=a.width/l,i*=a.height/c}let o=this.filmOffset;o!==0&&(r+=t*o/this.getFilmWidth()),this.projectionMatrix.makePerspective(r,r+s,e,e-i,t,this.far,this.coordinateSystem,this.reversedDepth),this.projectionMatrixInverse.copy(this.projectionMatrix).invert()}toJSON(t){let e=super.toJSON(t);return e.object.fov=this.fov,e.object.zoom=this.zoom,e.object.near=this.near,e.object.far=this.far,e.object.focus=this.focus,e.object.aspect=this.aspect,this.view!==null&&(e.object.view=Object.assign({},this.view)),e.object.filmGauge=this.filmGauge,e.object.filmOffset=this.filmOffset,e}},Tc=class extends Ps{constructor(){super(new Te(50,1,.5,500)),this.isSpotLightShadow=!0,this.focus=1,this.aspect=1}updateMatrices(t){let e=this.camera,i=On*2*t.angle*this.focus,s=this.mapSize.width/this.mapSize.height*this.aspect,r=t.distance||e.far;(i!==e.fov||s!==e.aspect||r!==e.far)&&(e.fov=i,e.aspect=s,e.far=r,e.updateProjectionMatrix()),super.updateMatrices(t)}copy(t){return super.copy(t),this.focus=t.focus,this.aspect=t.aspect,this}toJSON(){let t=super.toJSON();return t.focus=this.focus,t.aspect=this.aspect,t}},Xn=class extends Gn{constructor(t,e,i=0,s=Math.PI/3,r=0,a=2){super(t,e),this.isSpotLight=!0,this.type="SpotLight",this.position.copy(de.DEFAULT_UP),this.updateMatrix(),this.target=new de,this.distance=i,this.angle=s,this.penumbra=r,this.decay=a,this.map=null,this.shadow=new Tc}get power(){return this.intensity*Math.PI}set power(t){this.intensity=t/Math.PI}dispose(){super.dispose(),this.shadow.dispose()}copy(t,e){return super.copy(t,e),this.distance=t.distance,this.angle=t.angle,this.penumbra=t.penumbra,this.decay=t.decay,this.target=t.target.clone(),this.map=t.map,this.shadow=t.shadow.clone(),this}toJSON(t){let e=super.toJSON(t);return e.object.distance=this.distance,e.object.angle=this.angle,e.object.decay=this.decay,e.object.penumbra=this.penumbra,e.object.target=this.target.uuid,this.map&&this.map.isTexture&&(e.object.map=this.map.toJSON(t).uuid),e.object.shadow=this.shadow.toJSON(),e}},Ac=class extends Ps{constructor(){super(new Te(90,1,.5,500)),this.isPointLightShadow=!0}},Ur=class extends Gn{constructor(t,e,i=0,s=2){super(t,e),this.isPointLight=!0,this.type="PointLight",this.distance=i,this.decay=s,this.shadow=new Ac}get power(){return this.intensity*4*Math.PI}set power(t){this.intensity=t/(4*Math.PI)}dispose(){super.dispose(),this.shadow.dispose()}copy(t,e){return super.copy(t,e),this.distance=t.distance,this.decay=t.decay,this.shadow=t.shadow.clone(),this}toJSON(t){let e=super.toJSON(t);return e.object.distance=this.distance,e.object.decay=this.decay,e.object.shadow=this.shadow.toJSON(),e}},qn=class extends Wn{constructor(t=-1,e=1,i=1,s=-1,r=.1,a=2e3){super(),this.isOrthographicCamera=!0,this.type="OrthographicCamera",this.zoom=1,this.view=null,this.left=t,this.right=e,this.top=i,this.bottom=s,this.near=r,this.far=a,this.updateProjectionMatrix()}copy(t,e){return super.copy(t,e),this.left=t.left,this.right=t.right,this.top=t.top,this.bottom=t.bottom,this.near=t.near,this.far=t.far,this.zoom=t.zoom,this.view=t.view===null?null:Object.assign({},t.view),this}setViewOffset(t,e,i,s,r,a){this.view===null&&(this.view={enabled:!0,fullWidth:1,fullHeight:1,offsetX:0,offsetY:0,width:1,height:1}),this.view.enabled=!0,this.view.fullWidth=t,this.view.fullHeight=e,this.view.offsetX=i,this.view.offsetY=s,this.view.width=r,this.view.height=a,this.updateProjectionMatrix()}clearViewOffset(){this.view!==null&&(this.view.enabled=!1),this.updateProjectionMatrix()}updateProjectionMatrix(){let t=(this.right-this.left)/(2*this.zoom),e=(this.top-this.bottom)/(2*this.zoom),i=(this.right+this.left)/2,s=(this.top+this.bottom)/2,r=i-t,a=i+t,o=s+e,l=s-e;if(this.view!==null&&this.view.enabled){let c=(this.right-this.left)/this.view.fullWidth/this.zoom,h=(this.top-this.bottom)/this.view.fullHeight/this.zoom;r+=c*this.view.offsetX,a=r+c*this.view.width,o-=h*this.view.offsetY,l=o-h*this.view.height}this.projectionMatrix.makeOrthographic(r,a,o,l,this.near,this.far,this.coordinateSystem,this.reversedDepth),this.projectionMatrixInverse.copy(this.projectionMatrix).invert()}toJSON(t){let e=super.toJSON(t);return e.object.zoom=this.zoom,e.object.left=this.left,e.object.right=this.right,e.object.top=this.top,e.object.bottom=this.bottom,e.object.near=this.near,e.object.far=this.far,this.view!==null&&(e.object.view=Object.assign({},this.view)),e}},Rc=class extends Ps{constructor(){super(new qn(-5,5,5,-5,.5,500)),this.isDirectionalLightShadow=!0}},Fr=class extends Gn{constructor(t,e){super(t,e),this.isDirectionalLight=!0,this.type="DirectionalLight",this.position.copy(de.DEFAULT_UP),this.updateMatrix(),this.target=new de,this.shadow=new Rc}dispose(){super.dispose(),this.shadow.dispose()}copy(t){return super.copy(t),this.target=t.target.clone(),this.shadow=t.shadow.clone(),this}toJSON(t){let e=super.toJSON(t);return e.object.shadow=this.shadow.toJSON(),e.object.target=this.target.uuid,e}};var fs=-90,ps=1,Is=class extends de{constructor(t,e,i){super(),this.type="CubeCamera",this.renderTarget=i,this.coordinateSystem=null,this.activeMipmapLevel=0;let s=new Te(fs,ps,t,e);s.layers=this.layers,this.add(s);let r=new Te(fs,ps,t,e);r.layers=this.layers,this.add(r);let a=new Te(fs,ps,t,e);a.layers=this.layers,this.add(a);let o=new Te(fs,ps,t,e);o.layers=this.layers,this.add(o);let l=new Te(fs,ps,t,e);l.layers=this.layers,this.add(l);let c=new Te(fs,ps,t,e);c.layers=this.layers,this.add(c)}updateCoordinateSystem(){let t=this.coordinateSystem,e=this.children.concat(),[i,s,r,a,o,l]=e;for(let c of e)this.remove(c);if(t===mi)i.up.set(0,1,0),i.lookAt(1,0,0),s.up.set(0,1,0),s.lookAt(-1,0,0),r.up.set(0,0,-1),r.lookAt(0,1,0),a.up.set(0,0,1),a.lookAt(0,-1,0),o.up.set(0,1,0),o.lookAt(0,0,1),l.up.set(0,1,0),l.lookAt(0,0,-1);else if(t===_s)i.up.set(0,-1,0),i.lookAt(-1,0,0),s.up.set(0,-1,0),s.lookAt(1,0,0),r.up.set(0,0,1),r.lookAt(0,1,0),a.up.set(0,0,-1),a.lookAt(0,-1,0),o.up.set(0,-1,0),o.lookAt(0,0,1),l.up.set(0,-1,0),l.lookAt(0,0,-1);else throw new Error("THREE.CubeCamera.updateCoordinateSystem(): Invalid coordinate system: "+t);for(let c of e)this.add(c),c.updateMatrixWorld()}update(t,e){this.parent===null&&this.updateMatrixWorld();let{renderTarget:i,activeMipmapLevel:s}=this;this.coordinateSystem!==t.coordinateSystem&&(this.coordinateSystem=t.coordinateSystem,this.updateCoordinateSystem());let[r,a,o,l,c,h]=this.children,u=t.getRenderTarget(),d=t.getActiveCubeFace(),f=t.getActiveMipmapLevel(),p=t.xr.enabled;t.xr.enabled=!1;let v=i.texture.mipmapsAutoUpdate;i.texture.mipmapsAutoUpdate=!1;let g=!1;t.isWebGLRenderer===!0?g=t.state.buffers.depth.getReversed():g=t.reversedDepthBuffer,t.setRenderTarget(i,0,s),g&&t.autoClear===!1&&t.clearDepth(),t.render(e,r),t.setRenderTarget(i,1,s),g&&t.autoClear===!1&&t.clearDepth(),t.render(e,a),t.setRenderTarget(i,2,s),g&&t.autoClear===!1&&t.clearDepth(),t.render(e,o),t.setRenderTarget(i,3,s),g&&t.autoClear===!1&&t.clearDepth(),t.render(e,l),t.setRenderTarget(i,4,s),g&&t.autoClear===!1&&t.clearDepth(),t.render(e,c),i.texture.mipmapsAutoUpdate=v,t.setRenderTarget(i,5,s),g&&t.autoClear===!1&&t.clearDepth(),t.render(e,h),t.setRenderTarget(u,d,f),t.xr.enabled=p,i.texture.needsPMREMUpdate=!0}},Ao=class extends Te{constructor(t=[]){super(),this.isArrayCamera=!0,this.isMultiViewCamera=!1,this.cameras=t,this.perCameraFrustumCulled=!1}},Or=class{constructor(){this._previousTime=0,this._currentTime=0,this._startTime=performance.now(),this._delta=0,this._elapsed=0,this._timescale=1,this._document=null,this._pageVisibilityHandler=null}connect(t){this._document=t,t.hidden!==void 0&&(this._pageVisibilityHandler=Xp.bind(this),t.addEventListener("visibilitychange",this._pageVisibilityHandler,!1))}disconnect(){this._pageVisibilityHandler!==null&&(this._document.removeEventListener("visibilitychange",this._pageVisibilityHandler),this._pageVisibilityHandler=null),this._document=null}getDelta(){return this._delta/1e3}getElapsed(){return this._elapsed/1e3}getTimescale(){return this._timescale}setTimescale(t){return this._timescale=t,this}reset(){return this._currentTime=performance.now()-this._startTime,this}dispose(){this.disconnect()}update(t){return this._pageVisibilityHandler!==null&&this._document.hidden===!0?this._delta=0:(this._previousTime=this._currentTime,this._currentTime=(t!==void 0?t:performance.now())-this._startTime,this._delta=(this._currentTime-this._previousTime)*this._timescale,this._elapsed+=this._delta),this}};function Xp(){this._document.hidden===!1&&this.reset()}var jc="\\[\\]\\.:\\/",qp=new RegExp("["+jc+"]","g"),Qc="[^"+jc+"]",Yp="[^"+jc.replace("\\.","")+"]",Zp=/((?:WC+[\/:])*)/.source.replace("WC",Qc),Kp=/(WCOD+)?/.source.replace("WCOD",Yp),Jp=/(?:\.(WC+)(?:\[(.+)\])?)?/.source.replace("WC",Qc),$p=/\.(WC+)(?:\[(.+)\])?/.source.replace("WC",Qc),jp=new RegExp("^"+Zp+Kp+Jp+$p+"$"),Qp=["material","materials","bones","map"],Cc=class{constructor(t,e,i){let s=i||le.parseTrackName(e);this._targetGroup=t,this._bindings=t.subscribe_(e,s)}getValue(t,e){this.bind();let i=this._targetGroup.nCachedObjects_,s=this._bindings[i];s!==void 0&&s.getValue(t,e)}setValue(t,e){let i=this._bindings;for(let s=this._targetGroup.nCachedObjects_,r=i.length;s!==r;++s)i[s].setValue(t,e)}bind(){let t=this._bindings;for(let e=this._targetGroup.nCachedObjects_,i=t.length;e!==i;++e)t[e].bind()}unbind(){let t=this._bindings;for(let e=this._targetGroup.nCachedObjects_,i=t.length;e!==i;++e)t[e].unbind()}},le=class n{constructor(t,e,i){this.path=e,this.parsedPath=i||n.parseTrackName(e),this.node=n.findNode(t,this.parsedPath.nodeName),this.rootNode=t,this.getValue=this._getValue_unbound,this.setValue=this._setValue_unbound}static create(t,e,i){return t&&t.isAnimationObjectGroup?new n.Composite(t,e,i):new n(t,e,i)}static sanitizeNodeName(t){return t.replace(/\s/g,"_").replace(qp,"")}static parseTrackName(t){let e=jp.exec(t);if(e===null)throw new Error("THREE.PropertyBinding: Cannot parse trackName: "+t);let i={nodeName:e[2],objectName:e[3],objectIndex:e[4],propertyName:e[5],propertyIndex:e[6]},s=i.nodeName&&i.nodeName.lastIndexOf(".");if(s!==void 0&&s!==-1){let r=i.nodeName.substring(s+1);Qp.indexOf(r)!==-1&&(i.nodeName=i.nodeName.substring(0,s),i.objectName=r)}if(i.propertyName===null||i.propertyName.length===0)throw new Error("THREE.PropertyBinding: can not parse propertyName from trackName: "+t);return i}static findNode(t,e){if(e===void 0||e===""||e==="."||e===-1||e===t.name||e===t.uuid)return t;if(t.skeleton){let i=t.skeleton.getBoneByName(e);if(i!==void 0)return i}if(t.children){let i=function(r){for(let a=0;a<r.length;a++){let o=r[a];if(o.name===e||o.uuid===e)return o;let l=i(o.children);if(l)return l}return null},s=i(t.children);if(s)return s}return null}_getValue_unavailable(){}_setValue_unavailable(){}_getValue_direct(t,e){t[e]=this.targetObject[this.propertyName]}_getValue_array(t,e){let i=this.resolvedProperty;for(let s=0,r=i.length;s!==r;++s)t[e++]=i[s]}_getValue_arrayElement(t,e){t[e]=this.resolvedProperty[this.propertyIndex]}_getValue_toArray(t,e){this.resolvedProperty.toArray(t,e)}_setValue_direct(t,e){this.targetObject[this.propertyName]=t[e]}_setValue_direct_setNeedsUpdate(t,e){this.targetObject[this.propertyName]=t[e],this.targetObject.needsUpdate=!0}_setValue_direct_setMatrixWorldNeedsUpdate(t,e){this.targetObject[this.propertyName]=t[e],this.targetObject.matrixWorldNeedsUpdate=!0}_setValue_array(t,e){let i=this.resolvedProperty;for(let s=0,r=i.length;s!==r;++s)i[s]=t[e++]}_setValue_array_setNeedsUpdate(t,e){let i=this.resolvedProperty;for(let s=0,r=i.length;s!==r;++s)i[s]=t[e++];this.targetObject.needsUpdate=!0}_setValue_array_setMatrixWorldNeedsUpdate(t,e){let i=this.resolvedProperty;for(let s=0,r=i.length;s!==r;++s)i[s]=t[e++];this.targetObject.matrixWorldNeedsUpdate=!0}_setValue_arrayElement(t,e){this.resolvedProperty[this.propertyIndex]=t[e]}_setValue_arrayElement_setNeedsUpdate(t,e){this.resolvedProperty[this.propertyIndex]=t[e],this.targetObject.needsUpdate=!0}_setValue_arrayElement_setMatrixWorldNeedsUpdate(t,e){this.resolvedProperty[this.propertyIndex]=t[e],this.targetObject.matrixWorldNeedsUpdate=!0}_setValue_fromArray(t,e){this.resolvedProperty.fromArray(t,e)}_setValue_fromArray_setNeedsUpdate(t,e){this.resolvedProperty.fromArray(t,e),this.targetObject.needsUpdate=!0}_setValue_fromArray_setMatrixWorldNeedsUpdate(t,e){this.resolvedProperty.fromArray(t,e),this.targetObject.matrixWorldNeedsUpdate=!0}_getValue_unbound(t,e){this.bind(),this.getValue(t,e)}_setValue_unbound(t,e){this.bind(),this.setValue(t,e)}bind(){let t=this.node,e=this.parsedPath,i=e.objectName,s=e.propertyName,r=e.propertyIndex;if(t||(t=n.findNode(this.rootNode,e.nodeName),this.node=t),this.getValue=this._getValue_unavailable,this.setValue=this._setValue_unavailable,!t){Pt("PropertyBinding: No target node found for track: "+this.path+".");return}if(i){let c=e.objectIndex;switch(i){case"materials":if(!t.material){Lt("PropertyBinding: Can not bind to material as node does not have a material.",this);return}if(!t.material.materials){Lt("PropertyBinding: Can not bind to material.materials as node.material does not have a materials array.",this);return}t=t.material.materials;break;case"bones":if(!t.skeleton){Lt("PropertyBinding: Can not bind to bones as node does not have a skeleton.",this);return}t=t.skeleton.bones;for(let h=0;h<t.length;h++)if(t[h].name===c){c=h;break}break;case"map":if("map"in t){t=t.map;break}if(!t.material){Lt("PropertyBinding: Can not bind to material as node does not have a material.",this);return}if(!t.material.map){Lt("PropertyBinding: Can not bind to material.map as node.material does not have a map.",this);return}t=t.material.map;break;default:if(t[i]===void 0){Lt("PropertyBinding: Can not bind to objectName of node undefined.",this);return}t=t[i]}if(c!==void 0){if(t[c]===void 0){Lt("PropertyBinding: Trying to bind to objectIndex of objectName, but is undefined.",this,t);return}t=t[c]}}let a=t[s];if(a===void 0){let c=e.nodeName;Lt("PropertyBinding: Trying to update property for track: "+c+"."+s+" but it wasn't found.",t);return}let o=this.Versioning.None;this.targetObject=t,t.isMaterial===!0?o=this.Versioning.NeedsUpdate:t.isObject3D===!0&&(o=this.Versioning.MatrixWorldNeedsUpdate);let l=this.BindingType.Direct;if(r!==void 0){if(s==="morphTargetInfluences"){if(!t.geometry){Lt("PropertyBinding: Can not bind to morphTargetInfluences because node does not have a geometry.",this);return}if(!t.geometry.morphAttributes){Lt("PropertyBinding: Can not bind to morphTargetInfluences because node does not have a geometry.morphAttributes.",this);return}t.morphTargetDictionary[r]!==void 0&&(r=t.morphTargetDictionary[r])}l=this.BindingType.ArrayElement,this.resolvedProperty=a,this.propertyIndex=r}else a.fromArray!==void 0&&a.toArray!==void 0?(l=this.BindingType.HasFromToArray,this.resolvedProperty=a):Array.isArray(a)?(l=this.BindingType.EntireArray,this.resolvedProperty=a):this.propertyName=s;this.getValue=this.GetterByBindingType[l],this.setValue=this.SetterByBindingTypeAndVersioning[l][o]}unbind(){this.node=null,this.getValue=this._getValue_unbound,this.setValue=this._setValue_unbound}};le.Composite=Cc;le.prototype.BindingType={Direct:0,EntireArray:1,ArrayElement:2,HasFromToArray:3};le.prototype.Versioning={None:0,NeedsUpdate:1,MatrixWorldNeedsUpdate:2};le.prototype.GetterByBindingType=[le.prototype._getValue_direct,le.prototype._getValue_array,le.prototype._getValue_arrayElement,le.prototype._getValue_toArray];le.prototype.SetterByBindingTypeAndVersioning=[[le.prototype._setValue_direct,le.prototype._setValue_direct_setNeedsUpdate,le.prototype._setValue_direct_setMatrixWorldNeedsUpdate],[le.prototype._setValue_array,le.prototype._setValue_array_setNeedsUpdate,le.prototype._setValue_array_setMatrixWorldNeedsUpdate],[le.prototype._setValue_arrayElement,le.prototype._setValue_arrayElement_setNeedsUpdate,le.prototype._setValue_arrayElement_setMatrixWorldNeedsUpdate],[le.prototype._setValue_fromArray,le.prototype._setValue_fromArray_setNeedsUpdate,le.prototype._setValue_fromArray_setMatrixWorldNeedsUpdate]];var q1=new Float32Array(1);var Nu=new Gt,vn=class{constructor(t,e,i=0,s=1/0){this.ray=new tn(t,e),this.near=i,this.far=s,this.camera=null,this.layers=new Ms,this.params={Mesh:{},Line:{threshold:1},LOD:{},Points:{threshold:1},Sprite:{}}}set(t,e){this.ray.set(t,e)}setFromCamera(t,e){e.isPerspectiveCamera?(this.ray.origin.setFromMatrixPosition(e.matrixWorld),this.ray.direction.set(t.x,t.y,.5).unproject(e).sub(this.ray.origin).normalize(),this.camera=e):e.isOrthographicCamera?(this.ray.origin.set(t.x,t.y,e.projectionMatrix.elements[14]).unproject(e),this.ray.direction.set(0,0,-1).transformDirection(e.matrixWorld),this.camera=e):Lt("Raycaster: Unsupported camera type: "+e.type)}setFromXRController(t){return Nu.identity().extractRotation(t.matrixWorld),this.ray.origin.setFromMatrixPosition(t.matrixWorld),this.ray.direction.set(0,0,-1).applyMatrix4(Nu),this}intersectObject(t,e=!0,i=[]){return Pc(t,this,i,e),i.sort(Uu),i}intersectObjects(t,e=!0,i=[]){for(let s=0,r=t.length;s<r;s++)Pc(t[s],this,i,e);return i.sort(Uu),i}};function Uu(n,t){return n.distance-t.distance}function Pc(n,t,e,i){let s=!0;if(n.layers.test(t.layers)&&n.raycast(t,e)===!1&&(s=!1),s===!0&&i===!0){let r=n.children;for(let a=0,o=r.length;a<o;a++)Pc(r[a],t,e,!0)}}var Ds=class{constructor(t=1,e=0,i=0){this.radius=t,this.phi=e,this.theta=i}set(t,e,i){return this.radius=t,this.phi=e,this.theta=i,this}copy(t){return this.radius=t.radius,this.phi=t.phi,this.theta=t.theta,this}makeSafe(){return this.phi=Bt(this.phi,1e-6,Math.PI-1e-6),this}setFromVector3(t){return this.setFromCartesianCoords(t.x,t.y,t.z)}setFromCartesianCoords(t,e,i){return this.radius=Math.sqrt(t*t+e*e+i*i),this.radius===0?(this.theta=0,this.phi=0):(this.theta=Math.atan2(t,i),this.phi=Math.acos(Bt(e/this.radius,-1,1))),this}clone(){return new this.constructor().copy(this)}};var Ic=class{static{this.prototype.isMatrix2=!0}constructor(t,e,i,s){this.elements=[1,0,0,1],t!==void 0&&this.set(t,e,i,s)}identity(){return this.set(1,0,0,1),this}fromArray(t,e=0){for(let i=0;i<4;i++)this.elements[i]=t[i+e];return this}set(t,e,i,s){let r=this.elements;return r[0]=t,r[2]=e,r[1]=i,r[3]=s,this}};var Br=class extends Ri{constructor(t,e=null){super(),this.object=t,this.domElement=e,this.enabled=!0,this.state=-1,this.keys={},this.mouseButtons={LEFT:null,MIDDLE:null,RIGHT:null},this.touches={ONE:null,TWO:null}}connect(t){this.domElement!==null&&this.disconnect(),this.domElement=t}disconnect(){}dispose(){}update(){}};function th(n,t,e,i){let s=tm(i);switch(e){case Vc:return n*t;case Os:return n*t/s.components*s.byteLength;case Uo:return n*t/s.components*s.byteLength;case Sn:return n*t*2/s.components*s.byteLength;case Fo:return n*t*2/s.components*s.byteLength;case Gc:return n*t*3/s.components*s.byteLength;case ci:return n*t*4/s.components*s.byteLength;case Oo:return n*t*4/s.components*s.byteLength;case Yr:case Zr:return Math.floor((n+3)/4)*Math.floor((t+3)/4)*8;case Kr:case Jr:return Math.floor((n+3)/4)*Math.floor((t+3)/4)*16;case zo:case Ho:return Math.max(n,16)*Math.max(t,8)/4;case Bo:case ko:return Math.max(n,8)*Math.max(t,8)/2;case Vo:case Go:case Xo:case qo:return Math.floor((n+3)/4)*Math.floor((t+3)/4)*8;case Wo:case $r:case Yo:return Math.floor((n+3)/4)*Math.floor((t+3)/4)*16;case Zo:return Math.floor((n+3)/4)*Math.floor((t+3)/4)*16;case Ko:return Math.floor((n+4)/5)*Math.floor((t+3)/4)*16;case Jo:return Math.floor((n+4)/5)*Math.floor((t+4)/5)*16;case $o:return Math.floor((n+5)/6)*Math.floor((t+4)/5)*16;case jo:return Math.floor((n+5)/6)*Math.floor((t+5)/6)*16;case Qo:return Math.floor((n+7)/8)*Math.floor((t+4)/5)*16;case tl:return Math.floor((n+7)/8)*Math.floor((t+5)/6)*16;case el:return Math.floor((n+7)/8)*Math.floor((t+7)/8)*16;case il:return Math.floor((n+9)/10)*Math.floor((t+4)/5)*16;case nl:return Math.floor((n+9)/10)*Math.floor((t+5)/6)*16;case sl:return Math.floor((n+9)/10)*Math.floor((t+7)/8)*16;case rl:return Math.floor((n+9)/10)*Math.floor((t+9)/10)*16;case al:return Math.floor((n+11)/12)*Math.floor((t+9)/10)*16;case ol:return Math.floor((n+11)/12)*Math.floor((t+11)/12)*16;case ll:case cl:case hl:return Math.ceil(n/4)*Math.ceil(t/4)*16;case ul:case dl:return Math.ceil(n/4)*Math.ceil(t/4)*8;case jr:case fl:return Math.ceil(n/4)*Math.ceil(t/4)*16}throw new Error(`Unable to determine texture byte length for ${e} format.`)}function tm(n){switch(n){case ii:case Bc:return{byteLength:1,components:1};case Fs:case zc:case fe:return{byteLength:2,components:1};case Lo:case No:return{byteLength:2,components:4};case Qe:case Do:case yi:return{byteLength:4,components:1};case kc:case Hc:return{byteLength:4,components:3}}throw new Error(`THREE.TextureUtils: Unknown texture type ${n}.`)}typeof __THREE_DEVTOOLS__<"u"&&__THREE_DEVTOOLS__.dispatchEvent(new CustomEvent("register",{detail:{revision:"187"}}));typeof window<"u"&&(window.__THREE__?Pt("WARNING: Multiple instances of Three.js being imported."):window.__THREE__="187");function Xd(){let n=null,t=!1,e=null,i=null;function s(r,a){i=n.requestAnimationFrame(s),e(r,a)}return{start:function(){t!==!0&&e!==null&&n!==null&&(i=n.requestAnimationFrame(s),t=!0)},stop:function(){n!==null&&n.cancelAnimationFrame(i),t=!1},setAnimationLoop:function(r){e=r},setContext:function(r){n=r}}}function em(n){let t=new WeakMap;function e(o,l){let c=o.array,h=o.usage,u=c.byteLength,d=n.createBuffer();n.bindBuffer(l,d),n.bufferData(l,c,h),o.onUploadCallback();let f;if(c instanceof Float32Array)f=n.FLOAT;else if(typeof Float16Array<"u"&&c instanceof Float16Array)f=n.HALF_FLOAT;else if(c instanceof Uint16Array)o.isFloat16BufferAttribute?f=n.HALF_FLOAT:f=n.UNSIGNED_SHORT;else if(c instanceof Int16Array)f=n.SHORT;else if(c instanceof Uint32Array)f=n.UNSIGNED_INT;else if(c instanceof Int32Array)f=n.INT;else if(c instanceof Int8Array)f=n.BYTE;else if(c instanceof Uint8Array)f=n.UNSIGNED_BYTE;else if(c instanceof Uint8ClampedArray)f=n.UNSIGNED_BYTE;else throw new Error("THREE.WebGLAttributes: Unsupported buffer data format: "+c);return{buffer:d,type:f,bytesPerElement:c.BYTES_PER_ELEMENT,version:o.version,size:u}}function i(o,l,c){let h=l.array,u=l.updateRanges;if(n.bindBuffer(c,o),u.length===0)n.bufferSubData(c,0,h);else{u.sort((f,p)=>f.start-p.start);let d=0;for(let f=1;f<u.length;f++){let p=u[d],v=u[f];v.start<=p.start+p.count+1?p.count=Math.max(p.count,v.start+v.count-p.start):(++d,u[d]=v)}u.length=d+1;for(let f=0,p=u.length;f<p;f++){let v=u[f];n.bufferSubData(c,v.start*h.BYTES_PER_ELEMENT,h,v.start,v.count)}l.clearUpdateRanges()}l.onUploadCallback()}function s(o){return o.isInterleavedBufferAttribute&&(o=o.data),t.get(o)}function r(o){o.isInterleavedBufferAttribute&&(o=o.data);let l=t.get(o);l&&(n.deleteBuffer(l.buffer),t.delete(o))}function a(o,l){if(o.isInterleavedBufferAttribute&&(o=o.data),o.isGLBufferAttribute){let h=t.get(o);(!h||h.version<o.version)&&t.set(o,{buffer:o.buffer,type:o.type,bytesPerElement:o.elementSize,version:o.version});return}let c=t.get(o);if(c===void 0)t.set(o,e(o,l));else if(c.version<o.version){if(c.size!==o.array.byteLength)throw new Error("THREE.WebGLAttributes: The size of the buffer attribute's array buffer does not match the original size. Resizing buffer attributes is not supported.");i(c.buffer,o,l),c.version=o.version}}return{get:s,remove:r,update:a}}var im=`#ifdef USE_ALPHAHASH
	if ( diffuseColor.a < getAlphaHashThreshold( vPosition ) ) discard;
#endif`,nm=`#ifdef USE_ALPHAHASH
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
#endif`,sm=`#ifdef USE_ALPHAMAP
	diffuseColor.a *= texture2D( alphaMap, vAlphaMapUv ).g;
#endif`,rm=`#ifdef USE_ALPHAMAP
	uniform sampler2D alphaMap;
#endif`,am=`#ifdef USE_ALPHATEST
	#ifdef ALPHA_TO_COVERAGE
	diffuseColor.a = smoothstep( alphaTest, alphaTest + fwidth( diffuseColor.a ), diffuseColor.a );
	if ( diffuseColor.a == 0.0 ) discard;
	#else
	if ( diffuseColor.a < alphaTest ) discard;
	#endif
#endif`,om=`#ifdef USE_ALPHATEST
	uniform float alphaTest;
#endif`,lm=`#ifdef USE_AOMAP
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
#endif`,cm=`#ifdef USE_AOMAP
	uniform sampler2D aoMap;
	uniform float aoMapIntensity;
#endif`,hm=`#ifdef USE_BATCHING
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
#endif`,um=`#ifdef USE_BATCHING
	mat4 batchingMatrix = getBatchingMatrix( getIndirectIndex( gl_DrawID ) );
#endif`,dm=`vec3 transformed = vec3( position );
#ifdef USE_ALPHAHASH
	vPosition = vec3( position );
#endif`,fm=`vec3 objectNormal = vec3( normal );
#ifdef USE_TANGENT
	vec3 objectTangent = vec3( tangent.xyz );
#endif`,pm=`float G_BlinnPhong_Implicit( ) {
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
} // validated`,mm=`#ifdef USE_IRIDESCENCE
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
#endif`,gm=`#ifdef USE_BUMPMAP
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
#endif`,xm=`#if NUM_CLIPPING_PLANES > 0
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
#endif`,_m=`#if NUM_CLIPPING_PLANES > 0
	varying vec3 vClipPosition;
	uniform vec4 clippingPlanes[ NUM_CLIPPING_PLANES ];
#endif`,vm=`#if NUM_CLIPPING_PLANES > 0
	varying vec3 vClipPosition;
#endif`,ym=`#if NUM_CLIPPING_PLANES > 0
	vClipPosition = - mvPosition.xyz;
#endif`,Mm=`#if defined( USE_COLOR ) || defined( USE_COLOR_ALPHA )
	diffuseColor *= vColor;
#endif`,Sm=`#if defined( USE_COLOR ) || defined( USE_COLOR_ALPHA )
	varying vec4 vColor;
#endif`,bm=`#if defined( USE_COLOR ) || defined( USE_COLOR_ALPHA ) || defined( USE_INSTANCING_COLOR ) || defined( USE_BATCHING_COLOR )
	varying vec4 vColor;
#endif`,wm=`#if defined( USE_COLOR ) || defined( USE_COLOR_ALPHA ) || defined( USE_INSTANCING_COLOR ) || defined( USE_BATCHING_COLOR )
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
#endif`,Em=`#define PI 3.141592653589793
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
} // validated`,Tm=`vec3 transformedNormal = objectNormal;
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
#endif`,Am=`#ifdef USE_DISPLACEMENTMAP
	uniform sampler2D displacementMap;
	uniform float displacementScale;
	uniform float displacementBias;
#endif`,Rm=`#ifdef USE_DISPLACEMENTMAP
	transformed += normalize( objectNormal ) * ( texture2D( displacementMap, vDisplacementMapUv ).x * displacementScale + displacementBias );
#endif`,Cm=`#ifdef USE_EMISSIVEMAP
	vec4 emissiveColor = texture2D( emissiveMap, vEmissiveMapUv );
	#ifdef DECODE_VIDEO_TEXTURE_EMISSIVE
		emissiveColor = sRGBTransferEOTF( emissiveColor );
	#endif
	totalEmissiveRadiance *= emissiveColor.rgb;
#endif`,Pm=`#ifdef USE_EMISSIVEMAP
	uniform sampler2D emissiveMap;
#endif`,Im="gl_FragColor = linearToOutputTexel( gl_FragColor );",Dm=`vec4 LinearTransferOETF( in vec4 value ) {
	return value;
}
vec4 sRGBTransferEOTF( in vec4 value ) {
	return vec4( mix( pow( value.rgb * 0.9478672986 + vec3( 0.0521327014 ), vec3( 2.4 ) ), value.rgb * 0.0773993808, vec3( lessThanEqual( value.rgb, vec3( 0.04045 ) ) ) ), value.a );
}
vec4 sRGBTransferOETF( in vec4 value ) {
	return vec4( mix( pow( value.rgb, vec3( 0.41666 ) ) * 1.055 - vec3( 0.055 ), value.rgb * 12.92, vec3( lessThanEqual( value.rgb, vec3( 0.0031308 ) ) ) ), value.a );
}`,Lm=`#ifdef USE_ENVMAP
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
#endif`,Nm=`#ifdef USE_ENVMAP
	uniform float envMapIntensity;
	uniform mat3 envMapRotation;
	uniform samplerCube envMap;
	#ifdef ENVMAP_TYPE_PMREM
		float roughnessToMip( const in float roughness ) {
			float r = clamp( roughness, 0.0, 1.0 );
			return ENVMAP_MAX_LOD * r * ( 2.0 - r );
		}
	#endif
#endif`,Um=`#ifdef USE_ENVMAP
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
#endif`,Fm=`#ifdef USE_ENVMAP
	#if defined( USE_BUMPMAP ) || defined( USE_NORMALMAP ) || defined( PHONG ) || defined( LAMBERT )
		#define ENV_WORLDPOS
	#endif
	#ifdef ENV_WORLDPOS

		varying vec3 vWorldPosition;
	#else
		varying vec3 vReflect;
		uniform float refractionRatio;
	#endif
#endif`,Om=`#ifdef USE_ENVMAP
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
#endif`,Bm=`#ifdef USE_FOG
	vFogDepth = - mvPosition.z;
#endif`,zm=`#ifdef USE_FOG
	varying float vFogDepth;
#endif`,km=`#ifdef USE_FOG
	#ifdef FOG_EXP2
		float fogFactor = 1.0 - exp( - fogDensity * fogDensity * vFogDepth * vFogDepth );
	#else
		float fogFactor = smoothstep( fogNear, fogFar, vFogDepth );
	#endif
	gl_FragColor.rgb = mix( gl_FragColor.rgb, fogColor, fogFactor );
#endif`,Hm=`#ifdef USE_FOG
	uniform vec3 fogColor;
	varying float vFogDepth;
	#ifdef FOG_EXP2
		uniform float fogDensity;
	#else
		uniform float fogNear;
		uniform float fogFar;
	#endif
#endif`,Vm=`#ifdef USE_GRADIENTMAP
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
}`,Gm=`#ifdef USE_LIGHTMAP
	uniform sampler2D lightMap;
	uniform float lightMapIntensity;
#endif`,Wm=`LambertMaterial material;
material.diffuseColor = diffuseColor.rgb;
material.specularStrength = specularStrength;`,Xm=`varying vec3 vViewPosition;
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
#define RE_IndirectDiffuse		RE_IndirectDiffuse_Lambert`,qm=`uniform bool receiveShadow;
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
#include <lightprobes_pars_fragment>`,Ym=`#ifdef USE_ENVMAP
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
#endif`,Zm=`ToonMaterial material;
material.diffuseColor = diffuseColor.rgb;`,Km=`varying vec3 vViewPosition;
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
#define RE_IndirectDiffuse		RE_IndirectDiffuse_Toon`,Jm=`BlinnPhongMaterial material;
material.diffuseColor = diffuseColor.rgb;
material.specularColor = specular;
material.specularShininess = shininess;
material.specularStrength = specularStrength;`,$m=`varying vec3 vViewPosition;
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
#define RE_IndirectDiffuse		RE_IndirectDiffuse_BlinnPhong`,jm=`PhysicalMaterial material;
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
#endif`,Qm=`uniform sampler2D dfgLUT;
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
}`,tg=`
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
#endif`,eg=`#if defined( RE_IndirectDiffuse )
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
#endif`,ig=`#if defined( RE_IndirectDiffuse )
	#if defined( LAMBERT ) || defined( PHONG )
		irradiance += iblIrradiance;
	#endif
	RE_IndirectDiffuse( irradiance, geometryPosition, geometryNormal, geometryViewDir, geometryClearcoatNormal, material, reflectedLight );
#endif
#if defined( RE_IndirectSpecular )
	RE_IndirectSpecular( radiance, iblIrradiance, clearcoatRadiance, geometryPosition, geometryNormal, geometryViewDir, geometryClearcoatNormal, material, reflectedLight );
#endif`,ng=`#if NUM_LIGHT_PROBE_GRIDS > 0
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
#endif`,sg=`#if defined( USE_LOGARITHMIC_DEPTH_BUFFER )
	gl_FragDepth = vIsPerspective == 0.0 ? gl_FragCoord.z : log2( vFragDepth ) * logDepthBufFC * 0.5;
#endif`,rg=`#if defined( USE_LOGARITHMIC_DEPTH_BUFFER )
	uniform float logDepthBufFC;
	varying float vFragDepth;
	varying float vIsPerspective;
#endif`,ag=`#ifdef USE_LOGARITHMIC_DEPTH_BUFFER
	varying float vFragDepth;
	varying float vIsPerspective;
#endif`,og=`#ifdef USE_LOGARITHMIC_DEPTH_BUFFER
	vFragDepth = 1.0 + gl_Position.w;
	vIsPerspective = float( isPerspectiveMatrix( projectionMatrix ) );
#endif`,lg=`#ifdef USE_MAP
	vec4 sampledDiffuseColor = texture2D( map, vMapUv );
	#ifdef DECODE_VIDEO_TEXTURE
		sampledDiffuseColor = sRGBTransferEOTF( sampledDiffuseColor );
	#endif
	diffuseColor *= sampledDiffuseColor;
#endif`,cg=`#ifdef USE_MAP
	uniform sampler2D map;
#endif`,hg=`#if defined( USE_MAP ) || defined( USE_ALPHAMAP )
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
#endif`,ug=`#if defined( USE_POINTS_UV )
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
#endif`,dg=`float metalnessFactor = metalness;
#ifdef USE_METALNESSMAP
	vec4 texelMetalness = texture2D( metalnessMap, vMetalnessMapUv );
	metalnessFactor *= texelMetalness.b;
#endif`,fg=`#ifdef USE_METALNESSMAP
	uniform sampler2D metalnessMap;
#endif`,pg=`#ifdef USE_INSTANCING_MORPH
	float morphTargetInfluences[ MORPHTARGETS_COUNT ];
	float morphTargetBaseInfluence = texelFetch( morphTexture, ivec2( 0, gl_InstanceID ), 0 ).r;
	for ( int i = 0; i < MORPHTARGETS_COUNT; i ++ ) {
		morphTargetInfluences[i] =  texelFetch( morphTexture, ivec2( i + 1, gl_InstanceID ), 0 ).r;
	}
#endif`,mg=`#if defined( USE_MORPHCOLORS )
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
#endif`,gg=`#ifdef USE_MORPHNORMALS
	objectNormal *= morphTargetBaseInfluence;
	for ( int i = 0; i < MORPHTARGETS_COUNT; i ++ ) {
		if ( morphTargetInfluences[ i ] != 0.0 ) objectNormal += getMorph( gl_VertexID, i, 1 ).xyz * morphTargetInfluences[ i ];
	}
#endif`,xg=`#ifdef USE_MORPHTARGETS
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
#endif`,_g=`#ifdef USE_MORPHTARGETS
	transformed *= morphTargetBaseInfluence;
	for ( int i = 0; i < MORPHTARGETS_COUNT; i ++ ) {
		if ( morphTargetInfluences[ i ] != 0.0 ) transformed += getMorph( gl_VertexID, i, 0 ).xyz * morphTargetInfluences[ i ];
	}
#endif`,vg=`float faceDirection = gl_FrontFacing ? 1.0 : - 1.0;
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
vec3 nonPerturbedNormal = normal;`,yg=`#ifdef USE_NORMALMAP_OBJECTSPACE
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
#endif`,Mg=`#ifndef FLAT_SHADED
	varying vec3 vNormal;
	#ifdef USE_TANGENT
		varying vec3 vTangent;
		varying vec3 vBitangent;
	#endif
#endif`,Sg=`#ifndef FLAT_SHADED
	varying vec3 vNormal;
	#ifdef USE_TANGENT
		varying vec3 vTangent;
		varying vec3 vBitangent;
	#endif
#endif`,bg=`#ifndef FLAT_SHADED
	vNormal = normalize( transformedNormal );
	#ifdef USE_TANGENT
		vTangent = normalize( transformedTangent );
		vBitangent = normalize( cross( vNormal, vTangent ) * tangent.w );
		#ifdef FLIP_SIDED
			vBitangent = - vBitangent;
		#endif
	#endif
#endif`,wg=`#ifdef USE_NORMALMAP
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
#endif`,Eg=`#ifdef USE_CLEARCOAT
	vec3 clearcoatNormal = nonPerturbedNormal;
#endif`,Tg=`#ifdef USE_CLEARCOAT_NORMALMAP
	vec3 clearcoatMapN = texture2D( clearcoatNormalMap, vClearcoatNormalMapUv ).xyz * 2.0 - 1.0;
	clearcoatMapN.xy *= clearcoatNormalScale;
	clearcoatNormal = normalize( tbn2 * clearcoatMapN );
#endif`,Ag=`#ifdef USE_CLEARCOATMAP
	uniform sampler2D clearcoatMap;
#endif
#ifdef USE_CLEARCOAT_NORMALMAP
	uniform sampler2D clearcoatNormalMap;
	uniform vec2 clearcoatNormalScale;
#endif
#ifdef USE_CLEARCOAT_ROUGHNESSMAP
	uniform sampler2D clearcoatRoughnessMap;
#endif`,Rg=`#ifdef USE_IRIDESCENCEMAP
	uniform sampler2D iridescenceMap;
#endif
#ifdef USE_IRIDESCENCE_THICKNESSMAP
	uniform sampler2D iridescenceThicknessMap;
#endif`,Cg=`#ifdef OPAQUE
diffuseColor.a = 1.0;
#endif
#ifdef USE_TRANSMISSION
diffuseColor.a *= material.transmissionAlpha;
#endif
gl_FragColor = vec4( outgoingLight, diffuseColor.a );`,Pg=`vec3 packNormalToRGB( const in vec3 normal ) {
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
}`,Ig=`#ifdef PREMULTIPLIED_ALPHA
	gl_FragColor.rgb *= gl_FragColor.a;
#endif`,Dg=`vec4 mvPosition = vec4( transformed, 1.0 );
#ifdef USE_BATCHING
	mvPosition = batchingMatrix * mvPosition;
#endif
#ifdef USE_INSTANCING
	mvPosition = instanceMatrix * mvPosition;
#endif
mvPosition = modelViewMatrix * mvPosition;
gl_Position = projectionMatrix * mvPosition;`,Lg=`#ifdef DITHERING
	gl_FragColor.rgb = dithering( gl_FragColor.rgb );
#endif`,Ng=`#ifdef DITHERING
	vec3 dithering( vec3 color ) {
		float grid_position = rand( gl_FragCoord.xy );
		vec3 dither_shift_RGB = vec3( 0.25 / 255.0, -0.25 / 255.0, 0.25 / 255.0 );
		dither_shift_RGB = mix( 2.0 * dither_shift_RGB, -2.0 * dither_shift_RGB, grid_position );
		return color + dither_shift_RGB;
	}
#endif`,Ug=`float roughnessFactor = roughness;
#ifdef USE_ROUGHNESSMAP
	vec4 texelRoughness = texture2D( roughnessMap, vRoughnessMapUv );
	roughnessFactor *= texelRoughness.g;
#endif`,Fg=`#ifdef USE_ROUGHNESSMAP
	uniform sampler2D roughnessMap;
#endif`,Og=`#if NUM_SPOT_LIGHT_COORDS > 0
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
#endif`,Bg=`#if NUM_SPOT_LIGHT_COORDS > 0
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
#endif`,zg=`#if ( defined( USE_SHADOWMAP ) && ( NUM_DIR_LIGHT_SHADOWS > 0 || NUM_SUN_LIGHT_SHADOWS > 0 || NUM_POINT_LIGHT_SHADOWS > 0 ) ) || ( NUM_SPOT_LIGHT_COORDS > 0 )
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
#endif`,kg=`float getShadowMask() {
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
}`,Hg=`#ifdef USE_SKINNING
	mat4 boneMatX = getBoneMatrix( skinIndex.x );
	mat4 boneMatY = getBoneMatrix( skinIndex.y );
	mat4 boneMatZ = getBoneMatrix( skinIndex.z );
	mat4 boneMatW = getBoneMatrix( skinIndex.w );
#endif`,Vg=`#ifdef USE_SKINNING
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
#endif`,Gg=`#ifdef USE_SKINNING
	vec4 skinVertex = bindMatrix * vec4( transformed, 1.0 );
	vec4 skinned = vec4( 0.0 );
	skinned += boneMatX * skinVertex * skinWeight.x;
	skinned += boneMatY * skinVertex * skinWeight.y;
	skinned += boneMatZ * skinVertex * skinWeight.z;
	skinned += boneMatW * skinVertex * skinWeight.w;
	transformed = ( bindMatrixInverse * skinned ).xyz;
#endif`,Wg=`#ifdef USE_SKINNING
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
#endif`,Xg=`float specularStrength;
#ifdef USE_SPECULARMAP
	vec4 texelSpecular = texture2D( specularMap, vSpecularMapUv );
	specularStrength = texelSpecular.r;
#else
	specularStrength = 1.0;
#endif`,qg=`#ifdef USE_SPECULARMAP
	uniform sampler2D specularMap;
#endif`,Yg=`#if defined( TONE_MAPPING )
	gl_FragColor.rgb = toneMapping( gl_FragColor.rgb );
#endif`,Zg=`#ifndef saturate
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
vec3 CustomToneMapping( vec3 color ) { return color; }`,Kg=`#ifdef USE_TRANSMISSION
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
#endif`,Jg=`#ifdef USE_TRANSMISSION
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
#endif`,$g=`#if defined( USE_UV ) || defined( USE_ANISOTROPY )
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
#endif`,jg=`#if defined( USE_UV ) || defined( USE_ANISOTROPY )
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
#endif`,Qg=`#if defined( USE_UV ) || defined( USE_ANISOTROPY )
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
#endif`,t0=`#if defined( USE_ENVMAP ) || defined( DISTANCE ) || defined ( USE_SHADOWMAP ) || defined ( USE_TRANSMISSION ) || NUM_SPOT_LIGHT_COORDS > 0
	vec4 worldPosition = vec4( transformed, 1.0 );
	#ifdef USE_BATCHING
		worldPosition = batchingMatrix * worldPosition;
	#endif
	#ifdef USE_INSTANCING
		worldPosition = instanceMatrix * worldPosition;
	#endif
	worldPosition = modelMatrix * worldPosition;
#endif`,e0=`varying vec2 vUv;
uniform mat3 uvTransform;
void main() {
	vUv = ( uvTransform * vec3( uv, 1 ) ).xy;
	gl_Position = vec4( position.xy, 1.0, 1.0 );
}`,i0=`uniform sampler2D t2D;
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
}`,n0=`varying vec3 vWorldDirection;
#include <common>
void main() {
	vWorldDirection = transformDirection( position, modelMatrix );
	#include <begin_vertex>
	#include <project_vertex>
	gl_Position.z = gl_Position.w;
}`,s0=`uniform float backgroundBlurriness;
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
}`,r0=`varying vec3 vWorldDirection;
#include <common>
void main() {
	vWorldDirection = transformDirection( position, modelMatrix );
	#include <begin_vertex>
	#include <project_vertex>
	gl_Position.z = gl_Position.w;
}`,a0=`uniform samplerCube tCube;
uniform float tFlip;
uniform float opacity;
varying vec3 vWorldDirection;
void main() {
	vec4 texColor = textureCube( tCube, vec3( tFlip * vWorldDirection.x, vWorldDirection.yz ) );
	gl_FragColor = texColor;
	gl_FragColor.a *= opacity;
	#include <tonemapping_fragment>
	#include <colorspace_fragment>
}`,o0=`#include <common>
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
}`,l0=`#if DEPTH_PACKING == 3200
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
}`,c0=`#define DISTANCE
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
}`,h0=`#define DISTANCE
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
}`,u0=`varying vec3 vWorldDirection;
#include <common>
void main() {
	vWorldDirection = transformDirection( position, modelMatrix );
	#include <begin_vertex>
	#include <project_vertex>
}`,d0=`uniform sampler2D tEquirect;
varying vec3 vWorldDirection;
#include <common>
void main() {
	vec3 direction = normalize( vWorldDirection );
	vec2 sampleUV = equirectUv( direction );
	gl_FragColor = texture2D( tEquirect, sampleUV );
	#include <tonemapping_fragment>
	#include <colorspace_fragment>
}`,f0=`uniform float scale;
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
}`,p0=`uniform vec3 diffuse;
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
}`,m0=`#include <common>
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
}`,g0=`uniform vec3 diffuse;
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
}`,x0=`#define LAMBERT
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
}`,_0=`#define LAMBERT
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
}`,v0=`#define MATCAP
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
}`,y0=`#define MATCAP
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
}`,M0=`#define NORMAL
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
}`,S0=`#define NORMAL
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
}`,b0=`#define PHONG
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
}`,w0=`#define PHONG
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
}`,E0=`#define STANDARD
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
}`,T0=`#define STANDARD
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
}`,A0=`#define TOON
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
}`,R0=`#define TOON
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
}`,C0=`uniform float size;
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
}`,P0=`uniform vec3 diffuse;
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
}`,I0=`#include <common>
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
}`,D0=`uniform vec3 color;
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
}`,L0=`uniform float rotation;
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
}`,N0=`uniform vec3 diffuse;
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
}`,zt={alphahash_fragment:im,alphahash_pars_fragment:nm,alphamap_fragment:sm,alphamap_pars_fragment:rm,alphatest_fragment:am,alphatest_pars_fragment:om,aomap_fragment:lm,aomap_pars_fragment:cm,batching_pars_vertex:hm,batching_vertex:um,begin_vertex:dm,beginnormal_vertex:fm,bsdfs:pm,iridescence_fragment:mm,bumpmap_pars_fragment:gm,clipping_planes_fragment:xm,clipping_planes_pars_fragment:_m,clipping_planes_pars_vertex:vm,clipping_planes_vertex:ym,color_fragment:Mm,color_pars_fragment:Sm,color_pars_vertex:bm,color_vertex:wm,common:Em,defaultnormal_vertex:Tm,displacementmap_pars_vertex:Am,displacementmap_vertex:Rm,emissivemap_fragment:Cm,emissivemap_pars_fragment:Pm,colorspace_fragment:Im,colorspace_pars_fragment:Dm,envmap_fragment:Lm,envmap_common_pars_fragment:Nm,envmap_pars_fragment:Um,envmap_pars_vertex:Fm,envmap_physical_pars_fragment:Ym,envmap_vertex:Om,fog_vertex:Bm,fog_pars_vertex:zm,fog_fragment:km,fog_pars_fragment:Hm,gradientmap_pars_fragment:Vm,lightmap_pars_fragment:Gm,lights_lambert_fragment:Wm,lights_lambert_pars_fragment:Xm,lights_pars_begin:qm,lights_toon_fragment:Zm,lights_toon_pars_fragment:Km,lights_phong_fragment:Jm,lights_phong_pars_fragment:$m,lights_physical_fragment:jm,lights_physical_pars_fragment:Qm,lights_fragment_begin:tg,lights_fragment_maps:eg,lights_fragment_end:ig,lightprobes_pars_fragment:ng,logdepthbuf_fragment:sg,logdepthbuf_pars_fragment:rg,logdepthbuf_pars_vertex:ag,logdepthbuf_vertex:og,map_fragment:lg,map_pars_fragment:cg,map_particle_fragment:hg,map_particle_pars_fragment:ug,metalnessmap_fragment:dg,metalnessmap_pars_fragment:fg,morphinstance_vertex:pg,morphcolor_vertex:mg,morphnormal_vertex:gg,morphtarget_pars_vertex:xg,morphtarget_vertex:_g,normal_fragment_begin:vg,normal_fragment_maps:yg,normal_pars_fragment:Mg,normal_pars_vertex:Sg,normal_vertex:bg,normalmap_pars_fragment:wg,clearcoat_normal_fragment_begin:Eg,clearcoat_normal_fragment_maps:Tg,clearcoat_pars_fragment:Ag,iridescence_pars_fragment:Rg,opaque_fragment:Cg,packing:Pg,premultiplied_alpha_fragment:Ig,project_vertex:Dg,dithering_fragment:Lg,dithering_pars_fragment:Ng,roughnessmap_fragment:Ug,roughnessmap_pars_fragment:Fg,shadowmap_pars_fragment:Og,shadowmap_pars_vertex:Bg,shadowmap_vertex:zg,shadowmask_pars_fragment:kg,skinbase_vertex:Hg,skinning_pars_vertex:Vg,skinning_vertex:Gg,skinnormal_vertex:Wg,specularmap_fragment:Xg,specularmap_pars_fragment:qg,tonemapping_fragment:Yg,tonemapping_pars_fragment:Zg,transmission_fragment:Kg,transmission_pars_fragment:Jg,uv_pars_fragment:$g,uv_pars_vertex:jg,uv_vertex:Qg,worldpos_vertex:t0,background_vert:e0,background_frag:i0,backgroundCube_vert:n0,backgroundCube_frag:s0,cube_vert:r0,cube_frag:a0,depth_vert:o0,depth_frag:l0,distance_vert:c0,distance_frag:h0,equirect_vert:u0,equirect_frag:d0,linedashed_vert:f0,linedashed_frag:p0,meshbasic_vert:m0,meshbasic_frag:g0,meshlambert_vert:x0,meshlambert_frag:_0,meshmatcap_vert:v0,meshmatcap_frag:y0,meshnormal_vert:M0,meshnormal_frag:S0,meshphong_vert:b0,meshphong_frag:w0,meshphysical_vert:E0,meshphysical_frag:T0,meshtoon_vert:A0,meshtoon_frag:R0,points_vert:C0,points_frag:P0,shadow_vert:I0,shadow_frag:D0,sprite_vert:L0,sprite_frag:N0},_t={common:{diffuse:{value:new Mt(16777215)},opacity:{value:1},map:{value:null},mapTransform:{value:new Nt},alphaMap:{value:null},alphaMapTransform:{value:new Nt},alphaTest:{value:0}},specularmap:{specularMap:{value:null},specularMapTransform:{value:new Nt}},envmap:{envMap:{value:null},envMapRotation:{value:new Nt},reflectivity:{value:1},ior:{value:1.5},refractionRatio:{value:.98},dfgLUT:{value:null}},aomap:{aoMap:{value:null},aoMapIntensity:{value:1},aoMapTransform:{value:new Nt}},lightmap:{lightMap:{value:null},lightMapIntensity:{value:1},lightMapTransform:{value:new Nt}},bumpmap:{bumpMap:{value:null},bumpMapTransform:{value:new Nt},bumpScale:{value:1}},normalmap:{normalMap:{value:null},normalMapTransform:{value:new Nt},normalScale:{value:new et(1,1)}},displacementmap:{displacementMap:{value:null},displacementMapTransform:{value:new Nt},displacementScale:{value:1},displacementBias:{value:0}},emissivemap:{emissiveMap:{value:null},emissiveMapTransform:{value:new Nt}},metalnessmap:{metalnessMap:{value:null},metalnessMapTransform:{value:new Nt}},roughnessmap:{roughnessMap:{value:null},roughnessMapTransform:{value:new Nt}},gradientmap:{gradientMap:{value:null}},fog:{fogDensity:{value:25e-5},fogNear:{value:1},fogFar:{value:2e3},fogColor:{value:new Mt(16777215)}},lights:{ambientLightColor:{value:[]},lightProbe:{value:[]},sunLights:{value:[],properties:{direction:{},color:{}}},sunLightShadows:{value:[],properties:{shadowIntensity:1,shadowBias:{},shadowNormalBias:{},shadowRadius:{},shadowMapSize:{}}},sunShadowMatrix:{value:[]},sunShadowCascade:{value:[]},directionalLights:{value:[],properties:{direction:{},color:{}}},directionalLightShadows:{value:[],properties:{shadowIntensity:1,shadowBias:{},shadowNormalBias:{},shadowRadius:{},shadowMapSize:{}}},directionalShadowMatrix:{value:[]},spotLights:{value:[],properties:{color:{},position:{},direction:{},distance:{},coneCos:{},penumbraCos:{},decay:{}}},spotLightShadows:{value:[],properties:{shadowIntensity:1,shadowBias:{},shadowNormalBias:{},shadowRadius:{},shadowMapSize:{}}},spotLightMap:{value:[]},spotLightMatrix:{value:[]},pointLights:{value:[],properties:{color:{},position:{},decay:{},distance:{}}},pointLightShadows:{value:[],properties:{shadowIntensity:1,shadowBias:{},shadowNormalBias:{},shadowRadius:{},shadowMapSize:{},shadowCameraNear:{},shadowCameraFar:{}}},pointShadowMatrix:{value:[]},hemisphereLights:{value:[],properties:{direction:{},skyColor:{},groundColor:{}}},rectAreaLights:{value:[],properties:{color:{},position:{},width:{},height:{}}},ltc_1:{value:null},ltc_2:{value:null},probesSH:{value:[]},probesMin:{value:[]},probesMax:{value:[]},probesResolution:{value:[]}},points:{diffuse:{value:new Mt(16777215)},opacity:{value:1},size:{value:1},scale:{value:1},map:{value:null},alphaMap:{value:null},alphaMapTransform:{value:new Nt},alphaTest:{value:0},uvTransform:{value:new Nt}},sprite:{diffuse:{value:new Mt(16777215)},opacity:{value:1},center:{value:new et(.5,.5)},rotation:{value:0},map:{value:null},mapTransform:{value:new Nt},alphaMap:{value:null},alphaMapTransform:{value:new Nt},alphaTest:{value:0}}},Gi={basic:{uniforms:Ye([_t.common,_t.specularmap,_t.envmap,_t.aomap,_t.lightmap,_t.fog]),vertexShader:zt.meshbasic_vert,fragmentShader:zt.meshbasic_frag},lambert:{uniforms:Ye([_t.common,_t.specularmap,_t.envmap,_t.aomap,_t.lightmap,_t.emissivemap,_t.bumpmap,_t.normalmap,_t.displacementmap,_t.fog,_t.lights,{emissive:{value:new Mt(0)},envMapIntensity:{value:1}}]),vertexShader:zt.meshlambert_vert,fragmentShader:zt.meshlambert_frag},phong:{uniforms:Ye([_t.common,_t.specularmap,_t.envmap,_t.aomap,_t.lightmap,_t.emissivemap,_t.bumpmap,_t.normalmap,_t.displacementmap,_t.fog,_t.lights,{emissive:{value:new Mt(0)},specular:{value:new Mt(1118481)},shininess:{value:30},envMapIntensity:{value:1}}]),vertexShader:zt.meshphong_vert,fragmentShader:zt.meshphong_frag},standard:{uniforms:Ye([_t.common,_t.envmap,_t.aomap,_t.lightmap,_t.emissivemap,_t.bumpmap,_t.normalmap,_t.displacementmap,_t.roughnessmap,_t.metalnessmap,_t.fog,_t.lights,{emissive:{value:new Mt(0)},roughness:{value:1},metalness:{value:0},envMapIntensity:{value:1}}]),vertexShader:zt.meshphysical_vert,fragmentShader:zt.meshphysical_frag},toon:{uniforms:Ye([_t.common,_t.aomap,_t.lightmap,_t.emissivemap,_t.bumpmap,_t.normalmap,_t.displacementmap,_t.gradientmap,_t.fog,_t.lights,{emissive:{value:new Mt(0)}}]),vertexShader:zt.meshtoon_vert,fragmentShader:zt.meshtoon_frag},matcap:{uniforms:Ye([_t.common,_t.bumpmap,_t.normalmap,_t.displacementmap,_t.fog,{matcap:{value:null}}]),vertexShader:zt.meshmatcap_vert,fragmentShader:zt.meshmatcap_frag},points:{uniforms:Ye([_t.points,_t.fog]),vertexShader:zt.points_vert,fragmentShader:zt.points_frag},dashed:{uniforms:Ye([_t.common,_t.fog,{scale:{value:1},dashSize:{value:1},totalSize:{value:2}}]),vertexShader:zt.linedashed_vert,fragmentShader:zt.linedashed_frag},depth:{uniforms:Ye([_t.common,_t.displacementmap]),vertexShader:zt.depth_vert,fragmentShader:zt.depth_frag},normal:{uniforms:Ye([_t.common,_t.bumpmap,_t.normalmap,_t.displacementmap,{opacity:{value:1}}]),vertexShader:zt.meshnormal_vert,fragmentShader:zt.meshnormal_frag},sprite:{uniforms:Ye([_t.sprite,_t.fog]),vertexShader:zt.sprite_vert,fragmentShader:zt.sprite_frag},background:{uniforms:{uvTransform:{value:new Nt},t2D:{value:null},backgroundIntensity:{value:1}},vertexShader:zt.background_vert,fragmentShader:zt.background_frag},backgroundCube:{uniforms:{envMap:{value:null},backgroundBlurriness:{value:0},backgroundIntensity:{value:1},backgroundRotation:{value:new Nt}},vertexShader:zt.backgroundCube_vert,fragmentShader:zt.backgroundCube_frag},cube:{uniforms:{tCube:{value:null},tFlip:{value:-1},opacity:{value:1}},vertexShader:zt.cube_vert,fragmentShader:zt.cube_frag},equirect:{uniforms:{tEquirect:{value:null}},vertexShader:zt.equirect_vert,fragmentShader:zt.equirect_frag},distance:{uniforms:Ye([_t.common,_t.displacementmap,{referencePosition:{value:new A},nearDistance:{value:1},farDistance:{value:1e3}}]),vertexShader:zt.distance_vert,fragmentShader:zt.distance_frag},shadow:{uniforms:Ye([_t.lights,_t.fog,{color:{value:new Mt(0)},opacity:{value:1}}]),vertexShader:zt.shadow_vert,fragmentShader:zt.shadow_frag}};Gi.physical={uniforms:Ye([Gi.standard.uniforms,{clearcoat:{value:0},clearcoatMap:{value:null},clearcoatMapTransform:{value:new Nt},clearcoatNormalMap:{value:null},clearcoatNormalMapTransform:{value:new Nt},clearcoatNormalScale:{value:new et(1,1)},clearcoatRoughness:{value:0},clearcoatRoughnessMap:{value:null},clearcoatRoughnessMapTransform:{value:new Nt},diffuseRoughness:{value:0},diffuseRoughnessMap:{value:null},diffuseRoughnessMapTransform:{value:new Nt},dispersion:{value:0},retroreflectivity:{value:0},iridescence:{value:0},iridescenceMap:{value:null},iridescenceMapTransform:{value:new Nt},iridescenceIOR:{value:1.3},iridescenceThicknessMinimum:{value:100},iridescenceThicknessMaximum:{value:400},iridescenceThicknessMap:{value:null},iridescenceThicknessMapTransform:{value:new Nt},sheen:{value:0},sheenColor:{value:new Mt(0)},sheenColorMap:{value:null},sheenColorMapTransform:{value:new Nt},sheenRoughness:{value:1},sheenRoughnessMap:{value:null},sheenRoughnessMapTransform:{value:new Nt},transmission:{value:0},transmissionMap:{value:null},transmissionMapTransform:{value:new Nt},transmissionSamplerSize:{value:new et},transmissionSamplerMap:{value:null},thickness:{value:0},thicknessMap:{value:null},thicknessMapTransform:{value:new Nt},attenuationDistance:{value:0},attenuationColor:{value:new Mt(0)},specularColor:{value:new Mt(1,1,1)},specularColorMap:{value:null},specularColorMapTransform:{value:new Nt},specularIntensity:{value:1},specularIntensityMap:{value:null},specularIntensityMapTransform:{value:new Nt},anisotropyVector:{value:new et},anisotropyMap:{value:null},anisotropyMapTransform:{value:new Nt}}]),vertexShader:zt.meshphysical_vert,fragmentShader:zt.meshphysical_frag};var pl={r:0,b:0,g:0},U0=new Gt,qd=new Nt;qd.set(-1,0,0,0,1,0,0,0,1);function F0(n,t,e,i,s,r){let a=new Mt(0),o=s===!0?0:1,l,c,h=null,u=0,d=null;function f(_){let S=_.isScene===!0?_.background:null;if(S&&S.isTexture){let M=_.backgroundBlurriness>0;S=t.get(S,M)}return S}function p(_){let S=!1,M=f(_);M===null?g(a,o):M&&M.isColor&&(g(M,1),S=!0);let T=n.xr.getEnvironmentBlendMode();T==="additive"?e.buffers.color.setClear(0,0,0,1,r):T==="alpha-blend"&&e.buffers.color.setClear(0,0,0,0,r),(n.autoClear||S)&&(e.buffers.depth.setTest(!0),e.buffers.depth.setMask(!0),e.buffers.color.setMask(!0),n.clear(n.autoClearColor,n.autoClearDepth,n.autoClearStencil))}function v(_,S){let M=f(S);M&&M.isCubeTexture?(c===void 0&&(c=new Ut(new ve(1,1,1),new oe({name:"BackgroundCubeMaterial",uniforms:jn(Gi.backgroundCube.uniforms),vertexShader:Gi.backgroundCube.vertexShader,fragmentShader:Gi.backgroundCube.fragmentShader,side:ke,depthTest:!1,depthWrite:!1,fog:!1,allowOverride:!1})),c.geometry.deleteAttribute("normal"),c.geometry.deleteAttribute("uv"),c.onBeforeRender=function(T,w,x){this.matrixWorld.copyPosition(x.matrixWorld)},Object.defineProperty(c.material,"envMap",{get:function(){return this.uniforms.envMap.value}}),i.update(c)),c.material.uniforms.envMap.value=M,c.material.uniforms.backgroundBlurriness.value=S.backgroundBlurriness,c.material.uniforms.backgroundIntensity.value=S.backgroundIntensity,c.material.uniforms.backgroundRotation.value.setFromMatrix4(U0.makeRotationFromEuler(S.backgroundRotation)).transpose(),M.isRenderTargetTexture===!1&&c.material.uniforms.backgroundRotation.value.premultiply(qd),c.material.toneMapped=Wt.getTransfer(M.colorSpace)!==Jt,(h!==M||u!==M.version||d!==n.toneMapping)&&(c.material.needsUpdate=!0,h=M,u=M.version,d=n.toneMapping),c.layers.enableAll(),_.unshift(c,c.geometry,c.material,0,0,null)):M&&M.isTexture&&(l===void 0&&(l=new Ut(new Di(2,2),new oe({name:"BackgroundMaterial",uniforms:jn(Gi.background.uniforms),vertexShader:Gi.background.vertexShader,fragmentShader:Gi.background.fragmentShader,side:Mn,depthTest:!1,depthWrite:!1,fog:!1,allowOverride:!1})),l.geometry.deleteAttribute("normal"),Object.defineProperty(l.material,"map",{get:function(){return this.uniforms.t2D.value}}),i.update(l)),l.material.uniforms.t2D.value=M,l.material.uniforms.backgroundIntensity.value=S.backgroundIntensity,l.material.toneMapped=Wt.getTransfer(M.colorSpace)!==Jt,M.matrixAutoUpdate===!0&&M.updateMatrix(),l.material.uniforms.uvTransform.value.copy(M.matrix),(h!==M||u!==M.version||d!==n.toneMapping)&&(l.material.needsUpdate=!0,h=M,u=M.version,d=n.toneMapping),l.layers.enableAll(),_.unshift(l,l.geometry,l.material,0,0,null))}function g(_,S){_.getRGB(pl,$c(n)),e.buffers.color.setClear(pl.r,pl.g,pl.b,S,r)}function m(){c!==void 0&&(c.geometry.dispose(),c.material.dispose(),c=void 0),l!==void 0&&(l.geometry.dispose(),l.material.dispose(),l=void 0)}return{getClearColor:function(){return a},setClearColor:function(_,S=1){a.set(_),o=S,g(a,o)},getClearAlpha:function(){return o},setClearAlpha:function(_){o=_,g(a,o)},render:p,addToRenderList:v,dispose:m}}function O0(n,t){let e=n.getParameter(n.MAX_VERTEX_ATTRIBS),i={},s=d(null),r=s,a=!1;function o(I,L,F,D,B){let G=!1,k=u(I,D,F,L);r!==k&&(r=k,c(r.object)),G=f(I,D,F,B),G&&p(I,D,F,B),B!==null&&t.update(B,n.ELEMENT_ARRAY_BUFFER),(G||a)&&(a=!1,M(I,L,F,D),B!==null&&n.bindBuffer(n.ELEMENT_ARRAY_BUFFER,t.get(B).buffer))}function l(){return n.createVertexArray()}function c(I){return n.bindVertexArray(I)}function h(I){return n.deleteVertexArray(I)}function u(I,L,F,D){let B=D.wireframe===!0,G=i[L.id];G===void 0&&(G={},i[L.id]=G);let k=I.isInstancedMesh===!0?I.id:0,j=G[k];j===void 0&&(j={},G[k]=j);let W=j[F.id];W===void 0&&(W={},j[F.id]=W);let K=W[B];return K===void 0&&(K=d(l()),W[B]=K),K}function d(I){let L=[],F=[],D=[];for(let B=0;B<e;B++)L[B]=0,F[B]=0,D[B]=0;return{geometry:null,program:null,wireframe:!1,newAttributes:L,enabledAttributes:F,attributeDivisors:D,object:I,attributes:{},index:null}}function f(I,L,F,D){let B=r.attributes,G=L.attributes,k=0,j=F.getAttributes();for(let W in j)if(j[W].location>=0){let Q=B[W],dt=G[W];if(dt===void 0&&(W==="instanceMatrix"&&I.instanceMatrix&&(dt=I.instanceMatrix),W==="instanceColor"&&I.instanceColor&&(dt=I.instanceColor)),Q===void 0||Q.attribute!==dt||dt&&Q.data!==dt.data)return!0;k++}return r.attributesNum!==k||r.index!==D}function p(I,L,F,D){let B={},G=L.attributes,k=0,j=F.getAttributes();for(let W in j)if(j[W].location>=0){let Q=G[W];Q===void 0&&(W==="instanceMatrix"&&I.instanceMatrix&&(Q=I.instanceMatrix),W==="instanceColor"&&I.instanceColor&&(Q=I.instanceColor));let dt={};dt.attribute=Q,Q&&Q.data&&(dt.data=Q.data),B[W]=dt,k++}r.attributes=B,r.attributesNum=k,r.index=D}function v(){let I=r.newAttributes;for(let L=0,F=I.length;L<F;L++)I[L]=0}function g(I){m(I,0)}function m(I,L){let F=r.newAttributes,D=r.enabledAttributes,B=r.attributeDivisors;F[I]=1,D[I]===0&&(n.enableVertexAttribArray(I),D[I]=1),B[I]!==L&&(n.vertexAttribDivisor(I,L),B[I]=L)}function _(){let I=r.newAttributes,L=r.enabledAttributes;for(let F=0,D=L.length;F<D;F++)L[F]!==I[F]&&(n.disableVertexAttribArray(F),L[F]=0)}function S(I,L,F,D,B,G,k){k===!0?n.vertexAttribIPointer(I,L,F,B,G):n.vertexAttribPointer(I,L,F,D,B,G)}function M(I,L,F,D){v();let B=D.attributes,G=F.getAttributes(),k=L.defaultAttributeValues;for(let j in G){let W=G[j];if(W.location>=0){let K=B[j];if(K===void 0&&(j==="instanceMatrix"&&I.instanceMatrix&&(K=I.instanceMatrix),j==="instanceColor"&&I.instanceColor&&(K=I.instanceColor)),K!==void 0){let Q=K.normalized,dt=K.itemSize,yt=t.get(K);if(yt===void 0)continue;let q=yt.buffer,at=yt.type,gt=yt.bytesPerElement,Et=at===n.INT||at===n.UNSIGNED_INT||K.gpuType===Do;if(K.isInterleavedBufferAttribute){let rt=K.data,Dt=rt.isInterleavedBuffer===!0?rt.stride:rt.itemSize,se=K.offset;if(rt.isInstancedInterleavedBuffer||rt.isInstancedBufferAttribute){for(let Rt=0;Rt<W.locationSize;Rt++)m(W.location+Rt,rt.meshPerAttribute);I.isInstancedMesh!==!0&&D._maxInstanceCount===void 0&&(D._maxInstanceCount=rt.meshPerAttribute*rt.count)}else for(let Rt=0;Rt<W.locationSize;Rt++)g(W.location+Rt);n.bindBuffer(n.ARRAY_BUFFER,q);for(let Rt=0;Rt<W.locationSize;Rt++)S(W.location+Rt,dt/W.locationSize,at,Q,Dt*gt,(se+dt/W.locationSize*Rt)*gt,Et)}else{if(K.isInstancedBufferAttribute){for(let rt=0;rt<W.locationSize;rt++)m(W.location+rt,K.meshPerAttribute);I.isInstancedMesh!==!0&&D._maxInstanceCount===void 0&&(D._maxInstanceCount=K.meshPerAttribute*K.count)}else for(let rt=0;rt<W.locationSize;rt++)g(W.location+rt);n.bindBuffer(n.ARRAY_BUFFER,q);for(let rt=0;rt<W.locationSize;rt++)S(W.location+rt,dt/W.locationSize,at,Q,dt*gt,dt/W.locationSize*rt*gt,Et)}}else if(k!==void 0){let Q=k[j];if(Q!==void 0)switch(Q.length){case 2:n.vertexAttrib2fv(W.location,Q);break;case 3:n.vertexAttrib3fv(W.location,Q);break;case 4:n.vertexAttrib4fv(W.location,Q);break;default:n.vertexAttrib1fv(W.location,Q)}}}}_()}function T(){P();for(let I in i){let L=i[I];for(let F in L){let D=L[F];for(let B in D){let G=D[B];for(let k in G)h(G[k].object),delete G[k];delete D[B]}}delete i[I]}}function w(I){if(i[I.id]===void 0)return;let L=i[I.id];for(let F in L){let D=L[F];for(let B in D){let G=D[B];for(let k in G)h(G[k].object),delete G[k];delete D[B]}}delete i[I.id]}function x(I){for(let L in i){let F=i[L];for(let D in F){let B=F[D];if(B[I.id]===void 0)continue;let G=B[I.id];for(let k in G)h(G[k].object),delete G[k];delete B[I.id]}}}function y(I){for(let L in i){let F=i[L],D=I.isInstancedMesh===!0?I.id:0,B=F[D];if(B!==void 0){for(let G in B){let k=B[G];for(let j in k)h(k[j].object),delete k[j];delete B[G]}delete F[D],Object.keys(F).length===0&&delete i[L]}}}function P(){C(),a=!0,r!==s&&(r=s,c(r.object))}function C(){s.geometry=null,s.program=null,s.wireframe=!1}return{setup:o,reset:P,resetDefaultState:C,dispose:T,releaseStatesOfGeometry:w,releaseStatesOfObject:y,releaseStatesOfProgram:x,initAttributes:v,enableAttribute:g,disableUnusedAttributes:_}}function B0(n,t,e){let i;function s(l){i=l}function r(l,c){n.drawArrays(i,l,c),e.update(c,i,1)}function a(l,c,h){h!==0&&(n.drawArraysInstanced(i,l,c,h),e.update(c,i,h))}function o(l,c,h){if(h===0)return;t.get("WEBGL_multi_draw").multiDrawArraysWEBGL(i,l,0,c,0,h);let d=0;for(let f=0;f<h;f++)d+=c[f];e.update(d,i,1)}this.setMode=s,this.render=r,this.renderInstances=a,this.renderMultiDraw=o}function z0(n,t,e,i){let s;function r(){if(s!==void 0)return s;if(t.has("EXT_texture_filter_anisotropic")===!0){let x=t.get("EXT_texture_filter_anisotropic");s=n.getParameter(x.MAX_TEXTURE_MAX_ANISOTROPY_EXT)}else s=0;return s}function a(x){return!(x!==ci&&i.convert(x)!==n.getParameter(n.IMPLEMENTATION_COLOR_READ_FORMAT))}function o(x){let y=x===fe&&(t.has("EXT_color_buffer_half_float")||t.has("EXT_color_buffer_float"));return!(x!==ii&&x!==yi&&!y&&i.convert(x)!==n.getParameter(n.IMPLEMENTATION_COLOR_READ_TYPE))}function l(x){if(x==="highp"){if(n.getShaderPrecisionFormat(n.VERTEX_SHADER,n.HIGH_FLOAT).precision>0&&n.getShaderPrecisionFormat(n.FRAGMENT_SHADER,n.HIGH_FLOAT).precision>0)return"highp";x="mediump"}return x==="mediump"&&n.getShaderPrecisionFormat(n.VERTEX_SHADER,n.MEDIUM_FLOAT).precision>0&&n.getShaderPrecisionFormat(n.FRAGMENT_SHADER,n.MEDIUM_FLOAT).precision>0?"mediump":"lowp"}let c=e.precision!==void 0?e.precision:"highp",h=l(c);h!==c&&(Pt("WebGLRenderer:",c,"not supported, using",h,"instead."),c=h);let u=e.reversedDepthBuffer===!0&&t.has("EXT_clip_control"),d=e.logarithmicDepthBuffer===!0&&u===!1;e.reversedDepthBuffer===!0&&u===!1&&Pt("WebGLRenderer: Unable to use reversed depth buffer due to missing EXT_clip_control extension. Fallback to default depth buffer."),e.logarithmicDepthBuffer===!0&&u===!0&&Pt("WebGLRenderer: Logarithmic depth buffer is ignored when reversed depth buffer is enabled.");let f=n.getParameter(n.MAX_TEXTURE_IMAGE_UNITS),p=n.getParameter(n.MAX_VERTEX_TEXTURE_IMAGE_UNITS),v=n.getParameter(n.MAX_TEXTURE_SIZE),g=n.getParameter(n.MAX_CUBE_MAP_TEXTURE_SIZE),m=n.getParameter(n.MAX_VERTEX_ATTRIBS),_=n.getParameter(n.MAX_VERTEX_UNIFORM_VECTORS),S=n.getParameter(n.MAX_VARYING_VECTORS),M=n.getParameter(n.MAX_FRAGMENT_UNIFORM_VECTORS),T=n.getParameter(n.MAX_SAMPLES),w=n.getParameter(n.SAMPLES);return{isWebGL2:!0,getMaxAnisotropy:r,getMaxPrecision:l,textureFormatReadable:a,textureTypeReadable:o,precision:c,logarithmicDepthBuffer:d,reversedDepthBuffer:u,maxTextures:f,maxVertexTextures:p,maxTextureSize:v,maxCubemapSize:g,maxAttributes:m,maxVertexUniforms:_,maxVaryings:S,maxFragmentUniforms:M,maxSamples:T,samples:w}}function k0(n){let t=this,e=null,i=0,s=!1,r=!1,a=new Ke,o=new Nt,l={value:null,needsUpdate:!1};this.uniform=l,this.numPlanes=0,this.numIntersection=0,this.init=function(u,d){let f=u.length!==0||d||i!==0||s;return s=d,i=u.length,f},this.beginShadows=function(){r=!0,h(null)},this.endShadows=function(){r=!1},this.setGlobalState=function(u,d){e=h(u,d,0)},this.setState=function(u,d,f){let p=u.clippingPlanes,v=u.clipIntersection,g=u.clipShadows,m=n.get(u);if(!s||p===null||p.length===0||r&&!g)r?h(null):c();else{let _=r?0:i,S=_*4,M=m.clippingState||null;l.value=M,M=h(p,d,S,f);for(let T=0;T!==S;++T)M[T]=e[T];m.clippingState=M,this.numIntersection=v?this.numPlanes:0,this.numPlanes+=_}};function c(){l.value!==e&&(l.value=e,l.needsUpdate=i>0),t.numPlanes=i,t.numIntersection=0}function h(u,d,f,p){let v=u!==null?u.length:0,g=null;if(v!==0){if(g=l.value,p!==!0||g===null){let m=f+v*4,_=d.matrixWorldInverse;o.getNormalMatrix(_),(g===null||g.length<m)&&(g=new Float32Array(m));for(let S=0,M=f;S!==v;++S,M+=4)a.copy(u[S]).applyMatrix4(_,o),a.normal.toArray(g,M),g[M+3]=a.constant}l.value=g,l.needsUpdate=!0}return t.numPlanes=v,t.numIntersection=0,g}}var na=class extends ce{constructor(t=1,e={}){super(t,t,e),this.isWebGLCubeRenderTarget=!0;let i={width:t,height:t,depth:1},s=[i,i,i,i,i,i];this.texture=new wr(s),this._setTextureOptions(e),this.texture.isRenderTargetTexture=!0}fromEquirectangularTexture(t,e){this.texture.type=e.type,this.texture.colorSpace=e.colorSpace,this.texture.generateMipmaps=e.generateMipmaps,this.texture.minFilter=e.minFilter,this.texture.magFilter=e.magFilter;let i={uniforms:{tEquirect:{value:null}},vertexShader:`

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
			`},s=new ve(5,5,5),r=new oe({name:"CubemapFromEquirect",uniforms:jn(i.uniforms),vertexShader:i.vertexShader,fragmentShader:i.fragmentShader,side:ke,blending:Ni});r.uniforms.tEquirect.value=e;let a=new Ut(s,r),o=e.minFilter;return e.minFilter===sn&&(e.minFilter=ae),new Is(1,10,this).update(t,a),e.minFilter=o,a.geometry.dispose(),a.material.dispose(),this}clear(t,e=!0,i=!0,s=!0){let r=t.getRenderTarget();for(let a=0;a<6;a++)t.setRenderTarget(this,a),t.clear(e,i,s);t.setRenderTarget(r)}},wd=256,H0=3,V0=20,Yd=256,Zd=16,G0=3,W0=new A,X0=new Mt,Vs=class n{constructor(t){this._renderer=t,this._cubeSize=0,this._sourceTarget=null,this._cubeCamera=new Is(1,10,null),this._boxMesh=new Ut(new ve(5,5,5),null),this._cubemapMaterial=null,this._equirectMaterial=null,this._blurMaterial=null,this._ggxMaterial=null,this._integrationMaterial=null}fromScene(t,e=0,i=.1,s=100,r={}){let{size:a=256,position:o=W0}=r,l=this._renderer;this._setSize(a);let c=this._allocateTarget(),h=this._getSourceTarget(!0);e>0&&(h.texture.mipmapsAutoUpdate=!1);let u=l.autoClear,d=l.autoClearColor,f=l.autoClearDepth,p=l.autoClearStencil,v=t.background;l.autoClear=!0,l.autoClearColor=!0,l.autoClearDepth=!0,l.autoClearStencil=!0,v===null&&(t.background=l.getClearColor(X0));let g=new Is(i,s,h);return g.position.copy(o),g.update(l,t),l.autoClear=u,l.autoClearColor=d,l.autoClearDepth=f,l.autoClearStencil=p,t.background=v,e>0&&(h.texture.mipmapsAutoUpdate=!0,this._blur(c,e)),this._applyPMREM(c),c}fromEquirectangular(t,e=null){return this._fromTexture(t,e)}fromCubemap(t,e=null){return this._fromTexture(t,e)}compileCubemapShader(){this._cubemapMaterial===null&&(this._cubemapMaterial=Ad(),this._compileMaterial(this._cubemapMaterial))}compileEquirectangularShader(){this._equirectMaterial===null&&(this._equirectMaterial=Td(),this._compileMaterial(this._equirectMaterial))}dispose(){this._sourceTarget!==null&&this._sourceTarget.dispose(),this._cubemapMaterial!==null&&this._cubemapMaterial.dispose(),this._equirectMaterial!==null&&this._equirectMaterial.dispose(),this._blurMaterial!==null&&this._blurMaterial.dispose(),this._ggxMaterial!==null&&this._ggxMaterial.dispose(),this._integrationMaterial!==null&&this._integrationMaterial.dispose(),this._boxMesh.geometry.dispose()}static lodToRoughness(t,e){return e>0?1-Math.sqrt(1-t/e):0}_setSize(t){this._cubeSize=Math.max(wd,Zc(t))}_fromTexture(t,e){t.mapping===Jn||t.mapping===Us?this._setSize(t.image.length===0?wd:t.image[0].width||t.image[0].image.width):this._setSize(t.image.width/4);let i=e||this._allocateTarget();return this._textureToCubemap(t),this._applyPMREM(i),i}_allocateTarget(){let t=this._cubeSize,e=Ed(t,!1,!1),i=Math.log2(t)-H0;for(let s=0;s<=i;s++)e.texture.mipmaps.push({width:t>>s,height:t>>s});return e.texture.name="PMREM",e.texture.isPMREMTexture=!0,e}_getSourceTarget(t=!1){let e=this._cubeSize,i=this._sourceTarget;return(i===null||i.width!==e||t&&i.depthBuffer===!1)&&(i!==null&&i.dispose(),this._sourceTarget=Ed(e,!0,t)),this._sourceTarget}_compileMaterial(t){this._boxMesh.material=t,this._renderer.compile(this._boxMesh,this._cubeCamera.children[0])}_renderCube(t,e,i){let s=this._boxMesh,r=this._cubeCamera;s.material=i,r.renderTarget=t,r.activeMipmapLevel=e;let a=t.width>>e;t.viewport.set(0,0,a,a),r.update(this._renderer,s),t.viewport.set(0,0,t.width,t.height)}_textureToCubemap(t){let e;t.mapping===Jn||t.mapping===Us?(this._cubemapMaterial===null&&(this._cubemapMaterial=Ad()),e=this._cubemapMaterial,e.uniforms.flipEnvMap.value=t.isRenderTargetTexture===!1?-1:1):(this._equirectMaterial===null&&(this._equirectMaterial=Td()),e=this._equirectMaterial),e.uniforms.envMap.value=t,this._renderCube(this._getSourceTarget(),0,e)}_applyPMREM(t){this._ggxMaterial===null&&(this._ggxMaterial=q0(),this._integrationMaterial=Y0());let e=this._cubeSize,i=t.texture.mipmaps.length-1,s=this._ggxMaterial.uniforms;s.envMap.value=this._sourceTarget.texture;let r=this._integrationMaterial.uniforms;r.envMap.value=this._sourceTarget.texture,r.sourceLod.value=Math.log2(e/Zd);for(let a=0;a<=i;a++){let o=n.lodToRoughness(a,i);if(a>i-G0)r.roughness.value=o,this._renderCube(t,a,this._integrationMaterial);else{let l=o>0?Math.log2(e)+.5*Math.log2(6/(Yd*Math.pow(o,4)))+.5:0;s.roughness.value=o,s.lodBias.value=l,this._renderCube(t,a,this._ggxMaterial)}}}_blur(t,e){this._blurMaterial===null&&(this._blurMaterial=Z0());let i=this._blurMaterial,s=i.uniforms,r=this._sourceTarget;s.sigma.value=Math.min(e,Math.PI)/Math.SQRT2,s.envMap.value=r.texture,this._renderCube(t,0,i),s.envMap.value=t.texture,this._renderCube(r,0,i)}};function Ed(n,t,e){return new na(n,{minFilter:sn,generateMipmaps:t,type:fe,colorSpace:pn,depthBuffer:e})}function sa(n,t,e){return new oe({name:n,uniforms:t,vertexShader:zt.cube_vert,fragmentShader:e,side:ke,blending:Ni,depthTest:!1,depthWrite:!1})}function q0(){return sa("PMREMGGXConvolution",{envMap:{value:null},roughness:{value:0},lodBias:{value:0}},`

		#define GGX_SAMPLES ${Yd}u

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
	`)}function Y0(){return sa("PMREMGGXIntegration",{envMap:{value:null},roughness:{value:0},sourceLod:{value:0},sourceSize:{value:Zd}},`

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
	`)}function Z0(){return sa("PMREMSphericalGaussianBlur",{envMap:{value:null},sigma:{value:0}},`

		#define SAMPLES ${V0}
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
	`)}function Td(){return sa("PMREMEquirectangularToCubemap",{envMap:{value:null}},`

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
	`)}function Ad(){return sa("PMREMCubemapToCubemap",{envMap:{value:null},flipEnvMap:{value:-1}},`

		varying vec3 vWorldDirection;

		uniform samplerCube envMap;
		uniform float flipEnvMap;

		void main() {

			gl_FragColor = vec4( textureCube( envMap, vec3( flipEnvMap * vWorldDirection.x, vWorldDirection.yz ) ).rgb, 1.0 );

		}
	`)}function K0(n){let t=new WeakMap,e=new WeakMap,i=null;function s(d,f=!1){return d==null?null:f?a(d):r(d)}function r(d){if(d&&d.isTexture){let f=d.mapping;if(f===Co||f===Po)if(t.has(d)){let p=t.get(d).texture;return o(p,d.mapping)}else{let p=d.image;if(p&&p.height>0){let v=new na(p.height);return v.fromEquirectangularTexture(n,d),t.set(d,v),d.addEventListener("dispose",c),o(v.texture,d.mapping)}else return null}}return d}function a(d){if(d&&d.isTexture&&d.isPMREMTexture!==!0){let f=d.mapping,p=f===Co||f===Po,v=f===Jn||f===Us;if(p||v){let g=e.get(d),m=g!==void 0?g.texture.pmremVersion:0;if(d.isRenderTargetTexture&&d.pmremVersion!==m)return i===null&&(i=new Vs(n)),g=p?i.fromEquirectangular(d,g):i.fromCubemap(d,g),g.texture.pmremVersion=d.pmremVersion,e.set(d,g),g.texture;if(g!==void 0)return g.texture;{let _=d.image;return p&&_&&_.height>0||v&&_&&l(_)?(i===null&&(i=new Vs(n)),g=p?i.fromEquirectangular(d):i.fromCubemap(d),g.texture.pmremVersion=d.pmremVersion,e.set(d,g),d.addEventListener("dispose",h),g.texture):null}}}return d}function o(d,f){return f===Co?d.mapping=Jn:f===Po&&(d.mapping=Us),d}function l(d){let f=0,p=6;for(let v=0;v<p;v++)d[v]!==void 0&&f++;return f===p}function c(d){let f=d.target;f.removeEventListener("dispose",c);let p=t.get(f);p!==void 0&&(t.delete(f),p.dispose())}function h(d){let f=d.target;f.removeEventListener("dispose",h);let p=e.get(f);p!==void 0&&(e.delete(f),p.dispose())}function u(){t=new WeakMap,e=new WeakMap,i!==null&&(i.dispose(),i=null)}return{get:s,dispose:u}}function J0(n){let t={};function e(i){if(t[i]!==void 0)return t[i];let s=n.getExtension(i);return t[i]=s,s}return{has:function(i){return e(i)!==null},init:function(){e("EXT_color_buffer_float"),e("WEBGL_clip_cull_distance"),e("OES_texture_float_linear"),e("EXT_color_buffer_half_float"),e("WEBGL_multisampled_render_to_texture"),e("WEBGL_render_shared_exponent")},get:function(i){let s=e(i);return s===null&&Un("WebGLRenderer: "+i+" extension not supported."),s}}}function $0(n,t,e,i){let s={},r=new WeakMap,a=new FinalizationRegistry(p=>delete s[p]);function o(p){l(p.target)}function l(p){p.index!==null&&t.remove(p.index);for(let g in p.attributes)t.remove(p.attributes[g]);p.removeEventListener("dispose",o),delete s[p.id],a.unregister(p);let v=r.get(p);v&&(t.remove(v),r.delete(p)),i.releaseStatesOfGeometry(p),p.isInstancedBufferGeometry===!0&&delete p._maxInstanceCount,e.memory.geometries--}function c(p,v){return s[v.id]!==void 0||(v.addEventListener("dispose",o),s[v.id]=new WeakRef(v),a.register(v,v.id,v),e.memory.geometries++),v}function h(p){let v=p.attributes;for(let g in v)t.update(v[g],n.ARRAY_BUFFER)}function u(p){let v=p.index,g=p.attributes.position,m=0;if(g===void 0)return;let _=v!==null?v.array.length:g.array.length/3-1,S=g.count>=65535?yr:vr,M=new S(Math.ceil(_/3)*6,1),T=M.array;if(v!==null){let x=v.array;m=v.version;for(let y=0,P=0;y<_;y+=3){let C=x[y+0],I=x[y+1],L=x[y+2];T[P++]=C,T[P++]=I,T[P++]=I,T[P++]=L,T[P++]=L,T[P++]=C}}else{m=g.version;for(let x=0,y=0;x<_;x+=3){let P=x+0,C=x+1,I=x+2;T[y++]=P,T[y++]=C,T[y++]=C,T[y++]=I,T[y++]=I,T[y++]=P}}M.version=m;let w=r.get(p);w&&t.remove(w),r.set(p,M)}function d(p){let v=r.get(p);if(v){let g=p.index;g!==null&&v.version<g.version&&u(p)}else u(p);return r.get(p)}function f(){for(let p in s){let v=s[p].deref();v!==void 0&&l(v)}}return{get:c,update:h,getWireframeAttribute:d,dispose:f}}function j0(n,t,e){let i;function s(u){i=u}let r,a;function o(u){r=u.type,a=u.bytesPerElement}function l(u,d){n.drawElements(i,d,r,u*a),e.update(d,i,1)}function c(u,d,f){f!==0&&(n.drawElementsInstanced(i,d,r,u*a,f),e.update(d,i,f))}function h(u,d,f){if(f===0)return;t.get("WEBGL_multi_draw").multiDrawElementsWEBGL(i,d,0,r,u,0,f);let v=0;for(let g=0;g<f;g++)v+=d[g];e.update(v,i,1)}this.setMode=s,this.setIndex=o,this.render=l,this.renderInstances=c,this.renderMultiDraw=h}function Q0(n){let t={geometries:0,textures:0},e={frame:0,calls:0,triangles:0,points:0,lines:0};function i(r,a,o){switch(e.calls++,a){case n.TRIANGLES:e.triangles+=o*(r/3);break;case n.LINES:e.lines+=o*(r/2);break;case n.LINE_STRIP:e.lines+=o*(r-1);break;case n.LINE_LOOP:e.lines+=o*r;break;case n.POINTS:e.points+=o*r;break;default:Lt("WebGLInfo: Unknown draw mode:",a);break}}function s(){e.calls=0,e.triangles=0,e.points=0,e.lines=0}return{memory:t,render:e,programs:null,autoReset:!0,reset:s,update:i}}function tx(n,t,e){let i=new WeakMap,s=new ie;function r(a,o,l){let c=a.morphTargetInfluences,h=o.morphAttributes.position||o.morphAttributes.normal||o.morphAttributes.color,u=h!==void 0?h.length:0,d=i.get(o);if(d===void 0||d.count!==u){let P=function(){x.dispose(),i.delete(o),o.removeEventListener("dispose",P)};d!==void 0&&d.texture.dispose();let f=o.morphAttributes.position!==void 0,p=o.morphAttributes.normal!==void 0,v=o.morphAttributes.color!==void 0,g=o.morphAttributes.position||[],m=o.morphAttributes.normal||[],_=o.morphAttributes.color||[],S=0;f===!0&&(S=1),p===!0&&(S=2),v===!0&&(S=3);let M=o.attributes.position.count*S,T=1;M>t.maxTextureSize&&(T=Math.ceil(M/t.maxTextureSize),M=t.maxTextureSize);let w=new Float32Array(M*T*4*u),x=new xr(w,M,T,u);x.type=yi,x.needsUpdate=!0;let y=S*4;for(let C=0;C<u;C++){let I=g[C],L=m[C],F=_[C],D=M*T*4*C;for(let B=0;B<I.count;B++){let G=B*y;f===!0&&(s.fromBufferAttribute(I,B),w[D+G+0]=s.x,w[D+G+1]=s.y,w[D+G+2]=s.z,w[D+G+3]=0),p===!0&&(s.fromBufferAttribute(L,B),w[D+G+4]=s.x,w[D+G+5]=s.y,w[D+G+6]=s.z,w[D+G+7]=0),v===!0&&(s.fromBufferAttribute(F,B),w[D+G+8]=s.x,w[D+G+9]=s.y,w[D+G+10]=s.z,w[D+G+11]=F.itemSize===4?s.w:1)}}d={count:u,texture:x,size:new et(M,T)},i.set(o,d),o.addEventListener("dispose",P)}if(a.isInstancedMesh===!0&&a.morphTexture!==null)l.getUniforms().setValue(n,"morphTexture",a.morphTexture,e);else{let f=0;for(let v=0;v<c.length;v++)f+=c[v];let p=o.morphTargetsRelative?1:1-f;l.getUniforms().setValue(n,"morphTargetBaseInfluence",p),l.getUniforms().setValue(n,"morphTargetInfluences",c)}l.getUniforms().setValue(n,"morphTargetsTexture",d.texture,e),l.getUniforms().setValue(n,"morphTargetsTextureSize",d.size)}return{update:r}}function ex(n,t,e,i,s){let r=new WeakMap;function a(c){let h=s.render.frame,u=c.geometry,d=t.get(c,u);if(r.get(d)!==h&&(t.update(d),r.set(d,h)),c.isInstancedMesh&&(c.hasEventListener("dispose",l)===!1&&c.addEventListener("dispose",l),r.get(c)!==h&&(e.update(c.instanceMatrix,n.ARRAY_BUFFER),c.instanceColor!==null&&e.update(c.instanceColor,n.ARRAY_BUFFER),r.set(c,h))),c.isSkinnedMesh){let f=c.skeleton;r.get(f)!==h&&(f.update(),r.set(f,h))}return d}function o(){r=new WeakMap}function l(c){let h=c.target;h.removeEventListener("dispose",l),i.releaseStatesOfObject(h),e.remove(h.instanceMatrix),h.instanceColor!==null&&e.remove(h.instanceColor)}return{update:a,dispose:o}}var ix={[kr]:"LINEAR_TONE_MAPPING",[Hr]:"REINHARD_TONE_MAPPING",[Vr]:"CINEON_TONE_MAPPING",[Kn]:"ACES_FILMIC_TONE_MAPPING",[Wr]:"AGX_TONE_MAPPING",[Xr]:"NEUTRAL_TONE_MAPPING",[Gr]:"CUSTOM_TONE_MAPPING"};function nx(n,t,e,i,s,r){let a=new ce(t,e,{type:n,depthBuffer:s,stencilBuffer:r,samples:i?4:0,storeMultisampledDepthBuffer:!1,storeMultisampledStencilBuffer:!1,resolveDepthBuffer:!1,resolveStencilBuffer:!1}),o=null,l=null,c=null,h=new ee;h.setAttribute("position",new Kt([-1,3,0,-1,-1,0,3,-1,0],3)),h.setAttribute("uv",new Kt([0,2,0,0,2,0],2));let u=new Rs({uniforms:{tDiffuse:{value:null}},vertexShader:`
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
			}`,depthTest:!1,depthWrite:!1}),d=new Ut(h,u),f=new qn(-1,1,1,-1,0,1),p=null,v=null,g=!1,m,_=null,S=[],M=!1;this.setSize=function(T,w){a.setSize(T,w),o!==null&&o.setSize(T,w),l!==null&&l.setSize(T,w);for(let x=0;x<S.length;x++){let y=S[x];y.setSize&&y.setSize(T,w)}},this.setEffects=function(T){S=T,M=S.length>0&&S[0].isRenderPass===!0;let w=a.width,x=a.height;S.length>0&&o===null&&(o=new ce(w,x,{type:fe,depthBuffer:!1,stencilBuffer:!1}),l=new ce(w,x,{type:fe,depthBuffer:!1,stencilBuffer:!1}));let y=s===!0&&S.some(P=>P.needsDepthTexture===!0);y!==(a.depthTexture!==null)&&(y===!0&&c===null&&(c=new Ii(w,x),r===!0&&(c.format=rn,c.type=$n)),a.dispose(),a.depthTexture=y?c:null,a.resolveDepthBuffer=y,a.storeMultisampledDepthBuffer=y);for(let P=0;P<S.length;P++){let C=S[P];C.setSize&&C.setSize(w,x),y&&C.needsDepthTexture===!0&&C.setDepthTexture&&C.setDepthTexture(c)}},this.begin=function(T,w){if(g||T.toneMapping===Hi&&S.length===0)return!1;if(_=w,w!==null){let x=w.width,y=w.height;(a.width!==x||a.height!==y)&&this.setSize(x,y)}return M===!1&&T.setRenderTarget(a),m=T.toneMapping,T.toneMapping=Hi,!0},this.hasRenderPass=function(){return M},this.end=function(T,w){T.toneMapping=m,g=!0;let x=a,y=o;for(let P=0;P<S.length;P++){let C=S[P];C.enabled!==!1&&(C.render(T,y,x,w),C.needsSwap!==!1&&(x=y,y=y===o?l:o))}if(p!==T.outputColorSpace||v!==T.toneMapping){p=T.outputColorSpace,v=T.toneMapping,u.defines={},Wt.getTransfer(p)===Jt&&(u.defines.SRGB_TRANSFER="");let P=ix[v];P&&(u.defines[P]=""),u.needsUpdate=!0}u.uniforms.tDiffuse.value=x.texture,T.setRenderTarget(_),T.render(d,f),_=null,g=!1},this.isCompositing=function(){return g},this.dispose=function(){a.dispose(),o!==null&&o.dispose(),l!==null&&l.dispose(),h.dispose(),u.dispose()}}var Kd=new qe,Jd=new Ii(1,1),$d=new Er(1),jd=new xr,Qd=new ys,tf=new wr,Rd=[],Cd=[],Pd=new Float32Array(16),Id=new Float32Array(9),Dd=new Float32Array(4);function Gs(n,t,e){let i=n[0];if(i<=0||i>0)return n;let s=t*e,r=Rd[s];if(r===void 0&&(r=new Float32Array(s),Rd[s]=r),t!==0){i.toArray(r,0);for(let a=1,o=0;a!==t;++a)o+=e,n[a].toArray(r,o)}return r}function Ae(n,t){if(n.length!==t.length)return!1;for(let e=0,i=n.length;e<i;e++)if(n[e]!==t[e])return!1;return!0}function Re(n,t){for(let e=0,i=t.length;e<i;e++)n[e]=t[e]}function xl(n,t){let e=Cd[t];e===void 0&&(e=new Int32Array(t),Cd[t]=e);for(let i=0;i!==t;++i)e[i]=n.allocateTextureUnit();return e}function sx(n,t){let e=this.cache;e[0]!==t&&(n.uniform1f(this.addr,t),e[0]=t)}function rx(n,t){let e=this.cache;if(t.x!==void 0)(e[0]!==t.x||e[1]!==t.y)&&(n.uniform2f(this.addr,t.x,t.y),e[0]=t.x,e[1]=t.y);else{if(Ae(e,t))return;n.uniform2fv(this.addr,t),Re(e,t)}}function ax(n,t){let e=this.cache;if(t.x!==void 0)(e[0]!==t.x||e[1]!==t.y||e[2]!==t.z)&&(n.uniform3f(this.addr,t.x,t.y,t.z),e[0]=t.x,e[1]=t.y,e[2]=t.z);else if(t.r!==void 0)(e[0]!==t.r||e[1]!==t.g||e[2]!==t.b)&&(n.uniform3f(this.addr,t.r,t.g,t.b),e[0]=t.r,e[1]=t.g,e[2]=t.b);else{if(Ae(e,t))return;n.uniform3fv(this.addr,t),Re(e,t)}}function ox(n,t){let e=this.cache;if(t.x!==void 0)(e[0]!==t.x||e[1]!==t.y||e[2]!==t.z||e[3]!==t.w)&&(n.uniform4f(this.addr,t.x,t.y,t.z,t.w),e[0]=t.x,e[1]=t.y,e[2]=t.z,e[3]=t.w);else{if(Ae(e,t))return;n.uniform4fv(this.addr,t),Re(e,t)}}function lx(n,t){let e=this.cache,i=t.elements;if(i===void 0){if(Ae(e,t))return;n.uniformMatrix2fv(this.addr,!1,t),Re(e,t)}else{if(Ae(e,i))return;Dd.set(i),n.uniformMatrix2fv(this.addr,!1,Dd),Re(e,i)}}function cx(n,t){let e=this.cache,i=t.elements;if(i===void 0){if(Ae(e,t))return;n.uniformMatrix3fv(this.addr,!1,t),Re(e,t)}else{if(Ae(e,i))return;Id.set(i),n.uniformMatrix3fv(this.addr,!1,Id),Re(e,i)}}function hx(n,t){let e=this.cache,i=t.elements;if(i===void 0){if(Ae(e,t))return;n.uniformMatrix4fv(this.addr,!1,t),Re(e,t)}else{if(Ae(e,i))return;Pd.set(i),n.uniformMatrix4fv(this.addr,!1,Pd),Re(e,i)}}function ux(n,t){let e=this.cache;e[0]!==t&&(n.uniform1i(this.addr,t),e[0]=t)}function dx(n,t){let e=this.cache;if(t.x!==void 0)(e[0]!==t.x||e[1]!==t.y)&&(n.uniform2i(this.addr,t.x,t.y),e[0]=t.x,e[1]=t.y);else{if(Ae(e,t))return;n.uniform2iv(this.addr,t),Re(e,t)}}function fx(n,t){let e=this.cache;if(t.x!==void 0)(e[0]!==t.x||e[1]!==t.y||e[2]!==t.z)&&(n.uniform3i(this.addr,t.x,t.y,t.z),e[0]=t.x,e[1]=t.y,e[2]=t.z);else{if(Ae(e,t))return;n.uniform3iv(this.addr,t),Re(e,t)}}function px(n,t){let e=this.cache;if(t.x!==void 0)(e[0]!==t.x||e[1]!==t.y||e[2]!==t.z||e[3]!==t.w)&&(n.uniform4i(this.addr,t.x,t.y,t.z,t.w),e[0]=t.x,e[1]=t.y,e[2]=t.z,e[3]=t.w);else{if(Ae(e,t))return;n.uniform4iv(this.addr,t),Re(e,t)}}function mx(n,t){let e=this.cache;e[0]!==t&&(n.uniform1ui(this.addr,t),e[0]=t)}function gx(n,t){let e=this.cache;if(t.x!==void 0)(e[0]!==t.x||e[1]!==t.y)&&(n.uniform2ui(this.addr,t.x,t.y),e[0]=t.x,e[1]=t.y);else{if(Ae(e,t))return;n.uniform2uiv(this.addr,t),Re(e,t)}}function xx(n,t){let e=this.cache;if(t.x!==void 0)(e[0]!==t.x||e[1]!==t.y||e[2]!==t.z)&&(n.uniform3ui(this.addr,t.x,t.y,t.z),e[0]=t.x,e[1]=t.y,e[2]=t.z);else{if(Ae(e,t))return;n.uniform3uiv(this.addr,t),Re(e,t)}}function _x(n,t){let e=this.cache;if(t.x!==void 0)(e[0]!==t.x||e[1]!==t.y||e[2]!==t.z||e[3]!==t.w)&&(n.uniform4ui(this.addr,t.x,t.y,t.z,t.w),e[0]=t.x,e[1]=t.y,e[2]=t.z,e[3]=t.w);else{if(Ae(e,t))return;n.uniform4uiv(this.addr,t),Re(e,t)}}function _l(n){return n.version===0&&(n.compareFunction=Xc,n.needsUpdate=!0),n}function vx(n,t,e){let i=this.cache,s=e.allocateTextureUnit();i[0]!==s&&(n.uniform1i(this.addr,s),i[0]=s);let r;this.type===n.SAMPLER_2D_SHADOW?r=_l(Jd):r=Kd,e.setTexture2D(t||r,s)}function yx(n,t,e){let i=this.cache,s=e.allocateTextureUnit();i[0]!==s&&(n.uniform1i(this.addr,s),i[0]=s),e.setTexture3D(t||Qd,s)}function Mx(n,t,e){let i=this.cache,s=e.allocateTextureUnit();i[0]!==s&&(n.uniform1i(this.addr,s),i[0]=s);let r;this.type===n.SAMPLER_CUBE_SHADOW?r=_l($d):r=tf,e.setTextureCube(t||r,s)}function Sx(n,t,e){let i=this.cache,s=e.allocateTextureUnit();i[0]!==s&&(n.uniform1i(this.addr,s),i[0]=s),e.setTexture2DArray(t||jd,s)}function bx(n){switch(n){case 5126:return sx;case 35664:return rx;case 35665:return ax;case 35666:return ox;case 35674:return lx;case 35675:return cx;case 35676:return hx;case 5124:case 35670:return ux;case 35667:case 35671:return dx;case 35668:case 35672:return fx;case 35669:case 35673:return px;case 5125:return mx;case 36294:return gx;case 36295:return xx;case 36296:return _x;case 35678:case 36198:case 36298:case 36306:case 35682:return vx;case 35679:case 36299:case 36307:return yx;case 35680:case 36300:case 36308:case 36293:return Mx;case 36289:case 36303:case 36311:case 36292:return Sx}}function wx(n,t){n.uniform1fv(this.addr,t)}function Ex(n,t){let e=Gs(t,this.size,2);n.uniform2fv(this.addr,e)}function Tx(n,t){let e=Gs(t,this.size,3);n.uniform3fv(this.addr,e)}function Ax(n,t){let e=Gs(t,this.size,4);n.uniform4fv(this.addr,e)}function Rx(n,t){let e=Gs(t,this.size,4);n.uniformMatrix2fv(this.addr,!1,e)}function Cx(n,t){let e=Gs(t,this.size,9);n.uniformMatrix3fv(this.addr,!1,e)}function Px(n,t){let e=Gs(t,this.size,16);n.uniformMatrix4fv(this.addr,!1,e)}function Ix(n,t){n.uniform1iv(this.addr,t)}function Dx(n,t){n.uniform2iv(this.addr,t)}function Lx(n,t){n.uniform3iv(this.addr,t)}function Nx(n,t){n.uniform4iv(this.addr,t)}function Ux(n,t){n.uniform1uiv(this.addr,t)}function Fx(n,t){n.uniform2uiv(this.addr,t)}function Ox(n,t){n.uniform3uiv(this.addr,t)}function Bx(n,t){n.uniform4uiv(this.addr,t)}function zx(n,t,e){let i=this.cache,s=t.length,r=xl(e,s);Ae(i,r)||(n.uniform1iv(this.addr,r),Re(i,r));let a;this.type===n.SAMPLER_2D_SHADOW?a=_l(Jd):a=Kd;for(let o=0;o!==s;++o)e.setTexture2D(t[o]||a,r[o])}function kx(n,t,e){let i=this.cache,s=t.length,r=xl(e,s);Ae(i,r)||(n.uniform1iv(this.addr,r),Re(i,r));for(let a=0;a!==s;++a)e.setTexture3D(t[a]||Qd,r[a])}function Hx(n,t,e){let i=this.cache,s=t.length,r=xl(e,s);Ae(i,r)||(n.uniform1iv(this.addr,r),Re(i,r));let a;this.type===n.SAMPLER_CUBE_SHADOW?a=_l($d):a=tf;for(let o=0;o!==s;++o)e.setTextureCube(t[o]||a,r[o])}function Vx(n,t,e){let i=this.cache,s=t.length,r=xl(e,s);Ae(i,r)||(n.uniform1iv(this.addr,r),Re(i,r));for(let a=0;a!==s;++a)e.setTexture2DArray(t[a]||jd,r[a])}function Gx(n){switch(n){case 5126:return wx;case 35664:return Ex;case 35665:return Tx;case 35666:return Ax;case 35674:return Rx;case 35675:return Cx;case 35676:return Px;case 5124:case 35670:return Ix;case 35667:case 35671:return Dx;case 35668:case 35672:return Lx;case 35669:case 35673:return Nx;case 5125:return Ux;case 36294:return Fx;case 36295:return Ox;case 36296:return Bx;case 35678:case 36198:case 36298:case 36306:case 35682:return zx;case 35679:case 36299:case 36307:return kx;case 35680:case 36300:case 36308:case 36293:return Hx;case 36289:case 36303:case 36311:case 36292:return Vx}}var nh=class{constructor(t,e,i){this.id=t,this.addr=i,this.cache=[],this.type=e.type,this.setValue=bx(e.type)}},sh=class{constructor(t,e,i){this.id=t,this.addr=i,this.cache=[],this.type=e.type,this.size=e.size,this.setValue=Gx(e.type)}},rh=class{constructor(t){this.id=t,this.seq=[],this.map={}}setValue(t,e,i){let s=this.seq;for(let r=0,a=s.length;r!==a;++r){let o=s[r];o.setValue(t,e[o.id],i)}}},eh=/(\w+)(\])?(\[|\.)?/g;function Ld(n,t){n.seq.push(t),n.map[t.id]=t}function Wx(n,t,e){let i=n.name,s=i.length;for(eh.lastIndex=0;;){let r=eh.exec(i),a=eh.lastIndex,o=r[1],l=r[2]==="]",c=r[3];if(l&&(o=o|0),c===void 0||c==="["&&a+2===s){Ld(e,c===void 0?new nh(o,n,t):new sh(o,n,t));break}else{let u=e.map[o];u===void 0&&(u=new rh(o),Ld(e,u)),e=u}}}var Hs=class{constructor(t,e){this.seq=[],this.map={};let i=t.getProgramParameter(e,t.ACTIVE_UNIFORMS);for(let a=0;a<i;++a){let o=t.getActiveUniform(e,a),l=t.getUniformLocation(e,o.name);Wx(o,l,this)}let s=[],r=[];for(let a of this.seq)a.type===t.SAMPLER_2D_SHADOW||a.type===t.SAMPLER_CUBE_SHADOW||a.type===t.SAMPLER_2D_ARRAY_SHADOW?s.push(a):r.push(a);s.length>0&&(this.seq=s.concat(r))}setValue(t,e,i,s){let r=this.map[e];r!==void 0&&r.setValue(t,i,s)}setOptional(t,e,i){let s=e[i];s!==void 0&&this.setValue(t,i,s)}static upload(t,e,i,s){for(let r=0,a=e.length;r!==a;++r){let o=e[r],l=i[o.id];l.needsUpdate!==!1&&o.setValue(t,l.value,s)}}static seqWithValue(t,e){let i=[];for(let s=0,r=t.length;s!==r;++s){let a=t[s];a.id in e&&i.push(a)}return i}};function Nd(n,t,e){let i=n.createShader(t);return n.shaderSource(i,e),n.compileShader(i),i}var Xx=37297,qx=0;function Yx(n,t){let e=n.split(`
`),i=[],s=Math.max(t-6,0),r=Math.min(t+6,e.length);for(let a=s;a<r;a++){let o=a+1;i.push(`${o===t?">":" "} ${o}: ${e[a]}`)}return i.join(`
`)}var Ud=new Nt;function Zx(n){Wt._getMatrix(Ud,Wt.workingColorSpace,n);let t=`mat3( ${Ud.elements.map(e=>e.toFixed(4))} )`;switch(Wt.getTransfer(n)){case mr:return[t,"LinearTransferOETF"];case Jt:return[t,"sRGBTransferOETF"];default:return Pt("WebGLProgram: Unsupported color space: ",n),[t,"LinearTransferOETF"]}}function Fd(n,t,e){let i=n.getShaderParameter(t,n.COMPILE_STATUS),r=(n.getShaderInfoLog(t)||"").trim();if(i&&r==="")return"";let a=/ERROR: 0:(\d+)/.exec(r);if(a){let o=parseInt(a[1]);return e.toUpperCase()+`

`+r+`

`+Yx(n.getShaderSource(t),o)}else return r}function Kx(n,t){let e=Zx(t);return[`vec4 ${n}( vec4 value ) {`,`	return ${e[1]}( vec4( ${e[0]} * value.rgb, value.a ) );`,"}"].join(`
`)}var Jx={[kr]:"Linear",[Hr]:"Reinhard",[Vr]:"Cineon",[Kn]:"ACESFilmic",[Wr]:"AgX",[Xr]:"Neutral",[Gr]:"Custom"};function $x(n,t){let e=Jx[t];return e===void 0?(Pt("WebGLProgram: Unsupported toneMapping:",t),"vec3 "+n+"( vec3 color ) { return LinearToneMapping( color ); }"):"vec3 "+n+"( vec3 color ) { return "+e+"ToneMapping( color ); }"}var ml=new A;function jx(){Wt.getLuminanceCoefficients(ml);let n=ml.x.toFixed(4),t=ml.y.toFixed(4),e=ml.z.toFixed(4);return["float luminance( const in vec3 rgb ) {",`	const vec3 weights = vec3( ${n}, ${t}, ${e} );`,"	return dot( weights, rgb );","}"].join(`
`)}function Qx(n){return[n.extensionClipCullDistance?"#extension GL_ANGLE_clip_cull_distance : require":"",n.extensionMultiDraw?"#extension GL_ANGLE_multi_draw : require":""].filter(ia).join(`
`)}function t_(n){let t=[];for(let e in n){let i=n[e];i!==!1&&t.push("#define "+e+" "+i)}return t.join(`
`)}function e_(n,t){let e={},i=n.getProgramParameter(t,n.ACTIVE_ATTRIBUTES);for(let s=0;s<i;s++){let r=n.getActiveAttrib(t,s),a=r.name,o=1;r.type===n.FLOAT_MAT2&&(o=2),r.type===n.FLOAT_MAT3&&(o=3),r.type===n.FLOAT_MAT4&&(o=4),e[a]={type:r.type,location:n.getAttribLocation(t,a),locationSize:o}}return e}function ia(n){return n!==""}function Od(n,t){let e=t.numSpotLightShadows+t.numSpotLightMaps-t.numSpotLightShadowsWithMaps;return n.replace(/NUM_SUN_LIGHTS/g,t.numSunLights).replace(/NUM_DIR_LIGHTS/g,t.numDirLights).replace(/NUM_SPOT_LIGHTS/g,t.numSpotLights).replace(/NUM_SPOT_LIGHT_MAPS/g,t.numSpotLightMaps).replace(/NUM_SPOT_LIGHT_COORDS/g,e).replace(/NUM_RECT_AREA_LIGHTS/g,t.numRectAreaLights).replace(/NUM_POINT_LIGHTS/g,t.numPointLights).replace(/NUM_HEMI_LIGHTS/g,t.numHemiLights).replace(/NUM_SUN_LIGHT_SHADOWS/g,t.numSunLightShadows).replace(/NUM_DIR_LIGHT_SHADOWS/g,t.numDirLightShadows).replace(/NUM_SPOT_LIGHT_SHADOWS_WITH_MAPS/g,t.numSpotLightShadowsWithMaps).replace(/NUM_SPOT_LIGHT_SHADOWS/g,t.numSpotLightShadows).replace(/NUM_POINT_LIGHT_SHADOWS/g,t.numPointLightShadows).replace(/NUM_LIGHT_PROBE_GRIDS/g,t.numLightProbeGrids)}function Bd(n,t){return n.replace(/NUM_CLIPPING_PLANES/g,t.numClippingPlanes).replace(/UNION_CLIPPING_PLANES/g,t.numClippingPlanes-t.numClipIntersection)}var i_=/^[ \t]*#include +<([\w\d./]+)>/gm;function ah(n){return n.replace(i_,n_)}function n_(n,t){let e=zt[t];if(e===void 0)throw new Error("THREE.WebGLProgram: Can not resolve #include <"+t+">");return ah(e)}var s_=/#pragma unroll_loop_start\s+for\s*\(\s*int\s+i\s*=\s*(\d+)\s*;\s*i\s*<\s*(\d+)\s*;\s*i\s*\+\+\s*\)\s*{([\s\S]+?)}\s+#pragma unroll_loop_end/g;function zd(n){return n.replace(s_,r_)}function r_(n,t,e,i){let s="";for(let r=parseInt(t);r<parseInt(e);r++)s+=i.replace(/\[\s*i\s*\]/g,"[ "+r+" ]").replace(/UNROLLED_LOOP_INDEX/g,r);return s}function kd(n){let t=`precision ${n.precision} float;
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
#define LOW_PRECISION`),t}var a_={[Yn]:"SHADOWMAP_TYPE_PCF",[Ls]:"SHADOWMAP_TYPE_VSM"};function o_(n){return a_[n.shadowMapType]||"SHADOWMAP_TYPE_BASIC"}var l_={[Us]:"ENVMAP_MODE_REFRACTION"};function c_(n){return n.envMap===!1?"ENVMAP_MODE_REFLECTION":l_[n.envMapMode]||"ENVMAP_MODE_REFLECTION"}var h_={[Ro]:"ENVMAP_BLENDING_MULTIPLY",[nd]:"ENVMAP_BLENDING_MIX",[sd]:"ENVMAP_BLENDING_ADD"};function u_(n){return n.envMap===!1?"ENVMAP_BLENDING_NONE":h_[n.combine]||"ENVMAP_BLENDING_NONE"}function d_(n,t,e,i){let s=n.getContext(),r=e.defines,a=e.vertexShader,o=e.fragmentShader,l=o_(e),c=e.envMapPMREM===!0?"ENVMAP_TYPE_PMREM":"ENVMAP_TYPE_CUBE",h=c_(e),u=u_(e),d=Qx(e),f=t_(r),p=s.createProgram(),v,g,m=e.glslVersion?"#version "+e.glslVersion+`
`:"";e.isRawShaderMaterial?(v=["#define SHADER_TYPE "+e.shaderType,"#define SHADER_NAME "+e.shaderName,f].filter(ia).join(`
`),v.length>0&&(v+=`
`),g=["#define SHADER_TYPE "+e.shaderType,"#define SHADER_NAME "+e.shaderName,f].filter(ia).join(`
`),g.length>0&&(g+=`
`)):(v=[kd(e),"#define SHADER_TYPE "+e.shaderType,"#define SHADER_NAME "+e.shaderName,f,e.extensionClipCullDistance?"#define USE_CLIP_DISTANCE":"",e.batching?"#define USE_BATCHING":"",e.batchingColor?"#define USE_BATCHING_COLOR":"",e.instancing?"#define USE_INSTANCING":"",e.instancingColor?"#define USE_INSTANCING_COLOR":"",e.instancingMorph?"#define USE_INSTANCING_MORPH":"",e.useFog&&e.fog?"#define USE_FOG":"",e.useFog&&e.fogExp2?"#define FOG_EXP2":"",e.map?"#define USE_MAP":"",e.envMap?"#define USE_ENVMAP":"",e.envMap?"#define "+h:"",e.lightMap?"#define USE_LIGHTMAP":"",e.aoMap?"#define USE_AOMAP":"",e.bumpMap?"#define USE_BUMPMAP":"",e.normalMap?"#define USE_NORMALMAP":"",e.normalMapObjectSpace?"#define USE_NORMALMAP_OBJECTSPACE":"",e.normalMapTangentSpace?"#define USE_NORMALMAP_TANGENTSPACE":"",e.displacementMap?"#define USE_DISPLACEMENTMAP":"",e.emissiveMap?"#define USE_EMISSIVEMAP":"",e.anisotropy?"#define USE_ANISOTROPY":"",e.anisotropyMap?"#define USE_ANISOTROPYMAP":"",e.clearcoatMap?"#define USE_CLEARCOATMAP":"",e.clearcoatRoughnessMap?"#define USE_CLEARCOAT_ROUGHNESSMAP":"",e.clearcoatNormalMap?"#define USE_CLEARCOAT_NORMALMAP":"",e.diffuseRoughnessMap?"#define USE_DIFFUSE_ROUGHNESSMAP":"",e.iridescenceMap?"#define USE_IRIDESCENCEMAP":"",e.iridescenceThicknessMap?"#define USE_IRIDESCENCE_THICKNESSMAP":"",e.specularMap?"#define USE_SPECULARMAP":"",e.specularColorMap?"#define USE_SPECULAR_COLORMAP":"",e.specularIntensityMap?"#define USE_SPECULAR_INTENSITYMAP":"",e.roughnessMap?"#define USE_ROUGHNESSMAP":"",e.metalnessMap?"#define USE_METALNESSMAP":"",e.alphaMap?"#define USE_ALPHAMAP":"",e.alphaHash?"#define USE_ALPHAHASH":"",e.transmission?"#define USE_TRANSMISSION":"",e.transmissionMap?"#define USE_TRANSMISSIONMAP":"",e.thicknessMap?"#define USE_THICKNESSMAP":"",e.sheenColorMap?"#define USE_SHEEN_COLORMAP":"",e.sheenRoughnessMap?"#define USE_SHEEN_ROUGHNESSMAP":"",e.mapUv?"#define MAP_UV "+e.mapUv:"",e.alphaMapUv?"#define ALPHAMAP_UV "+e.alphaMapUv:"",e.lightMapUv?"#define LIGHTMAP_UV "+e.lightMapUv:"",e.aoMapUv?"#define AOMAP_UV "+e.aoMapUv:"",e.emissiveMapUv?"#define EMISSIVEMAP_UV "+e.emissiveMapUv:"",e.bumpMapUv?"#define BUMPMAP_UV "+e.bumpMapUv:"",e.normalMapUv?"#define NORMALMAP_UV "+e.normalMapUv:"",e.displacementMapUv?"#define DISPLACEMENTMAP_UV "+e.displacementMapUv:"",e.metalnessMapUv?"#define METALNESSMAP_UV "+e.metalnessMapUv:"",e.roughnessMapUv?"#define ROUGHNESSMAP_UV "+e.roughnessMapUv:"",e.anisotropyMapUv?"#define ANISOTROPYMAP_UV "+e.anisotropyMapUv:"",e.clearcoatMapUv?"#define CLEARCOATMAP_UV "+e.clearcoatMapUv:"",e.clearcoatNormalMapUv?"#define CLEARCOAT_NORMALMAP_UV "+e.clearcoatNormalMapUv:"",e.clearcoatRoughnessMapUv?"#define CLEARCOAT_ROUGHNESSMAP_UV "+e.clearcoatRoughnessMapUv:"",e.diffuseRoughnessMapUv?"#define DIFFUSE_ROUGHNESSMAP_UV "+e.diffuseRoughnessMapUv:"",e.iridescenceMapUv?"#define IRIDESCENCEMAP_UV "+e.iridescenceMapUv:"",e.iridescenceThicknessMapUv?"#define IRIDESCENCE_THICKNESSMAP_UV "+e.iridescenceThicknessMapUv:"",e.sheenColorMapUv?"#define SHEEN_COLORMAP_UV "+e.sheenColorMapUv:"",e.sheenRoughnessMapUv?"#define SHEEN_ROUGHNESSMAP_UV "+e.sheenRoughnessMapUv:"",e.specularMapUv?"#define SPECULARMAP_UV "+e.specularMapUv:"",e.specularColorMapUv?"#define SPECULAR_COLORMAP_UV "+e.specularColorMapUv:"",e.specularIntensityMapUv?"#define SPECULAR_INTENSITYMAP_UV "+e.specularIntensityMapUv:"",e.transmissionMapUv?"#define TRANSMISSIONMAP_UV "+e.transmissionMapUv:"",e.thicknessMapUv?"#define THICKNESSMAP_UV "+e.thicknessMapUv:"",e.vertexTangents&&e.flatShading===!1?"#define USE_TANGENT":"",e.vertexNormals?"#define HAS_NORMAL":"",e.vertexColors?"#define USE_COLOR":"",e.vertexAlphas?"#define USE_COLOR_ALPHA":"",e.vertexUv1s?"#define USE_UV1":"",e.vertexUv2s?"#define USE_UV2":"",e.vertexUv3s?"#define USE_UV3":"",e.pointsUvs?"#define USE_POINTS_UV":"",e.flatShading?"#define FLAT_SHADED":"",e.skinning?"#define USE_SKINNING":"",e.morphTargets?"#define USE_MORPHTARGETS":"",e.morphNormals&&e.flatShading===!1?"#define USE_MORPHNORMALS":"",e.morphColors?"#define USE_MORPHCOLORS":"",e.morphTargetsCount>0?"#define MORPHTARGETS_TEXTURE_STRIDE "+e.morphTextureStride:"",e.morphTargetsCount>0?"#define MORPHTARGETS_COUNT "+e.morphTargetsCount:"",e.doubleSided?"#define DOUBLE_SIDED":"",e.flipSided?"#define FLIP_SIDED":"",e.shadowMapEnabled?"#define USE_SHADOWMAP":"",e.shadowMapEnabled?"#define "+l:"",e.sizeAttenuation?"#define USE_SIZEATTENUATION":"",e.numLightProbes>0?"#define USE_LIGHT_PROBES":"",e.logarithmicDepthBuffer?"#define USE_LOGARITHMIC_DEPTH_BUFFER":"",e.reversedDepthBuffer?"#define USE_REVERSED_DEPTH_BUFFER":"","uniform mat4 modelMatrix;","uniform mat4 modelViewMatrix;","uniform mat4 projectionMatrix;","uniform mat4 viewMatrix;","uniform mat3 normalMatrix;","uniform vec3 cameraPosition;","uniform bool isOrthographic;","#ifdef USE_INSTANCING","	attribute mat4 instanceMatrix;","#endif","#ifdef USE_INSTANCING_COLOR","	attribute vec3 instanceColor;","#endif","#ifdef USE_INSTANCING_MORPH","	uniform sampler2D morphTexture;","#endif","attribute vec3 position;","attribute vec3 normal;","attribute vec2 uv;","#ifdef USE_UV1","	attribute vec2 uv1;","#endif","#ifdef USE_UV2","	attribute vec2 uv2;","#endif","#ifdef USE_UV3","	attribute vec2 uv3;","#endif","#ifdef USE_TANGENT","	attribute vec4 tangent;","#endif","#if defined( USE_COLOR_ALPHA )","	attribute vec4 color;","#elif defined( USE_COLOR )","	attribute vec3 color;","#endif","#ifdef USE_SKINNING","	attribute vec4 skinIndex;","	attribute vec4 skinWeight;","#endif",`
`].filter(ia).join(`
`),g=[kd(e),"#define SHADER_TYPE "+e.shaderType,"#define SHADER_NAME "+e.shaderName,f,e.useFog&&e.fog?"#define USE_FOG":"",e.useFog&&e.fogExp2?"#define FOG_EXP2":"",e.alphaToCoverage?"#define ALPHA_TO_COVERAGE":"",e.map?"#define USE_MAP":"",e.matcap?"#define USE_MATCAP":"",e.envMap?"#define USE_ENVMAP":"",e.envMap?"#define "+c:"",e.envMap?"#define "+h:"",e.envMap?"#define "+u:"",e.envMapPMREM?"#define ENVMAP_MAX_LOD "+e.envMapMaxLod+".0":"",e.envMapPMREM?"#define ENVMAP_SIZE "+e.envMapSize+".0":"",e.lightMap?"#define USE_LIGHTMAP":"",e.aoMap?"#define USE_AOMAP":"",e.bumpMap?"#define USE_BUMPMAP":"",e.normalMap?"#define USE_NORMALMAP":"",e.normalMapObjectSpace?"#define USE_NORMALMAP_OBJECTSPACE":"",e.normalMapTangentSpace?"#define USE_NORMALMAP_TANGENTSPACE":"",e.packedNormalMap?"#define USE_PACKED_NORMALMAP":"",e.emissiveMap?"#define USE_EMISSIVEMAP":"",e.anisotropy?"#define USE_ANISOTROPY":"",e.anisotropyMap?"#define USE_ANISOTROPYMAP":"",e.clearcoat?"#define USE_CLEARCOAT":"",e.clearcoatMap?"#define USE_CLEARCOATMAP":"",e.clearcoatRoughnessMap?"#define USE_CLEARCOAT_ROUGHNESSMAP":"",e.clearcoatNormalMap?"#define USE_CLEARCOAT_NORMALMAP":"",e.diffuseRoughness?"#define USE_DIFFUSE_ROUGHNESS":"",e.diffuseRoughnessMap?"#define USE_DIFFUSE_ROUGHNESSMAP":"",e.dispersion?"#define USE_DISPERSION":"",e.retroreflection?"#define USE_RETROREFLECTION":"",e.iridescence?"#define USE_IRIDESCENCE":"",e.iridescenceMap?"#define USE_IRIDESCENCEMAP":"",e.iridescenceThicknessMap?"#define USE_IRIDESCENCE_THICKNESSMAP":"",e.specularMap?"#define USE_SPECULARMAP":"",e.specularColorMap?"#define USE_SPECULAR_COLORMAP":"",e.specularIntensityMap?"#define USE_SPECULAR_INTENSITYMAP":"",e.roughnessMap?"#define USE_ROUGHNESSMAP":"",e.metalnessMap?"#define USE_METALNESSMAP":"",e.alphaMap?"#define USE_ALPHAMAP":"",e.alphaTest?"#define USE_ALPHATEST":"",e.alphaHash?"#define USE_ALPHAHASH":"",e.sheen?"#define USE_SHEEN":"",e.sheenColorMap?"#define USE_SHEEN_COLORMAP":"",e.sheenRoughnessMap?"#define USE_SHEEN_ROUGHNESSMAP":"",e.transmission?"#define USE_TRANSMISSION":"",e.transmissionMap?"#define USE_TRANSMISSIONMAP":"",e.thicknessMap?"#define USE_THICKNESSMAP":"",e.vertexTangents&&e.flatShading===!1?"#define USE_TANGENT":"",e.vertexColors||e.instancingColor?"#define USE_COLOR":"",e.vertexAlphas||e.batchingColor?"#define USE_COLOR_ALPHA":"",e.vertexUv1s?"#define USE_UV1":"",e.vertexUv2s?"#define USE_UV2":"",e.vertexUv3s?"#define USE_UV3":"",e.pointsUvs?"#define USE_POINTS_UV":"",e.gradientMap?"#define USE_GRADIENTMAP":"",e.flatShading?"#define FLAT_SHADED":"",e.doubleSided?"#define DOUBLE_SIDED":"",e.flipSided?"#define FLIP_SIDED":"",e.shadowMapEnabled?"#define USE_SHADOWMAP":"",e.shadowMapEnabled?"#define "+l:"",e.premultipliedAlpha?"#define PREMULTIPLIED_ALPHA":"",e.numLightProbes>0?"#define USE_LIGHT_PROBES":"",e.decodeVideoTexture?"#define DECODE_VIDEO_TEXTURE":"",e.decodeVideoTextureEmissive?"#define DECODE_VIDEO_TEXTURE_EMISSIVE":"",e.logarithmicDepthBuffer?"#define USE_LOGARITHMIC_DEPTH_BUFFER":"",e.reversedDepthBuffer?"#define USE_REVERSED_DEPTH_BUFFER":"","uniform mat4 viewMatrix;","uniform vec3 cameraPosition;","uniform bool isOrthographic;",e.toneMapping!==Hi?"#define TONE_MAPPING":"",e.toneMapping!==Hi?zt.tonemapping_pars_fragment:"",e.toneMapping!==Hi?$x("toneMapping",e.toneMapping):"",e.dithering?"#define DITHERING":"",e.opaque?"#define OPAQUE":"",zt.colorspace_pars_fragment,Kx("linearToOutputTexel",e.outputColorSpace),jx(),e.useDepthPacking?"#define DEPTH_PACKING "+e.depthPacking:"",`
`].filter(ia).join(`
`)),a=ah(a),a=Od(a,e),a=Bd(a,e),o=ah(o),o=Od(o,e),o=Bd(o,e),a=zd(a),o=zd(o),e.isRawShaderMaterial!==!0&&(m=`#version 300 es
`,v=[d,"#define attribute in","#define varying out","#define texture2D texture"].join(`
`)+`
`+v,g=["#define varying in",e.glslVersion===ta?"":"layout(location = 0) out highp vec4 pc_fragColor;",e.glslVersion===ta?"":"#define gl_FragColor pc_fragColor","#define gl_FragDepthEXT gl_FragDepth","#define texture2D texture","#define textureCube texture","#define texture2DProj textureProj","#define texture2DLodEXT textureLod","#define texture2DProjLodEXT textureProjLod","#define textureCubeLodEXT textureLod","#define texture2DGradEXT textureGrad","#define texture2DProjGradEXT textureProjGrad","#define textureCubeGradEXT textureGrad"].join(`
`)+`
`+g);let _=m+v+a,S=m+g+o,M=Nd(s,s.VERTEX_SHADER,_),T=Nd(s,s.FRAGMENT_SHADER,S);s.attachShader(p,M),s.attachShader(p,T),e.index0AttributeName!==void 0?s.bindAttribLocation(p,0,e.index0AttributeName):e.hasPositionAttribute===!0&&s.bindAttribLocation(p,0,"position"),s.linkProgram(p);function w(C){if(n.debug.checkShaderErrors){let I=s.getProgramInfoLog(p)||"",L=s.getShaderInfoLog(M)||"",F=s.getShaderInfoLog(T)||"",D=I.trim(),B=L.trim(),G=F.trim(),k=!0,j=!0;if(s.getProgramParameter(p,s.LINK_STATUS)===!1)if(k=!1,typeof n.debug.onShaderError=="function")n.debug.onShaderError(s,p,M,T);else{let W=Fd(s,M,"vertex"),K=Fd(s,T,"fragment");Lt("WebGLProgram: Shader Error "+s.getError()+" - VALIDATE_STATUS "+s.getProgramParameter(p,s.VALIDATE_STATUS)+`

Material Name: `+C.name+`
Material Type: `+C.type+`

Program Info Log: `+D+`
`+W+`
`+K)}else D!==""?Pt("WebGLProgram: Program Info Log:",D):(B===""||G==="")&&(j=!1);j&&(C.diagnostics={runnable:k,programLog:D,vertexShader:{log:B,prefix:v},fragmentShader:{log:G,prefix:g}})}s.deleteShader(M),s.deleteShader(T),x=new Hs(s,p),y=e_(s,p)}let x;this.getUniforms=function(){return x===void 0&&w(this),x};let y;this.getAttributes=function(){return y===void 0&&w(this),y};let P=e.rendererExtensionParallelShaderCompile===!1;return this.isReady=function(){return P===!1&&(P=s.getProgramParameter(p,Xx)),P},this.destroy=function(){i.releaseStatesOfProgram(this),s.deleteProgram(p),this.program=void 0},this.type=e.shaderType,this.name=e.shaderName,this.id=qx++,this.cacheKey=t,this.usedTimes=1,this.program=p,this.vertexShader=M,this.fragmentShader=T,this}var f_=0,oh=class{constructor(){this.shaderCache=new Map,this.materialCache=new Map}update(t,e,i){let s=this._getShaderCacheForMaterial(t);return s.has(e)===!1&&(s.add(e),e.usedTimes++),s.has(i)===!1&&(s.add(i),i.usedTimes++),this}remove(t){let e=this.materialCache.get(t);for(let i of e)i.usedTimes--,i.usedTimes===0&&this.shaderCache.delete(i.code);return this.materialCache.delete(t),this}getVertexShaderStage(t){return this._getShaderStage(t.vertexShader)}getFragmentShaderStage(t){return this._getShaderStage(t.fragmentShader)}dispose(){this.shaderCache.clear(),this.materialCache.clear()}_getShaderCacheForMaterial(t){let e=this.materialCache,i=e.get(t);return i===void 0&&(i=new Set,e.set(t,i)),i}_getShaderStage(t){let e=this.shaderCache,i=e.get(t);return i===void 0&&(i=new lh(t),e.set(t,i)),i}},lh=class{constructor(t){this.id=f_++,this.code=t,this.usedTimes=0}};function p_(n){return n===Sn||n===$r||n===jr}function m_(n,t,e,i,s,r){let a=new Ms,o=new oh,l=new Set,c=[],h=new Map,u=i.logarithmicDepthBuffer,d={MeshDepthMaterial:"depth",MeshDistanceMaterial:"distance",MeshNormalMaterial:"normal",MeshBasicMaterial:"basic",MeshLambertMaterial:"lambert",MeshPhongMaterial:"phong",MeshToonMaterial:"toon",MeshStandardMaterial:"physical",MeshPhysicalMaterial:"physical",MeshMatcapMaterial:"matcap",LineBasicMaterial:"basic",LineDashedMaterial:"dashed",PointsMaterial:"points",ShadowMaterial:"shadow",SpriteMaterial:"sprite"};function f(x){return l.add(x),x===0?"uv":`uv${x}`}function p(x,y,P,C,I,L){let F=C.fog,D=I.geometry,B=x.isMeshStandardMaterial||x.isMeshLambertMaterial||x.isMeshPhongMaterial?C.environment:null,G=x.isMeshStandardMaterial||x.isMeshLambertMaterial&&!x.envMap||x.isMeshPhongMaterial&&!x.envMap,k=t.get(x.envMap||B,G),j=d[x.type],W=i.precision;x.precision!==null&&(W=i.getMaxPrecision(x.precision),W!==x.precision&&Pt("WebGLProgram.getParameters:",x.precision,"not supported, using",W,"instead."));let K=D.morphAttributes.position||D.morphAttributes.normal||D.morphAttributes.color,Q=K!==void 0?K.length:0,dt=0;D.morphAttributes.position!==void 0&&(dt=1),D.morphAttributes.normal!==void 0&&(dt=2),D.morphAttributes.color!==void 0&&(dt=3);let yt,q,at,gt;if(j){let xt=Gi[j];yt=xt.vertexShader,q=xt.fragmentShader}else{yt=x.vertexShader,q=x.fragmentShader;let xt=o.getVertexShaderStage(x),te=o.getFragmentShaderStage(x);o.update(x,xt,te),at=xt.id,gt=te.id}let Et=n.getRenderTarget(),rt=n.state.buffers.depth.getReversed(),Dt=I.isInstancedMesh===!0,se=I.isBatchedMesh===!0,Rt=!!x.map,Ht=!!x.matcap,$t=!!k,Xt=$t&&k.isPMREMTexture===!0,Be=!!x.aoMap,we=!!x.lightMap,ei=!!x.bumpMap&&x.wireframe===!1,pe=!!x.normalMap,Oi=!!x.displacementMap,De=!!x.emissiveMap,hi=!!x.metalnessMap,ui=!!x.roughnessMap,di=x.anisotropy>0,O=x.clearcoat>0,fi=x.diffuseRoughness>0,he=x.dispersion>0,ue=x.retroreflectivity>0,mt=x.iridescence>0,Ee=x.sheen>0,It=x.transmission>0,Ft=di&&!!x.anisotropyMap,R=O&&!!x.clearcoatMap,b=O&&!!x.clearcoatNormalMap,H=O&&!!x.clearcoatRoughnessMap,it=fi&&!!x.diffuseRoughnessMap,tt=mt&&!!x.iridescenceMap,ot=mt&&!!x.iridescenceThicknessMap,ft=Ee&&!!x.sheenColorMap,Z=Ee&&!!x.sheenRoughnessMap,J=!!x.specularMap,ct=!!x.specularColorMap,N=!!x.specularIntensityMap,$=It&&!!x.transmissionMap,Y=It&&!!x.thicknessMap,ht=!!x.gradientMap,st=!!x.alphaMap,lt=x.alphaTest>0,At=!!x.alphaHash,Vt=!!x.extensions,nt=Hi;x.toneMapped&&(Et===null||Et.isXRRenderTarget===!0)&&(nt=n.toneMapping);let Ct=Xt?k.isCompressedCubeTexture?k.image[0].mipmaps:k.mipmaps:null,kt={shaderID:j,shaderType:x.type,shaderName:x.name,vertexShader:yt,fragmentShader:q,defines:x.defines,customVertexShaderID:at,customFragmentShaderID:gt,isRawShaderMaterial:x.isRawShaderMaterial===!0,glslVersion:x.glslVersion,precision:W,batching:se,batchingColor:se&&I._colorsTexture!==null,instancing:Dt,instancingColor:Dt&&I.instanceColor!==null,instancingMorph:Dt&&I.morphTexture!==null,outputColorSpace:Et===null?n.outputColorSpace:Et.isXRRenderTarget===!0?Et.texture.colorSpace:Wt.workingColorSpace,alphaToCoverage:!!x.alphaToCoverage,map:Rt,matcap:Ht,envMap:$t,envMapMode:$t&&k.mapping,envMapPMREM:Xt,envMapMaxLod:Xt?Ct.length-1:null,envMapSize:Xt?Ct[0].width:null,aoMap:Be,lightMap:we,bumpMap:ei,normalMap:pe,displacementMap:Oi,emissiveMap:De,normalMapObjectSpace:pe&&x.normalMapType===od,normalMapTangentSpace:pe&&x.normalMapType===Qr,packedNormalMap:pe&&x.normalMapType===Qr&&p_(x.normalMap.format),metalnessMap:hi,roughnessMap:ui,anisotropy:di,anisotropyMap:Ft,clearcoat:O,clearcoatMap:R,clearcoatNormalMap:b,clearcoatRoughnessMap:H,diffuseRoughness:fi,diffuseRoughnessMap:it,dispersion:he,retroreflection:ue,iridescence:mt,iridescenceMap:tt,iridescenceThicknessMap:ot,sheen:Ee,sheenColorMap:ft,sheenRoughnessMap:Z,specularMap:J,specularColorMap:ct,specularIntensityMap:N,transmission:It,transmissionMap:$,thicknessMap:Y,gradientMap:ht,opaque:x.transparent===!1&&x.blending===Ns&&x.alphaToCoverage===!1,alphaMap:st,alphaTest:lt,alphaHash:At,combine:x.combine,mapUv:Rt&&f(x.map.channel),aoMapUv:Be&&f(x.aoMap.channel),lightMapUv:we&&f(x.lightMap.channel),bumpMapUv:ei&&f(x.bumpMap.channel),normalMapUv:pe&&f(x.normalMap.channel),displacementMapUv:Oi&&f(x.displacementMap.channel),emissiveMapUv:De&&f(x.emissiveMap.channel),metalnessMapUv:hi&&f(x.metalnessMap.channel),roughnessMapUv:ui&&f(x.roughnessMap.channel),anisotropyMapUv:Ft&&f(x.anisotropyMap.channel),clearcoatMapUv:R&&f(x.clearcoatMap.channel),clearcoatNormalMapUv:b&&f(x.clearcoatNormalMap.channel),clearcoatRoughnessMapUv:H&&f(x.clearcoatRoughnessMap.channel),diffuseRoughnessMapUv:it&&f(x.diffuseRoughnessMap.channel),iridescenceMapUv:tt&&f(x.iridescenceMap.channel),iridescenceThicknessMapUv:ot&&f(x.iridescenceThicknessMap.channel),sheenColorMapUv:ft&&f(x.sheenColorMap.channel),sheenRoughnessMapUv:Z&&f(x.sheenRoughnessMap.channel),specularMapUv:J&&f(x.specularMap.channel),specularColorMapUv:ct&&f(x.specularColorMap.channel),specularIntensityMapUv:N&&f(x.specularIntensityMap.channel),transmissionMapUv:$&&f(x.transmissionMap.channel),thicknessMapUv:Y&&f(x.thicknessMap.channel),alphaMapUv:st&&f(x.alphaMap.channel),vertexTangents:!!D.attributes.tangent&&(pe||di),vertexNormals:!!D.attributes.normal,vertexColors:x.vertexColors,vertexAlphas:x.vertexColors===!0&&!!D.attributes.color&&D.attributes.color.itemSize===4,pointsUvs:I.isPoints===!0&&!!D.attributes.uv&&(Rt||st),fog:!!F,useFog:x.fog===!0,fogExp2:!!F&&F.isFogExp2,flatShading:x.wireframe===!1&&(x.flatShading===!0||D.attributes.normal===void 0&&pe===!1&&(x.isMeshLambertMaterial||x.isMeshPhongMaterial||x.isMeshStandardMaterial||x.isMeshPhysicalMaterial)),sizeAttenuation:x.sizeAttenuation===!0,logarithmicDepthBuffer:u,reversedDepthBuffer:rt,skinning:I.isSkinnedMesh===!0,hasPositionAttribute:D.attributes.position!==void 0,morphTargets:D.morphAttributes.position!==void 0,morphNormals:D.morphAttributes.normal!==void 0,morphColors:D.morphAttributes.color!==void 0,morphTargetsCount:Q,morphTextureStride:dt,numSunLights:y.sun.length,numDirLights:y.directional.length,numPointLights:y.point.length,numSpotLights:y.spot.length,numSpotLightMaps:y.spotLightMap.length,numRectAreaLights:y.rectArea.length,numHemiLights:y.hemi.length,numSunLightShadows:y.sunShadowMap.length,numDirLightShadows:y.directionalShadowMap.length,numPointLightShadows:y.pointShadowMap.length,numSpotLightShadows:y.spotShadowMap.length,numSpotLightShadowsWithMaps:y.numSpotLightShadowsWithMaps,numLightProbes:y.numLightProbes,numLightProbeGrids:L.filter(xt=>xt.texture!==null).length,numClippingPlanes:r.numPlanes,numClipIntersection:r.numIntersection,dithering:x.dithering,shadowMapEnabled:n.shadowMap.enabled&&P.length>0,shadowMapType:n.shadowMap.type,toneMapping:nt,decodeVideoTexture:Rt&&x.map.isVideoTexture===!0&&Wt.getTransfer(x.map.colorSpace)===Jt,decodeVideoTextureEmissive:De&&x.emissiveMap.isVideoTexture===!0&&Wt.getTransfer(x.emissiveMap.colorSpace)===Jt,premultipliedAlpha:x.premultipliedAlpha,doubleSided:x.side===je,flipSided:x.side===ke,useDepthPacking:x.depthPacking>=0,depthPacking:x.depthPacking||0,index0AttributeName:x.index0AttributeName,extensionClipCullDistance:Vt&&x.extensions.clipCullDistance===!0&&e.has("WEBGL_clip_cull_distance"),extensionMultiDraw:(Vt&&x.extensions.multiDraw===!0||se)&&e.has("WEBGL_multi_draw"),rendererExtensionParallelShaderCompile:e.has("KHR_parallel_shader_compile"),customProgramCacheKey:x.customProgramCacheKey()};return kt.vertexUv1s=l.has(1),kt.vertexUv2s=l.has(2),kt.vertexUv3s=l.has(3),l.clear(),kt}function v(x){let y=[];if(x.shaderID?y.push(x.shaderID):(y.push(x.customVertexShaderID),y.push(x.customFragmentShaderID)),x.defines!==void 0)for(let P in x.defines)y.push(P),y.push(x.defines[P]);return x.isRawShaderMaterial===!1&&(g(y,x),m(y,x),y.push(n.outputColorSpace)),y.push(x.customProgramCacheKey),y.join()}function g(x,y){x.push(y.precision),x.push(y.outputColorSpace),x.push(y.envMapMode),x.push(y.envMapPMREM),x.push(y.envMapMaxLod),x.push(y.envMapSize),x.push(y.mapUv),x.push(y.alphaMapUv),x.push(y.lightMapUv),x.push(y.aoMapUv),x.push(y.bumpMapUv),x.push(y.normalMapUv),x.push(y.displacementMapUv),x.push(y.emissiveMapUv),x.push(y.metalnessMapUv),x.push(y.roughnessMapUv),x.push(y.anisotropyMapUv),x.push(y.clearcoatMapUv),x.push(y.clearcoatNormalMapUv),x.push(y.clearcoatRoughnessMapUv),x.push(y.diffuseRoughnessMapUv),x.push(y.iridescenceMapUv),x.push(y.iridescenceThicknessMapUv),x.push(y.sheenColorMapUv),x.push(y.sheenRoughnessMapUv),x.push(y.specularMapUv),x.push(y.specularColorMapUv),x.push(y.specularIntensityMapUv),x.push(y.transmissionMapUv),x.push(y.thicknessMapUv),x.push(y.combine),x.push(y.fogExp2),x.push(y.sizeAttenuation),x.push(y.morphTargetsCount),x.push(y.numSunLights),x.push(y.numDirLights),x.push(y.numPointLights),x.push(y.numSpotLights),x.push(y.numSpotLightMaps),x.push(y.numHemiLights),x.push(y.numRectAreaLights),x.push(y.numSunLightShadows),x.push(y.numDirLightShadows),x.push(y.numPointLightShadows),x.push(y.numSpotLightShadows),x.push(y.numSpotLightShadowsWithMaps),x.push(y.numLightProbes),x.push(y.numLightProbeGrids),x.push(y.shadowMapType),x.push(y.toneMapping),x.push(y.numClippingPlanes),x.push(y.numClipIntersection),x.push(y.depthPacking)}function m(x,y){a.disableAll(),y.instancing&&a.enable(0),y.instancingColor&&a.enable(1),y.instancingMorph&&a.enable(2),y.matcap&&a.enable(3),y.envMap&&a.enable(4),y.normalMapObjectSpace&&a.enable(5),y.normalMapTangentSpace&&a.enable(6),y.clearcoat&&a.enable(7),y.iridescence&&a.enable(8),y.alphaTest&&a.enable(9),y.vertexColors&&a.enable(10),y.vertexAlphas&&a.enable(11),y.vertexUv1s&&a.enable(12),y.vertexUv2s&&a.enable(13),y.vertexUv3s&&a.enable(14),y.vertexTangents&&a.enable(15),y.anisotropy&&a.enable(16),y.alphaHash&&a.enable(17),y.batching&&a.enable(18),y.dispersion&&a.enable(19),y.retroreflection&&a.enable(24),y.diffuseRoughness&&a.enable(25),y.batchingColor&&a.enable(20),y.gradientMap&&a.enable(21),y.packedNormalMap&&a.enable(22),y.vertexNormals&&a.enable(23),x.push(a.mask),a.disableAll(),y.fog&&a.enable(0),y.useFog&&a.enable(1),y.flatShading&&a.enable(2),y.logarithmicDepthBuffer&&a.enable(3),y.reversedDepthBuffer&&a.enable(4),y.skinning&&a.enable(5),y.morphTargets&&a.enable(6),y.morphNormals&&a.enable(7),y.morphColors&&a.enable(8),y.premultipliedAlpha&&a.enable(9),y.shadowMapEnabled&&a.enable(10),y.doubleSided&&a.enable(11),y.flipSided&&a.enable(12),y.useDepthPacking&&a.enable(13),y.dithering&&a.enable(14),y.transmission&&a.enable(15),y.sheen&&a.enable(16),y.opaque&&a.enable(17),y.pointsUvs&&a.enable(18),y.decodeVideoTexture&&a.enable(19),y.decodeVideoTextureEmissive&&a.enable(20),y.alphaToCoverage&&a.enable(21),y.numLightProbeGrids>0&&a.enable(22),y.hasPositionAttribute&&a.enable(23),x.push(a.mask)}function _(x){let y=d[x.type],P;if(y){let C=Gi[y];P=Fi.clone(C.uniforms)}else P=x.uniforms;return P}function S(x,y){let P=h.get(y);return P!==void 0?++P.usedTimes:(P=new d_(n,y,x,s),c.push(P),h.set(y,P)),P}function M(x){if(--x.usedTimes===0){let y=c.indexOf(x);c[y]=c[c.length-1],c.pop(),h.delete(x.cacheKey),x.destroy()}}function T(x){o.remove(x)}function w(){o.dispose()}return{getParameters:p,getProgramCacheKey:v,getUniforms:_,acquireProgram:S,releaseProgram:M,releaseShaderCache:T,programs:c,dispose:w}}function g_(){let n=new WeakMap;function t(a){return n.has(a)}function e(a){let o=n.get(a);return o===void 0&&(o={},n.set(a,o)),o}function i(a){n.delete(a)}function s(a,o,l){n.get(a)[o]=l}function r(){n=new WeakMap}return{has:t,get:e,remove:i,update:s,dispose:r}}function x_(n,t){return n.groupOrder!==t.groupOrder?n.groupOrder-t.groupOrder:n.renderOrder!==t.renderOrder?n.renderOrder-t.renderOrder:n.material.id!==t.material.id?n.material.id-t.material.id:n.materialVariant!==t.materialVariant?n.materialVariant-t.materialVariant:n.z!==t.z?n.z-t.z:n.id-t.id}function Hd(n,t){return n.groupOrder!==t.groupOrder?n.groupOrder-t.groupOrder:n.renderOrder!==t.renderOrder?n.renderOrder-t.renderOrder:n.z!==t.z?t.z-n.z:n.id-t.id}function Vd(){let n=[],t=0,e=[],i=[],s=[];function r(){t=0,e.length=0,i.length=0,s.length=0}function a(d){let f=0;return d.isInstancedMesh&&(f+=2),d.isSkinnedMesh&&(f+=1),f}function o(d,f,p,v,g,m){let _=n[t];return _===void 0?(_={id:d.id,object:d,geometry:f,material:p,materialVariant:a(d),groupOrder:v,renderOrder:d.renderOrder,z:g,group:m},n[t]=_):(_.id=d.id,_.object=d,_.geometry=f,_.material=p,_.materialVariant=a(d),_.groupOrder=v,_.renderOrder=d.renderOrder,_.z=g,_.group=m),t++,_}function l(d,f,p,v,g,m,_){_.reversedDepth===!0&&(g=-g);let S=o(d,f,p,v,g,m);p.transmission>0?i.push(S):p.transparent===!0?s.push(S):e.push(S)}function c(d,f,p,v,g,m){let _=o(d,f,p,v,g,m);p.transmission>0?i.unshift(_):p.transparent===!0?s.unshift(_):e.unshift(_)}function h(d,f){e.length>1&&e.sort(d||x_),i.length>1&&i.sort(f||Hd),s.length>1&&s.sort(f||Hd)}function u(){for(let d=t,f=n.length;d<f;d++){let p=n[d];if(p.id===null)break;p.id=null,p.object=null,p.geometry=null,p.material=null,p.group=null}}return{opaque:e,transmissive:i,transparent:s,init:r,push:l,unshift:c,finish:u,sort:h}}function __(){let n=new WeakMap;function t(i,s){let r=n.get(i),a;return r===void 0?(a=new Vd,n.set(i,[a])):s>=r.length?(a=new Vd,r.push(a)):a=r[s],a}function e(){n=new WeakMap}return{get:t,dispose:e}}function v_(){let n={};return{get:function(t){if(n[t.id]!==void 0)return n[t.id];let e;switch(t.type){case"SunLight":case"DirectionalLight":e={direction:new A,color:new Mt};break;case"SpotLight":e={position:new A,direction:new A,color:new Mt,distance:0,coneCos:0,penumbraCos:0,decay:0};break;case"PointLight":e={position:new A,color:new Mt,distance:0,decay:0};break;case"HemisphereLight":e={direction:new A,skyColor:new Mt,groundColor:new Mt};break;case"RectAreaLight":e={color:new Mt,position:new A,halfWidth:new A,halfHeight:new A};break}return n[t.id]=e,e}}}function y_(){let n={};return{get:function(t){if(n[t.id]!==void 0)return n[t.id];let e;switch(t.type){case"SunLight":case"DirectionalLight":e={shadowIntensity:1,shadowBias:0,shadowNormalBias:0,shadowRadius:1,shadowMapSize:new et};break;case"SpotLight":e={shadowIntensity:1,shadowBias:0,shadowNormalBias:0,shadowRadius:1,shadowMapSize:new et};break;case"PointLight":e={shadowIntensity:1,shadowBias:0,shadowNormalBias:0,shadowRadius:1,shadowMapSize:new et,shadowCameraNear:1,shadowCameraFar:1e3};break}return n[t.id]=e,e}}}var M_=0;function S_(n,t){return(t.castShadow?2:0)-(n.castShadow?2:0)+(t.map?1:0)-(n.map?1:0)}function b_(n){let t=new v_,e=y_(),i={version:0,hash:{sunLength:-1,directionalLength:-1,pointLength:-1,spotLength:-1,rectAreaLength:-1,hemiLength:-1,numSunShadows:-1,numDirectionalShadows:-1,numPointShadows:-1,numSpotShadows:-1,numSpotMaps:-1,numLightProbes:-1},ambient:[0,0,0],probe:[],sun:[],sunShadow:[],sunShadowMap:[],sunShadowMatrix:[],sunShadowCascade:[],directional:[],directionalShadow:[],directionalShadowMap:[],directionalShadowMatrix:[],spot:[],spotLightMap:[],spotShadow:[],spotShadowMap:[],spotLightMatrix:[],rectArea:[],rectAreaLTC1:null,rectAreaLTC2:null,point:[],pointShadow:[],pointShadowMap:[],pointShadowMatrix:[],hemi:[],numSpotLightShadowsWithMaps:0,numLightProbes:0};for(let c=0;c<9;c++)i.probe.push(new A);let s=new A,r=new Gt,a=new Gt;function o(c){let h=0,u=0,d=0;for(let L=0;L<9;L++)i.probe[L].set(0,0,0);let f=0,p=0,v=0,g=0,m=0,_=0,S=0,M=0,T=0,w=0,x=0,y=0,P=0,C=0;c.sort(S_);for(let L=0,F=c.length;L<F;L++){let D=c[L],B=D.color,G=D.intensity,k=D.distance,j=null;if(D.shadow&&D.shadow.map&&(D.shadow.map.texture.format===Sn?j=D.shadow.map.texture:j=D.shadow.map.depthTexture||D.shadow.map.texture),D.isAmbientLight)h+=B.r*G,u+=B.g*G,d+=B.b*G;else if(D.isLightProbe){for(let W=0;W<9;W++)i.probe[W].addScaledVector(D.sh.coefficients[W],G);C++}else if(D.isSunLight){let W=t.get(D);if(W.color.copy(D.color).multiplyScalar(D.intensity),D.castShadow){let K=D.shadow,Q=e.get(D);Q.shadowIntensity=K.intensity,Q.shadowBias=K.bias,Q.shadowNormalBias=K.normalBias,Q.shadowRadius=K.radius,Q.shadowMapSize.copy(K.mapSize).multiply(K.getFrameExtents()),i.sunShadow[p]=Q,i.sunShadowMap[p]=j;let dt=K.getViewportCount();for(let yt=0;yt<dt;yt++)i.sunShadowMatrix[v+yt]=K.getMatrix(yt),i.sunShadowCascade[v+yt]=K._cascadeData[yt];v+=dt,p++}i.sun[f]=W,f++}else if(D.isDirectionalLight){let W=t.get(D);if(W.color.copy(D.color).multiplyScalar(D.intensity),D.castShadow){let K=D.shadow,Q=e.get(D);Q.shadowIntensity=K.intensity,Q.shadowBias=K.bias,Q.shadowNormalBias=K.normalBias,Q.shadowRadius=K.radius,Q.shadowMapSize=K.mapSize,i.directionalShadow[g]=Q,i.directionalShadowMap[g]=j,i.directionalShadowMatrix[g]=D.shadow.matrix,T++}i.directional[g]=W,g++}else if(D.isSpotLight){let W=t.get(D);W.position.setFromMatrixPosition(D.matrixWorld),W.color.copy(B).multiplyScalar(G),W.distance=k,W.coneCos=Math.cos(D.angle),W.penumbraCos=Math.cos(D.angle*(1-D.penumbra)),W.decay=D.decay,i.spot[_]=W;let K=D.shadow;if(D.map&&(i.spotLightMap[y]=D.map,y++,K.updateMatrices(D),D.castShadow&&P++),i.spotLightMatrix[_]=K.matrix,D.castShadow){let Q=e.get(D);Q.shadowIntensity=K.intensity,Q.shadowBias=K.bias,Q.shadowNormalBias=K.normalBias,Q.shadowRadius=K.radius,Q.shadowMapSize=K.mapSize,i.spotShadow[_]=Q,i.spotShadowMap[_]=j,x++}_++}else if(D.isRectAreaLight){let W=t.get(D);W.color.copy(B).multiplyScalar(G),W.halfWidth.set(D.width*.5,0,0),W.halfHeight.set(0,D.height*.5,0),i.rectArea[S]=W,S++}else if(D.isPointLight){let W=t.get(D);if(W.color.copy(D.color).multiplyScalar(D.intensity),W.distance=D.distance,W.decay=D.decay,D.castShadow){let K=D.shadow,Q=e.get(D);Q.shadowIntensity=K.intensity,Q.shadowBias=K.bias,Q.shadowNormalBias=K.normalBias,Q.shadowRadius=K.radius,Q.shadowMapSize=K.mapSize,Q.shadowCameraNear=K.camera.near,Q.shadowCameraFar=K.camera.far,i.pointShadow[m]=Q,i.pointShadowMap[m]=j,i.pointShadowMatrix[m]=D.shadow.matrix,w++}i.point[m]=W,m++}else if(D.isHemisphereLight){let W=t.get(D);W.skyColor.copy(D.color).multiplyScalar(G),W.groundColor.copy(D.groundColor).multiplyScalar(G),i.hemi[M]=W,M++}}S>0&&(n.has("OES_texture_float_linear")===!0?(i.rectAreaLTC1=_t.LTC_FLOAT_1,i.rectAreaLTC2=_t.LTC_FLOAT_2):(i.rectAreaLTC1=_t.LTC_HALF_1,i.rectAreaLTC2=_t.LTC_HALF_2)),i.ambient[0]=h,i.ambient[1]=u,i.ambient[2]=d;let I=i.hash;(I.sunLength!==f||I.directionalLength!==g||I.pointLength!==m||I.spotLength!==_||I.rectAreaLength!==S||I.hemiLength!==M||I.numSunShadows!==p||I.numDirectionalShadows!==T||I.numPointShadows!==w||I.numSpotShadows!==x||I.numSpotMaps!==y||I.numLightProbes!==C)&&(i.sun.length=f,i.directional.length=g,i.spot.length=_,i.rectArea.length=S,i.point.length=m,i.hemi.length=M,i.sunShadow.length=p,i.sunShadowMap.length=p,i.sunShadowMatrix.length=v,i.sunShadowCascade.length=v,i.directionalShadow.length=T,i.directionalShadowMap.length=T,i.directionalShadowMatrix.length=T,i.pointShadow.length=w,i.pointShadowMap.length=w,i.pointShadowMatrix.length=w,i.spotShadow.length=x,i.spotShadowMap.length=x,i.spotLightMatrix.length=x+y-P,i.spotLightMap.length=y,i.numSpotLightShadowsWithMaps=P,i.numLightProbes=C,I.sunLength=f,I.directionalLength=g,I.pointLength=m,I.spotLength=_,I.rectAreaLength=S,I.hemiLength=M,I.numSunShadows=p,I.numDirectionalShadows=T,I.numPointShadows=w,I.numSpotShadows=x,I.numSpotMaps=y,I.numLightProbes=C,i.version=M_++)}function l(c,h){let u=0,d=0,f=0,p=0,v=0,g=0,m=h.matrixWorldInverse;for(let _=0,S=c.length;_<S;_++){let M=c[_];if(M.isSunLight){let T=i.sun[u];T.direction.setFromMatrixPosition(M.matrixWorld),T.direction.transformDirection(m),u++}else if(M.isDirectionalLight){let T=i.directional[d];T.direction.setFromMatrixPosition(M.matrixWorld),s.setFromMatrixPosition(M.target.matrixWorld),T.direction.sub(s),T.direction.transformDirection(m),d++}else if(M.isSpotLight){let T=i.spot[p];T.position.setFromMatrixPosition(M.matrixWorld),T.position.applyMatrix4(m),T.direction.setFromMatrixPosition(M.matrixWorld),s.setFromMatrixPosition(M.target.matrixWorld),T.direction.sub(s),T.direction.transformDirection(m),p++}else if(M.isRectAreaLight){let T=i.rectArea[v];T.position.setFromMatrixPosition(M.matrixWorld),T.position.applyMatrix4(m),a.identity(),r.copy(M.matrixWorld),r.premultiply(m),a.extractRotation(r),T.halfWidth.set(M.width*.5,0,0),T.halfHeight.set(0,M.height*.5,0),T.halfWidth.applyMatrix4(a),T.halfHeight.applyMatrix4(a),v++}else if(M.isPointLight){let T=i.point[f];T.position.setFromMatrixPosition(M.matrixWorld),T.position.applyMatrix4(m),f++}else if(M.isHemisphereLight){let T=i.hemi[g];T.direction.setFromMatrixPosition(M.matrixWorld),T.direction.transformDirection(m),g++}}}return{setup:o,setupView:l,state:i}}function Gd(n){let t=new b_(n),e=[],i=[],s=[];function r(u){h.camera=u,e.length=0,i.length=0,s.length=0}function a(u){u.isLightProbeGrid?s.push(u):e.push(u)}function o(u){i.push(u)}function l(){t.setup(e),s.sort((u,d)=>u.priority-d.priority||u.id-d.id)}function c(u){t.setupView(e,u)}let h={lightsArray:e,shadowsArray:i,lightProbeGridArray:s,camera:null,lights:t,transmissionRenderTarget:{},textureUnits:0};return{init:r,state:h,setupLights:l,setupLightsView:c,pushLight:a,pushShadow:o}}function w_(n){let t=new WeakMap;function e(s,r=0){let a=t.get(s),o;return a===void 0?(o=new Gd(n),t.set(s,[o])):r>=a.length?(o=new Gd(n),a.push(o)):o=a[r],o}function i(){t=new WeakMap}return{get:e,dispose:i}}var E_=`void main() {
	gl_Position = vec4( position, 1.0 );
}`,T_=`uniform sampler2D shadow_pass;
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
}`,A_=[new A(1,0,0),new A(-1,0,0),new A(0,1,0),new A(0,-1,0),new A(0,0,1),new A(0,0,-1)],R_=[new A(0,-1,0),new A(0,-1,0),new A(0,0,1),new A(0,0,-1),new A(0,-1,0),new A(0,-1,0)],Wd=new Gt,ea=new A,ih=new A;function C_(n,t,e){let i=new mn,s=new et,r=new et,a=new ie,o=new mo,l=new go,c={},h=e.maxTextureSize,u={[Mn]:ke,[ke]:Mn,[je]:je},d=new oe({defines:{VSM_SAMPLES:8},uniforms:{shadow_pass:{value:null},resolution:{value:new et},radius:{value:4}},vertexShader:E_,fragmentShader:T_}),f=d.clone();f.defines.HORIZONTAL_PASS=1;let p=new ee;p.setAttribute("position",new _e(new Float32Array([-1,-1,.5,3,-1,.5,-1,3,.5]),3));let v=new Ut(p,d),g=this;this.enabled=!1,this.autoUpdate=!0,this.needsUpdate=!1,this.type=Yn;let m=this.type;this.render=function(w,x,y){if(g.enabled===!1||g.autoUpdate===!1&&g.needsUpdate===!1||w.length===0)return;this.type===Bu&&(Pt("WebGLShadowMap: PCFSoftShadowMap has been removed. Using PCFShadowMap instead."),this.type=Yn);let P=n.getRenderTarget(),C=n.getActiveCubeFace(),I=n.getActiveMipmapLevel(),L=n.state;L.setBlending(Ni),L.buffers.depth.getReversed()===!0?L.buffers.color.setClear(0,0,0,0):L.buffers.color.setClear(1,1,1,1),L.buffers.depth.setTest(!0),L.setScissorTest(!1);let F=m!==this.type;F&&x.traverse(function(D){D.material&&(Array.isArray(D.material)?D.material.forEach(B=>B.needsUpdate=!0):D.material.needsUpdate=!0)});for(let D=0,B=w.length;D<B;D++){let G=w[D],k=G.shadow;if(k===void 0){Pt("WebGLShadowMap:",G,"has no shadow.");continue}if(k.autoUpdate===!1&&k.needsUpdate===!1)continue;s.copy(k.mapSize);let j=k.getFrameExtents();s.multiply(j),r.copy(k.mapSize),(s.x>h||s.y>h)&&(s.x>h&&(r.x=Math.floor(h/j.x),s.x=r.x*j.x,k.mapSize.x=r.x),s.y>h&&(r.y=Math.floor(h/j.y),s.y=r.y*j.y,k.mapSize.y=r.y));let W=n.state.buffers.depth.getReversed();if(k.camera._reversedDepth=W,k.map===null||F===!0){if(k.map!==null&&(k.map.depthTexture!==null&&(k.map.depthTexture.dispose(),k.map.depthTexture=null),k.map.dispose()),this.type===Ls){if(G.isPointLight){Pt("WebGLShadowMap: VSM shadow maps are not supported for PointLights. Use PCF or BasicShadowMap instead.");continue}k.map=new ce(s.x,s.y,{format:Sn,type:fe,minFilter:ae,magFilter:ae,generateMipmaps:!1}),k.map.texture.name=G.name+".shadowMap",k.map.depthTexture=new Ii(s.x,s.y,yi),k.map.depthTexture.name=G.name+".shadowMapDepth",k.map.depthTexture.format=Ai,k.map.depthTexture.compareFunction=null,k.map.depthTexture.minFilter=Se,k.map.depthTexture.magFilter=Se}else G.isPointLight?(k.map=new na(s.x),k.map.depthTexture=new Er(s.x,Qe)):(k.map=new ce(s.x,s.y),k.map.depthTexture=new Ii(s.x,s.y,Qe)),k.map.depthTexture.name=G.name+".shadowMap",k.map.depthTexture.format=Ai,this.type===Yn?(k.map.depthTexture.compareFunction=W?Wc:Bs,k.map.depthTexture.minFilter=ae,k.map.depthTexture.magFilter=ae):(k.map.depthTexture.compareFunction=null,k.map.depthTexture.minFilter=Se,k.map.depthTexture.magFilter=Se);k.camera.updateProjectionMatrix()}k.map.isWebGLCubeRenderTarget!==!0&&(k.map.width!==s.x||k.map.height!==s.y)&&k.map.setSize(s.x,s.y);let K=k.map.isWebGLCubeRenderTarget?6:k.getViewportCount();G.isPointLight!==!0&&k.updateMatrices(G,y);let Q=(k.camera.layers.mask&4294967294)!==0?k.camera.layers:y.layers;for(let dt=0;dt<K;dt++){let yt=k.getCamera(dt);if(G.isPointLight){let q=k.camera,at=k.matrix,gt=G.distance||q.far;gt!==q.far&&(q.far=gt,q.updateProjectionMatrix()),ea.setFromMatrixPosition(G.matrixWorld),q.position.copy(ea),ih.copy(q.position),ih.add(A_[dt]),q.up.copy(R_[dt]),q.lookAt(ih),q.updateMatrixWorld(),at.makeTranslation(-ea.x,-ea.y,-ea.z),Wd.multiplyMatrices(q.projectionMatrix,q.matrixWorldInverse),k._frustum.setFromProjectionMatrix(Wd,q.coordinateSystem,q.reversedDepth)}if(k.map.isWebGLCubeRenderTarget)n.setRenderTarget(k.map,dt),n.clear();else{dt===0&&(n.setRenderTarget(k.map),n.clear());let q=k.getViewport(dt);a.set(r.x*q.x,r.y*q.y,r.x*q.z,r.y*q.w),L.viewport(a)}i=k.getFrustum(dt),M(x,y,yt,G,Q,this.type)}k.isPointLightShadow!==!0&&this.type===Ls&&_(k,y),k.needsUpdate=!1}m=this.type,g.needsUpdate=!1,n.setRenderTarget(P,C,I)};function _(w,x){let y=t.update(v);d.defines.VSM_SAMPLES!==w.blurSamples&&(d.defines.VSM_SAMPLES=w.blurSamples,f.defines.VSM_SAMPLES=w.blurSamples,d.needsUpdate=!0,f.needsUpdate=!0),w.mapPass===null?w.mapPass=new ce(s.x,s.y,{format:Sn,type:fe}):(w.mapPass.width!==w.map.width||w.mapPass.height!==w.map.height)&&w.mapPass.setSize(w.map.width,w.map.height),d.uniforms.shadow_pass.value=w.map.depthTexture,d.uniforms.resolution.value.set(w.map.width,w.map.height),d.uniforms.radius.value=w.radius,n.setRenderTarget(w.mapPass),n.clear(),n.renderBufferDirect(x,null,y,d,v,null),f.uniforms.shadow_pass.value=w.mapPass.texture,f.uniforms.resolution.value.set(w.map.width,w.map.height),f.uniforms.radius.value=w.radius,n.setRenderTarget(w.map),n.clear(),n.renderBufferDirect(x,null,y,f,v,null)}function S(w,x,y,P){let C=null,I=y.isPointLight===!0?w.customDistanceMaterial:w.customDepthMaterial;if(I!==void 0)C=I;else if(C=y.isPointLight===!0?l:o,n.localClippingEnabled&&x.clipShadows===!0&&Array.isArray(x.clippingPlanes)&&x.clippingPlanes.length!==0||x.displacementMap&&x.displacementScale!==0||x.alphaMap&&x.alphaTest>0||x.map&&x.alphaTest>0||x.alphaToCoverage===!0){let L=C.uuid,F=x.uuid,D=c[L];D===void 0&&(D={},c[L]=D);let B=D[F];B===void 0&&(B=C.clone(),D[F]=B,x.addEventListener("dispose",T)),C=B}if(C.visible=x.visible,C.wireframe=x.wireframe,P===Ls?C.side=x.shadowSide!==null?x.shadowSide:x.side:C.side=x.shadowSide!==null?x.shadowSide:u[x.side],C.alphaMap=x.alphaMap,C.alphaTest=x.alphaToCoverage===!0?.5:x.alphaTest,C.map=x.map,C.clipShadows=x.clipShadows,C.clippingPlanes=x.clippingPlanes,C.clipIntersection=x.clipIntersection,C.displacementMap=x.displacementMap,C.displacementScale=x.displacementScale,C.displacementBias=x.displacementBias,C.wireframeLinewidth=x.wireframeLinewidth,C.linewidth=x.linewidth,y.isPointLight===!0&&C.isMeshDistanceMaterial===!0){let L=n.properties.get(C);L.light=y}return C}function M(w,x,y,P,C,I){if(w.visible===!1)return;if(w.layers.test(C)&&(w.isMesh||w.isLine||w.isPoints)&&(w.castShadow||w.receiveShadow&&I===Ls)&&(!w.frustumCulled||w.intersectsFrustum(i))){w.modelViewMatrix.multiplyMatrices(y.matrixWorldInverse,w.matrixWorld);let D=t.update(w),B=w.material;if(Array.isArray(B)){let G=D.groups;for(let k=0,j=G.length;k<j;k++){let W=G[k],K=B[W.materialIndex];if(K&&K.visible){let Q=S(w,K,P,I);w.onBeforeShadow(n,w,x,y,D,Q,W),n.renderBufferDirect(y,null,D,Q,w,W),w.onAfterShadow(n,w,x,y,D,Q,W)}}}else if(B.visible){let G=S(w,B,P,I);w.onBeforeShadow(n,w,x,y,D,G,null),n.renderBufferDirect(y,null,D,G,w,null),w.onAfterShadow(n,w,x,y,D,G,null)}}let F=w.children;for(let D=0,B=F.length;D<B;D++)M(F[D],x,y,P,C,I)}function T(w){w.target.removeEventListener("dispose",T);for(let y in c){let P=c[y],C=w.target.uuid;C in P&&(P[C].dispose(),delete P[C])}}}function P_(n,t){function e(){let N=!1,$=new ie,Y=null,ht=new ie(0,0,0,0);return{setMask:function(st){Y!==st&&!N&&(n.colorMask(st,st,st,st),Y=st)},setLocked:function(st){N=st},setClear:function(st,lt,At,Vt,nt){nt===!0&&(st*=Vt,lt*=Vt,At*=Vt),$.set(st,lt,At,Vt),ht.equals($)===!1&&(n.clearColor(st,lt,At,Vt),ht.copy($))},reset:function(){N=!1,Y=null,ht.set(-1,0,0,0)}}}function i(){let N=!1,$=!1,Y=null,ht=null,st=null;return{setReversed:function(lt){if($!==lt){let At=t.get("EXT_clip_control");lt?At.clipControlEXT(At.LOWER_LEFT_EXT,At.ZERO_TO_ONE_EXT):At.clipControlEXT(At.LOWER_LEFT_EXT,At.NEGATIVE_ONE_TO_ONE_EXT),$=lt;let Vt=st;st=null,this.setClear(Vt)}},getReversed:function(){return $},setTest:function(lt){lt?rt(n.DEPTH_TEST):Dt(n.DEPTH_TEST)},setMask:function(lt){Y!==lt&&!N&&(n.depthMask(lt),Y=lt)},setFunc:function(lt){if($&&(lt=_d[lt]),ht!==lt){switch(lt){case Za:n.depthFunc(n.NEVER);break;case Ka:n.depthFunc(n.ALWAYS);break;case Ja:n.depthFunc(n.LESS);break;case xs:n.depthFunc(n.LEQUAL);break;case $a:n.depthFunc(n.EQUAL);break;case ja:n.depthFunc(n.GEQUAL);break;case Qa:n.depthFunc(n.GREATER);break;case to:n.depthFunc(n.NOTEQUAL);break;default:n.depthFunc(n.LEQUAL)}ht=lt}},setLocked:function(lt){N=lt},setClear:function(lt){st!==lt&&(st=lt,$&&(lt=1-lt),n.clearDepth(lt))},reset:function(){N=!1,Y=null,ht=null,st=null,$=!1}}}function s(){let N=!1,$=null,Y=null,ht=null,st=null,lt=null,At=null,Vt=null,nt=null;return{setTest:function(Ct){N||(Ct?rt(n.STENCIL_TEST):Dt(n.STENCIL_TEST))},setMask:function(Ct){$!==Ct&&!N&&(n.stencilMask(Ct),$=Ct)},setFunc:function(Ct,kt,xt){(Y!==Ct||ht!==kt||st!==xt)&&(n.stencilFunc(Ct,kt,xt),Y=Ct,ht=kt,st=xt)},setOp:function(Ct,kt,xt){(lt!==Ct||At!==kt||Vt!==xt)&&(n.stencilOp(Ct,kt,xt),lt=Ct,At=kt,Vt=xt)},setLocked:function(Ct){N=Ct},setClear:function(Ct){nt!==Ct&&(n.clearStencil(Ct),nt=Ct)},reset:function(){N=!1,$=null,Y=null,ht=null,st=null,lt=null,At=null,Vt=null,nt=null}}}let r=new e,a=new i,o=new s,l={},c=new WeakMap,h={},u={},d={},f=new WeakMap,p=[],v=null,g=!1,m=null,_=null,S=null,M=null,T=null,w=null,x=null,y=new Mt(0,0,0),P=0,C=!1,I=null,L=null,F=null,D=null,B=null,G=n.getParameter(n.MAX_COMBINED_TEXTURE_IMAGE_UNITS),k=!1,j=0,W=n.getParameter(n.VERSION);W.indexOf("WebGL")!==-1?(j=parseFloat(/^WebGL (\d)/.exec(W)[1]),k=j>=1):W.indexOf("OpenGL ES")!==-1&&(j=parseFloat(/^OpenGL ES (\d)/.exec(W)[1]),k=j>=2);let K=null,Q={},dt=n.getParameter(n.SCISSOR_BOX),yt=n.getParameter(n.VIEWPORT),q=new ie().fromArray(dt),at=new ie().fromArray(yt);function gt(N,$,Y,ht){let st=new Uint8Array(4),lt=n.createTexture();n.bindTexture(N,lt),n.texParameteri(N,n.TEXTURE_MIN_FILTER,n.NEAREST),n.texParameteri(N,n.TEXTURE_MAG_FILTER,n.NEAREST);for(let At=0;At<Y;At++)N===n.TEXTURE_3D||N===n.TEXTURE_2D_ARRAY?n.texImage3D($,0,n.RGBA,1,1,ht,0,n.RGBA,n.UNSIGNED_BYTE,st):n.texImage2D($+At,0,n.RGBA,1,1,0,n.RGBA,n.UNSIGNED_BYTE,st);return lt}let Et={};Et[n.TEXTURE_2D]=gt(n.TEXTURE_2D,n.TEXTURE_2D,1),Et[n.TEXTURE_CUBE_MAP]=gt(n.TEXTURE_CUBE_MAP,n.TEXTURE_CUBE_MAP_POSITIVE_X,6),Et[n.TEXTURE_2D_ARRAY]=gt(n.TEXTURE_2D_ARRAY,n.TEXTURE_2D_ARRAY,1,1),Et[n.TEXTURE_3D]=gt(n.TEXTURE_3D,n.TEXTURE_3D,1,1),r.setClear(0,0,0,1),a.setClear(1),o.setClear(0),rt(n.DEPTH_TEST),a.setFunc(xs),ei(!1),pe(Dc),rt(n.CULL_FACE),Be(Ni);function rt(N){h[N]!==!0&&(n.enable(N),h[N]=!0)}function Dt(N){h[N]!==!1&&(n.disable(N),h[N]=!1)}function se(N,$){return d[N]!==$?(n.bindFramebuffer(N,$),d[N]=$,N===n.DRAW_FRAMEBUFFER&&(d[n.FRAMEBUFFER]=$),N===n.FRAMEBUFFER&&(d[n.DRAW_FRAMEBUFFER]=$),!0):!1}function Rt(N,$){let Y=p,ht=!1;if(N){Y=f.get($),Y===void 0&&(Y=[],f.set($,Y));let st=N.textures;if(Y.length!==st.length||Y[0]!==n.COLOR_ATTACHMENT0){for(let lt=0,At=st.length;lt<At;lt++)Y[lt]=n.COLOR_ATTACHMENT0+lt;Y.length=st.length,ht=!0}}else Y[0]!==n.BACK&&(Y[0]=n.BACK,ht=!0);ht&&n.drawBuffers(Y)}function Ht(N){return v!==N?(n.useProgram(N),v=N,!0):!1}let $t={[Zn]:n.FUNC_ADD,[ku]:n.FUNC_SUBTRACT,[Hu]:n.FUNC_REVERSE_SUBTRACT};$t[Vu]=n.MIN,$t[Gu]=n.MAX;let Xt={[Wu]:n.ZERO,[Xu]:n.ONE,[qu]:n.SRC_COLOR,[Uc]:n.SRC_ALPHA,[ju]:n.SRC_ALPHA_SATURATE,[Ju]:n.DST_COLOR,[Zu]:n.DST_ALPHA,[Yu]:n.ONE_MINUS_SRC_COLOR,[Fc]:n.ONE_MINUS_SRC_ALPHA,[$u]:n.ONE_MINUS_DST_COLOR,[Ku]:n.ONE_MINUS_DST_ALPHA,[Qu]:n.CONSTANT_COLOR,[td]:n.ONE_MINUS_CONSTANT_COLOR,[ed]:n.CONSTANT_ALPHA,[id]:n.ONE_MINUS_CONSTANT_ALPHA};function Be(N,$,Y,ht,st,lt,At,Vt,nt,Ct){if(N===Ni){g===!0&&(Dt(n.BLEND),g=!1);return}if(g===!1&&(rt(n.BLEND),g=!0),N!==zu){if(N!==m||Ct!==C){if((_!==Zn||T!==Zn)&&(n.blendEquation(n.FUNC_ADD),_=Zn,T=Zn),Ct)switch(N){case Ns:n.blendFuncSeparate(n.ONE,n.ONE_MINUS_SRC_ALPHA,n.ONE,n.ONE_MINUS_SRC_ALPHA);break;case zr:n.blendFunc(n.ONE,n.ONE);break;case Lc:n.blendFuncSeparate(n.ZERO,n.ONE_MINUS_SRC_COLOR,n.ZERO,n.ONE);break;case Nc:n.blendFuncSeparate(n.DST_COLOR,n.ONE_MINUS_SRC_ALPHA,n.ZERO,n.ONE);break;default:Lt("WebGLState: Invalid blending: ",N);break}else switch(N){case Ns:n.blendFuncSeparate(n.SRC_ALPHA,n.ONE_MINUS_SRC_ALPHA,n.ONE,n.ONE_MINUS_SRC_ALPHA);break;case zr:n.blendFuncSeparate(n.SRC_ALPHA,n.ONE,n.ONE,n.ONE);break;case Lc:Lt("WebGLState: SubtractiveBlending requires material.premultipliedAlpha = true");break;case Nc:Lt("WebGLState: MultiplyBlending requires material.premultipliedAlpha = true");break;default:Lt("WebGLState: Invalid blending: ",N);break}S=null,M=null,w=null,x=null,y.set(0,0,0),P=0,m=N,C=Ct}return}st=st||$,lt=lt||Y,At=At||ht,($!==_||st!==T)&&(n.blendEquationSeparate($t[$],$t[st]),_=$,T=st),(Y!==S||ht!==M||lt!==w||At!==x)&&(n.blendFuncSeparate(Xt[Y],Xt[ht],Xt[lt],Xt[At]),S=Y,M=ht,w=lt,x=At),(Vt.equals(y)===!1||nt!==P)&&(n.blendColor(Vt.r,Vt.g,Vt.b,nt),y.copy(Vt),P=nt),m=N,C=!1}function we(N,$){N.side===je?Dt(n.CULL_FACE):rt(n.CULL_FACE);let Y=N.side===ke;$&&(Y=!Y),ei(Y),N.blending===Ns&&N.transparent===!1?Be(Ni):Be(N.blending,N.blendEquation,N.blendSrc,N.blendDst,N.blendEquationAlpha,N.blendSrcAlpha,N.blendDstAlpha,N.blendColor,N.blendAlpha,N.premultipliedAlpha),a.setFunc(N.depthFunc),a.setTest(N.depthTest),a.setMask(N.depthWrite),r.setMask(N.colorWrite);let ht=N.stencilWrite;o.setTest(ht),ht&&(o.setMask(N.stencilWriteMask),o.setFunc(N.stencilFunc,N.stencilRef,N.stencilFuncMask),o.setOp(N.stencilFail,N.stencilZFail,N.stencilZPass)),De(N.polygonOffset,N.polygonOffsetFactor,N.polygonOffsetUnits),N.alphaToCoverage===!0?rt(n.SAMPLE_ALPHA_TO_COVERAGE):Dt(n.SAMPLE_ALPHA_TO_COVERAGE)}function ei(N){I!==N&&(N?n.frontFace(n.CW):n.frontFace(n.CCW),I=N)}function pe(N){N!==Fu?(rt(n.CULL_FACE),N!==L&&(N===Dc?n.cullFace(n.BACK):N===Ou?n.cullFace(n.FRONT):n.cullFace(n.FRONT_AND_BACK))):Dt(n.CULL_FACE),L=N}function Oi(N){N!==F&&(k&&n.lineWidth(N),F=N)}function De(N,$,Y){N?(rt(n.POLYGON_OFFSET_FILL),(D!==$||B!==Y)&&(D=$,B=Y,a.getReversed()&&($=-$,Y=-Y),n.polygonOffset($,Y))):Dt(n.POLYGON_OFFSET_FILL)}function hi(N){N?rt(n.SCISSOR_TEST):Dt(n.SCISSOR_TEST)}function ui(N){N===void 0&&(N=n.TEXTURE0+G-1),K!==N&&(n.activeTexture(N),K=N)}function di(N,$,Y){Y===void 0&&(K===null?Y=n.TEXTURE0+G-1:Y=K);let ht=Q[Y];ht===void 0&&(ht={type:void 0,texture:void 0},Q[Y]=ht),(ht.type!==N||ht.texture!==$)&&(K!==Y&&(n.activeTexture(Y),K=Y),n.bindTexture(N,$||Et[N]),ht.type=N,ht.texture=$)}function O(){let N=Q[K];N!==void 0&&N.type!==void 0&&(n.bindTexture(N.type,null),N.type=void 0,N.texture=void 0)}function fi(){try{n.compressedTexImage2D(...arguments)}catch(N){Lt("WebGLState:",N)}}function he(){try{n.compressedTexImage3D(...arguments)}catch(N){Lt("WebGLState:",N)}}function ue(){try{n.texSubImage2D(...arguments)}catch(N){Lt("WebGLState:",N)}}function mt(){try{n.texSubImage3D(...arguments)}catch(N){Lt("WebGLState:",N)}}function Ee(){try{n.compressedTexSubImage2D(...arguments)}catch(N){Lt("WebGLState:",N)}}function It(){try{n.compressedTexSubImage3D(...arguments)}catch(N){Lt("WebGLState:",N)}}function Ft(){try{n.texStorage2D(...arguments)}catch(N){Lt("WebGLState:",N)}}function R(){try{n.texStorage3D(...arguments)}catch(N){Lt("WebGLState:",N)}}function b(){try{n.texImage2D(...arguments)}catch(N){Lt("WebGLState:",N)}}function H(){try{n.texImage3D(...arguments)}catch(N){Lt("WebGLState:",N)}}function it(N){return u[N]!==void 0?u[N]:n.getParameter(N)}function tt(N,$){u[N]!==$&&(n.pixelStorei(N,$),u[N]=$)}function ot(N){q.equals(N)===!1&&(n.scissor(N.x,N.y,N.z,N.w),q.copy(N))}function ft(N){at.equals(N)===!1&&(n.viewport(N.x,N.y,N.z,N.w),at.copy(N))}function Z(N,$){let Y=c.get($);Y===void 0&&(Y=new WeakMap,c.set($,Y));let ht=Y.get(N);ht===void 0&&(ht=n.getUniformBlockIndex($,N.name),n.uniformBlockBinding($,ht,ht),Y.set(N,ht))}function J(N,$,Y){let st=c.get($).get(N);l[st]!==Y&&(n.bindBufferBase(n.UNIFORM_BUFFER,st,Y),l[st]=Y)}function ct(){n.disable(n.BLEND),n.disable(n.CULL_FACE),n.disable(n.DEPTH_TEST),n.disable(n.POLYGON_OFFSET_FILL),n.disable(n.SCISSOR_TEST),n.disable(n.STENCIL_TEST),n.disable(n.SAMPLE_ALPHA_TO_COVERAGE),n.blendEquation(n.FUNC_ADD),n.blendFunc(n.ONE,n.ZERO),n.blendFuncSeparate(n.ONE,n.ZERO,n.ONE,n.ZERO),n.blendColor(0,0,0,0),n.colorMask(!0,!0,!0,!0),n.clearColor(0,0,0,0),n.depthMask(!0),n.depthFunc(n.LESS),a.setReversed(!1),n.clearDepth(1),n.stencilMask(4294967295),n.stencilFunc(n.ALWAYS,0,4294967295),n.stencilOp(n.KEEP,n.KEEP,n.KEEP),n.clearStencil(0),n.cullFace(n.BACK),n.frontFace(n.CCW),n.polygonOffset(0,0),n.activeTexture(n.TEXTURE0),n.bindFramebuffer(n.FRAMEBUFFER,null),n.bindFramebuffer(n.DRAW_FRAMEBUFFER,null),n.bindFramebuffer(n.READ_FRAMEBUFFER,null),n.useProgram(null),n.lineWidth(1),n.scissor(0,0,n.canvas.width,n.canvas.height),n.viewport(0,0,n.canvas.width,n.canvas.height),n.pixelStorei(n.PACK_ALIGNMENT,4),n.pixelStorei(n.UNPACK_ALIGNMENT,4),n.pixelStorei(n.UNPACK_FLIP_Y_WEBGL,!1),n.pixelStorei(n.UNPACK_PREMULTIPLY_ALPHA_WEBGL,!1),n.pixelStorei(n.UNPACK_COLORSPACE_CONVERSION_WEBGL,n.BROWSER_DEFAULT_WEBGL),n.pixelStorei(n.PACK_ROW_LENGTH,0),n.pixelStorei(n.PACK_SKIP_PIXELS,0),n.pixelStorei(n.PACK_SKIP_ROWS,0),n.pixelStorei(n.UNPACK_ROW_LENGTH,0),n.pixelStorei(n.UNPACK_IMAGE_HEIGHT,0),n.pixelStorei(n.UNPACK_SKIP_PIXELS,0),n.pixelStorei(n.UNPACK_SKIP_ROWS,0),n.pixelStorei(n.UNPACK_SKIP_IMAGES,0),h={},u={},K=null,Q={},d={},l={},f=new WeakMap,p=[],v=null,g=!1,m=null,_=null,S=null,M=null,T=null,w=null,x=null,y=new Mt(0,0,0),P=0,C=!1,I=null,L=null,F=null,D=null,B=null,q.set(0,0,n.canvas.width,n.canvas.height),at.set(0,0,n.canvas.width,n.canvas.height),r.reset(),a.reset(),o.reset()}return{buffers:{color:r,depth:a,stencil:o},enable:rt,disable:Dt,bindFramebuffer:se,drawBuffers:Rt,useProgram:Ht,setBlending:Be,setMaterial:we,setFlipSided:ei,setCullFace:pe,setLineWidth:Oi,setPolygonOffset:De,setScissorTest:hi,activeTexture:ui,bindTexture:di,unbindTexture:O,compressedTexImage2D:fi,compressedTexImage3D:he,texImage2D:b,texImage3D:H,pixelStorei:tt,getParameter:it,updateUBOMapping:Z,uniformBlockBinding:J,texStorage2D:Ft,texStorage3D:R,texSubImage2D:ue,texSubImage3D:mt,compressedTexSubImage2D:Ee,compressedTexSubImage3D:It,scissor:ot,viewport:ft,reset:ct}}function I_(n,t,e,i,s,r,a){let o=t.has("WEBGL_multisampled_render_to_texture")?t.get("WEBGL_multisampled_render_to_texture"):null,l=typeof navigator>"u"?!1:/OculusBrowser/g.test(navigator.userAgent),c=new et,h=new WeakMap,u=new Set,d,f=new WeakMap,p=new Set,v=new FinalizationRegistry(R=>p.delete(R)),g=!1;try{g=typeof OffscreenCanvas<"u"&&new OffscreenCanvas(1,1).getContext("2d")!==null}catch{}function m(R,b){return g?new OffscreenCanvas(R,b):gr("canvas")}function _(R,b,H){let it=1,tt=Ft(R);if((tt.width>H||tt.height>H)&&(it=H/Math.max(tt.width,tt.height)),it<1)if(typeof HTMLImageElement<"u"&&R instanceof HTMLImageElement||typeof HTMLCanvasElement<"u"&&R instanceof HTMLCanvasElement||typeof ImageBitmap<"u"&&R instanceof ImageBitmap||typeof VideoFrame<"u"&&R instanceof VideoFrame){let ot=Math.floor(it*tt.width),ft=Math.floor(it*tt.height);d===void 0&&(d=m(ot,ft));let Z=b?m(ot,ft):d;return Z.width=ot,Z.height=ft,Z.getContext("2d").drawImage(R,0,0,ot,ft),Pt("WebGLRenderer: Texture has been resized from ("+tt.width+"x"+tt.height+") to ("+ot+"x"+ft+")."),Z}else return"data"in R&&Pt("WebGLRenderer: Image in DataTexture is too big ("+tt.width+"x"+tt.height+")."),R;return R}function S(R){return R.generateMipmaps}function M(R){n.generateMipmap(R)}function T(R){return R.isWebGLCubeRenderTarget?n.TEXTURE_CUBE_MAP:R.isWebGL3DRenderTarget?n.TEXTURE_3D:R.isWebGLArrayRenderTarget||R.isCompressedArrayTexture?n.TEXTURE_2D_ARRAY:n.TEXTURE_2D}function w(R,b,H,it,tt,ot=!1){if(R!==null){if(n[R]!==void 0)return n[R];Pt("WebGLRenderer: Attempt to use non-existing WebGL internal format '"+R+"'")}let ft;it&&(ft=t.get("EXT_texture_norm16"),ft||Pt("WebGLRenderer: Unable to use normalized textures without EXT_texture_norm16 extension"));let Z=b;if(b===n.RED&&(H===n.FLOAT&&(Z=n.R32F),H===n.HALF_FLOAT&&(Z=n.R16F),H===n.UNSIGNED_BYTE&&(Z=n.R8),H===n.UNSIGNED_SHORT&&ft&&(Z=ft.R16_EXT),H===n.SHORT&&ft&&(Z=ft.R16_SNORM_EXT)),b===n.RED_INTEGER&&(H===n.UNSIGNED_BYTE&&(Z=n.R8UI),H===n.UNSIGNED_SHORT&&(Z=n.R16UI),H===n.UNSIGNED_INT&&(Z=n.R32UI),H===n.BYTE&&(Z=n.R8I),H===n.SHORT&&(Z=n.R16I),H===n.INT&&(Z=n.R32I)),b===n.RG&&(H===n.FLOAT&&(Z=n.RG32F),H===n.HALF_FLOAT&&(Z=n.RG16F),H===n.UNSIGNED_BYTE&&(Z=n.RG8),H===n.UNSIGNED_SHORT&&ft&&(Z=ft.RG16_EXT),H===n.SHORT&&ft&&(Z=ft.RG16_SNORM_EXT)),b===n.RG_INTEGER&&(H===n.UNSIGNED_BYTE&&(Z=n.RG8UI),H===n.UNSIGNED_SHORT&&(Z=n.RG16UI),H===n.UNSIGNED_INT&&(Z=n.RG32UI),H===n.BYTE&&(Z=n.RG8I),H===n.SHORT&&(Z=n.RG16I),H===n.INT&&(Z=n.RG32I)),b===n.RGB_INTEGER&&(H===n.UNSIGNED_BYTE&&(Z=n.RGB8UI),H===n.UNSIGNED_SHORT&&(Z=n.RGB16UI),H===n.UNSIGNED_INT&&(Z=n.RGB32UI),H===n.BYTE&&(Z=n.RGB8I),H===n.SHORT&&(Z=n.RGB16I),H===n.INT&&(Z=n.RGB32I)),b===n.RGBA_INTEGER&&(H===n.UNSIGNED_BYTE&&(Z=n.RGBA8UI),H===n.UNSIGNED_SHORT&&(Z=n.RGBA16UI),H===n.UNSIGNED_INT&&(Z=n.RGBA32UI),H===n.BYTE&&(Z=n.RGBA8I),H===n.SHORT&&(Z=n.RGBA16I),H===n.INT&&(Z=n.RGBA32I)),b===n.RGB&&(H===n.UNSIGNED_SHORT&&ft&&(Z=ft.RGB16_EXT),H===n.SHORT&&ft&&(Z=ft.RGB16_SNORM_EXT),H===n.UNSIGNED_INT_5_9_9_9_REV&&(Z=n.RGB9_E5),H===n.UNSIGNED_INT_10F_11F_11F_REV&&(Z=n.R11F_G11F_B10F)),b===n.RGBA){let J=ot?mr:Wt.getTransfer(tt);H===n.FLOAT&&(Z=n.RGBA32F),H===n.HALF_FLOAT&&(Z=n.RGBA16F),H===n.UNSIGNED_BYTE&&(Z=J===Jt?n.SRGB8_ALPHA8:n.RGBA8),H===n.UNSIGNED_SHORT&&ft&&(Z=ft.RGBA16_EXT),H===n.SHORT&&ft&&(Z=ft.RGBA16_SNORM_EXT),H===n.UNSIGNED_SHORT_4_4_4_4&&(Z=n.RGBA4),H===n.UNSIGNED_SHORT_5_5_5_1&&(Z=n.RGB5_A1)}return(Z===n.R16F||Z===n.R32F||Z===n.RG16F||Z===n.RG32F||Z===n.RGBA16F||Z===n.RGBA32F)&&t.get("EXT_color_buffer_float"),Z}function x(R,b){let H;return R?b===null||b===Qe||b===$n?H=n.DEPTH24_STENCIL8:b===yi?H=n.DEPTH32F_STENCIL8:b===Fs&&(H=n.DEPTH24_STENCIL8,Pt("DepthTexture: 16 bit depth attachment is not supported with stencil. Using 24-bit attachment.")):b===null||b===Qe||b===$n?H=n.DEPTH_COMPONENT24:b===yi?H=n.DEPTH_COMPONENT32F:b===Fs&&(H=n.DEPTH_COMPONENT16),H}function y(R,b){return S(R)===!0||R.isFramebufferTexture&&R.minFilter!==Se&&R.minFilter!==ae?Math.log2(Math.max(b.width,b.height))+1:R.mipmaps.length>0?R.mipmaps.length:R.isCompressedTexture&&Array.isArray(R.image)?b.mipmaps.length:1}function P(R){let b=new WeakRef(R);i.get(R).__ref=b,p.add(b),v.register(R,b,b)}function C(R){let b=i.get(R).__ref;b!==void 0&&(p.delete(b),v.unregister(b))}function I(R){F(R.target)}function L(R){D(R.target)}function F(R){R.removeEventListener("dispose",I),C(R),G(R),R.isVideoTexture&&h.delete(R),R.isHTMLTexture&&u.delete(R)}function D(R){R.removeEventListener("dispose",L),C(R),j(R)}function B(){for(let R of p){let b=R.deref();b!==void 0&&(b.isRenderTarget===!0?D(b):F(b))}p.clear(),u.clear()}function G(R){let b=i.get(R);if(b.__webglInit===void 0)return;let H=R.source,it=f.get(H);if(it){let tt=it[b.__cacheKey];tt.usedTimes--,tt.usedTimes===0&&k(R),Object.keys(it).length===0&&f.delete(H)}i.remove(R)}function k(R){let b=i.get(R);n.deleteTexture(b.__webglTexture);let H=R.source,it=f.get(H);delete it[b.__cacheKey],a.memory.textures--}function j(R){let b=i.get(R);b.__depthDisposeCallback&&b.__depthDisposeCallback();let H=R.depthTexture;if(H&&H.renderTarget===R&&F(H),R.isWebGLCubeRenderTarget)for(let tt=0;tt<6;tt++){if(Array.isArray(b.__webglFramebuffer[tt]))for(let ot=0;ot<b.__webglFramebuffer[tt].length;ot++)n.deleteFramebuffer(b.__webglFramebuffer[tt][ot]);else n.deleteFramebuffer(b.__webglFramebuffer[tt]);b.__webglDepthbuffer&&n.deleteRenderbuffer(b.__webglDepthbuffer[tt])}else{if(Array.isArray(b.__webglFramebuffer))for(let tt=0;tt<b.__webglFramebuffer.length;tt++)n.deleteFramebuffer(b.__webglFramebuffer[tt]);else n.deleteFramebuffer(b.__webglFramebuffer);if(b.__webglDepthbuffer&&n.deleteRenderbuffer(b.__webglDepthbuffer),b.__webglMultisampledFramebuffer&&n.deleteFramebuffer(b.__webglMultisampledFramebuffer),b.__webglColorRenderbuffer)for(let tt=0;tt<b.__webglColorRenderbuffer.length;tt++)b.__webglColorRenderbuffer[tt]&&n.deleteRenderbuffer(b.__webglColorRenderbuffer[tt]);b.__webglDepthRenderbuffer&&n.deleteRenderbuffer(b.__webglDepthRenderbuffer)}let it=R.textures;for(let tt=0,ot=it.length;tt<ot;tt++){let ft=i.get(it[tt]);ft.__webglTexture&&(n.deleteTexture(ft.__webglTexture),a.memory.textures--),i.remove(it[tt])}i.remove(R)}let W=0;function K(){W=0}function Q(){return W}function dt(R){W=R}function yt(){let R=W;return R>=s.maxTextures&&Pt("WebGLTextures: Trying to use "+(R+1)+" texture units while this GPU supports only "+s.maxTextures),W+=1,R}function q(R){let b=[];return b.push(R.wrapS),b.push(R.wrapT),b.push(R.wrapR||0),b.push(R.magFilter),b.push(R.minFilter),b.push(R.anisotropy),b.push(R.internalFormat),b.push(R.format),b.push(R.type),b.push(R.generateMipmaps),b.push(R.mipmapsAutoUpdate),b.push(R.premultiplyAlpha),b.push(R.flipY),b.push(R.unpackAlignment),b.push(R.colorSpace),b.join()}function at(R,b){let H=i.get(R);if(R.isVideoTexture&&Ee(R),R.isRenderTargetTexture===!1&&R.isExternalTexture!==!0&&R.version>0&&H.__version!==R.version){let it=R.image;if(it===null)Pt("WebGLRenderer: Texture marked for update but no image data found.");else if(it.complete===!1)Pt("WebGLRenderer: Texture marked for update but image is incomplete");else{we(H,R,b);return}}else R.isExternalTexture&&(H.__webglTexture=R.sourceTexture?R.sourceTexture:null);e.bindTexture(n.TEXTURE_2D,H.__webglTexture,n.TEXTURE0+b)}function gt(R,b){let H=i.get(R);if(R.isRenderTargetTexture===!1&&R.version>0&&H.__version!==R.version){we(H,R,b);return}else R.isExternalTexture&&(H.__webglTexture=R.sourceTexture?R.sourceTexture:null);e.bindTexture(n.TEXTURE_2D_ARRAY,H.__webglTexture,n.TEXTURE0+b)}function Et(R,b){let H=i.get(R);if(R.isRenderTargetTexture===!1&&R.version>0&&H.__version!==R.version){we(H,R,b);return}e.bindTexture(n.TEXTURE_3D,H.__webglTexture,n.TEXTURE0+b)}function rt(R,b){let H=i.get(R);if(R.version>0&&H.__version!==R.version){ei(H,R,b);return}e.bindTexture(n.TEXTURE_CUBE_MAP,H.__webglTexture,n.TEXTURE0+b)}let Dt={[Fn]:n.REPEAT,[ki]:n.CLAMP_TO_EDGE,[eo]:n.MIRRORED_REPEAT},se={[Se]:n.NEAREST,[rd]:n.NEAREST_MIPMAP_NEAREST,[qr]:n.NEAREST_MIPMAP_LINEAR,[ae]:n.LINEAR,[Io]:n.LINEAR_MIPMAP_NEAREST,[sn]:n.LINEAR_MIPMAP_LINEAR},Rt={[cd]:n.NEVER,[Xc]:n.ALWAYS,[hd]:n.LESS,[Bs]:n.LEQUAL,[ud]:n.EQUAL,[Wc]:n.GEQUAL,[dd]:n.GREATER,[fd]:n.NOTEQUAL};function Ht(R,b){if(b.type===yi&&t.has("OES_texture_float_linear")===!1&&(b.magFilter===ae||b.magFilter===Io||b.magFilter===qr||b.magFilter===sn||b.minFilter===ae||b.minFilter===Io||b.minFilter===qr||b.minFilter===sn)&&Pt("WebGLRenderer: Unable to use linear filtering with floating point textures. OES_texture_float_linear not supported on this device."),n.texParameteri(R,n.TEXTURE_WRAP_S,Dt[b.wrapS]),n.texParameteri(R,n.TEXTURE_WRAP_T,Dt[b.wrapT]),(R===n.TEXTURE_3D||R===n.TEXTURE_2D_ARRAY)&&n.texParameteri(R,n.TEXTURE_WRAP_R,Dt[b.wrapR]),n.texParameteri(R,n.TEXTURE_MAG_FILTER,se[b.magFilter]),n.texParameteri(R,n.TEXTURE_MIN_FILTER,se[b.minFilter]),b.compareFunction&&(n.texParameteri(R,n.TEXTURE_COMPARE_MODE,n.COMPARE_REF_TO_TEXTURE),n.texParameteri(R,n.TEXTURE_COMPARE_FUNC,Rt[b.compareFunction])),t.has("EXT_texture_filter_anisotropic")===!0){if(b.magFilter===Se||b.minFilter!==qr&&b.minFilter!==sn||b.type===yi&&t.has("OES_texture_float_linear")===!1)return;if(b.anisotropy>1||i.get(b).__currentAnisotropy){let H=t.get("EXT_texture_filter_anisotropic");n.texParameterf(R,H.TEXTURE_MAX_ANISOTROPY_EXT,Math.min(b.anisotropy,s.getMaxAnisotropy())),i.get(b).__currentAnisotropy=b.anisotropy}}}function $t(R,b){let H=!1;R.__webglInit===void 0&&(R.__webglInit=!0,b.addEventListener("dispose",I),P(b));let it=b.source,tt=f.get(it);tt===void 0&&(tt={},f.set(it,tt));let ot=q(b);if(ot!==R.__cacheKey){tt[ot]===void 0&&(tt[ot]={texture:n.createTexture(),usedTimes:0},a.memory.textures++,H=!0),tt[ot].usedTimes++;let ft=tt[R.__cacheKey];ft!==void 0&&(tt[R.__cacheKey].usedTimes--,ft.usedTimes===0&&k(b)),R.__cacheKey=ot,R.__webglTexture=tt[ot].texture}return H}function Xt(R,b,H){return Math.floor(Math.floor(R/H)/b)}function Be(R,b,H,it){let ot=R.updateRanges;if(ot.length===0)e.texSubImage2D(n.TEXTURE_2D,0,0,0,b.width,b.height,H,it,b.data);else{ot.sort((N,$)=>N.start-$.start);let ft=0;for(let N=1;N<ot.length;N++){let $=ot[ft],Y=ot[N],ht=$.start+$.count,st=Xt(Y.start,b.width,4),lt=Xt($.start,b.width,4);Y.start<=ht+1&&st===lt&&Xt(Y.start+Y.count-1,b.width,4)===st?$.count=Math.max($.count,Y.start+Y.count-$.start):(++ft,ot[ft]=Y)}ot.length=ft+1;let Z=e.getParameter(n.UNPACK_ROW_LENGTH),J=e.getParameter(n.UNPACK_SKIP_PIXELS),ct=e.getParameter(n.UNPACK_SKIP_ROWS);e.pixelStorei(n.UNPACK_ROW_LENGTH,b.width);for(let N=0,$=ot.length;N<$;N++){let Y=ot[N],ht=Math.floor(Y.start/4),st=Math.ceil(Y.count/4),lt=ht%b.width,At=Math.floor(ht/b.width),Vt=st,nt=1;e.pixelStorei(n.UNPACK_SKIP_PIXELS,lt),e.pixelStorei(n.UNPACK_SKIP_ROWS,At),e.texSubImage2D(n.TEXTURE_2D,0,lt,At,Vt,nt,H,it,b.data)}R.clearUpdateRanges(),e.pixelStorei(n.UNPACK_ROW_LENGTH,Z),e.pixelStorei(n.UNPACK_SKIP_PIXELS,J),e.pixelStorei(n.UNPACK_SKIP_ROWS,ct)}}function we(R,b,H){let it=n.TEXTURE_2D;(b.isDataArrayTexture||b.isCompressedArrayTexture)&&(it=n.TEXTURE_2D_ARRAY),b.isData3DTexture&&(it=n.TEXTURE_3D);let tt=$t(R,b),ot=b.source;e.bindTexture(it,R.__webglTexture,n.TEXTURE0+H);let ft=i.get(ot);if(ot.version!==ft.__version||tt===!0){if(e.activeTexture(n.TEXTURE0+H),(typeof ImageBitmap<"u"&&b.image instanceof ImageBitmap)===!1){let nt=Wt.getPrimaries(Wt.workingColorSpace),Ct=b.colorSpace===Ui?null:Wt.getPrimaries(b.colorSpace),kt=b.colorSpace===Ui||nt===Ct?n.NONE:n.BROWSER_DEFAULT_WEBGL;e.pixelStorei(n.UNPACK_FLIP_Y_WEBGL,b.flipY),e.pixelStorei(n.UNPACK_PREMULTIPLY_ALPHA_WEBGL,b.premultiplyAlpha),e.pixelStorei(n.UNPACK_COLORSPACE_CONVERSION_WEBGL,kt)}e.pixelStorei(n.UNPACK_ALIGNMENT,b.unpackAlignment);let J=_(b.image,!1,s.maxTextureSize);J=It(b,J);let ct=r.convert(b.format,b.colorSpace),N=r.convert(b.type),$=w(b.internalFormat,ct,N,b.normalized,b.colorSpace,b.isVideoTexture);Ht(it,b);let Y,ht=b.mipmaps,st=b.isVideoTexture!==!0,lt=ft.__version===void 0||tt===!0,At=ot.dataReady,Vt=y(b,J);if(b.isDepthTexture)$=x(b.format===rn,b.type),lt&&(st?e.texStorage2D(n.TEXTURE_2D,1,$,J.width,J.height):e.texImage2D(n.TEXTURE_2D,0,$,J.width,J.height,0,ct,N,null));else if(b.isDataTexture)if(ht.length>0){st&&lt&&e.texStorage2D(n.TEXTURE_2D,Vt,$,ht[0].width,ht[0].height);for(let nt=0,Ct=ht.length;nt<Ct;nt++)Y=ht[nt],st?At&&e.texSubImage2D(n.TEXTURE_2D,nt,0,0,Y.width,Y.height,ct,N,Y.data):e.texImage2D(n.TEXTURE_2D,nt,$,Y.width,Y.height,0,ct,N,Y.data);b.generateMipmaps=!1}else st?(lt&&e.texStorage2D(n.TEXTURE_2D,Vt,$,J.width,J.height),At&&Be(b,J,ct,N)):e.texImage2D(n.TEXTURE_2D,0,$,J.width,J.height,0,ct,N,J.data);else if(b.isCompressedTexture)if(b.isCompressedArrayTexture){st&&lt&&e.texStorage3D(n.TEXTURE_2D_ARRAY,Vt,$,ht[0].width,ht[0].height,J.depth);for(let nt=0,Ct=ht.length;nt<Ct;nt++)if(Y=ht[nt],b.format!==ci)if(ct!==null)if(st){if(At)if(b.layerUpdates.size>0){let kt=th(Y.width,Y.height,b.format,b.type);for(let xt of b.layerUpdates){let te=Y.data.subarray(xt*kt/Y.data.BYTES_PER_ELEMENT,(xt+1)*kt/Y.data.BYTES_PER_ELEMENT);e.compressedTexSubImage3D(n.TEXTURE_2D_ARRAY,nt,0,0,xt,Y.width,Y.height,1,ct,te)}}else e.compressedTexSubImage3D(n.TEXTURE_2D_ARRAY,nt,0,0,0,Y.width,Y.height,J.depth,ct,Y.data)}else e.compressedTexImage3D(n.TEXTURE_2D_ARRAY,nt,$,Y.width,Y.height,J.depth,0,Y.data,0,0);else Pt("WebGLRenderer: Attempt to load unsupported compressed texture format in .uploadTexture()");else st?At&&e.texSubImage3D(n.TEXTURE_2D_ARRAY,nt,0,0,0,Y.width,Y.height,J.depth,ct,N,Y.data):e.texImage3D(n.TEXTURE_2D_ARRAY,nt,$,Y.width,Y.height,J.depth,0,ct,N,Y.data);b.layerUpdates.size>0&&b.clearLayerUpdates()}else{st&&lt&&e.texStorage2D(n.TEXTURE_2D,Vt,$,ht[0].width,ht[0].height);for(let nt=0,Ct=ht.length;nt<Ct;nt++)Y=ht[nt],b.format!==ci?ct!==null?st?At&&e.compressedTexSubImage2D(n.TEXTURE_2D,nt,0,0,Y.width,Y.height,ct,Y.data):e.compressedTexImage2D(n.TEXTURE_2D,nt,$,Y.width,Y.height,0,Y.data):Pt("WebGLRenderer: Attempt to load unsupported compressed texture format in .uploadTexture()"):st?At&&e.texSubImage2D(n.TEXTURE_2D,nt,0,0,Y.width,Y.height,ct,N,Y.data):e.texImage2D(n.TEXTURE_2D,nt,$,Y.width,Y.height,0,ct,N,Y.data)}else if(b.isDataArrayTexture)if(st){if(lt&&e.texStorage3D(n.TEXTURE_2D_ARRAY,Vt,$,J.width,J.height,J.depth),At)if(b.layerUpdates.size>0){let nt=th(J.width,J.height,b.format,b.type);for(let Ct of b.layerUpdates){let kt=J.data.subarray(Ct*nt/J.data.BYTES_PER_ELEMENT,(Ct+1)*nt/J.data.BYTES_PER_ELEMENT);e.texSubImage3D(n.TEXTURE_2D_ARRAY,0,0,0,Ct,J.width,J.height,1,ct,N,kt)}b.clearLayerUpdates()}else e.texSubImage3D(n.TEXTURE_2D_ARRAY,0,0,0,0,J.width,J.height,J.depth,ct,N,J.data)}else e.texImage3D(n.TEXTURE_2D_ARRAY,0,$,J.width,J.height,J.depth,0,ct,N,J.data);else if(b.isData3DTexture)st?(lt&&e.texStorage3D(n.TEXTURE_3D,Vt,$,J.width,J.height,J.depth),At&&e.texSubImage3D(n.TEXTURE_3D,0,0,0,0,J.width,J.height,J.depth,ct,N,J.data)):e.texImage3D(n.TEXTURE_3D,0,$,J.width,J.height,J.depth,0,ct,N,J.data);else if(b.isFramebufferTexture){if(lt)if(st)e.texStorage2D(n.TEXTURE_2D,Vt,$,J.width,J.height);else{let nt=J.width,Ct=J.height;for(let kt=0;kt<Vt;kt++)e.texImage2D(n.TEXTURE_2D,kt,$,nt,Ct,0,ct,N,null),nt>>=1,Ct>>=1}}else if(b.isHTMLTexture){if("texElementImage2D"in n){let nt=n.canvas;if(nt.hasAttribute("layoutsubtree")||nt.setAttribute("layoutsubtree","true"),J.parentNode!==nt){nt.appendChild(J),u.add(b),nt.onpaint=Ct=>{let kt=Ct.changedElements;for(let xt of u)kt.includes(xt.image)&&(xt.needsUpdate=!0)},nt.requestPaint();return}if(n.texElementImage2D.length===3)n.texElementImage2D(n.TEXTURE_2D,n.RGBA8,J);else{let kt=n.RGBA,xt=n.RGBA,te=n.UNSIGNED_BYTE;n.texElementImage2D(n.TEXTURE_2D,0,kt,xt,te,J)}n.texParameteri(n.TEXTURE_2D,n.TEXTURE_MIN_FILTER,n.LINEAR),n.texParameteri(n.TEXTURE_2D,n.TEXTURE_WRAP_S,n.CLAMP_TO_EDGE),n.texParameteri(n.TEXTURE_2D,n.TEXTURE_WRAP_T,n.CLAMP_TO_EDGE)}}else if(ht.length>0){if(st&&lt){let nt=Ft(ht[0]);e.texStorage2D(n.TEXTURE_2D,Vt,$,nt.width,nt.height)}for(let nt=0,Ct=ht.length;nt<Ct;nt++)Y=ht[nt],st?At&&e.texSubImage2D(n.TEXTURE_2D,nt,0,0,ct,N,Y):e.texImage2D(n.TEXTURE_2D,nt,$,ct,N,Y);b.generateMipmaps=!1}else if(st){if(lt){let nt=Ft(J);e.texStorage2D(n.TEXTURE_2D,Vt,$,nt.width,nt.height)}At&&e.texSubImage2D(n.TEXTURE_2D,0,0,0,ct,N,J)}else e.texImage2D(n.TEXTURE_2D,0,$,ct,N,J);S(b)&&b.mipmapsAutoUpdate===!0&&M(it),ft.__version=ot.version,b.onUpdate&&b.onUpdate(b)}R.__version=b.version}function ei(R,b,H){if(b.image.length!==6)return;let it=$t(R,b),tt=b.source;e.bindTexture(n.TEXTURE_CUBE_MAP,R.__webglTexture,n.TEXTURE0+H);let ot=i.get(tt);if(tt.version!==ot.__version||it===!0){e.activeTexture(n.TEXTURE0+H);let ft=Wt.getPrimaries(Wt.workingColorSpace),Z=b.colorSpace===Ui?null:Wt.getPrimaries(b.colorSpace),J=b.colorSpace===Ui||ft===Z?n.NONE:n.BROWSER_DEFAULT_WEBGL;e.pixelStorei(n.UNPACK_FLIP_Y_WEBGL,b.flipY),e.pixelStorei(n.UNPACK_PREMULTIPLY_ALPHA_WEBGL,b.premultiplyAlpha),e.pixelStorei(n.UNPACK_ALIGNMENT,b.unpackAlignment),e.pixelStorei(n.UNPACK_COLORSPACE_CONVERSION_WEBGL,J);let ct=b.isCompressedTexture||b.image[0].isCompressedTexture,N=b.image[0]&&b.image[0].isDataTexture,$=[];for(let xt=0;xt<6;xt++)!ct&&!N?$[xt]=_(b.image[xt],!0,s.maxCubemapSize):$[xt]=N?b.image[xt].image:b.image[xt],$[xt]=It(b,$[xt]);let Y=$[0],ht=r.convert(b.format,b.colorSpace),st=r.convert(b.type),lt=w(b.internalFormat,ht,st,b.normalized,b.colorSpace),At=b.isVideoTexture!==!0,Vt=ot.__version===void 0||it===!0,nt=tt.dataReady,Ct=y(b,Y);Ht(n.TEXTURE_CUBE_MAP,b);let kt;if(b.isDepthTexture){if(lt=x(b.format===rn,b.type),Vt)if(At)e.texStorage2D(n.TEXTURE_CUBE_MAP,1,lt,Y.width,Y.height);else for(let xt=0;xt<6;xt++)e.texImage2D(n.TEXTURE_CUBE_MAP_POSITIVE_X+xt,0,lt,Y.width,Y.height,0,ht,st,null)}else if(ct){At&&Vt&&e.texStorage2D(n.TEXTURE_CUBE_MAP,Ct,lt,Y.width,Y.height);for(let xt=0;xt<6;xt++){kt=$[xt].mipmaps;for(let te=0;te<kt.length;te++){let Le=kt[te];b.format!==ci?ht!==null?At?nt&&e.compressedTexSubImage2D(n.TEXTURE_CUBE_MAP_POSITIVE_X+xt,te,0,0,Le.width,Le.height,ht,Le.data):e.compressedTexImage2D(n.TEXTURE_CUBE_MAP_POSITIVE_X+xt,te,lt,Le.width,Le.height,0,Le.data):Pt("WebGLRenderer: Attempt to load unsupported compressed texture format in .setTextureCube()"):At?nt&&e.texSubImage2D(n.TEXTURE_CUBE_MAP_POSITIVE_X+xt,te,0,0,Le.width,Le.height,ht,st,Le.data):e.texImage2D(n.TEXTURE_CUBE_MAP_POSITIVE_X+xt,te,lt,Le.width,Le.height,0,ht,st,Le.data)}}}else{if(kt=b.mipmaps,At&&Vt){kt.length>0&&Ct++;let xt=Ft($[0]);e.texStorage2D(n.TEXTURE_CUBE_MAP,Ct,lt,xt.width,xt.height)}for(let xt=0;xt<6;xt++)if(N){At?nt&&e.texSubImage2D(n.TEXTURE_CUBE_MAP_POSITIVE_X+xt,0,0,0,$[xt].width,$[xt].height,ht,st,$[xt].data):e.texImage2D(n.TEXTURE_CUBE_MAP_POSITIVE_X+xt,0,lt,$[xt].width,$[xt].height,0,ht,st,$[xt].data);for(let te=0;te<kt.length;te++){let on=kt[te].image[xt].image;At?nt&&e.texSubImage2D(n.TEXTURE_CUBE_MAP_POSITIVE_X+xt,te+1,0,0,on.width,on.height,ht,st,on.data):e.texImage2D(n.TEXTURE_CUBE_MAP_POSITIVE_X+xt,te+1,lt,on.width,on.height,0,ht,st,on.data)}}else{At?nt&&e.texSubImage2D(n.TEXTURE_CUBE_MAP_POSITIVE_X+xt,0,0,0,ht,st,$[xt]):e.texImage2D(n.TEXTURE_CUBE_MAP_POSITIVE_X+xt,0,lt,ht,st,$[xt]);for(let te=0;te<kt.length;te++){let Le=kt[te];At?nt&&e.texSubImage2D(n.TEXTURE_CUBE_MAP_POSITIVE_X+xt,te+1,0,0,ht,st,Le.image[xt]):e.texImage2D(n.TEXTURE_CUBE_MAP_POSITIVE_X+xt,te+1,lt,ht,st,Le.image[xt])}}}S(b)&&b.mipmapsAutoUpdate===!0&&M(n.TEXTURE_CUBE_MAP),ot.__version=tt.version,b.onUpdate&&b.onUpdate(b)}R.__version=b.version}function pe(R,b,H,it,tt,ot){let ft=r.convert(H.format,H.colorSpace),Z=r.convert(H.type),J=w(H.internalFormat,ft,Z,H.normalized,H.colorSpace),ct=i.get(b),N=i.get(H);if(N.__renderTarget=b,!ct.__hasExternalTextures){let $=Math.max(1,b.width>>ot),Y=Math.max(1,b.height>>ot);tt===n.TEXTURE_3D||tt===n.TEXTURE_2D_ARRAY?e.texImage3D(tt,ot,J,$,Y,b.depth,0,ft,Z,null):e.texImage2D(tt,ot,J,$,Y,0,ft,Z,null)}e.bindFramebuffer(n.FRAMEBUFFER,R),mt(b)?o.framebufferTexture2DMultisampleEXT(n.FRAMEBUFFER,it,tt,N.__webglTexture,0,ue(b)):(tt===n.TEXTURE_2D||tt>=n.TEXTURE_CUBE_MAP_POSITIVE_X&&tt<=n.TEXTURE_CUBE_MAP_NEGATIVE_Z)&&n.framebufferTexture2D(n.FRAMEBUFFER,it,tt,N.__webglTexture,ot),e.bindFramebuffer(n.FRAMEBUFFER,null)}function Oi(R,b,H){if(n.bindRenderbuffer(n.RENDERBUFFER,R),b.depthBuffer){let it=b.depthTexture,tt=it&&it.isDepthTexture?it.type:null,ot=x(b.stencilBuffer,tt),ft=b.stencilBuffer?n.DEPTH_STENCIL_ATTACHMENT:n.DEPTH_ATTACHMENT;mt(b)?o.renderbufferStorageMultisampleEXT(n.RENDERBUFFER,ue(b),ot,b.width,b.height):H?n.renderbufferStorageMultisample(n.RENDERBUFFER,ue(b),ot,b.width,b.height):n.renderbufferStorage(n.RENDERBUFFER,ot,b.width,b.height),n.framebufferRenderbuffer(n.FRAMEBUFFER,ft,n.RENDERBUFFER,R)}else{let it=b.textures;for(let tt=0;tt<it.length;tt++){let ot=it[tt],ft=r.convert(ot.format,ot.colorSpace),Z=r.convert(ot.type),J=w(ot.internalFormat,ft,Z,ot.normalized,ot.colorSpace);mt(b)?o.renderbufferStorageMultisampleEXT(n.RENDERBUFFER,ue(b),J,b.width,b.height):H?n.renderbufferStorageMultisample(n.RENDERBUFFER,ue(b),J,b.width,b.height):n.renderbufferStorage(n.RENDERBUFFER,J,b.width,b.height)}}n.bindRenderbuffer(n.RENDERBUFFER,null)}function De(R,b,H){let it=b.isWebGLCubeRenderTarget===!0;if(e.bindFramebuffer(n.FRAMEBUFFER,R),!(b.depthTexture&&b.depthTexture.isDepthTexture))throw new Error("THREE.WebGLTextures: renderTarget.depthTexture must be an instance of THREE.DepthTexture.");let tt=i.get(b.depthTexture);tt.__renderTarget=b;let ot=it?b.depthTexture.image[0]:b.depthTexture.image;(!tt.__webglTexture||ot.width!==b.width||ot.height!==b.height)&&(ot.width=b.width,ot.height=b.height,b.depthTexture.needsUpdate=!0),it?rt(b.depthTexture,0):at(b.depthTexture,0);let ft=tt.__webglTexture,Z=ue(b),J=it?n.TEXTURE_CUBE_MAP_POSITIVE_X+H:n.TEXTURE_2D,ct=b.depthTexture.format===rn?n.DEPTH_STENCIL_ATTACHMENT:n.DEPTH_ATTACHMENT;if(b.depthTexture.format===Ai)mt(b)?o.framebufferTexture2DMultisampleEXT(n.FRAMEBUFFER,ct,J,ft,0,Z):n.framebufferTexture2D(n.FRAMEBUFFER,ct,J,ft,0);else if(b.depthTexture.format===rn)mt(b)?o.framebufferTexture2DMultisampleEXT(n.FRAMEBUFFER,ct,J,ft,0,Z):n.framebufferTexture2D(n.FRAMEBUFFER,ct,J,ft,0);else throw new Error("THREE.WebGLTextures: Unknown depthTexture format.")}function hi(R){let b=i.get(R),H=R.isWebGLCubeRenderTarget===!0;if(b.__boundDepthTexture!==R.depthTexture){let it=R.depthTexture;if(b.__depthDisposeCallback&&b.__depthDisposeCallback(),it){let tt=()=>{delete b.__boundDepthTexture,delete b.__depthDisposeCallback,it.removeEventListener("dispose",tt)};it.addEventListener("dispose",tt),b.__depthDisposeCallback=tt}b.__boundDepthTexture=it}if(R.depthTexture&&!b.__autoAllocateDepthBuffer)if(H)for(let it=0;it<6;it++)De(b.__webglFramebuffer[it],R,it);else R.texture.mipmaps.length>0?De(b.__webglFramebuffer[0],R,0):De(b.__webglFramebuffer,R,0);else if(H){b.__webglDepthbuffer=[];for(let it=0;it<6;it++)if(e.bindFramebuffer(n.FRAMEBUFFER,b.__webglFramebuffer[it]),b.__webglDepthbuffer[it]===void 0)b.__webglDepthbuffer[it]=n.createRenderbuffer(),Oi(b.__webglDepthbuffer[it],R,!1);else{let tt=R.stencilBuffer?n.DEPTH_STENCIL_ATTACHMENT:n.DEPTH_ATTACHMENT,ot=b.__webglDepthbuffer[it];n.bindRenderbuffer(n.RENDERBUFFER,ot),n.framebufferRenderbuffer(n.FRAMEBUFFER,tt,n.RENDERBUFFER,ot)}}else if(R.texture.mipmaps.length>0?e.bindFramebuffer(n.FRAMEBUFFER,b.__webglFramebuffer[0]):e.bindFramebuffer(n.FRAMEBUFFER,b.__webglFramebuffer),b.__webglDepthbuffer===void 0)b.__webglDepthbuffer=n.createRenderbuffer(),Oi(b.__webglDepthbuffer,R,!1);else{let tt=R.stencilBuffer?n.DEPTH_STENCIL_ATTACHMENT:n.DEPTH_ATTACHMENT,ot=b.__webglDepthbuffer;n.bindRenderbuffer(n.RENDERBUFFER,ot),n.framebufferRenderbuffer(n.FRAMEBUFFER,tt,n.RENDERBUFFER,ot)}e.bindFramebuffer(n.FRAMEBUFFER,null)}function ui(R,b,H){let it=i.get(R);b!==void 0&&pe(it.__webglFramebuffer,R,R.texture,n.COLOR_ATTACHMENT0,n.TEXTURE_2D,0),H!==void 0&&hi(R)}function di(R){let b=R.texture,H=i.get(R),it=i.get(b);R.addEventListener("dispose",L),P(R);let tt=R.textures,ot=R.isWebGLCubeRenderTarget===!0,ft=tt.length>1;if(ft||(it.__webglTexture===void 0&&(it.__webglTexture=n.createTexture()),it.__version=b.version,a.memory.textures++),ot){H.__webglFramebuffer=[];for(let Z=0;Z<6;Z++)if(b.mipmaps.length>0){H.__webglFramebuffer[Z]=[];for(let J=0;J<b.mipmaps.length;J++)H.__webglFramebuffer[Z][J]=n.createFramebuffer()}else H.__webglFramebuffer[Z]=n.createFramebuffer()}else{if(b.mipmaps.length>0){H.__webglFramebuffer=[];for(let Z=0;Z<b.mipmaps.length;Z++)H.__webglFramebuffer[Z]=n.createFramebuffer()}else H.__webglFramebuffer=n.createFramebuffer();if(ft)for(let Z=0,J=tt.length;Z<J;Z++){let ct=i.get(tt[Z]);ct.__webglTexture===void 0&&(ct.__webglTexture=n.createTexture(),a.memory.textures++)}if(R.samples>0&&mt(R)===!1){H.__webglMultisampledFramebuffer=n.createFramebuffer(),H.__webglColorRenderbuffer=[],e.bindFramebuffer(n.FRAMEBUFFER,H.__webglMultisampledFramebuffer);for(let Z=0;Z<tt.length;Z++){let J=tt[Z];H.__webglColorRenderbuffer[Z]=n.createRenderbuffer(),n.bindRenderbuffer(n.RENDERBUFFER,H.__webglColorRenderbuffer[Z]);let ct=r.convert(J.format,J.colorSpace),N=r.convert(J.type),$=w(J.internalFormat,ct,N,J.normalized,J.colorSpace,R.isXRRenderTarget===!0),Y=ue(R);n.renderbufferStorageMultisample(n.RENDERBUFFER,Y,$,R.width,R.height),n.framebufferRenderbuffer(n.FRAMEBUFFER,n.COLOR_ATTACHMENT0+Z,n.RENDERBUFFER,H.__webglColorRenderbuffer[Z])}n.bindRenderbuffer(n.RENDERBUFFER,null),R.depthBuffer&&(H.__webglDepthRenderbuffer=n.createRenderbuffer(),Oi(H.__webglDepthRenderbuffer,R,!0)),e.bindFramebuffer(n.FRAMEBUFFER,null)}}if(ot){e.bindTexture(n.TEXTURE_CUBE_MAP,it.__webglTexture),Ht(n.TEXTURE_CUBE_MAP,b);for(let Z=0;Z<6;Z++)if(b.mipmaps.length>0)for(let J=0;J<b.mipmaps.length;J++)pe(H.__webglFramebuffer[Z][J],R,b,n.COLOR_ATTACHMENT0,n.TEXTURE_CUBE_MAP_POSITIVE_X+Z,J);else pe(H.__webglFramebuffer[Z],R,b,n.COLOR_ATTACHMENT0,n.TEXTURE_CUBE_MAP_POSITIVE_X+Z,0);S(b)?M(n.TEXTURE_CUBE_MAP):b.mipmaps.length>0&&n.texParameteri(n.TEXTURE_CUBE_MAP,n.TEXTURE_MAX_LEVEL,b.mipmaps.length-1),e.unbindTexture()}else if(ft){for(let Z=0,J=tt.length;Z<J;Z++){let ct=tt[Z],N=i.get(ct),$=n.TEXTURE_2D;(R.isWebGL3DRenderTarget||R.isWebGLArrayRenderTarget)&&($=R.isWebGL3DRenderTarget?n.TEXTURE_3D:n.TEXTURE_2D_ARRAY),e.bindTexture($,N.__webglTexture),Ht($,ct),pe(H.__webglFramebuffer,R,ct,n.COLOR_ATTACHMENT0+Z,$,0),S(ct)&&M($)}e.unbindTexture()}else{let Z=n.TEXTURE_2D;if((R.isWebGL3DRenderTarget||R.isWebGLArrayRenderTarget)&&(Z=R.isWebGL3DRenderTarget?n.TEXTURE_3D:n.TEXTURE_2D_ARRAY),e.bindTexture(Z,it.__webglTexture),Ht(Z,b),b.mipmaps.length>0)for(let J=0;J<b.mipmaps.length;J++)pe(H.__webglFramebuffer[J],R,b,n.COLOR_ATTACHMENT0,Z,J);else pe(H.__webglFramebuffer,R,b,n.COLOR_ATTACHMENT0,Z,0);S(b)?M(Z):b.mipmaps.length>0&&n.texParameteri(Z,n.TEXTURE_MAX_LEVEL,b.mipmaps.length-1),e.unbindTexture()}R.depthBuffer&&hi(R)}function O(R){let b=R.textures;for(let H=0,it=b.length;H<it;H++){let tt=b[H];if(S(tt)&&tt.mipmapsAutoUpdate===!0){let ot=T(R),ft=i.get(tt).__webglTexture;e.bindTexture(ot,ft),M(ot),e.unbindTexture()}}}let fi=[];function he(R){if(R.samples>0){if(mt(R)===!1){let b=R.textures,H=R.width,it=R.height,tt=n.COLOR_BUFFER_BIT,ot=R.stencilBuffer?n.DEPTH_STENCIL_ATTACHMENT:n.DEPTH_ATTACHMENT,ft=i.get(R),Z=b.length>1;if(Z)for(let ct=0;ct<b.length;ct++)e.bindFramebuffer(n.FRAMEBUFFER,ft.__webglMultisampledFramebuffer),n.framebufferRenderbuffer(n.FRAMEBUFFER,n.COLOR_ATTACHMENT0+ct,n.RENDERBUFFER,null),e.bindFramebuffer(n.FRAMEBUFFER,ft.__webglFramebuffer),n.framebufferTexture2D(n.DRAW_FRAMEBUFFER,n.COLOR_ATTACHMENT0+ct,n.TEXTURE_2D,null,0);e.bindFramebuffer(n.READ_FRAMEBUFFER,ft.__webglMultisampledFramebuffer),R.texture.mipmaps.length>0?e.bindFramebuffer(n.DRAW_FRAMEBUFFER,ft.__webglFramebuffer[0]):e.bindFramebuffer(n.DRAW_FRAMEBUFFER,ft.__webglFramebuffer);for(let ct=0;ct<b.length;ct++){if(R.resolveDepthBuffer&&(R.depthBuffer&&(tt|=n.DEPTH_BUFFER_BIT),R.stencilBuffer&&R.resolveStencilBuffer&&(tt|=n.STENCIL_BUFFER_BIT)),Z){n.framebufferRenderbuffer(n.READ_FRAMEBUFFER,n.COLOR_ATTACHMENT0,n.RENDERBUFFER,ft.__webglColorRenderbuffer[ct]);let N=i.get(b[ct]).__webglTexture;n.framebufferTexture2D(n.DRAW_FRAMEBUFFER,n.COLOR_ATTACHMENT0,n.TEXTURE_2D,N,0)}n.blitFramebuffer(0,0,H,it,0,0,H,it,tt,n.NEAREST),l===!0&&(fi.length=0,fi.push(n.COLOR_ATTACHMENT0+ct),R.depthBuffer&&R.storeMultisampledDepthBuffer===!1&&fi.push(ot),n.invalidateFramebuffer(n.READ_FRAMEBUFFER,fi))}if(e.bindFramebuffer(n.READ_FRAMEBUFFER,null),e.bindFramebuffer(n.DRAW_FRAMEBUFFER,null),Z)for(let ct=0;ct<b.length;ct++){e.bindFramebuffer(n.FRAMEBUFFER,ft.__webglMultisampledFramebuffer),n.framebufferRenderbuffer(n.FRAMEBUFFER,n.COLOR_ATTACHMENT0+ct,n.RENDERBUFFER,ft.__webglColorRenderbuffer[ct]);let N=i.get(b[ct]).__webglTexture;e.bindFramebuffer(n.FRAMEBUFFER,ft.__webglFramebuffer),n.framebufferTexture2D(n.DRAW_FRAMEBUFFER,n.COLOR_ATTACHMENT0+ct,n.TEXTURE_2D,N,0)}e.bindFramebuffer(n.DRAW_FRAMEBUFFER,ft.__webglMultisampledFramebuffer)}else if(R.depthBuffer&&R.storeMultisampledDepthBuffer===!1&&l){let b=R.stencilBuffer?n.DEPTH_STENCIL_ATTACHMENT:n.DEPTH_ATTACHMENT;n.invalidateFramebuffer(n.DRAW_FRAMEBUFFER,[b])}}}function ue(R){return Math.min(s.maxSamples,R.samples)}function mt(R){let b=i.get(R);return R.samples>0&&t.has("WEBGL_multisampled_render_to_texture")===!0&&b.__useRenderToTexture!==!1}function Ee(R){let b=a.render.frame;h.get(R)!==b&&(h.set(R,b),R.update())}function It(R,b){let H=R.colorSpace,it=R.format,tt=R.type;return R.isCompressedTexture===!0||R.isVideoTexture===!0||H!==pn&&H!==Ui&&(Wt.getTransfer(H)===Jt?(it!==ci||tt!==ii)&&Pt("WebGLTextures: sRGB encoded textures have to use RGBAFormat and UnsignedByteType."):Lt("WebGLTextures: Unsupported texture color space:",H)),b}function Ft(R){return typeof HTMLImageElement<"u"&&R instanceof HTMLImageElement?(c.width=R.naturalWidth||R.width,c.height=R.naturalHeight||R.height):typeof VideoFrame<"u"&&R instanceof VideoFrame?(c.width=R.displayWidth,c.height=R.displayHeight):(c.width=R.width,c.height=R.height),c}this.allocateTextureUnit=yt,this.resetTextureUnits=K,this.getTextureUnits=Q,this.setTextureUnits=dt,this.setTexture2D=at,this.setTexture2DArray=gt,this.setTexture3D=Et,this.setTextureCube=rt,this.rebindTextures=ui,this.setupRenderTarget=di,this.updateRenderTargetMipmap=O,this.updateMultisampleRenderTarget=he,this.setupDepthRenderbuffer=hi,this.setupFrameBufferTexture=pe,this.useMultisampledRTT=mt,this.dispose=B}function D_(n,t){function e(i,s=Ui){let r,a=Wt.getTransfer(s);if(i===ii)return n.UNSIGNED_BYTE;if(i===Lo)return n.UNSIGNED_SHORT_4_4_4_4;if(i===No)return n.UNSIGNED_SHORT_5_5_5_1;if(i===kc)return n.UNSIGNED_INT_5_9_9_9_REV;if(i===Hc)return n.UNSIGNED_INT_10F_11F_11F_REV;if(i===Bc)return n.BYTE;if(i===zc)return n.SHORT;if(i===Fs)return n.UNSIGNED_SHORT;if(i===Do)return n.INT;if(i===Qe)return n.UNSIGNED_INT;if(i===yi)return n.FLOAT;if(i===fe)return n.HALF_FLOAT;if(i===Vc)return n.ALPHA;if(i===Gc)return n.RGB;if(i===ci)return n.RGBA;if(i===Ai)return n.DEPTH_COMPONENT;if(i===rn)return n.DEPTH_STENCIL;if(i===Os)return n.RED;if(i===Uo)return n.RED_INTEGER;if(i===Sn)return n.RG;if(i===Fo)return n.RG_INTEGER;if(i===Oo)return n.RGBA_INTEGER;if(i===Yr||i===Zr||i===Kr||i===Jr)if(a===Jt)if(r=t.get("WEBGL_compressed_texture_s3tc_srgb"),r!==null){if(i===Yr)return r.COMPRESSED_SRGB_S3TC_DXT1_EXT;if(i===Zr)return r.COMPRESSED_SRGB_ALPHA_S3TC_DXT1_EXT;if(i===Kr)return r.COMPRESSED_SRGB_ALPHA_S3TC_DXT3_EXT;if(i===Jr)return r.COMPRESSED_SRGB_ALPHA_S3TC_DXT5_EXT}else return null;else if(r=t.get("WEBGL_compressed_texture_s3tc"),r!==null){if(i===Yr)return r.COMPRESSED_RGB_S3TC_DXT1_EXT;if(i===Zr)return r.COMPRESSED_RGBA_S3TC_DXT1_EXT;if(i===Kr)return r.COMPRESSED_RGBA_S3TC_DXT3_EXT;if(i===Jr)return r.COMPRESSED_RGBA_S3TC_DXT5_EXT}else return null;if(i===Bo||i===zo||i===ko||i===Ho)if(r=t.get("WEBGL_compressed_texture_pvrtc"),r!==null){if(i===Bo)return r.COMPRESSED_RGB_PVRTC_4BPPV1_IMG;if(i===zo)return r.COMPRESSED_RGB_PVRTC_2BPPV1_IMG;if(i===ko)return r.COMPRESSED_RGBA_PVRTC_4BPPV1_IMG;if(i===Ho)return r.COMPRESSED_RGBA_PVRTC_2BPPV1_IMG}else return null;if(i===Vo||i===Go||i===Wo||i===Xo||i===qo||i===$r||i===Yo)if(r=t.get("WEBGL_compressed_texture_etc"),r!==null){if(i===Vo||i===Go)return a===Jt?r.COMPRESSED_SRGB8_ETC2:r.COMPRESSED_RGB8_ETC2;if(i===Wo)return a===Jt?r.COMPRESSED_SRGB8_ALPHA8_ETC2_EAC:r.COMPRESSED_RGBA8_ETC2_EAC;if(i===Xo)return r.COMPRESSED_R11_EAC;if(i===qo)return r.COMPRESSED_SIGNED_R11_EAC;if(i===$r)return r.COMPRESSED_RG11_EAC;if(i===Yo)return r.COMPRESSED_SIGNED_RG11_EAC}else return null;if(i===Zo||i===Ko||i===Jo||i===$o||i===jo||i===Qo||i===tl||i===el||i===il||i===nl||i===sl||i===rl||i===al||i===ol)if(r=t.get("WEBGL_compressed_texture_astc"),r!==null){if(i===Zo)return a===Jt?r.COMPRESSED_SRGB8_ALPHA8_ASTC_4x4_KHR:r.COMPRESSED_RGBA_ASTC_4x4_KHR;if(i===Ko)return a===Jt?r.COMPRESSED_SRGB8_ALPHA8_ASTC_5x4_KHR:r.COMPRESSED_RGBA_ASTC_5x4_KHR;if(i===Jo)return a===Jt?r.COMPRESSED_SRGB8_ALPHA8_ASTC_5x5_KHR:r.COMPRESSED_RGBA_ASTC_5x5_KHR;if(i===$o)return a===Jt?r.COMPRESSED_SRGB8_ALPHA8_ASTC_6x5_KHR:r.COMPRESSED_RGBA_ASTC_6x5_KHR;if(i===jo)return a===Jt?r.COMPRESSED_SRGB8_ALPHA8_ASTC_6x6_KHR:r.COMPRESSED_RGBA_ASTC_6x6_KHR;if(i===Qo)return a===Jt?r.COMPRESSED_SRGB8_ALPHA8_ASTC_8x5_KHR:r.COMPRESSED_RGBA_ASTC_8x5_KHR;if(i===tl)return a===Jt?r.COMPRESSED_SRGB8_ALPHA8_ASTC_8x6_KHR:r.COMPRESSED_RGBA_ASTC_8x6_KHR;if(i===el)return a===Jt?r.COMPRESSED_SRGB8_ALPHA8_ASTC_8x8_KHR:r.COMPRESSED_RGBA_ASTC_8x8_KHR;if(i===il)return a===Jt?r.COMPRESSED_SRGB8_ALPHA8_ASTC_10x5_KHR:r.COMPRESSED_RGBA_ASTC_10x5_KHR;if(i===nl)return a===Jt?r.COMPRESSED_SRGB8_ALPHA8_ASTC_10x6_KHR:r.COMPRESSED_RGBA_ASTC_10x6_KHR;if(i===sl)return a===Jt?r.COMPRESSED_SRGB8_ALPHA8_ASTC_10x8_KHR:r.COMPRESSED_RGBA_ASTC_10x8_KHR;if(i===rl)return a===Jt?r.COMPRESSED_SRGB8_ALPHA8_ASTC_10x10_KHR:r.COMPRESSED_RGBA_ASTC_10x10_KHR;if(i===al)return a===Jt?r.COMPRESSED_SRGB8_ALPHA8_ASTC_12x10_KHR:r.COMPRESSED_RGBA_ASTC_12x10_KHR;if(i===ol)return a===Jt?r.COMPRESSED_SRGB8_ALPHA8_ASTC_12x12_KHR:r.COMPRESSED_RGBA_ASTC_12x12_KHR}else return null;if(i===ll||i===cl||i===hl)if(r=t.get("EXT_texture_compression_bptc"),r!==null){if(i===ll)return a===Jt?r.COMPRESSED_SRGB_ALPHA_BPTC_UNORM_EXT:r.COMPRESSED_RGBA_BPTC_UNORM_EXT;if(i===cl)return r.COMPRESSED_RGB_BPTC_SIGNED_FLOAT_EXT;if(i===hl)return r.COMPRESSED_RGB_BPTC_UNSIGNED_FLOAT_EXT}else return null;if(i===ul||i===dl||i===jr||i===fl)if(r=t.get("EXT_texture_compression_rgtc"),r!==null){if(i===ul)return r.COMPRESSED_RED_RGTC1_EXT;if(i===dl)return r.COMPRESSED_SIGNED_RED_RGTC1_EXT;if(i===jr)return r.COMPRESSED_RED_GREEN_RGTC2_EXT;if(i===fl)return r.COMPRESSED_SIGNED_RED_GREEN_RGTC2_EXT}else return null;return i===$n?n.UNSIGNED_INT_24_8:n[i]!==void 0?n[i]:null}return{convert:e}}var L_=`
void main() {

	gl_Position = vec4( position, 1.0 );

}`,N_=`
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

}`,ch=class{constructor(){this.texture=null,this.mesh=null,this.depthNear=0,this.depthFar=0}init(t,e){if(this.texture===null){let i=new Tr(t.texture);(t.depthNear!==e.depthNear||t.depthFar!==e.depthFar)&&(this.depthNear=t.depthNear,this.depthFar=t.depthFar),this.texture=i}}getMesh(t){if(this.texture!==null&&this.mesh===null){let e=t.cameras[0].viewport,i=new oe({vertexShader:L_,fragmentShader:N_,uniforms:{depthColor:{value:this.texture},depthWidth:{value:e.z},depthHeight:{value:e.w}}});this.mesh=new Ut(new Di(20,20),i)}return this.mesh}reset(){this.texture=null,this.mesh=null}getDepthTexture(){return this.texture}},hh=class extends Ri{constructor(t,e){super();let i=this,s=null,r=1,a=null,o="local-floor",l=1,c=null,h=null,u=null,d=null,f=null,p=null,v=typeof XRWebGLBinding<"u",g=new ch,m={},_=e.getContextAttributes(),S=null,M=null,T=[],w=[],x=new et,y=null,P=null,C=new Te;C.viewport=new ie;let I=new Te;I.viewport=new ie;let L=[C,I],F=new Ao,D=null,B=null;this.cameraAutoUpdate=!0,this.enabled=!1,this.isPresenting=!1,this.getController=function(q){let at=T[q];return at===void 0&&(at=new Ss,T[q]=at),at.getTargetRaySpace()},this.getControllerGrip=function(q){let at=T[q];return at===void 0&&(at=new Ss,T[q]=at),at.getGripSpace()},this.getHand=function(q){let at=T[q];return at===void 0&&(at=new Ss,T[q]=at),at.getHandSpace()};function G(q){let at=w.indexOf(q.inputSource);if(at===-1)return;let gt=T[at];gt!==void 0&&(gt.update(q.inputSource,q.frame,c||a),gt.dispatchEvent({type:q.type,data:q.inputSource}))}function k(){s.removeEventListener("select",G),s.removeEventListener("selectstart",G),s.removeEventListener("selectend",G),s.removeEventListener("squeeze",G),s.removeEventListener("squeezestart",G),s.removeEventListener("squeezeend",G),s.removeEventListener("end",k),s.removeEventListener("inputsourceschange",j);for(let q=0;q<T.length;q++){let at=w[q];at!==null&&(w[q]=null,T[q].disconnect(at))}D=null,B=null,g.reset();for(let q in m)delete m[q];if(t.setRenderTarget(S),f=null,d=null,u=null,s=null,M=null,yt.stop(),i.isPresenting=!1,t.setPixelRatio(y),t.setSize(x.width,x.height,!1),P!==null){let q=P.camera;q.fov=P.fov,q.zoom=P.zoom,q.updateProjectionMatrix(),P=null}i.dispatchEvent({type:"sessionend"})}this.setFramebufferScaleFactor=function(q){r=q,i.isPresenting===!0&&Pt("WebXRManager: Cannot change framebuffer scale while presenting.")},this.setReferenceSpaceType=function(q){o=q,i.isPresenting===!0&&Pt("WebXRManager: Cannot change reference space type while presenting.")},this.getReferenceSpace=function(){return c||a},this.setReferenceSpace=function(q){c=q},this.getBaseLayer=function(){return d!==null?d:f},this.getBinding=function(){return u===null&&v&&(u=new XRWebGLBinding(s,e)),u},this.getFrame=function(){return p},this.getSession=function(){return s},this.setSession=async function(q){if(s=q,s!==null){if(S=t.getRenderTarget(),s.addEventListener("select",G),s.addEventListener("selectstart",G),s.addEventListener("selectend",G),s.addEventListener("squeeze",G),s.addEventListener("squeezestart",G),s.addEventListener("squeezeend",G),s.addEventListener("end",k),s.addEventListener("inputsourceschange",j),_.xrCompatible!==!0&&await e.makeXRCompatible(),y=t.getPixelRatio(),t.getSize(x),v&&"createProjectionLayer"in XRWebGLBinding.prototype){let gt=null,Et=null,rt=null;_.depth&&(rt=_.stencil?e.DEPTH24_STENCIL8:e.DEPTH_COMPONENT24,gt=_.stencil?rn:Ai,Et=_.stencil?$n:Qe);let Dt={colorFormat:e.RGBA8,depthFormat:rt,scaleFactor:r};u=this.getBinding(),d=u.createProjectionLayer(Dt),s.updateRenderState({layers:[d]}),t.setPixelRatio(1),t.setSize(d.textureWidth,d.textureHeight,!1),M=new ce(d.textureWidth,d.textureHeight,{format:ci,type:ii,depthTexture:new Ii(d.textureWidth,d.textureHeight,Et,void 0,void 0,void 0,void 0,void 0,void 0,gt),stencilBuffer:_.stencil,colorSpace:t.outputColorSpace,samples:_.antialias?4:0,resolveDepthBuffer:d.ignoreDepthValues===!1,resolveStencilBuffer:d.ignoreDepthValues===!1,storeMultisampledDepthBuffer:d.ignoreDepthValues===!1,storeMultisampledStencilBuffer:d.ignoreDepthValues===!1})}else{let gt={antialias:_.antialias,alpha:!0,depth:_.depth,stencil:_.stencil,framebufferScaleFactor:r};f=new XRWebGLLayer(s,e,gt),s.updateRenderState({baseLayer:f}),t.setPixelRatio(1),t.setSize(f.framebufferWidth,f.framebufferHeight,!1),M=new ce(f.framebufferWidth,f.framebufferHeight,{format:ci,type:ii,colorSpace:t.outputColorSpace,stencilBuffer:_.stencil,resolveDepthBuffer:f.ignoreDepthValues===!1,resolveStencilBuffer:f.ignoreDepthValues===!1,storeMultisampledDepthBuffer:f.ignoreDepthValues===!1,storeMultisampledStencilBuffer:f.ignoreDepthValues===!1})}M.isXRRenderTarget=!0,this.setFoveation(l),c=null,a=await s.requestReferenceSpace(o),yt.setContext(s),yt.start(),i.isPresenting=!0,i.dispatchEvent({type:"sessionstart"})}},this.getEnvironmentBlendMode=function(){if(s!==null)return s.environmentBlendMode},this.getDepthTexture=function(){return g.getDepthTexture()};function j(q){for(let at=0;at<q.removed.length;at++){let gt=q.removed[at],Et=w.indexOf(gt);Et>=0&&(w[Et]=null,T[Et].disconnect(gt))}for(let at=0;at<q.added.length;at++){let gt=q.added[at],Et=w.indexOf(gt);if(Et===-1){for(let Dt=0;Dt<T.length;Dt++)if(Dt>=w.length){w.push(gt),Et=Dt;break}else if(w[Dt]===null){w[Dt]=gt,Et=Dt;break}if(Et===-1)break}let rt=T[Et];rt&&rt.connect(gt)}}function W(q,at){at===null?q.matrixWorld.copy(q.matrix):q.matrixWorld.multiplyMatrices(at.matrixWorld,q.matrix),q.matrixWorldInverse.copy(q.matrixWorld).invert()}this.updateCamera=function(q){if(s===null)return;let at=q.near,gt=q.far;g.texture!==null&&(g.depthNear>0&&(at=g.depthNear),g.depthFar>0&&(gt=g.depthFar)),F.near=I.near=C.near=at,F.far=I.far=C.far=gt,(D!==F.near||B!==F.far)&&(s.updateRenderState({depthNear:F.near,depthFar:F.far}),D=F.near,B=F.far),F.layers.mask=q.layers.mask|6,C.layers.mask=F.layers.mask&-5,I.layers.mask=F.layers.mask&-3;let Et=q.parent,rt=F.cameras;W(F,Et);for(let Dt=0;Dt<rt.length;Dt++)W(rt[Dt],Et);F.projectionMatrix.copy(C.projectionMatrix),F.projectionMatrixInverse.copy(C.projectionMatrixInverse),P===null&&q.isPerspectiveCamera&&(P={camera:q,fov:q.fov,zoom:q.zoom}),K(q,F,Et)};function K(q,at,gt){gt===null?q.matrix.copy(at.matrixWorld):(q.matrix.copy(gt.matrixWorld),q.matrix.invert(),q.matrix.multiply(at.matrixWorld)),q.matrix.decompose(q.position,q.quaternion,q.scale),q.updateMatrixWorld(!0),q.projectionMatrix.copy(at.projectionMatrix),q.projectionMatrixInverse.copy(at.projectionMatrixInverse),q.isPerspectiveCamera&&(q.fov=On*2*Math.atan(1/q.projectionMatrix.elements[5]),q.zoom=1)}this.getCamera=function(){return F},this.getFoveation=function(){if(!(d===null&&f===null))return l},this.setFoveation=function(q){l=q,d!==null&&(d.fixedFoveation=q),f!==null&&f.fixedFoveation!==void 0&&(f.fixedFoveation=q)},this.hasDepthSensing=function(){return g.texture!==null},this.getDepthSensingMesh=function(){return g.getMesh(F)},this.getCameraTexture=function(q){return m[q]};let Q=null;function dt(q,at){if(h=at.getViewerPose(c||a),p=at,h!==null){let gt=h.views;f!==null&&(t.setRenderTargetFramebuffer(M,f.framebuffer),t.setRenderTarget(M));let Et=!1;gt.length!==F.cameras.length&&(F.cameras.length=0,Et=!0);for(let Rt=0;Rt<gt.length;Rt++){let Ht=gt[Rt],$t=null;if(f!==null)$t=f.getViewport(Ht);else{let Be=u.getViewSubImage(d,Ht);$t=Be.viewport,Rt===0&&(t.setRenderTargetTextures(M,Be.colorTexture,Be.depthStencilTexture),t.setRenderTarget(M))}let Xt=L[Rt];Xt===void 0&&(Xt=new Te,Xt.layers.enable(Rt),Xt.viewport=new ie,L[Rt]=Xt),Xt.matrix.fromArray(Ht.transform.matrix),Xt.matrix.decompose(Xt.position,Xt.quaternion,Xt.scale),Xt.projectionMatrix.fromArray(Ht.projectionMatrix),Xt.projectionMatrixInverse.copy(Xt.projectionMatrix).invert(),Xt.viewport.set($t.x,$t.y,$t.width,$t.height),Rt===0&&(F.matrix.copy(Xt.matrix),F.matrix.decompose(F.position,F.quaternion,F.scale)),Et===!0&&F.cameras.push(Xt)}let rt=s.enabledFeatures;if(rt&&rt.includes("depth-sensing")&&s.depthUsage=="gpu-optimized"&&v){u=i.getBinding();let Rt=u.getDepthInformation(gt[0]);Rt&&Rt.isValid&&Rt.texture&&g.init(Rt,s.renderState)}if(rt&&rt.includes("camera-access")&&v){t.state.unbindTexture(),u=i.getBinding();for(let Rt=0;Rt<gt.length;Rt++){let Ht=gt[Rt].camera;if(Ht){let $t=m[Ht];$t||($t=new Tr,m[Ht]=$t);let Xt=u.getCameraImage(Ht);$t.sourceTexture=Xt}}}}for(let gt=0;gt<T.length;gt++){let Et=w[gt],rt=T[gt];Et!==null&&rt!==void 0&&rt.update(Et,at,c||a)}Q&&Q(q,at),at.detectedPlanes&&i.dispatchEvent({type:"planesdetected",data:at}),p=null}let yt=new Xd;yt.setAnimationLoop(dt),this.setAnimationLoop=function(q){Q=q},this.dispose=function(){}}},U_=new Gt,ef=new Nt;ef.set(-1,0,0,0,1,0,0,0,1);function F_(n,t){function e(g,m){g.matrixAutoUpdate===!0&&g.updateMatrix(),m.value.copy(g.matrix)}function i(g,m){m.color.getRGB(g.fogColor.value,$c(n)),m.isFog?(g.fogNear.value=m.near,g.fogFar.value=m.far):m.isFogExp2&&(g.fogDensity.value=m.density)}function s(g,m,_,S,M){m.isNodeMaterial?m.uniformsNeedUpdate=!1:m.isMeshBasicMaterial?r(g,m):m.isMeshLambertMaterial?(r(g,m),m.envMap&&(g.envMapIntensity.value=m.envMapIntensity)):m.isMeshToonMaterial?(r(g,m),u(g,m)):m.isMeshPhongMaterial?(r(g,m),h(g,m),m.envMap&&(g.envMapIntensity.value=m.envMapIntensity)):m.isMeshStandardMaterial?(r(g,m),d(g,m),m.isMeshPhysicalMaterial&&f(g,m,M)):m.isMeshMatcapMaterial?(r(g,m),p(g,m)):m.isMeshDepthMaterial?r(g,m):m.isMeshDistanceMaterial?(r(g,m),v(g,m)):m.isMeshNormalMaterial?r(g,m):m.isLineBasicMaterial?(a(g,m),m.isLineDashedMaterial&&o(g,m)):m.isPointsMaterial?l(g,m,_,S):m.isSpriteMaterial?c(g,m):m.isShadowMaterial?(g.color.value.copy(m.color),g.opacity.value=m.opacity):m.isShaderMaterial&&(m.uniformsNeedUpdate=!1)}function r(g,m){g.opacity.value=m.opacity,m.color&&g.diffuse.value.copy(m.color),m.emissive&&g.emissive.value.copy(m.emissive).multiplyScalar(m.emissiveIntensity),m.map&&(g.map.value=m.map,e(m.map,g.mapTransform)),m.alphaMap&&(g.alphaMap.value=m.alphaMap,e(m.alphaMap,g.alphaMapTransform)),m.bumpMap&&(g.bumpMap.value=m.bumpMap,e(m.bumpMap,g.bumpMapTransform),g.bumpScale.value=m.bumpScale,m.side===ke&&(g.bumpScale.value*=-1)),m.normalMap&&(g.normalMap.value=m.normalMap,e(m.normalMap,g.normalMapTransform),g.normalScale.value.copy(m.normalScale),m.side===ke&&g.normalScale.value.negate()),m.displacementMap&&(g.displacementMap.value=m.displacementMap,e(m.displacementMap,g.displacementMapTransform),g.displacementScale.value=m.displacementScale,g.displacementBias.value=m.displacementBias),m.emissiveMap&&(g.emissiveMap.value=m.emissiveMap,e(m.emissiveMap,g.emissiveMapTransform)),m.specularMap&&(g.specularMap.value=m.specularMap,e(m.specularMap,g.specularMapTransform)),m.alphaTest>0&&(g.alphaTest.value=m.alphaTest);let _=t.get(m),S=_.envMap,M=_.envMapRotation;S&&(g.envMap.value=S,g.envMapRotation.value.setFromMatrix4(U_.makeRotationFromEuler(M)).transpose(),S.isCubeTexture&&S.isRenderTargetTexture===!1&&g.envMapRotation.value.premultiply(ef),g.reflectivity.value=m.reflectivity,g.ior.value=m.ior,g.refractionRatio.value=m.refractionRatio),m.lightMap&&(g.lightMap.value=m.lightMap,g.lightMapIntensity.value=m.lightMapIntensity,e(m.lightMap,g.lightMapTransform)),m.aoMap&&(g.aoMap.value=m.aoMap,g.aoMapIntensity.value=m.aoMapIntensity,e(m.aoMap,g.aoMapTransform))}function a(g,m){g.diffuse.value.copy(m.color),g.opacity.value=m.opacity,m.map&&(g.map.value=m.map,e(m.map,g.mapTransform))}function o(g,m){g.dashSize.value=m.dashSize,g.totalSize.value=m.dashSize+m.gapSize,g.scale.value=m.scale}function l(g,m,_,S){g.diffuse.value.copy(m.color),g.opacity.value=m.opacity,g.size.value=m.size*_,g.scale.value=S*.5,m.map&&(g.map.value=m.map,e(m.map,g.uvTransform)),m.alphaMap&&(g.alphaMap.value=m.alphaMap,e(m.alphaMap,g.alphaMapTransform)),m.alphaTest>0&&(g.alphaTest.value=m.alphaTest)}function c(g,m){g.diffuse.value.copy(m.color),g.opacity.value=m.opacity,g.rotation.value=m.rotation,m.map&&(g.map.value=m.map,e(m.map,g.mapTransform)),m.alphaMap&&(g.alphaMap.value=m.alphaMap,e(m.alphaMap,g.alphaMapTransform)),m.alphaTest>0&&(g.alphaTest.value=m.alphaTest)}function h(g,m){g.specular.value.copy(m.specular),g.shininess.value=Math.max(m.shininess,1e-4)}function u(g,m){m.gradientMap&&(g.gradientMap.value=m.gradientMap)}function d(g,m){g.metalness.value=m.metalness,m.metalnessMap&&(g.metalnessMap.value=m.metalnessMap,e(m.metalnessMap,g.metalnessMapTransform)),g.roughness.value=m.roughness,m.roughnessMap&&(g.roughnessMap.value=m.roughnessMap,e(m.roughnessMap,g.roughnessMapTransform)),m.envMap&&(g.envMapIntensity.value=m.envMapIntensity)}function f(g,m,_){g.ior.value=m.ior,m.diffuseRoughness>0&&(g.diffuseRoughness.value=m.diffuseRoughness,m.diffuseRoughnessMap&&(g.diffuseRoughnessMap.value=m.diffuseRoughnessMap,e(m.diffuseRoughnessMap,g.diffuseRoughnessMapTransform))),m.sheen>0&&(g.sheenColor.value.copy(m.sheenColor).multiplyScalar(m.sheen),g.sheenRoughness.value=m.sheenRoughness,m.sheenColorMap&&(g.sheenColorMap.value=m.sheenColorMap,e(m.sheenColorMap,g.sheenColorMapTransform)),m.sheenRoughnessMap&&(g.sheenRoughnessMap.value=m.sheenRoughnessMap,e(m.sheenRoughnessMap,g.sheenRoughnessMapTransform))),m.clearcoat>0&&(g.clearcoat.value=m.clearcoat,g.clearcoatRoughness.value=m.clearcoatRoughness,m.clearcoatMap&&(g.clearcoatMap.value=m.clearcoatMap,e(m.clearcoatMap,g.clearcoatMapTransform)),m.clearcoatRoughnessMap&&(g.clearcoatRoughnessMap.value=m.clearcoatRoughnessMap,e(m.clearcoatRoughnessMap,g.clearcoatRoughnessMapTransform)),m.clearcoatNormalMap&&(g.clearcoatNormalMap.value=m.clearcoatNormalMap,e(m.clearcoatNormalMap,g.clearcoatNormalMapTransform),g.clearcoatNormalScale.value.copy(m.clearcoatNormalScale),m.side===ke&&g.clearcoatNormalScale.value.negate())),m.dispersion>0&&(g.dispersion.value=m.dispersion),m.retroreflectivity>0&&(g.retroreflectivity.value=m.retroreflectivity),m.iridescence>0&&(g.iridescence.value=m.iridescence,g.iridescenceIOR.value=m.iridescenceIOR,g.iridescenceThicknessMinimum.value=m.iridescenceThicknessMinimum,g.iridescenceThicknessMaximum.value=m.iridescenceThicknessMaximum,m.iridescenceMap&&(g.iridescenceMap.value=m.iridescenceMap,e(m.iridescenceMap,g.iridescenceMapTransform)),m.iridescenceThicknessMap&&(g.iridescenceThicknessMap.value=m.iridescenceThicknessMap,e(m.iridescenceThicknessMap,g.iridescenceThicknessMapTransform))),m.transmission>0&&(g.transmission.value=m.transmission,g.transmissionSamplerMap.value=_.texture,g.transmissionSamplerSize.value.set(_.width,_.height),m.transmissionMap&&(g.transmissionMap.value=m.transmissionMap,e(m.transmissionMap,g.transmissionMapTransform)),g.thickness.value=m.thickness,m.thicknessMap&&(g.thicknessMap.value=m.thicknessMap,e(m.thicknessMap,g.thicknessMapTransform)),g.attenuationDistance.value=m.attenuationDistance,g.attenuationColor.value.copy(m.attenuationColor)),m.anisotropy>0&&(g.anisotropyVector.value.set(m.anisotropy*Math.cos(m.anisotropyRotation),m.anisotropy*Math.sin(m.anisotropyRotation)),m.anisotropyMap&&(g.anisotropyMap.value=m.anisotropyMap,e(m.anisotropyMap,g.anisotropyMapTransform))),g.specularIntensity.value=m.specularIntensity,g.specularColor.value.copy(m.specularColor),m.specularColorMap&&(g.specularColorMap.value=m.specularColorMap,e(m.specularColorMap,g.specularColorMapTransform)),m.specularIntensityMap&&(g.specularIntensityMap.value=m.specularIntensityMap,e(m.specularIntensityMap,g.specularIntensityMapTransform))}function p(g,m){m.matcap&&(g.matcap.value=m.matcap)}function v(g,m){let _=t.get(m).light;g.referencePosition.value.setFromMatrixPosition(_.matrixWorld),g.nearDistance.value=_.shadow.camera.near,g.farDistance.value=_.shadow.camera.far}return{refreshFogUniforms:i,refreshMaterialUniforms:s}}function O_(n,t,e,i){let s={},r={};function a(_,S){let M=S.program;i.uniformBlockBinding(_,M,s[_.id])}function o(_,S){let M=s[_.id];M===void 0&&(p(_),M=l(_),s[_.id]=M,_.addEventListener("dispose",g));let T=S.program;i.updateUBOMapping(_,T);let w=t.render.frame;r[_.id]!==w&&(c(_),r[_.id]=w)}function l(_){let S=n.createBuffer(),M=_.__size,T=_.usage;return n.bindBuffer(n.UNIFORM_BUFFER,S),n.bufferData(n.UNIFORM_BUFFER,M,T),n.bindBuffer(n.UNIFORM_BUFFER,null),S}function c(_){let S=s[_.id],M=_.uniforms,T=_.__cache;n.bindBuffer(n.UNIFORM_BUFFER,S);for(let w=0,x=M.length;w<x;w++){let y=M[w];if(Array.isArray(y))for(let P=0,C=y.length;P<C;P++)h(y[P],w,P,T);else h(y,w,0,T)}n.bindBuffer(n.UNIFORM_BUFFER,null)}function h(_,S,M,T){if(d(_,S,M,T)===!0){let w=_.__offset,x=_.value;if(Array.isArray(x)){let y=0;for(let P=0;P<x.length;P++){let C=x[P],I=v(C,!0);u(C,_.__data,y),y+=I.storage/Float32Array.BYTES_PER_ELEMENT}}else u(x,_.__data,0);n.bufferSubData(n.UNIFORM_BUFFER,w,_.__data)}}function u(_,S,M){typeof _=="number"||typeof _=="boolean"?S[M]=_:_.isMatrix3?(S[M]=_.elements[0],S[M+1]=_.elements[1],S[M+2]=_.elements[2],S[M+3]=0,S[M+4]=_.elements[3],S[M+5]=_.elements[4],S[M+6]=_.elements[5],S[M+7]=0,S[M+8]=_.elements[6],S[M+9]=_.elements[7],S[M+10]=_.elements[8],S[M+11]=0):ArrayBuffer.isView(_)?S.set(_.subarray(0,S.length-M),M):_.toArray(S,M)}function d(_,S,M,T){let w=_.value,x=S+"_"+M;if(Array.isArray(w)){T[x]===void 0&&(T[x]=[]);let y=T[x],P=!1;for(let C=0;C<w.length;C++)f(w[C],y,C)===!0&&(P=!0);return P}return f(w,T,x)}function f(_,S,M){if(S[M]===void 0)return typeof _=="number"||typeof _=="boolean"?S[M]=_:ArrayBuffer.isView(_)?S[M]=_.slice():S[M]=_.clone(),!0;{let T=S[M];if(typeof _=="number"||typeof _=="boolean"){if(T!==_)return S[M]=_,!0}else{if(ArrayBuffer.isView(_))return!0;if(T.equals(_)===!1)return T.copy(_),!0}}return!1}function p(_){let S=_.uniforms,M=0,T=16;for(let x=0,y=S.length;x<y;x++){let P=Array.isArray(S[x])?S[x]:[S[x]];for(let C=0,I=P.length;C<I;C++){let L=P[C],F=Array.isArray(L.value)?L.value:[L.value],D=Array.isArray(S[x])||Array.isArray(L.value),B=M;for(let k=0,j=F.length;k<j;k++){let W=F[k],K=v(W,D),Q=M%T,dt=Q%K.boundary,yt=Q+dt;M+=dt,yt!==0&&T-yt<K.storage&&(M+=T-yt),k===0&&(B=M),M+=K.storage}let G=L.type==="int"?Int32Array:L.type==="uint"?Uint32Array:Float32Array;L.__data=new G((M-B)/Float32Array.BYTES_PER_ELEMENT),L.__offset=B}}let w=M%T;return w>0&&(M+=T-w),_.__size=M,_.__cache={},this}function v(_,S){let M={boundary:0,storage:0};return typeof _=="number"||typeof _=="boolean"?(M.boundary=4,M.storage=4):_.isVector2?(M.boundary=8,M.storage=8):_.isVector3||_.isColor?(M.boundary=16,M.storage=12):_.isVector4?(M.boundary=16,M.storage=16):_.isMatrix3?(M.boundary=16,M.storage=48):_.isMatrix4?(M.boundary=16,M.storage=64):_.isTexture?Pt("WebGLRenderer: Texture samplers can not be part of an uniforms group."):ArrayBuffer.isView(_)?(M.boundary=16,M.storage=_.byteLength):Pt("WebGLRenderer: Unsupported uniform value type.",_),S===!0&&(M.boundary=16,M.storage=Math.ceil(M.storage/16)*16),M}function g(_){let S=_.target;S.removeEventListener("dispose",g),n.deleteBuffer(s[S.id]),delete s[S.id],delete r[S.id]}function m(){for(let _ in s)n.deleteBuffer(s[_]);s={},r={}}return{bind:a,update:o,dispose:m}}var B_=new Uint16Array([12469,15057,12620,14925,13266,14620,13807,14376,14323,13990,14545,13625,14713,13328,14840,12882,14931,12528,14996,12233,15039,11829,15066,11525,15080,11295,15085,10976,15082,10705,15073,10495,13880,14564,13898,14542,13977,14430,14158,14124,14393,13732,14556,13410,14702,12996,14814,12596,14891,12291,14937,11834,14957,11489,14958,11194,14943,10803,14921,10506,14893,10278,14858,9960,14484,14039,14487,14025,14499,13941,14524,13740,14574,13468,14654,13106,14743,12678,14818,12344,14867,11893,14889,11509,14893,11180,14881,10751,14852,10428,14812,10128,14765,9754,14712,9466,14764,13480,14764,13475,14766,13440,14766,13347,14769,13070,14786,12713,14816,12387,14844,11957,14860,11549,14868,11215,14855,10751,14825,10403,14782,10044,14729,9651,14666,9352,14599,9029,14967,12835,14966,12831,14963,12804,14954,12723,14936,12564,14917,12347,14900,11958,14886,11569,14878,11247,14859,10765,14828,10401,14784,10011,14727,9600,14660,9289,14586,8893,14508,8533,15111,12234,15110,12234,15104,12216,15092,12156,15067,12010,15028,11776,14981,11500,14942,11205,14902,10752,14861,10393,14812,9991,14752,9570,14682,9252,14603,8808,14519,8445,14431,8145,15209,11449,15208,11451,15202,11451,15190,11438,15163,11384,15117,11274,15055,10979,14994,10648,14932,10343,14871,9936,14803,9532,14729,9218,14645,8742,14556,8381,14461,8020,14365,7603,15273,10603,15272,10607,15267,10619,15256,10631,15231,10614,15182,10535,15118,10389,15042,10167,14963,9787,14883,9447,14800,9115,14710,8665,14615,8318,14514,7911,14411,7507,14279,7198,15314,9675,15313,9683,15309,9712,15298,9759,15277,9797,15229,9773,15166,9668,15084,9487,14995,9274,14898,8910,14800,8539,14697,8234,14590,7790,14479,7409,14367,7067,14178,6621,15337,8619,15337,8631,15333,8677,15325,8769,15305,8871,15264,8940,15202,8909,15119,8775,15022,8565,14916,8328,14804,8009,14688,7614,14569,7287,14448,6888,14321,6483,14088,6171,15350,7402,15350,7419,15347,7480,15340,7613,15322,7804,15287,7973,15229,8057,15148,8012,15046,7846,14933,7611,14810,7357,14682,7069,14552,6656,14421,6316,14251,5948,14007,5528,15356,5942,15356,5977,15353,6119,15348,6294,15332,6551,15302,6824,15249,7044,15171,7122,15070,7050,14949,6861,14818,6611,14679,6349,14538,6067,14398,5651,14189,5311,13935,4958,15359,4123,15359,4153,15356,4296,15353,4646,15338,5160,15311,5508,15263,5829,15188,6042,15088,6094,14966,6001,14826,5796,14678,5543,14527,5287,14377,4985,14133,4586,13869,4257,15360,1563,15360,1642,15358,2076,15354,2636,15341,3350,15317,4019,15273,4429,15203,4732,15105,4911,14981,4932,14836,4818,14679,4621,14517,4386,14359,4156,14083,3795,13808,3437,15360,122,15360,137,15358,285,15355,636,15344,1274,15322,2177,15281,2765,15215,3223,15120,3451,14995,3569,14846,3567,14681,3466,14511,3305,14344,3121,14037,2800,13753,2467,15360,0,15360,1,15359,21,15355,89,15346,253,15325,479,15287,796,15225,1148,15133,1492,15008,1749,14856,1882,14685,1886,14506,1783,14324,1608,13996,1398,13702,1183]),Vi=null;function z_(){return Vi===null&&(Vi=new zn(B_,16,16,Sn,fe),Vi.name="DFG_LUT",Vi.minFilter=ae,Vi.magFilter=ae,Vi.wrapS=ki,Vi.wrapT=ki,Vi.generateMipmaps=!1,Vi.needsUpdate=!0),Vi}var gl=class{constructor(t={}){let{canvas:e=md(),context:i=null,depth:s=!0,stencil:r=!1,alpha:a=!1,antialias:o=!1,premultipliedAlpha:l=!0,preserveDrawingBuffer:c=!1,powerPreference:h="default",failIfMajorPerformanceCaveat:u=!1,reversedDepthBuffer:d=!1,outputBufferType:f=ii}=t;this.isWebGLRenderer=!0;let p;if(i!==null){if(typeof WebGLRenderingContext<"u"&&i instanceof WebGLRenderingContext)throw new Error("THREE.WebGLRenderer: WebGL 1 is not supported since r163.");p=i.getContextAttributes().alpha}else p=a;let v=f,g=new Set([Oo,Fo,Uo]),m=new Set([ii,Qe,Fs,$n,Lo,No]),_=new Uint32Array(4),S=new Int32Array(4),M={textures:[],min:[],max:[],resolution:[]},T=null,w=null,x=[],y=[],P=null;this.domElement=e,this.debug={checkShaderErrors:!0,diagnostics:{keywords:!1},onShaderError:null},this.autoClear=!0,this.autoClearColor=!0,this.autoClearDepth=!0,this.autoClearStencil=!0,this.sortObjects=!0,this.clippingPlanes=[],this.localClippingEnabled=!1,this.toneMapping=Hi,this.toneMappingExposure=1,this.transmissionResolutionScale=1;let C=this,I=!1,L=null,F=null,D=null,B=null;this._outputColorSpace=Fe;let G=0,k=0,j=null,W=-1,K=null,Q=new ie,dt=new ie,yt=null,q=new Mt(0),at=0,gt=e.width,Et=e.height,rt=1,Dt=null,se=null,Rt=new ie(0,0,gt,Et),Ht=new ie(0,0,gt,Et),$t=!1,Xt=new mn,Be=new ro,we=!1,ei=!1,pe=new Gt,Oi=new A,De=new ie,hi={background:null,fog:null,environment:null,overrideMaterial:null,isScene:!0},ui=!1;function di(){return j===null?rt:1}let O=i;function fi(E,U){return e.getContext(E,U)}let he,ue,mt,Ee,It,Ft,R,b,H,it,tt,ot,ft,Z,J,ct,N,$,Y,ht,st,lt,At;try{let E={alpha:!0,depth:s,stencil:r,antialias:o,premultipliedAlpha:l,preserveDrawingBuffer:c,powerPreference:h,failIfMajorPerformanceCaveat:u};if("setAttribute"in e&&e.setAttribute("data-engine",`three.js r${"187"}`),e.addEventListener("webglcontextlost",Ct,!1),e.addEventListener("webglcontextrestored",kt,!1),e.addEventListener("webglcontextcreationerror",xt,!1),O===null){let U="webgl2";if(O=fi(U,E),O===null)throw fi(U)?new Error("THREE.WebGLRenderer: Error creating WebGL context with your selected attributes."):new Error("THREE.WebGLRenderer: Error creating WebGL context.")}Vt()}catch(E){throw e.removeEventListener("webglcontextlost",Ct,!1),e.removeEventListener("webglcontextrestored",kt,!1),e.removeEventListener("webglcontextcreationerror",xt,!1),Lt("WebGLRenderer: "+E.message),E}function Vt(){he=new J0(O),he.init(),st=new D_(O,he),ue=new z0(O,he,t,st),mt=new P_(O,he),ue.reversedDepthBuffer&&d&&mt.buffers.depth.setReversed(!0),F=O.createFramebuffer(),D=O.createFramebuffer(),B=O.createFramebuffer(),Ee=new Q0(O),It=new g_,Ft=new I_(O,he,mt,It,ue,st,Ee),R=new K0(C),b=new em(O),lt=new O0(O,b),H=new $0(O,b,Ee,lt),it=new ex(O,H,b,lt,Ee),$=new tx(O,ue,Ft),J=new k0(It),tt=new m_(C,R,he,ue,lt,J),ot=new F_(C,It),ft=new __,Z=new w_(he),N=new F0(C,R,mt,it,p,l),ct=new C_(C,it,ue),At=new O_(O,Ee,ue,mt),Y=new B0(O,he,Ee),ht=new j0(O,he,Ee),Ee.programs=tt.programs,C.capabilities=ue,C.extensions=he,C.properties=It,C.renderLists=ft,C.shadowMap=ct,C.state=mt,C.info=Ee}v!==ii&&(P=new nx(v,e.width,e.height,o,s,r));let nt=new hh(C,O);this.xr=nt,this.getContext=function(){return O},this.getContextAttributes=function(){return O.getContextAttributes()},this.forceContextLoss=function(){let E=he.get("WEBGL_lose_context");E&&E.loseContext()},this.forceContextRestore=function(){let E=he.get("WEBGL_lose_context");E&&E.restoreContext()},this.getPixelRatio=function(){return rt},this.setPixelRatio=function(E){E!==void 0&&(rt=E,this.setSize(gt,Et,!1))},this.getSize=function(E){return E.set(gt,Et)},this.setSize=function(E,U,X=!0){if(nt.isPresenting){Pt("WebGLRenderer: Can't change size while VR device is presenting.");return}gt=E,Et=U,e.width=Math.floor(E*rt),e.height=Math.floor(U*rt),X===!0&&(e.style.width=E+"px",e.style.height=U+"px"),P!==null&&P.setSize(e.width,e.height),this.setViewport(0,0,E,U)},this.getDrawingBufferSize=function(E){return E.set(gt*rt,Et*rt).floor()},this.setDrawingBufferSize=function(E,U,X){gt=E,Et=U,rt=X,e.width=Math.floor(E*X),e.height=Math.floor(U*X),this.setViewport(0,0,E,U)},this.setEffects=function(E){if(v===ii){Lt("WebGLRenderer: setEffects() requires outputBufferType set to HalfFloatType or FloatType.");return}if(E){for(let U=0;U<E.length;U++)if(E[U].isOutputPass===!0){Pt("WebGLRenderer: OutputPass is not needed in setEffects(). Tone mapping and color space conversion are applied automatically.");break}}P.setEffects(E||[])},this.getCurrentViewport=function(E){return E.copy(Q)},this.getViewport=function(E){return E.copy(Rt)},this.setViewport=function(E,U,X,z){E.isVector4?Rt.set(E.x,E.y,E.z,E.w):Rt.set(E,U,X,z),mt.viewport(Q.copy(Rt).multiplyScalar(di()).round())},this.getScissor=function(E){return E.copy(Ht)},this.setScissor=function(E,U,X,z){E.isVector4?Ht.set(E.x,E.y,E.z,E.w):Ht.set(E,U,X,z),mt.scissor(dt.copy(Ht).multiplyScalar(di()).round())},this.getScissorTest=function(){return $t},this.setScissorTest=function(E){mt.setScissorTest($t=E)},this.setOpaqueSort=function(E){Dt=E},this.setTransparentSort=function(E){se=E},this.getClearColor=function(E){return E.copy(N.getClearColor())},this.setClearColor=function(){N.setClearColor(...arguments)},this.getClearAlpha=function(){return N.getClearAlpha()},this.setClearAlpha=function(){N.setClearAlpha(...arguments)},this.clear=function(E=!0,U=!0,X=!0){let z=0;if(E){let V=!1;if(j!==null){let pt=j.texture.format;V=g.has(pt)}if(V){let pt=j.texture.type,St=m.has(pt),ut=N.getClearColor(),vt=N.getClearAlpha(),Tt=ut.r,Ot=ut.g,qt=ut.b;St?(_[0]=Tt,_[1]=Ot,_[2]=qt,_[3]=vt,O.clearBufferuiv(O.COLOR,0,_)):(S[0]=Tt,S[1]=Ot,S[2]=qt,S[3]=vt,O.clearBufferiv(O.COLOR,0,S))}else z|=O.COLOR_BUFFER_BIT}U&&(z|=O.DEPTH_BUFFER_BIT,this.state.buffers.depth.setMask(!0)),X&&(z|=O.STENCIL_BUFFER_BIT,this.state.buffers.stencil.setMask(4294967295)),z!==0&&O.clear(z)},this.clearColor=function(){this.clear(!0,!1,!1)},this.clearDepth=function(){this.clear(!1,!0,!1)},this.clearStencil=function(){this.clear(!1,!1,!0)},this.setNodesHandler=function(E){E.setRenderer(this),L=E},this.dispose=function(){e.removeEventListener("webglcontextlost",Ct,!1),e.removeEventListener("webglcontextrestored",kt,!1),e.removeEventListener("webglcontextcreationerror",xt,!1),N.dispose(),ft.dispose(),Z.dispose(),Ft.dispose(),It.dispose(),R.dispose(),it.dispose(),H.dispose(),lt.dispose(),At.dispose(),tt.dispose(),nt.dispose(),nt.removeEventListener("sessionstart",Fh),nt.removeEventListener("sessionend",Oh),Rn.stop()};function Ct(E){E.preventDefault(),qc("WebGLRenderer: Context Lost."),I=!0}function kt(){qc("WebGLRenderer: Context Restored."),I=!1;let E=Ee.autoReset,U=ct.enabled,X=ct.autoUpdate,z=ct.needsUpdate,V=ct.type;Vt(),Ee.autoReset=E,ct.enabled=U,ct.autoUpdate=X,ct.needsUpdate=z,ct.type=V}function xt(E){Lt("WebGLRenderer: A WebGL context could not be created. Reason: ",E.statusMessage)}function te(E){let U=E.target;U.removeEventListener("dispose",te),Le(U)}function Le(E){on(E),It.remove(E)}function on(E){let U=It.get(E).programs;U!==void 0&&(U.forEach(function(X){tt.releaseProgram(X)}),E.isShaderMaterial&&tt.releaseShaderCache(E))}this.renderBufferDirect=function(E,U,X,z,V,pt){U===null&&(U=hi);let St=V.isMesh&&V.matrixWorld.determinantAffine()<0,ut=Xf(E,U,X,z,V);mt.setMaterial(z,St);let vt=X.index,Tt=1;if(z.wireframe===!0){if(vt=H.getWireframeAttribute(X),vt===void 0)return;Tt=2}let Ot=X.drawRange,qt=X.attributes.position,bt=Ot.start*Tt,Yt=(Ot.start+Ot.count)*Tt;pt!==null&&(bt=Math.max(bt,pt.start*Tt),Yt=Math.min(Yt,(pt.start+pt.count)*Tt)),vt!==null?(bt=Math.max(bt,0),Yt=Math.min(Yt,vt.count)):qt!=null&&(bt=Math.max(bt,0),Yt=Math.min(Yt,qt.count));let ye=Yt-bt;if(ye<0||ye===1/0)return;lt.setup(V,z,ut,X,vt);let re,ne=Y;if(vt!==null&&(re=b.get(vt),ne=ht,ne.setIndex(re)),V.isMesh)z.wireframe===!0?(mt.setLineWidth(z.wireframeLinewidth*di()),ne.setMode(O.LINES)):ne.setMode(O.TRIANGLES);else if(V.isLine){let Ve=z.linewidth;Ve===void 0&&(Ve=1),mt.setLineWidth(Ve*di()),V.isLineSegments?ne.setMode(O.LINES):V.isLineLoop?ne.setMode(O.LINE_LOOP):ne.setMode(O.LINE_STRIP)}else V.isPoints?ne.setMode(O.POINTS):V.isSprite&&ne.setMode(O.TRIANGLES);if(V.isBatchedMesh)if(he.get("WEBGL_multi_draw"))ne.renderMultiDraw(V._multiDrawStarts,V._multiDrawCounts,V._multiDrawCount);else{let Ve=V._multiDrawStarts,wt=V._multiDrawCounts,Ze=V._multiDrawCount,jt=vt?b.get(vt).bytesPerElement:1,Cn=It.get(z).currentProgram.getUniforms();for(let Ge=0;Ge<Ze;Ge++)Cn.setValue(O,"_gl_DrawID",Ge),ne.render(Ve[Ge]/jt,wt[Ge])}else if(V.isInstancedMesh)ne.renderInstances(bt,ye,V.count);else if(X.isInstancedBufferGeometry){let Ve=X._maxInstanceCount!==void 0?X._maxInstanceCount:1/0,wt=Math.min(X.instanceCount,Ve);ne.renderInstances(bt,ye,wt)}else ne.render(bt,ye)};function Uh(E,U,X,z){L!==null&&E.isNodeMaterial&&L.setObject(z,E),we===!0&&J.setState(E,X,!1),E.transparent===!0&&E.side===je&&E.forceSinglePass===!1?(E.side=ke,E.needsUpdate=!0,ir(E,U,z),E.side=Mn,E.needsUpdate=!0,ir(E,U,z),E.side=je):ir(E,U,z)}this.compile=function(E,U,X=null){X===null&&(X=E),L!==null&&L.renderStart(E,U,X),w=Z.get(X),w.init(U),y.push(w),X.traverseVisible(function(V){V.isLight&&V.layers.test(U.layers)&&(w.pushLight(V),V.castShadow&&w.pushShadow(V))}),E!==X&&E.traverseVisible(function(V){V.isLight&&V.layers.test(U.layers)&&(w.pushLight(V),V.castShadow&&w.pushShadow(V))}),w.setupLights(),L!==null&&L.updateLights(w.state.lightsArray),ei=this.localClippingEnabled,we=J.init(this.clippingPlanes,ei),we===!0&&J.setGlobalState(this.clippingPlanes,U),L!==null&&ct.render(w.state.shadowsArray,X,U);let z=new Set;return E.traverse(function(V){if(!(V.isMesh||V.isPoints||V.isLine||V.isSprite))return;let pt=V.material;if(pt)if(Array.isArray(pt))for(let St=0;St<pt.length;St++){let ut=pt[St];Uh(ut,X,U,V),z.add(ut)}else Uh(pt,X,U,V),z.add(pt)}),w=y.pop(),L!==null&&L.renderEnd(),z},this.compileAsync=function(E,U,X=null){let z=this.compile(E,U,X);return new Promise(V=>{function pt(){if(z.forEach(function(St){let vt=It.get(St).currentProgram;(vt===void 0||vt.isReady())&&z.delete(St)}),z.size===0){V(E);return}setTimeout(pt,10)}he.get("KHR_parallel_shader_compile")!==null?pt():setTimeout(pt,10)})};let Yl=null;function Gf(E){Yl&&Yl(E)}function Fh(){Rn.stop()}function Oh(){Rn.start()}let Rn=new Xd;Rn.setAnimationLoop(Gf),typeof self<"u"&&Rn.setContext(self),this.setAnimationLoop=function(E){Yl=E,nt.setAnimationLoop(E),E===null?Rn.stop():Rn.start()},nt.addEventListener("sessionstart",Fh),nt.addEventListener("sessionend",Oh),this.render=function(E,U){if(U!==void 0&&U.isCamera!==!0){Lt("WebGLRenderer.render: camera is not an instance of THREE.Camera.");return}if(I===!0)return;L!==null&&L.renderStart(E,U);let X=nt.enabled===!0&&nt.isPresenting===!0,z=P!==null&&(j===null||X)&&P.begin(C,j);if(E.matrixWorldAutoUpdate===!0&&E.updateMatrixWorld(),U.parent===null&&U.matrixWorldAutoUpdate===!0&&U.updateMatrixWorld(),nt.enabled===!0&&nt.isPresenting===!0&&(P===null||P.isCompositing()===!1)&&(nt.cameraAutoUpdate===!0&&nt.updateCamera(U),U=nt.getCamera()),E.isScene===!0&&E.onBeforeRender(C,E,U,j),w=Z.get(E,y.length),w.init(U),w.state.textureUnits=Ft.getTextureUnits(),y.push(w),pe.multiplyMatrices(U.projectionMatrix,U.matrixWorldInverse),U.isArrayCamera?Be.setFromArrayCamera(U):Xt.setFromProjectionMatrix(pe,mi,U.reversedDepth),ei=this.localClippingEnabled,we=J.init(this.clippingPlanes,ei),T=ft.get(E,x.length),T.init(),x.push(T),this.info.render.frame++,nt.enabled===!0&&nt.isPresenting===!0){let St=C.xr.getDepthSensingMesh();St!==null&&Zl(St,U,-1/0,C.sortObjects)}Zl(E,U,0,C.sortObjects),T.finish(),L!==null&&L.updateLights(w.state.lightsArray),C.sortObjects===!0&&T.sort(Dt,se),ui=nt.enabled===!1||nt.isPresenting===!1||nt.hasDepthSensing()===!1,ui&&N.addToRenderList(T,E),this.info.autoReset===!0&&this.info.reset(),we===!0&&J.beginShadows();let V=w.state.shadowsArray;if(ct.render(V,E,U),we===!0&&J.endShadows(),(z&&P.hasRenderPass())===!1){let St=T.opaque,ut=T.transmissive;if(w.setupLights(),U.isArrayCamera){let vt=U.cameras;if(ut.length>0)for(let Tt=0,Ot=vt.length;Tt<Ot;Tt++){let qt=vt[Tt];zh(St,ut,E,qt)}ui&&N.render(E);for(let Tt=0,Ot=vt.length;Tt<Ot;Tt++){let qt=vt[Tt];Bh(T,E,qt,qt.viewport)}}else ut.length>0&&zh(St,ut,E,U),ui&&N.render(E),Bh(T,E,U)}j!==null&&k===0&&(Ft.updateMultisampleRenderTarget(j),Ft.updateRenderTargetMipmap(j)),z&&P.end(C),E.isScene===!0&&E.onAfterRender(C,E,U),lt.resetDefaultState(),W=-1,K=null,y.pop(),y.length>0?(w=y[y.length-1],Ft.setTextureUnits(w.state.textureUnits),we===!0&&J.setGlobalState(C.clippingPlanes,w.state.camera)):w=null,x.pop(),x.length>0?T=x[x.length-1]:T=null,L!==null&&L.renderEnd()};function Zl(E,U,X,z){if(E.visible===!1)return;if(E.layers.test(U.layers)){if(E.isGroup)X=E.renderOrder;else if(E.isLOD)E.autoUpdate===!0&&E.update(U);else if(E.isLight)w.pushLight(E),E.castShadow&&w.pushShadow(E);else if(E.isSprite){let St=U.isArrayCamera?Be:Xt;if(!E.frustumCulled||E.intersectsFrustum(St)){z&&De.setFromMatrixPosition(E.matrixWorld).applyMatrix4(pe);let ut=it.update(E),vt=E.material;vt.visible&&T.push(E,ut,vt,X,De.z,null,U)}}else if(E.isMesh||E.isLine||E.isPoints){let St=U.isArrayCamera?Be:Xt;if(!E.frustumCulled||E.intersectsFrustum(St)){let ut=it.update(E),vt=E.material;if(z&&(E.boundingSphere!==void 0?(E.boundingSphere===null&&E.computeBoundingSphere(),De.copy(E.boundingSphere.center)):(ut.boundingSphere===null&&ut.computeBoundingSphere(),De.copy(ut.boundingSphere.center)),De.applyMatrix4(E.matrixWorld).applyMatrix4(pe)),Array.isArray(vt)){let Tt=ut.groups;for(let Ot=0,qt=Tt.length;Ot<qt;Ot++){let bt=Tt[Ot],Yt=vt[bt.materialIndex];Yt&&Yt.visible&&T.push(E,ut,Yt,X,De.z,bt,U)}}else vt.visible&&T.push(E,ut,vt,X,De.z,null,U)}}}let pt=E.children;for(let St=0,ut=pt.length;St<ut;St++)Zl(pt[St],U,X,z)}function Bh(E,U,X,z){let{opaque:V,transmissive:pt,transparent:St}=E;w.setupLightsView(X),we===!0&&J.setGlobalState(C.clippingPlanes,X),z&&mt.viewport(Q.copy(z)),V.length>0&&pa(V,U,X),pt.length>0&&pa(pt,U,X),St.length>0&&pa(St,U,X),mt.buffers.depth.setTest(!0),mt.buffers.depth.setMask(!0),mt.buffers.color.setMask(!0),mt.setPolygonOffset(!1)}function zh(E,U,X,z){if((X.isScene===!0?X.overrideMaterial:null)!==null)return;if(w.state.transmissionRenderTarget[z.id]===void 0){let bt=he.has("EXT_color_buffer_half_float")||he.has("EXT_color_buffer_float");w.state.transmissionRenderTarget[z.id]=new ce(1,1,{generateMipmaps:!0,type:bt?fe:ii,minFilter:sn,samples:Math.max(4,ue.samples),stencilBuffer:r,resolveDepthBuffer:!1,resolveStencilBuffer:!1,storeMultisampledDepthBuffer:!1,storeMultisampledStencilBuffer:!1,colorSpace:Wt.workingColorSpace})}let pt=w.state.transmissionRenderTarget[z.id],St=z.viewport||Q;pt.setSize(St.z*C.transmissionResolutionScale,St.w*C.transmissionResolutionScale);let ut=C.getRenderTarget(),vt=C.getActiveCubeFace(),Tt=C.getActiveMipmapLevel();C.setRenderTarget(pt),C.getClearColor(q),at=C.getClearAlpha(),at<1&&C.setClearColor(16777215,.5),C.clear(),ui&&N.render(X);let Ot=C.toneMapping;C.toneMapping=Hi;let qt=z.viewport;if(z.viewport!==void 0&&(z.viewport=void 0),w.setupLightsView(z),we===!0&&J.setGlobalState(C.clippingPlanes,z),pa(E,X,z),Ft.updateMultisampleRenderTarget(pt),Ft.updateRenderTargetMipmap(pt),he.has("WEBGL_multisampled_render_to_texture")===!1){let bt=!1;for(let Yt=0,ye=U.length;Yt<ye;Yt++){let re=U[Yt],{object:ne,geometry:Ve,material:wt,group:Ze}=re;if(wt.side===je&&ne.layers.test(z.layers)){let jt=wt.side;wt.side=ke,wt.needsUpdate=!0,kh(ne,X,z,Ve,wt,Ze),wt.side=jt,wt.needsUpdate=!0,bt=!0}}bt===!0&&(Ft.updateMultisampleRenderTarget(pt),Ft.updateRenderTargetMipmap(pt))}C.setRenderTarget(ut,vt,Tt),C.setClearColor(q,at),qt!==void 0&&(z.viewport=qt),C.toneMapping=Ot}function pa(E,U,X){let z=U.isScene===!0?U.overrideMaterial:null;for(let V=0,pt=E.length;V<pt;V++){let St=E[V],{object:ut,geometry:vt,group:Tt}=St,Ot=St.material;Ot.allowOverride===!0&&z!==null&&(Ot=z),ut.layers.test(X.layers)&&kh(ut,U,X,vt,Ot,Tt)}}function kh(E,U,X,z,V,pt){L!==null&&V.isNodeMaterial&&L.setObject(E,V),E.onBeforeRender(C,U,X,z,V,pt),E.modelViewMatrix.multiplyMatrices(X.matrixWorldInverse,E.matrixWorld),E.normalMatrix.getNormalMatrix(E.modelViewMatrix),V.onBeforeRender(C,U,X,z,E,pt),V.transparent===!0&&V.side===je&&V.forceSinglePass===!1?(V.side=ke,V.needsUpdate=!0,C.renderBufferDirect(X,U,z,V,E,pt),V.side=Mn,V.needsUpdate=!0,C.renderBufferDirect(X,U,z,V,E,pt),V.side=je):C.renderBufferDirect(X,U,z,V,E,pt),E.onAfterRender(C,U,X,z,V,pt)}function Hh(E){let U=0;for(let X of E)X.texture!==null&&U++;return U}function ir(E,U,X){U.isScene!==!0&&(U=hi);let z=It.get(E),V=w.state.lights,pt=w.state.shadowsArray,St=V.state.version,ut=tt.getParameters(E,V.state,pt,U,X,w.state.lightProbeGridArray),vt=tt.getProgramCacheKey(ut),Tt=z.programs;z.environment=E.isMeshStandardMaterial||E.isMeshLambertMaterial||E.isMeshPhongMaterial?U.environment:null,z.fog=U.fog;let Ot=E.isMeshStandardMaterial||E.isMeshLambertMaterial&&!E.envMap||E.isMeshPhongMaterial&&!E.envMap;z.envMap=R.get(E.envMap||z.environment,Ot),z.envMapRotation=z.environment!==null&&E.envMap===null?U.environmentRotation:E.envMapRotation,Tt===void 0&&(E.addEventListener("dispose",te),Tt=new Map,z.programs=Tt,z.programVariants=new Map);let qt=Tt.get(vt);if(qt!==void 0){if(z.currentProgram===qt&&z.lightsStateVersion===St)return Gh(E,ut),qt}else ut.uniforms=tt.getUniforms(E),L!==null&&E.isNodeMaterial&&L.build(E,X,ut),E.onBeforeCompile(ut,C),qt=tt.acquireProgram(ut,vt),Tt.set(vt,qt),z.uniforms=ut.uniforms;let bt=z.uniforms;return(!E.isShaderMaterial&&!E.isRawShaderMaterial||E.clipping===!0)&&(bt.clippingPlanes=J.uniform),Gh(E,ut),z.needsLights=Yf(E),z.lightsStateVersion=St,z.needsLights&&(bt.ambientLightColor.value=V.state.ambient,bt.lightProbe.value=V.state.probe,bt.sunLights.value=V.state.sun,bt.sunLightShadows.value=V.state.sunShadow,bt.directionalLights.value=V.state.directional,bt.directionalLightShadows.value=V.state.directionalShadow,bt.spotLights.value=V.state.spot,bt.spotLightShadows.value=V.state.spotShadow,bt.rectAreaLights.value=V.state.rectArea,bt.ltc_1.value=V.state.rectAreaLTC1,bt.ltc_2.value=V.state.rectAreaLTC2,bt.pointLights.value=V.state.point,bt.pointLightShadows.value=V.state.pointShadow,bt.hemisphereLights.value=V.state.hemi,bt.sunShadowMatrix.value=V.state.sunShadowMatrix,bt.sunShadowCascade.value=V.state.sunShadowCascade,bt.directionalShadowMatrix.value=V.state.directionalShadowMatrix,bt.spotLightMatrix.value=V.state.spotLightMatrix,bt.spotLightMap.value=V.state.spotLightMap,bt.pointShadowMatrix.value=V.state.pointShadowMatrix),z.numLightProbeGrids=Hh(w.state.lightProbeGridArray),z.currentProgram=qt,z.uniformsList=null,qt}function Wf(E,U,X,z,V){let pt=It.get(E),St=pt.currentProgram.id+","+z+","+V,ut=pt.programVariants.get(St);return ut===void 0?(ut=ir(E,U,X),pt.programVariants.set(St,ut)):(pt.currentProgram=ut,pt.uniformsList=null,pt.outputColorSpace=z,pt.toneMapping=V),ut}function Vh(E){if(E.uniformsList===null){let U=E.currentProgram.getUniforms();E.uniformsList=Hs.seqWithValue(U.seq,E.uniforms)}return E.uniformsList}function Gh(E,U){let X=It.get(E);X.outputColorSpace=U.outputColorSpace,X.batching=U.batching,X.batchingColor=U.batchingColor,X.instancing=U.instancing,X.instancingColor=U.instancingColor,X.instancingMorph=U.instancingMorph,X.skinning=U.skinning,X.morphTargets=U.morphTargets,X.morphNormals=U.morphNormals,X.morphColors=U.morphColors,X.morphTargetsCount=U.morphTargetsCount,X.numClippingPlanes=U.numClippingPlanes,X.numIntersection=U.numClipIntersection,X.vertexAlphas=U.vertexAlphas,X.vertexTangents=U.vertexTangents,X.toneMapping=U.toneMapping}function Xf(E,U,X,z,V){U.isScene!==!0&&(U=hi),Ft.resetTextureUnits();let pt=U.fog,St=z.isMeshStandardMaterial||z.isMeshLambertMaterial||z.isMeshPhongMaterial?U.environment:null,ut=j===null?C.outputColorSpace:j.isXRRenderTarget===!0?j.texture.colorSpace:Wt.workingColorSpace,vt=z.isMeshStandardMaterial||z.isMeshLambertMaterial&&!z.envMap||z.isMeshPhongMaterial&&!z.envMap,Tt=R.get(z.envMap||St,vt),Ot=z.vertexColors===!0&&!!X.attributes.color&&X.attributes.color.itemSize===4,qt=!!X.attributes.tangent&&(!!z.normalMap||z.anisotropy>0),bt=!!X.morphAttributes.position,Yt=!!X.morphAttributes.normal,ye=!!X.morphAttributes.color,re=Hi;z.toneMapped&&(j===null||j.isXRRenderTarget===!0)&&(re=C.toneMapping);let ne=X.morphAttributes.position||X.morphAttributes.normal||X.morphAttributes.color,Ve=ne!==void 0?ne.length:0,wt=It.get(z),Ze=w.state.lights;if(we===!0&&(ei===!0||E!==K)){let me=E===K&&z.id===W;J.setState(z,E,me)}let jt=!1,Cn=!1;z.version===wt.__version?wt.needsLights&&wt.lightsStateVersion!==Ze.state.version||V.isBatchedMesh&&wt.batching===!1||!V.isBatchedMesh&&wt.batching===!0||V.isBatchedMesh&&wt.batchingColor===!0&&V._colorsTexture===null||V.isBatchedMesh&&wt.batchingColor===!1&&V._colorsTexture!==null||V.isInstancedMesh&&wt.instancing===!1||!V.isInstancedMesh&&wt.instancing===!0||V.isSkinnedMesh&&wt.skinning===!1||!V.isSkinnedMesh&&wt.skinning===!0||V.isInstancedMesh&&wt.instancingColor===!0&&V.instanceColor===null||V.isInstancedMesh&&wt.instancingColor===!1&&V.instanceColor!==null||V.isInstancedMesh&&wt.instancingMorph===!0&&V.morphTexture===null||V.isInstancedMesh&&wt.instancingMorph===!1&&V.morphTexture!==null||wt.envMap!==Tt||z.fog===!0&&wt.fog!==pt||wt.numClippingPlanes!==void 0&&(wt.numClippingPlanes!==J.numPlanes||wt.numIntersection!==J.numIntersection)||wt.vertexAlphas!==Ot||wt.vertexTangents!==qt||wt.morphTargets!==bt||wt.morphNormals!==Yt||wt.morphColors!==ye||wt.morphTargetsCount!==Ve||wt.numLightProbeGrids!==Hh(w.state.lightProbeGridArray)?jt=!0:(wt.outputColorSpace!==ut||wt.toneMapping!==re)&&(Cn=!0):(jt=!0,wt.__version=z.version);let Ge=wt.currentProgram;(jt===!0||Cn===!0)&&(Ge=Cn===!0?Wf(z,U,V,ut,re):ir(z,U,V),L&&z.isNodeMaterial&&L.onUpdateProgram(z,Ge,wt));let ma=!1,ln=!1,Pn=!1,Zt=Ge.getUniforms(),ze=wt.uniforms;if(mt.useProgram(Ge.program)&&(ma=!0,ln=!0,Pn=!0),z.id!==W&&(W=z.id,ln=!0),ma||K!==E){mt.buffers.depth.getReversed()&&E.reversedDepth!==!0&&(E._reversedDepth=!0,E.updateProjectionMatrix()),Zt.setValue(O,"projectionMatrix",E.projectionMatrix),Zt.setValue(O,"viewMatrix",E.matrixWorldInverse);let Ne=Zt.map.cameraPosition;Ne!==void 0&&Ne.setValue(O,Oi.setFromMatrixPosition(E.matrixWorld)),ue.logarithmicDepthBuffer&&Zt.setValue(O,"logDepthBufFC",2/(Math.log(E.far+1)/Math.LN2)),(z.isMeshPhongMaterial||z.isMeshToonMaterial||z.isMeshLambertMaterial||z.isMeshBasicMaterial||z.isMeshStandardMaterial||z.isShaderMaterial)&&Zt.setValue(O,"isOrthographic",E.isOrthographicCamera===!0),K!==E&&(K=E,ln=!0,Pn=!0)}if(wt.needsLights&&(Ze.state.sunShadowMap.length>0&&Zt.setValue(O,"sunShadowMap",Ze.state.sunShadowMap,Ft),Ze.state.directionalShadowMap.length>0&&Zt.setValue(O,"directionalShadowMap",Ze.state.directionalShadowMap,Ft),Ze.state.spotShadowMap.length>0&&Zt.setValue(O,"spotShadowMap",Ze.state.spotShadowMap,Ft),Ze.state.pointShadowMap.length>0&&Zt.setValue(O,"pointShadowMap",Ze.state.pointShadowMap,Ft)),V.isSkinnedMesh){Zt.setOptional(O,V,"bindMatrix"),Zt.setOptional(O,V,"bindMatrixInverse");let me=V.skeleton;me&&(me.boneTexture===null&&me.computeBoneTexture(),Zt.setValue(O,"boneTexture",me.boneTexture,Ft))}V.isBatchedMesh&&(Zt.setOptional(O,V,"batchingTexture"),Zt.setValue(O,"batchingTexture",V._matricesTexture,Ft),Zt.setOptional(O,V,"batchingIdTexture"),Zt.setValue(O,"batchingIdTexture",V._indirectTexture,Ft),Zt.setOptional(O,V,"batchingColorTexture"),V._colorsTexture!==null&&Zt.setValue(O,"batchingColorTexture",V._colorsTexture,Ft));let bi=X.morphAttributes;if((bi.position!==void 0||bi.normal!==void 0||bi.color!==void 0)&&$.update(V,X,Ge),(ln||wt.receiveShadow!==V.receiveShadow)&&(wt.receiveShadow=V.receiveShadow,Zt.setValue(O,"receiveShadow",V.receiveShadow)),(z.isMeshStandardMaterial||z.isMeshLambertMaterial||z.isMeshPhongMaterial)&&z.envMap===null&&U.environment!==null&&(ze.envMapIntensity.value=U.environmentIntensity),ze.dfgLUT!==void 0&&(ze.dfgLUT.value=z_()),ln){if(Zt.setValue(O,"toneMappingExposure",C.toneMappingExposure),wt.needsLights&&qf(ze,Pn),pt&&z.fog===!0&&ot.refreshFogUniforms(ze,pt),ot.refreshMaterialUniforms(ze,z,rt,Et,w.state.transmissionRenderTarget[E.id]),wt.needsLights&&wt.numLightProbeGrids>0){let me=M,Ne=0;for(let Wi of w.state.lightProbeGridArray)Wi.texture!==null&&(me.textures[Ne]=Wi.texture,me.min[Ne]=Wi.boundingBox.min,me.max[Ne]=Wi.boundingBox.max,me.resolution[Ne]=Wi.resolution,Ne++);for(let Wi in me)me[Wi].length=Ne;ze.probesSH.value=me.textures,ze.probesMin.value=me.min,ze.probesMax.value=me.max,ze.probesResolution.value=me.resolution}Hs.upload(O,Vh(wt),ze,Ft)}if(z.isShaderMaterial&&z.uniformsNeedUpdate===!0&&(Hs.upload(O,Vh(wt),ze,Ft),z.uniformsNeedUpdate=!1),z.isSpriteMaterial&&Zt.setValue(O,"center",V.center),Zt.setValue(O,"modelViewMatrix",V.modelViewMatrix),Zt.setValue(O,"normalMatrix",V.normalMatrix),Zt.setValue(O,"modelMatrix",V.matrixWorld),z.uniformsGroups!==void 0){let me=z.uniformsGroups;for(let Ne=0,Wi=me.length;Ne<Wi;Ne++){let Xh=me[Ne];At.update(Xh,Ge),At.bind(Xh,Ge)}}return Ge}function qf(E,U){E.ambientLightColor.needsUpdate=U,E.lightProbe.needsUpdate=U,E.sunLights.needsUpdate=U,E.sunLightShadows.needsUpdate=U,E.directionalLights.needsUpdate=U,E.directionalLightShadows.needsUpdate=U,E.pointLights.needsUpdate=U,E.pointLightShadows.needsUpdate=U,E.spotLights.needsUpdate=U,E.spotLightShadows.needsUpdate=U,E.rectAreaLights.needsUpdate=U,E.hemisphereLights.needsUpdate=U}function Yf(E){return E.isMeshLambertMaterial||E.isMeshToonMaterial||E.isMeshPhongMaterial||E.isMeshStandardMaterial||E.isShadowMaterial||E.isShaderMaterial&&E.lights===!0}this.getActiveCubeFace=function(){return G},this.getActiveMipmapLevel=function(){return k},this.getRenderTarget=function(){return j},this.setRenderTargetTextures=function(E,U,X){let z=It.get(E);z.__autoAllocateDepthBuffer=E.resolveDepthBuffer===!1,z.__autoAllocateDepthBuffer===!1&&(z.__useRenderToTexture=!1),It.get(E.texture).__webglTexture=U,It.get(E.depthTexture).__webglTexture=z.__autoAllocateDepthBuffer?void 0:X,z.__hasExternalTextures=!0},this.setRenderTargetFramebuffer=function(E,U){let X=It.get(E);X.__webglFramebuffer=U,X.__useDefaultFramebuffer=U===void 0},this.setRenderTarget=function(E,U=0,X=0){j=E,G=U,k=X;let z=null,V=!1,pt=!1;if(E){let ut=It.get(E);if(ut.__useDefaultFramebuffer!==void 0){mt.bindFramebuffer(O.FRAMEBUFFER,ut.__webglFramebuffer),Q.copy(E.viewport),dt.copy(E.scissor),yt=E.scissorTest,mt.viewport(Q),mt.scissor(dt),mt.setScissorTest(yt),W=-1;return}else if(ut.__webglFramebuffer===void 0)Ft.setupRenderTarget(E);else if(ut.__hasExternalTextures)Ft.rebindTextures(E,It.get(E.texture).__webglTexture,It.get(E.depthTexture).__webglTexture);else if(E.depthBuffer){let Ot=E.depthTexture;if(ut.__boundDepthTexture!==Ot){if(Ot!==null&&It.has(Ot)&&(E.width!==Ot.image.width||E.height!==Ot.image.height))throw new Error("THREE.WebGLRenderer: Attached DepthTexture is initialized to the incorrect size.");Ft.setupDepthRenderbuffer(E)}}let vt=E.texture;(vt.isData3DTexture||vt.isDataArrayTexture||vt.isCompressedArrayTexture)&&(pt=!0);let Tt=It.get(E).__webglFramebuffer;E.isWebGLCubeRenderTarget?(Array.isArray(Tt[U])?z=Tt[U][X]:z=Tt[U],V=!0):E.samples>0&&Ft.useMultisampledRTT(E)===!1?z=It.get(E).__webglMultisampledFramebuffer:Array.isArray(Tt)?z=Tt[X]:z=Tt,Q.copy(E.viewport),dt.copy(E.scissor),yt=E.scissorTest}else Q.copy(Rt).multiplyScalar(rt).floor(),dt.copy(Ht).multiplyScalar(rt).floor(),yt=$t;if(X!==0&&(z=F),mt.bindFramebuffer(O.FRAMEBUFFER,z)&&mt.drawBuffers(E,z),mt.viewport(Q),mt.scissor(dt),mt.setScissorTest(yt),V){let ut=It.get(E.texture);O.framebufferTexture2D(O.FRAMEBUFFER,O.COLOR_ATTACHMENT0,O.TEXTURE_CUBE_MAP_POSITIVE_X+U,ut.__webglTexture,X)}else if(pt){let ut=U;for(let vt=0;vt<E.textures.length;vt++){let Tt=It.get(E.textures[vt]);O.framebufferTextureLayer(O.FRAMEBUFFER,O.COLOR_ATTACHMENT0+vt,Tt.__webglTexture,X,ut)}}else if(E!==null&&X!==0){let ut=It.get(E.texture);O.framebufferTexture2D(O.FRAMEBUFFER,O.COLOR_ATTACHMENT0,O.TEXTURE_2D,ut.__webglTexture,X)}W=-1};function Wh(E){let U=It.get(E);return(U.__readFormat!==E.format||U.__readType!==E.type)&&(U.__readFormat=E.format,U.__readType=E.type,U.__formatReadable=ue.textureFormatReadable(E.format),U.__typeReadable=ue.textureTypeReadable(E.type)),U}this.readRenderTargetPixels=function(E,U,X,z,V,pt,St,ut=0){if(!(E&&E.isWebGLRenderTarget)){Lt("WebGLRenderer.readRenderTargetPixels: renderTarget is not THREE.WebGLRenderTarget.");return}let vt=It.get(E).__webglFramebuffer;if(E.isWebGLCubeRenderTarget&&St!==void 0&&(vt=vt[St],Array.isArray(vt)&&(vt=vt[0])),vt){mt.bindFramebuffer(O.FRAMEBUFFER,vt);try{let Tt=E.textures[ut],Ot=Tt.format,qt=Tt.type;E.textures.length>1&&O.readBuffer(O.COLOR_ATTACHMENT0+ut);let bt=Wh(Tt);if(bt.__formatReadable===!1){Lt("WebGLRenderer.readRenderTargetPixels: renderTarget is not in RGBA or implementation defined format.");return}if(bt.__typeReadable===!1){Lt("WebGLRenderer.readRenderTargetPixels: renderTarget is not in UnsignedByteType or implementation defined type.");return}U>=0&&U<=E.width-z&&X>=0&&X<=E.height-V&&O.readPixels(U,X,z,V,st.convert(Ot),st.convert(qt),pt)}finally{let Tt=j!==null?It.get(j).__webglFramebuffer:null;mt.bindFramebuffer(O.FRAMEBUFFER,Tt)}}},this.readRenderTargetPixelsAsync=async function(E,U,X,z,V,pt,St,ut=0){if(!(E&&E.isWebGLRenderTarget))throw new Error("THREE.WebGLRenderer.readRenderTargetPixels: renderTarget is not THREE.WebGLRenderTarget.");let vt=It.get(E).__webglFramebuffer;if(E.isWebGLCubeRenderTarget&&St!==void 0&&(vt=vt[St],Array.isArray(vt)&&(vt=vt[0])),vt)if(U>=0&&U<=E.width-z&&X>=0&&X<=E.height-V){mt.bindFramebuffer(O.FRAMEBUFFER,vt);let Tt=E.textures[ut],Ot=Tt.format,qt=Tt.type;E.textures.length>1&&O.readBuffer(O.COLOR_ATTACHMENT0+ut);let bt=Wh(Tt);if(bt.__formatReadable===!1)throw new Error("THREE.WebGLRenderer.readRenderTargetPixelsAsync: renderTarget is not in RGBA or implementation defined format.");if(bt.__typeReadable===!1)throw new Error("THREE.WebGLRenderer.readRenderTargetPixelsAsync: renderTarget is not in UnsignedByteType or implementation defined type.");let Yt=O.createBuffer();O.bindBuffer(O.PIXEL_PACK_BUFFER,Yt),O.bufferData(O.PIXEL_PACK_BUFFER,pt.byteLength,O.STREAM_READ),O.readPixels(U,X,z,V,st.convert(Ot),st.convert(qt),0),O.bindBuffer(O.PIXEL_PACK_BUFFER,null);let ye=j!==null?It.get(j).__webglFramebuffer:null;mt.bindFramebuffer(O.FRAMEBUFFER,ye);let re=O.fenceSync(O.SYNC_GPU_COMMANDS_COMPLETE,0);return O.flush(),await xd(O,re,4),O.bindBuffer(O.PIXEL_PACK_BUFFER,Yt),O.getBufferSubData(O.PIXEL_PACK_BUFFER,0,pt),O.bindBuffer(O.PIXEL_PACK_BUFFER,null),O.deleteBuffer(Yt),O.deleteSync(re),pt}else throw new Error("THREE.WebGLRenderer.readRenderTargetPixelsAsync: requested read bounds are out of range.")},this.copyFramebufferToTexture=function(E,U=null,X=0){let z=Math.pow(2,-X),V=Math.floor(E.image.width*z),pt=Math.floor(E.image.height*z),St=U!==null?U.x:0,ut=U!==null?U.y:0;Ft.setTexture2D(E,0),O.copyTexSubImage2D(O.TEXTURE_2D,X,0,0,St,ut,V,pt),mt.unbindTexture()},this.copyTextureToTexture=function(E,U,X=null,z=null,V=0,pt=0){let St,ut,vt,Tt,Ot,qt,bt,Yt,ye,re=E.isCompressedTexture?E.mipmaps[pt]:E.image;if(X!==null)St=X.max.x-X.min.x,ut=X.max.y-X.min.y,vt=X.isBox3?X.max.z-X.min.z:1,Tt=X.min.x,Ot=X.min.y,qt=X.isBox3?X.min.z:0;else{let Zt=Math.pow(2,-V);St=Math.floor(re.width*Zt),ut=Math.floor(re.height*Zt),E.isDataArrayTexture?vt=re.depth:E.isData3DTexture?vt=Math.floor(re.depth*Zt):vt=1,Tt=0,Ot=0,qt=0}z!==null?(bt=z.x,Yt=z.y,ye=z.z):(bt=0,Yt=0,ye=0);let ne=st.convert(U.format),Ve=st.convert(U.type),wt;U.isData3DTexture?(Ft.setTexture3D(U,0),wt=O.TEXTURE_3D):U.isDataArrayTexture||U.isCompressedArrayTexture?(Ft.setTexture2DArray(U,0),wt=O.TEXTURE_2D_ARRAY):(Ft.setTexture2D(U,0),wt=O.TEXTURE_2D),mt.activeTexture(O.TEXTURE0),mt.pixelStorei(O.UNPACK_FLIP_Y_WEBGL,U.flipY),mt.pixelStorei(O.UNPACK_PREMULTIPLY_ALPHA_WEBGL,U.premultiplyAlpha),mt.pixelStorei(O.UNPACK_ALIGNMENT,U.unpackAlignment);let Ze=mt.getParameter(O.UNPACK_ROW_LENGTH),jt=mt.getParameter(O.UNPACK_IMAGE_HEIGHT),Cn=mt.getParameter(O.UNPACK_SKIP_PIXELS),Ge=mt.getParameter(O.UNPACK_SKIP_ROWS),ma=mt.getParameter(O.UNPACK_SKIP_IMAGES);mt.pixelStorei(O.UNPACK_ROW_LENGTH,re.width),mt.pixelStorei(O.UNPACK_IMAGE_HEIGHT,re.height),mt.pixelStorei(O.UNPACK_SKIP_PIXELS,Tt),mt.pixelStorei(O.UNPACK_SKIP_ROWS,Ot),mt.pixelStorei(O.UNPACK_SKIP_IMAGES,qt);let ln=E.isDataArrayTexture||E.isData3DTexture,Pn=U.isDataArrayTexture||U.isData3DTexture;if(E.isDepthTexture){let Zt=It.get(E),ze=It.get(U),bi=It.get(Zt.__renderTarget),me=It.get(ze.__renderTarget);mt.bindFramebuffer(O.READ_FRAMEBUFFER,bi.__webglFramebuffer),mt.bindFramebuffer(O.DRAW_FRAMEBUFFER,me.__webglFramebuffer);for(let Ne=0;Ne<vt;Ne++)ln&&(O.framebufferTextureLayer(O.READ_FRAMEBUFFER,O.COLOR_ATTACHMENT0,It.get(E).__webglTexture,V,qt+Ne),O.framebufferTextureLayer(O.DRAW_FRAMEBUFFER,O.COLOR_ATTACHMENT0,It.get(U).__webglTexture,pt,ye+Ne)),O.blitFramebuffer(Tt,Ot,Tt+St,Ot+ut,bt,Yt,bt+St,Yt+ut,O.DEPTH_BUFFER_BIT,O.NEAREST);mt.bindFramebuffer(O.READ_FRAMEBUFFER,null),mt.bindFramebuffer(O.DRAW_FRAMEBUFFER,null)}else if(V!==0||E.isRenderTargetTexture||It.has(E)){let Zt=It.get(E),ze=It.get(U);mt.bindFramebuffer(O.READ_FRAMEBUFFER,D),mt.bindFramebuffer(O.DRAW_FRAMEBUFFER,B);for(let bi=0;bi<vt;bi++)ln?O.framebufferTextureLayer(O.READ_FRAMEBUFFER,O.COLOR_ATTACHMENT0,Zt.__webglTexture,V,qt+bi):O.framebufferTexture2D(O.READ_FRAMEBUFFER,O.COLOR_ATTACHMENT0,O.TEXTURE_2D,Zt.__webglTexture,V),Pn?O.framebufferTextureLayer(O.DRAW_FRAMEBUFFER,O.COLOR_ATTACHMENT0,ze.__webglTexture,pt,ye+bi):O.framebufferTexture2D(O.DRAW_FRAMEBUFFER,O.COLOR_ATTACHMENT0,O.TEXTURE_2D,ze.__webglTexture,pt),V!==0?O.blitFramebuffer(Tt,Ot,Tt+St,Ot+ut,bt,Yt,bt+St,Yt+ut,O.COLOR_BUFFER_BIT,O.NEAREST):Pn?O.copyTexSubImage3D(wt,pt,bt,Yt,ye+bi,Tt,Ot,St,ut):O.copyTexSubImage2D(wt,pt,bt,Yt,Tt,Ot,St,ut);mt.bindFramebuffer(O.READ_FRAMEBUFFER,null),mt.bindFramebuffer(O.DRAW_FRAMEBUFFER,null)}else Pn?E.isDataTexture||E.isData3DTexture?O.texSubImage3D(wt,pt,bt,Yt,ye,St,ut,vt,ne,Ve,re.data):U.isCompressedArrayTexture?O.compressedTexSubImage3D(wt,pt,bt,Yt,ye,St,ut,vt,ne,re.data):O.texSubImage3D(wt,pt,bt,Yt,ye,St,ut,vt,ne,Ve,re):E.isDataTexture?O.texSubImage2D(O.TEXTURE_2D,pt,bt,Yt,St,ut,ne,Ve,re.data):E.isCompressedTexture?O.compressedTexSubImage2D(O.TEXTURE_2D,pt,bt,Yt,re.width,re.height,ne,re.data):O.texSubImage2D(O.TEXTURE_2D,pt,bt,Yt,St,ut,ne,Ve,re);mt.pixelStorei(O.UNPACK_ROW_LENGTH,Ze),mt.pixelStorei(O.UNPACK_IMAGE_HEIGHT,jt),mt.pixelStorei(O.UNPACK_SKIP_PIXELS,Cn),mt.pixelStorei(O.UNPACK_SKIP_ROWS,Ge),mt.pixelStorei(O.UNPACK_SKIP_IMAGES,ma),pt===0&&U.generateMipmaps===!0&&U.mipmapsAutoUpdate===!0&&O.generateMipmap(wt),mt.unbindTexture()},this.initRenderTarget=function(E){It.get(E).__webglFramebuffer===void 0&&Ft.setupRenderTarget(E)},this.initTexture=function(E){E.isCubeTexture?Ft.setTextureCube(E,0):E.isData3DTexture?Ft.setTexture3D(E,0):E.isDataArrayTexture||E.isCompressedArrayTexture?Ft.setTexture2DArray(E,0):Ft.setTexture2D(E,0),mt.unbindTexture()},this.resetState=function(){G=0,k=0,j=null,mt.reset(),lt.reset()},typeof __THREE_DEVTOOLS__<"u"&&__THREE_DEVTOOLS__.dispatchEvent(new CustomEvent("observe",{detail:this}))}get coordinateSystem(){return mi}get outputColorSpace(){return this._outputColorSpace}set outputColorSpace(t){this._outputColorSpace=t;let e=this.getContext();e.drawingBufferColorSpace=Wt._getDrawingBufferColorSpace(t),e.unpackColorSpace=Wt._getUnpackColorSpace()}};var k_=[15432046,9231257,9285362];function dh(n){if(!n?.center)return[];let t=[{position:n.center,kind:"centre"}];for(let e=0;e<3;e++){let i=(n.max[e]-n.min[e])/2,s=Math.max(.32,i),r=[...n.center];r[e]+=s,t.push({position:r,kind:"stretch",axis:e,extent:i})}return t}var Ws=class{constructor(){this.group=new xe,this.key=null}clear(){for(let t of this.group.children)t.geometry.dispose(),t.material.dispose();this.group.clear()}update(t){if(!t){this.group.children.length&&this.clear(),this.key=null;return}if(t.key===this.key)return;this.key=t.key,this.clear();let e=(s,r,a=1)=>{if(!s?.length)return;let o=new Float32Array(s.length*3);s.forEach((h,u)=>o.set(h,u*3));let l=new ee;l.setAttribute("position",new _e(o,3));let c=new Pi({color:r,opacity:a,transparent:a<1,depthTest:!1,toneMapped:!1});this.group.add(new en(l,c))},i=new Mt().fromArray(t.color||[.4,.9,.8]);if(e(t.outer,i,.28),e(t.surface,i,.9),t["handles?"]!==!1)for(let{position:s,kind:r,axis:a}of dh(t)){let o=r==="centre",l=o?16767920:k_[a];o||e([t.center,s],l);let c=o?new nn(.11,12,8):new ve(.13,.13,.13),h=new Ut(c,new _i({color:l,depthTest:!1,toneMapped:!1}));h.position.fromArray(s),this.group.add(h)}}dispose(){this.clear(),this.key=null}};var Ce=(n,t,e)=>Math.max(t,Math.min(e,n)),vl=n=>n*Math.PI/180,ra=n=>Array.isArray(n)&&n.length===3&&n.every(Number.isFinite);function yl(n){return[n?.width,n?.height,n?.depth].map((t,e)=>Number.isFinite(t)&&t>0?t:[12,5,10][e])}function fh(n){let t=Math.hypot(...n);return t>1e-9?n.map(e=>e/t):[0,-1,0]}function wn(n,t){let e=n[t]??n.channels?.[t];return Number.isFinite(e)&&e>=0&&e<=1?e:void 0}function an(n,t,e){let i=e+"-deg";if(Number.isFinite(t.pose?.[i]))return t.pose[i];let s=wn(t,e);if(s===void 0&&Number.isFinite(t[i]))return t[i];let r=s??0,a=n.calibration?.[e];if(Array.isArray(a)&&a.length===2&&a[1]!==0)return(r*255-a[0])/a[1]*180;let o=a?.["min-deg"]??(e==="pan"?-180:-90),l=a?.["max-deg"]??(e==="pan"?180:90);return o+r*(l-o)}function ph(n,t=[0,0,0]){let[e,i,s]=n;for(let r=0;r<3;r++){let a=vl(Number(t[r])||0),o=Math.cos(a),l=Math.sin(a);r===0&&([i,s]=[i*o-s*l,i*l+s*o]),r===1&&([e,s]=[e*o+s*l,-e*l+s*o]),r===2&&([e,i]=[e*o-i*l,e*l+i*o])}return[e,i,s]}function nf(n,t={}){if(ra(t.pose?.direction))return t.pose.direction;if(!(wn(t,"pan")!==void 0&&wn(t,"tilt")!==void 0||Number.isFinite(t.pose?.["pan-deg"])&&Number.isFinite(t.pose?.["tilt-deg"]))){if(ra(t.direction)&&Math.hypot(...t.direction)>1e-9)return fh(t.direction);if(ra(t.aim)&&ra(n.position))return fh(t.aim.map((r,a)=>r-n.position[a]))}let i=vl(an(n,t,"pan")),s=vl(an(n,t,"tilt"));return fh(ph([Math.sin(i)*Math.cos(s),Math.sin(s),Math.cos(i)*Math.cos(s)],n["rotation-deg"]))}function Xs(n={}){return ra(n["emitted-color"])?n["emitted-color"].map(t=>Ce(t,0,8)):[0,0,0]}function sf(n,t={}){let e=n["zoom-range-deg"],i=Ce(Number(t.zoom??t.channels?.zoom)||0,0,1),s=a=>Number.isFinite(a)&&a>0&&a<180,r=Array.isArray(e)&&e.length===2&&e.every(s)?e[0]+i*(e[1]-e[0]):s(n["beam-angle"])?n["beam-angle"]:12+i*34;return vl(r/2)}function rf(n){let t=n?.fixtures||[];return JSON.stringify({room:n?.room,surfaces:n?.surfaces,reflections:t.filter(e=>e["pseudo-fixture"]).length,fixtures:t.map(e=>({id:e.id,type:e["fixture-type"],aperture:e["lens-radius-m"],diffuser:e["diffuser-radius-m"]&&[e["diffuser-radius-m"],e["pixel-layout-id"],e["pixel-index"],e.position]})),trusses:(n?.trusses||[]).map(e=>({id:e.id,length:e.length}))})}var oa=n=>({xz:1,xy:2,yz:0})[n]??1,H_=n=>JSON.stringify([n?.["runtime-id"],n?.["show-id"],n?.epoch]),Oe=n=>Array.isArray(n)&&n.length===3&&n.every(t=>Number.isFinite(t)&&Math.abs(t)<=1e4);function qs(n,t,e,i,s="xz",r=0,a=0){if(!(t.width>0&&t.height>0)||!Number.isFinite(r))return null;n.updateMatrixWorld();let o=new vn;o.setFromCamera(new et(2*(e-t.left)/t.width-1,1-2*(i-t.top)/t.height),n);let l=oa(s),c=new A().setComponent(l,1),h=new A;if(!o.ray.intersectPlane(new Ke(c,-r),h))return null;let u=h.toArray().map((d,f)=>f===l?r:a>0?Math.round(d/a)*a:d);return Oe(u)?u:null}function V_(n,t,e,i,s){if(!(e.width>0&&e.height>0))return null;t.updateMatrixWorld();let r=new vn;r.setFromCamera(new et((i-e.left)*2/e.width-1,1-(s-e.top)*2/e.height),t);let{origin:a,direction:o}=r.ray,l=1/0,c=null;for(let h of n?.["receiver-surfaces"]||[]){let u=new A().fromArray(h.point),d=new A().fromArray(h.normal).normalize(),f=d.dot(o);if(f>=-1e-8)continue;let p=u.clone().sub(a).dot(d)/f;if(p<=0||p>=l)continue;let v=r.ray.at(p,new A),g=v.clone().sub(u),m=new A().fromArray(h["u-axis"]).projectOnPlane(d).normalize(),_=new A().crossVectors(d,m);Math.abs(g.dot(m))<=h.size[0]/2+1e-8&&Math.abs(g.dot(_))<=h.size[1]/2+1e-8&&(l=p,c={point:v.toArray(),"surface-id":h.id})}return c}function aa(n,t,e){if(!Oe(e))return null;let i=new A(...e).project(n);return i.z>=-1&&i.z<=1?{x:t.left+(i.x+1)*t.width/2,y:t.top+(1-i.y)*t.height/2,depth:i.z}:null}var G_=(n,t,e,i)=>{let s=i.x-e.x,r=i.y-e.y,a=Math.max(0,Math.min(1,((n-e.x)*s+(t-e.y)*r)/(s*s+r*r||1)));return Math.hypot(n-e.x-a*s,t-e.y-a*r)};function W_(n,t,e,i,s){t.updateMatrixWorld();let r=n?.editor||{},a=[],o=(l,c,h=24)=>{let u=aa(t,e,l);if(!u)return;let d=Math.hypot(u.x-i,u.y-s);d<=h&&a.push({...c,position:l,score:d/h+u.depth*.001})};if(r.tool==="sculpt-volume")for(let l of dh(n["sculpt-guide"]))o(l.position,l,22);else if(r.tool==="volume-select")for(let l of n["spatial-volumes"]||[]){let c=n["volume-guides"]?.[l.id],h=(c?.["moving?"]?l.signal?.position||l.position:c?.center)||l.signal?.position||l.position||(Oe(l.min)&&Oe(l.max)?l.min.map((u,d)=>(u+l.max[d])/2):null);h&&o(h,{kind:"volumes",id:l.id},32)}else if(r.tool==="pixels"&&r["pixel-mode"]==="pixel"){let l=r["pixel-indices"]||[];(r["pixel-positions"]||[]).forEach((c,h)=>o(c,{kind:"pixels",index:l[h]},22)),o(r["pixel-position"],{kind:"pixels",index:r["selected-pixel"]},22)}else r.tool==="draw"||r.tool==="pixels"?(r.points||[]).slice(0,256).forEach((l,c)=>o(l,{kind:"points",index:r["point-indices"]?.[c]??c},22)):((n?.rig?.fixtures||[]).slice(0,2048).filter(l=>l["pixel-layout-id"]==null).forEach(l=>o(l.position,{kind:"fixtures",id:String(l.id)})),(n?.rig?.trusses||[]).slice(0,24).forEach(l=>{if(!Oe(l.position))return;let c=ph([Math.min(40,l.length||1)/2,0,0],l["rotation-deg"]),h=aa(t,e,l.position.map((d,f)=>d-c[f])),u=aa(t,e,l.position.map((d,f)=>d+c[f]));if(h&&u){let d=G_(i,s,h,u);d<=18&&a.push({kind:"trusses",id:String(l.id),position:l.position,score:d/18+h.depth*.001+.2})}}));return a.sort((l,c)=>l.score-c.score)[0]||null}var Ys=class{constructor(t){this.engine=t,this.canvas=t.canvas,this.abort=new AbortController,this.sequence=0;let e={signal:this.abort.signal,capture:!0};this.canvas.addEventListener("pointerdown",i=>this.down(i),e),this.canvas.addEventListener("pointermove",i=>this.move(i),e),this.canvas.addEventListener("pointerup",i=>this.finish(i,!1),e),this.canvas.addEventListener("pointercancel",i=>this.finish(i,!0),e),this.canvas.addEventListener("lostpointercapture",i=>this.finish(i,!0),e),this.canvas.addEventListener("keydown",i=>{i.key==="Escape"&&this.snapshot?.editor&&(this.cancel(),this.emit("navigate"))},e),globalThis.addEventListener?.("blur",()=>this.cancel(),{signal:this.abort.signal}),globalThis.document?.addEventListener("visibilitychange",()=>{document.hidden&&this.cancel()},{signal:this.abort.signal})}emit(t,e={}){this.canvas.dispatchEvent(new CustomEvent("tolglow-editor",{detail:{...e,type:t,identity:this.identity,token:this.gesture?.token}}))}update(t){let e=H_(t),i=t?.editor,s=this.snapshot?.editor;(this.identity!==e||i?.tool!==s?.tool||i?.["layout-id"]!==s?.["layout-id"]||i?.["pixel-mode"]!==s?.["pixel-mode"]||i?.plane!==s?.plane)&&this.cancel(),this.identity=e,this.snapshot=t;let r=!!(t?.editor&&t.editor.tool!=="navigate");r&&!this.interacting?(this.previousInteraction=!!this.engine.getNavigationState?.().interacting,this.engine.setInteractionActive?.(!0)):!r&&this.interacting&&this.engine.setInteractionActive?.(this.previousInteraction),this.interacting=r,this.canvas.style.cursor=r?["draw","pixels","aim"].includes(t.editor.tool)?"crosshair":"grab":""}down(t){let e=this.snapshot?.editor;if(this.gesture?.tool==="aim"&&t.pointerId!==this.gesture.pointer){this.gesture.dragged=!0,t.preventDefault(),t.stopImmediatePropagation();return}if(!e||e.tool==="navigate"||t.button!==0||this.gesture)return;if(t.preventDefault(),t.stopImmediatePropagation(),this.canvas.focus(),e.tool==="aim"){this.gesture={tool:"aim",token:++this.sequence,pointer:t.pointerId,x:t.clientX,y:t.clientY,dragged:!1},this.canvas.setPointerCapture(t.pointerId);return}if(e.tool==="throw"){let o=qs(this.engine.camera,this.canvas.getBoundingClientRect(),t.clientX,t.clientY,e.plane,e["plane-value"],e.snap);if(!o){this.emit("unreachable");return}this.gesture={tool:"throw",token:++this.sequence,pointer:t.pointerId,point:o,start:t.timeStamp,x:t.clientX,y:t.clientY,plane:e.plane,value:e["plane-value"]},this.canvas.setPointerCapture(t.pointerId);return}let i=this.canvas.getBoundingClientRect(),s=W_(this.snapshot,this.engine.camera,i,t.clientX,t.clientY);if(e.tool==="sculpt-volume"){if(!s)return;let o=s.position[oa(e.plane)],l=qs(this.engine.camera,i,t.clientX,t.clientY,e.plane,o,e.snap),c=aa(this.engine.camera,i,s.position),h=[...s.position];s.axis!=null&&(h[s.axis]+=1);let u=aa(this.engine.camera,i,h);if(!l||!c||!u){this.emit("unreachable");return}let d=u.x-c.x,f=u.y-c.y;if(s.kind==="stretch"&&d*d+f*f<4){this.emit("unreachable");return}this.gesture={token:++this.sequence,tool:e.tool,pointer:t.pointerId,x:t.clientX,y:t.clientY,axis:s.axis,extent:s.extent,value:o,plane:e.plane,snap:e.snap,dx:d,dy:f},this.canvas.setPointerCapture(t.pointerId),this.emit("sculpt-begin",{point:l});return}if(e.tool==="volume-select"){s&&this.emit("select-volume",{"volume-id":s.id});return}if(e.tool==="pixels"&&!s&&!e["append?"])return;if(e.tool!=="draw"&&e.tool!=="pixels"&&!s){this.emit("select",{selection:null});return}let r=s?s.position[oa(e.plane)]:e["plane-value"],a=qs(this.engine.camera,i,t.clientX,t.clientY,e.plane,r,e.snap);if(!a){this.emit("unreachable");return}this.gesture={token:++this.sequence,pointer:t.pointerId,hit:s,point:a,x:t.clientX,value:r,tool:e.tool,offset:s?s.position.map((o,l)=>o-a[l]):[0,0,0]},this.canvas.setPointerCapture(t.pointerId),this.emit("begin",{selection:s,point:a})}move(t){let e=this.gesture;if(!e||t.pointerId!==e.pointer||(t.preventDefault(),t.stopImmediatePropagation(),e.tool==="throw"))return;if(e.tool==="sculpt-volume"){if(e.axis!=null){let r=((t.clientX-e.x)*e.dx+(t.clientY-e.y)*e.dy)/(e.dx*e.dx+e.dy*e.dy),a=e.snap>0?Math.round(r/e.snap)*e.snap:r;this.emit("sculpt-move",{axis:e.axis,delta:a,"half-extent":e.extent})}else{let r=qs(this.engine.camera,this.canvas.getBoundingClientRect(),t.clientX,t.clientY,e.plane,e.value,e.snap);r&&this.emit("sculpt-move",{point:r})}return}if(e.tool==="aim"){Math.hypot(t.clientX-e.x,t.clientY-e.y)>10&&(e.dragged=!0);return}let i=this.snapshot.editor,s=qs(this.engine.camera,this.canvas.getBoundingClientRect(),t.clientX,t.clientY,i.plane,e.value,i.snap);s&&this.emit("move",{point:e.hit&&["move","draw","pixels"].includes(e.tool)?s.map((r,a)=>r+e.offset[a]):s,angle:(t.clientX-e.x)*.5})}finish(t,e){if(!this.gesture||t.pointerId!==this.gesture.pointer)return;t.preventDefault(),t.stopImmediatePropagation();let i=this.gesture;if(i.tool==="aim"){if(!e&&!i.dragged&&Math.hypot(t.clientX-i.x,t.clientY-i.y)<=10){let r=V_(this.snapshot,this.engine.camera,this.canvas.getBoundingClientRect(),t.clientX,t.clientY);this.emit(r?"surface":"missed-surface",r||{})}}else if(i.tool==="throw"){if(!e&&Math.hypot(t.clientX-i.x,t.clientY-i.y)>=8){let r=qs(this.engine.camera,this.canvas.getBoundingClientRect(),t.clientX,t.clientY,i.plane,i.value);if(r){let a=Math.max(.08,(t.timeStamp-i.start)/1e3),o=r.map((c,h)=>(c-i.point[h])/a),l=Math.min(1,50/(Math.hypot(...o)||1));this.emit("throw",{point:i.point,velocity:o.map(c=>c*l)})}}}else i.tool==="sculpt-volume"?this.emit(e?"sculpt-cancel":"sculpt-end"):this.emit(e?"cancel":"end");let s=i.pointer;this.gesture=null,this.canvas.hasPointerCapture?.(s)&&this.canvas.releasePointerCapture(s)}cancel(){if(!this.gesture)return;this.emit(this.gesture.tool==="sculpt-volume"?"sculpt-cancel":"cancel");let t=this.gesture.pointer;this.gesture=null,this.canvas.hasPointerCapture?.(t)&&this.canvas.releasePointerCapture(t)}dispose(){this.cancel(),this.abort.abort(),this.interacting&&this.engine.setInteractionActive?.(this.previousInteraction),this.interacting=!1,this.canvas.style.cursor=""}};function X_(n){return Oe(n.signal?.position)?n.signal.position:Oe(n.position)?n.position:n.shape==="box"&&Oe(n.min)&&Oe(n.max)?n.min.map((t,e)=>(t+n.max[e])/2):null}function q_(n){let t=X_(n),e=n.signal?.radius??n.radius;if(!t)return null;if(n.shape==="box"&&Oe(n.min)&&Oe(n.max))return{position:t,scale:n.max.map((r,a)=>Math.max(0,r-n.min[a]))};if(!["ball","sphere","cylinder"].includes(n.shape)||!Number.isFinite(e)||e<=0)return null;if(n.shape==="cylinder")return{position:t,scale:[e,n.height,e]};let i=Math.min(.5,Math.max(0,n.signal?.deformation||0)),s=1/Math.sqrt(1-i);return{position:t,scale:[e*s,e*(1-i),e*s]}}function Y_(n){if(n==="box"||n==="cylinder"){let e=n==="box"?new ve(1,1,1):new vi(1,1,1,40),i=new Hn(e);return e.dispose(),i}let t=[];for(let e=0;e<3;e++)for(let i=0;i<48;i++)for(let s of[i,i+1]){let r=[0,0,0];r[(e+1)%3]=Math.cos(s*Math.PI/24),r[(e+2)%3]=Math.sin(s*Math.PI/24),t.push(new A(...r))}return new ee().setFromPoints(t)}var Ml=class{constructor(){this.group=new xe,this.byId=new Map,this.fields=new Map}remove(t){let e=this.byId.get(t);e.geometry.dispose(),e.material.dispose(),this.group.remove(e),this.byId.delete(t)}update(t=[],e){let i=new Set,s=new Set;for(let r of t){let a=e?.[r.id];if(a){let c=this.fields.get(r.id);c||(c=new Ws,this.fields.set(r.id,c),this.group.add(c.group)),c.update(a),a["moving?"]&&c.group.position.fromArray(r.signal?.position||r.position),s.add(r.id);continue}let o=q_(r);if(!o)continue;i.add(r.id);let l=this.byId.get(r.id);l&&l.userData.shape!==r.shape&&(this.remove(r.id),l=null),l||(l=new en(Y_(r.shape),new Pi({color:r.shape==="ball"?16760184:9427403,transparent:!0,opacity:.84,depthTest:!1,toneMapped:!1})),l.userData.shape=r.shape,this.group.add(l),this.byId.set(r.id,l)),l.position.fromArray(o.position),l.scale.fromArray(o.scale),Oe(r.signal?.rotation)&&l.rotation.set(...r.signal.rotation)}for(let r of this.byId.keys())i.has(r)||this.remove(r);for(let[r,a]of this.fields)s.has(r)||(a.dispose(),this.group.remove(a.group),this.fields.delete(r))}dispose(){this.update([])}};var Sl=class{constructor(){this.scene=new ai,this.rig=new xe,this.volumes=new Ml,this.sculpt=new Ws,this.scene.add(this.rig,this.volumes.group,this.sculpt.group),this.key=null}clear(){this.rig.traverse(t=>{t.geometry?.dispose(),t.material?.dispose()}),this.rig.clear()}update(t){this.volumes.update(t?.["spatial-volumes"],t?.["volume-guides"]),this.sculpt.update(t?.["sculpt-guide"]);let e=t?.editor,i=e?.selection;if(!e){this.key!==null&&(this.clear(),this.key=null);return}this.markerRig!==t.rig&&(this.markerRig=t.rig,this.markers=(t.rig?.fixtures||[]).slice(0,2048).filter(c=>c["pixel-layout-id"]==null).map(c=>c.position).filter(Oe),this.key=null);let s=t?.rig?.[i?.kind]?.find(c=>String(c.id)===String(i.id)),r=this.markers;if(this.key===e&&this.item===s)return;this.key=e,this.item=s,this.clear();let a=(c,h,u=1)=>{let d=new ee().setFromPoints(c.map(f=>new A(...f)));this.rig.add(new bs(d,new Pi({color:h,transparent:u<1,opacity:u,depthTest:!1,toneMapped:!1})))},o=(c,h,u)=>{if(!c.length)return;let d=new ee().setFromPoints(c.map(f=>new A(...f)));this.rig.add(new Sr(d,new ws({color:h,size:u,sizeAttenuation:!1,depthTest:!1,toneMapped:!1})))};o(r,10663625,6),e.tool==="pixels"&&(o(e["pixel-positions"]||[],7923154,5),e["pixel-mode"]==="pixel"&&Oe(e["pixel-position"])&&o([e["pixel-position"]],16763264,12));let l=(e.points||[]).filter(Oe).slice(0,256);if(l.length>1&&a(e["closed?"]?[...l,l[0]]:l,11979177),e["pixel-mode"]!=="pixel"&&l.forEach((c,h)=>{let u=new Ut(new nn(h===e["selected-point"]?.11:.075,10,6),new _i({color:h===e["selected-point"]?16763264:11979177,depthTest:!1,toneMapped:!1}));u.position.fromArray(c),this.rig.add(u)}),s&&Oe(s.position)){let c=new ve(i.kind==="trusses"?Math.min(40,s.length||1):.7,.9,.65),h=new Hn(c);c.dispose();let u=new en(h,new Pi({color:16763264,depthTest:!1,toneMapped:!1}));u.position.fromArray(s.position),u.rotation.set(...(s["rotation-deg"]||[0,0,0]).map(d=>d*Math.PI/180),"ZYX"),this.rig.add(u)}if(e.tool==="draw"||e.tool==="pixels"){let c=oa(e.plane),h=[0,1,2].filter(d=>d!==c),u=Number(e["plane-value"])||0;for(let d=-10;d<=10;d++)for(let f=0;f<2;f++){let p=[0,0,0],v=[0,0,0];p[c]=v[c]=u,p[h[f]]=v[h[f]]=d,p[h[1-f]]=-10,v[h[1-f]]=10,a([p,v],11979177,.14)}}}render(t,e){if(!this.rig.children.length&&!this.volumes.group.children.length&&!this.sculpt.group.children.length)return;let i=t.autoClear,s=t.getRenderTarget();try{t.setRenderTarget(null),t.autoClear=!1,t.clearDepth(),t.render(this.scene,e)}finally{t.autoClear=i,t.setRenderTarget(s)}}dispose(){this.clear(),this.volumes.dispose(),this.sculpt.dispose()}};var af={type:"change"},gh={type:"start"},lf={type:"end"},bl=new tn,of=new Ke,Z_=Math.cos(70*ks.DEG2RAD),Pe=new A,ni=2*Math.PI,Qt={NONE:-1,ROTATE:0,DOLLY:1,PAN:2,TOUCH_ROTATE:3,TOUCH_PAN:4,TOUCH_DOLLY_PAN:5,TOUCH_DOLLY_ROTATE:6},mh=1e-6,wl=class extends Br{constructor(t,e=null){super(t,e),this.state=Qt.NONE,this.target=new A,this.cursor=new A,this.minDistance=0,this.maxDistance=1/0,this.minZoom=0,this.maxZoom=1/0,this.minTargetRadius=0,this.maxTargetRadius=1/0,this.minPolarAngle=0,this.maxPolarAngle=Math.PI,this.minAzimuthAngle=-1/0,this.maxAzimuthAngle=1/0,this.enableDamping=!1,this.dampingFactor=.05,this.enableZoom=!0,this.zoomSpeed=1,this.enableRotate=!0,this.rotateSpeed=1,this.keyRotateSpeed=1,this.enablePan=!0,this.panSpeed=1,this.screenSpacePanning=!0,this.keyPanSpeed=7,this.zoomToCursor=!1,this.autoRotate=!1,this.autoRotateSpeed=2,this.keys={LEFT:"ArrowLeft",UP:"ArrowUp",RIGHT:"ArrowRight",BOTTOM:"ArrowDown"},this.mouseButtons={LEFT:yn.ROTATE,MIDDLE:yn.DOLLY,RIGHT:yn.PAN},this.touches={ONE:Li.ROTATE,TWO:Li.DOLLY_PAN},this.target0=this.target.clone(),this.position0=this.object.position.clone(),this.zoom0=this.object.zoom,this._cursorStyle="auto",this._domElementKeyEvents=null,this._lastPosition=new A,this._lastQuaternion=new Je,this._lastTargetPosition=new A,this._quat=new Je().setFromUnitVectors(t.up,new A(0,1,0)),this._quatInverse=this._quat.clone().invert(),this._spherical=new Ds,this._sphericalDelta=new Ds,this._scale=1,this._panOffset=new A,this._rotateStart=new et,this._rotateEnd=new et,this._rotateDelta=new et,this._panStart=new et,this._panEnd=new et,this._panDelta=new et,this._dollyStart=new et,this._dollyEnd=new et,this._dollyDelta=new et,this._dollyDirection=new A,this._mouse=new et,this._performCursorZoom=!1,this._pointers=[],this._pointerPositions={},this._controlActive=!1,this._onPointerMove=J_.bind(this),this._onPointerDown=K_.bind(this),this._onPointerUp=$_.bind(this),this._onContextMenu=s1.bind(this),this._onMouseWheel=t1.bind(this),this._onKeyDown=e1.bind(this),this._onTouchStart=i1.bind(this),this._onTouchMove=n1.bind(this),this._onMouseDown=j_.bind(this),this._onMouseMove=Q_.bind(this),this._interceptControlDown=r1.bind(this),this._interceptControlUp=a1.bind(this),this.domElement!==null&&this.connect(this.domElement),this.update()}set cursorStyle(t){this._cursorStyle=t,t==="grab"?this.domElement.style.cursor="grab":this.domElement.style.cursor="auto"}get cursorStyle(){return this._cursorStyle}connect(t){super.connect(t),this.domElement.addEventListener("pointerdown",this._onPointerDown),this.domElement.addEventListener("pointercancel",this._onPointerUp),this.domElement.addEventListener("contextmenu",this._onContextMenu),this.domElement.addEventListener("wheel",this._onMouseWheel,{passive:!1}),this.domElement.ownerDocument.addEventListener("keydown",this._interceptControlDown,{passive:!0,capture:!0}),this.domElement.style.touchAction="none"}disconnect(){this.state=Qt.NONE,this.domElement.removeEventListener("pointerdown",this._onPointerDown),this.domElement.ownerDocument.removeEventListener("pointermove",this._onPointerMove),this.domElement.ownerDocument.removeEventListener("pointerup",this._onPointerUp),this.domElement.removeEventListener("pointercancel",this._onPointerUp),this.domElement.removeEventListener("wheel",this._onMouseWheel),this.domElement.removeEventListener("contextmenu",this._onContextMenu),this.stopListenToKeyEvents(),this.domElement.ownerDocument.removeEventListener("keydown",this._interceptControlDown,{capture:!0}),this.domElement.ownerDocument.removeEventListener("keyup",this._interceptControlUp,{capture:!0}),this._controlActive=!1,this._pointers.length=0,this._pointerPositions={},this.domElement.style.touchAction="",this.domElement.style.cursor="auto"}dispose(){this.disconnect()}getPolarAngle(){return this._spherical.phi}getAzimuthalAngle(){return this._spherical.theta}getDistance(){return this.object.position.distanceTo(this.target)}listenToKeyEvents(t){t.addEventListener("keydown",this._onKeyDown),this._domElementKeyEvents=t}stopListenToKeyEvents(){this._domElementKeyEvents!==null&&(this._domElementKeyEvents.removeEventListener("keydown",this._onKeyDown),this._domElementKeyEvents=null)}saveState(){this.target0.copy(this.target),this.position0.copy(this.object.position),this.zoom0=this.object.zoom}reset(){this.target.copy(this.target0),this.object.position.copy(this.position0),this.object.zoom=this.zoom0,this.object.updateProjectionMatrix(),this.dispatchEvent(af),this.update(),this.state=Qt.NONE}pan(t,e){this._pan(t,e),this.update()}dollyIn(t){this._dollyIn(t),this.update()}dollyOut(t){this._dollyOut(t),this.update()}rotateLeft(t){this._rotateLeft(t),this.update()}rotateUp(t){this._rotateUp(t),this.update()}update(t=null){let e=this.object.position;Pe.copy(e).sub(this.target),Pe.applyQuaternion(this._quat),this._spherical.setFromVector3(Pe),this.autoRotate&&this.state===Qt.NONE&&this._rotateLeft(this._getAutoRotationAngle(t)),this.enableDamping?(this._spherical.theta+=this._sphericalDelta.theta*this.dampingFactor,this._spherical.phi+=this._sphericalDelta.phi*this.dampingFactor):(this._spherical.theta+=this._sphericalDelta.theta,this._spherical.phi+=this._sphericalDelta.phi);let i=this.minAzimuthAngle,s=this.maxAzimuthAngle;isFinite(i)&&isFinite(s)&&(i<-Math.PI?i+=ni:i>Math.PI&&(i-=ni),s<-Math.PI?s+=ni:s>Math.PI&&(s-=ni),i<=s?this._spherical.theta=Math.max(i,Math.min(s,this._spherical.theta)):this._spherical.theta=this._spherical.theta>(i+s)/2?Math.max(i,this._spherical.theta):Math.min(s,this._spherical.theta)),this._spherical.phi=Math.max(this.minPolarAngle,Math.min(this.maxPolarAngle,this._spherical.phi)),this._spherical.makeSafe(),this.enableDamping===!0?this.target.addScaledVector(this._panOffset,this.dampingFactor):this.target.add(this._panOffset),this.target.sub(this.cursor),this.target.clampLength(this.minTargetRadius,this.maxTargetRadius),this.target.add(this.cursor);let r=!1;if(this.zoomToCursor&&this._performCursorZoom||this.object.isOrthographicCamera)this._spherical.radius=this._clampDistance(this._spherical.radius);else{let a=this._spherical.radius;this._spherical.radius=this._clampDistance(this._spherical.radius*this._scale),r=a!=this._spherical.radius}if(Pe.setFromSpherical(this._spherical),Pe.applyQuaternion(this._quatInverse),e.copy(this.target).add(Pe),this.object.lookAt(this.target),this.enableDamping===!0?(this._sphericalDelta.theta*=1-this.dampingFactor,this._sphericalDelta.phi*=1-this.dampingFactor,this._panOffset.multiplyScalar(1-this.dampingFactor)):(this._sphericalDelta.set(0,0,0),this._panOffset.set(0,0,0)),this.zoomToCursor&&this._performCursorZoom){let a=null;if(this.object.isPerspectiveCamera){let o=Pe.length();a=this._clampDistance(o*this._scale);let l=o-a;this.object.position.addScaledVector(this._dollyDirection,l),this.object.updateMatrixWorld(),r=!!l}else if(this.object.isOrthographicCamera){let o=new A(this._mouse.x,this._mouse.y,0);o.unproject(this.object);let l=this.object.zoom;this.object.zoom=Math.max(this.minZoom,Math.min(this.maxZoom,this.object.zoom/this._scale)),this.object.updateProjectionMatrix(),r=l!==this.object.zoom;let c=new A(this._mouse.x,this._mouse.y,0);c.unproject(this.object),this.object.position.sub(c).add(o),this.object.updateMatrixWorld(),a=Pe.length()}else console.warn("WARNING: OrbitControls.js encountered an unknown camera type - zoom to cursor disabled."),this.zoomToCursor=!1;a!==null&&(this.screenSpacePanning?this.target.set(0,0,-1).transformDirection(this.object.matrix).multiplyScalar(a).add(this.object.position):(bl.origin.copy(this.object.position),bl.direction.set(0,0,-1).transformDirection(this.object.matrix),Math.abs(this.object.up.dot(bl.direction))<Z_?this.object.lookAt(this.target):(of.setFromNormalAndCoplanarPoint(this.object.up,this.target),bl.intersectPlane(of,this.target))))}else if(this.object.isOrthographicCamera){let a=this.object.zoom;this.object.zoom=Math.max(this.minZoom,Math.min(this.maxZoom,this.object.zoom/this._scale)),a!==this.object.zoom&&(this.object.updateProjectionMatrix(),r=!0)}return this._scale=1,this._performCursorZoom=!1,r||this._lastPosition.distanceToSquared(this.object.position)>mh||8*(1-this._lastQuaternion.dot(this.object.quaternion))>mh||this._lastTargetPosition.distanceToSquared(this.target)>mh?(this.dispatchEvent(af),this._lastPosition.copy(this.object.position),this._lastQuaternion.copy(this.object.quaternion),this._lastTargetPosition.copy(this.target),!0):!1}_getAutoRotationAngle(t){return t!==null?ni/60*this.autoRotateSpeed*t:ni/60/60*this.autoRotateSpeed}_getZoomScale(t){let e=Math.abs(t*.01);return Math.pow(.95,this.zoomSpeed*e)}_rotateLeft(t){this._sphericalDelta.theta-=t}_rotateUp(t){this._sphericalDelta.phi-=t}_panLeft(t,e){Pe.setFromMatrixColumn(e,0),Pe.multiplyScalar(-t),this._panOffset.add(Pe)}_panUp(t,e){this.screenSpacePanning===!0?Pe.setFromMatrixColumn(e,1):(Pe.setFromMatrixColumn(e,0),Pe.crossVectors(this.object.up,Pe)),Pe.multiplyScalar(t),this._panOffset.add(Pe)}_pan(t,e){let i=this.domElement;if(this.object.isPerspectiveCamera){let s=this.object.position;Pe.copy(s).sub(this.target);let r=Pe.length();r*=Math.tan(this.object.fov/2*Math.PI/180),this._panLeft(2*t*r/i.clientHeight,this.object.matrix),this._panUp(2*e*r/i.clientHeight,this.object.matrix)}else this.object.isOrthographicCamera?(this._panLeft(t*(this.object.right-this.object.left)/this.object.zoom/i.clientWidth,this.object.matrix),this._panUp(e*(this.object.top-this.object.bottom)/this.object.zoom/i.clientHeight,this.object.matrix)):(console.warn("WARNING: OrbitControls.js encountered an unknown camera type - pan disabled."),this.enablePan=!1)}_dollyOut(t){this.object.isPerspectiveCamera||this.object.isOrthographicCamera?this._scale/=t:(console.warn("WARNING: OrbitControls.js encountered an unknown camera type - dolly/zoom disabled."),this.enableZoom=!1)}_dollyIn(t){this.object.isPerspectiveCamera||this.object.isOrthographicCamera?this._scale*=t:(console.warn("WARNING: OrbitControls.js encountered an unknown camera type - dolly/zoom disabled."),this.enableZoom=!1)}_updateZoomParameters(t,e){if(!this.zoomToCursor)return;this._performCursorZoom=!0;let i=this.domElement.getBoundingClientRect(),s=t-i.left,r=e-i.top,a=i.width,o=i.height;this._mouse.x=s/a*2-1,this._mouse.y=-(r/o)*2+1,this._dollyDirection.set(this._mouse.x,this._mouse.y,1).unproject(this.object).sub(this.object.position).normalize()}_clampDistance(t){return Math.max(this.minDistance,Math.min(this.maxDistance,t))}_handleMouseDownRotate(t){this._rotateStart.set(t.clientX,t.clientY)}_handleMouseDownDolly(t){this._updateZoomParameters(t.clientX,t.clientY),this._dollyStart.set(t.clientX,t.clientY)}_handleMouseDownPan(t){this._panStart.set(t.clientX,t.clientY)}_handleMouseMoveRotate(t){this._rotateEnd.set(t.clientX,t.clientY),this._rotateDelta.subVectors(this._rotateEnd,this._rotateStart).multiplyScalar(this.rotateSpeed);let e=this.domElement;this._rotateLeft(ni*this._rotateDelta.x/e.clientHeight),this._rotateUp(ni*this._rotateDelta.y/e.clientHeight),this._rotateStart.copy(this._rotateEnd),this.update()}_handleMouseMoveDolly(t){this._dollyEnd.set(t.clientX,t.clientY),this._dollyDelta.subVectors(this._dollyEnd,this._dollyStart),this._dollyDelta.y>0?this._dollyOut(this._getZoomScale(this._dollyDelta.y)):this._dollyDelta.y<0&&this._dollyIn(this._getZoomScale(this._dollyDelta.y)),this._dollyStart.copy(this._dollyEnd),this.zoomToCursor&&(this._performCursorZoom=!0,this._dollyDirection.set(this._mouse.x,this._mouse.y,1).unproject(this.object).sub(this.object.position).normalize()),this.update()}_handleMouseMovePan(t){this._panEnd.set(t.clientX,t.clientY),this._panDelta.subVectors(this._panEnd,this._panStart).multiplyScalar(this.panSpeed),this._pan(this._panDelta.x,this._panDelta.y),this._panStart.copy(this._panEnd),this.update()}_handleMouseWheel(t){this._updateZoomParameters(t.clientX,t.clientY),t.deltaY<0?this._dollyIn(this._getZoomScale(t.deltaY)):t.deltaY>0&&this._dollyOut(this._getZoomScale(t.deltaY)),this.update()}_handleKeyDown(t){let e=!1;switch(t.code){case this.keys.UP:t.ctrlKey||t.metaKey||t.shiftKey?this.enableRotate&&this._rotateUp(ni*this.keyRotateSpeed/this.domElement.clientHeight):this.enablePan&&this._pan(0,this.keyPanSpeed),e=!0;break;case this.keys.BOTTOM:t.ctrlKey||t.metaKey||t.shiftKey?this.enableRotate&&this._rotateUp(-ni*this.keyRotateSpeed/this.domElement.clientHeight):this.enablePan&&this._pan(0,-this.keyPanSpeed),e=!0;break;case this.keys.LEFT:t.ctrlKey||t.metaKey||t.shiftKey?this.enableRotate&&this._rotateLeft(ni*this.keyRotateSpeed/this.domElement.clientHeight):this.enablePan&&this._pan(this.keyPanSpeed,0),e=!0;break;case this.keys.RIGHT:t.ctrlKey||t.metaKey||t.shiftKey?this.enableRotate&&this._rotateLeft(-ni*this.keyRotateSpeed/this.domElement.clientHeight):this.enablePan&&this._pan(-this.keyPanSpeed,0),e=!0;break}e&&(t.preventDefault(),this.update())}_handleTouchStartRotate(t){if(this._pointers.length===1)this._rotateStart.set(t.pageX,t.pageY);else{let e=this._getSecondPointerPosition(t),i=.5*(t.pageX+e.x),s=.5*(t.pageY+e.y);this._rotateStart.set(i,s)}}_handleTouchStartPan(t){if(this._pointers.length===1)this._panStart.set(t.pageX,t.pageY);else{let e=this._getSecondPointerPosition(t),i=.5*(t.pageX+e.x),s=.5*(t.pageY+e.y);this._panStart.set(i,s)}}_handleTouchStartDolly(t){let e=this._getSecondPointerPosition(t),i=t.pageX-e.x,s=t.pageY-e.y,r=Math.sqrt(i*i+s*s);this._dollyStart.set(0,r)}_handleTouchStartDollyPan(t){this.enableZoom&&this._handleTouchStartDolly(t),this.enablePan&&this._handleTouchStartPan(t)}_handleTouchStartDollyRotate(t){this.enableZoom&&this._handleTouchStartDolly(t),this.enableRotate&&this._handleTouchStartRotate(t)}_handleTouchMoveRotate(t){if(this._pointers.length==1)this._rotateEnd.set(t.pageX,t.pageY);else{let i=this._getSecondPointerPosition(t),s=.5*(t.pageX+i.x),r=.5*(t.pageY+i.y);this._rotateEnd.set(s,r)}this._rotateDelta.subVectors(this._rotateEnd,this._rotateStart).multiplyScalar(this.rotateSpeed);let e=this.domElement;this._rotateLeft(ni*this._rotateDelta.x/e.clientHeight),this._rotateUp(ni*this._rotateDelta.y/e.clientHeight),this._rotateStart.copy(this._rotateEnd)}_handleTouchMovePan(t){if(this._pointers.length===1)this._panEnd.set(t.pageX,t.pageY);else{let e=this._getSecondPointerPosition(t),i=.5*(t.pageX+e.x),s=.5*(t.pageY+e.y);this._panEnd.set(i,s)}this._panDelta.subVectors(this._panEnd,this._panStart).multiplyScalar(this.panSpeed),this._pan(this._panDelta.x,this._panDelta.y),this._panStart.copy(this._panEnd)}_handleTouchMoveDolly(t){let e=this._getSecondPointerPosition(t),i=t.pageX-e.x,s=t.pageY-e.y,r=Math.sqrt(i*i+s*s);this._dollyEnd.set(0,r),this._dollyDelta.set(0,Math.pow(this._dollyEnd.y/this._dollyStart.y,this.zoomSpeed)),this._dollyOut(this._dollyDelta.y),this._dollyStart.copy(this._dollyEnd);let a=(t.pageX+e.x)*.5,o=(t.pageY+e.y)*.5;this._updateZoomParameters(a,o)}_handleTouchMoveDollyPan(t){this.enableZoom&&this._handleTouchMoveDolly(t),this.enablePan&&this._handleTouchMovePan(t)}_handleTouchMoveDollyRotate(t){this.enableZoom&&this._handleTouchMoveDolly(t),this.enableRotate&&this._handleTouchMoveRotate(t)}_addPointer(t){this._pointers.push(t.pointerId)}_removePointer(t){delete this._pointerPositions[t.pointerId];for(let e=0;e<this._pointers.length;e++)if(this._pointers[e]==t.pointerId){this._pointers.splice(e,1);return}}_isTrackingPointer(t){for(let e=0;e<this._pointers.length;e++)if(this._pointers[e]==t.pointerId)return!0;return!1}_trackPointer(t){let e=this._pointerPositions[t.pointerId];e===void 0&&(e=new et,this._pointerPositions[t.pointerId]=e),e.set(t.pageX,t.pageY)}_getSecondPointerPosition(t){let e=t.pointerId===this._pointers[0]?this._pointers[1]:this._pointers[0];return this._pointerPositions[e]}_customWheelEvent(t){let e=t.deltaMode,i={clientX:t.clientX,clientY:t.clientY,deltaY:t.deltaY};switch(e){case 1:i.deltaY*=16;break;case 2:i.deltaY*=100;break}return t.ctrlKey&&!this._controlActive&&(i.deltaY*=10),i}};function K_(n){this.enabled!==!1&&(this._pointers.length===0&&(this.domElement.setPointerCapture(n.pointerId),this.domElement.ownerDocument.addEventListener("pointermove",this._onPointerMove),this.domElement.ownerDocument.addEventListener("pointerup",this._onPointerUp)),!this._isTrackingPointer(n)&&(this._addPointer(n),n.pointerType==="touch"?this._onTouchStart(n):this._onMouseDown(n),this._cursorStyle==="grab"&&(this.domElement.style.cursor="grabbing")))}function J_(n){this.enabled!==!1&&(n.pointerType==="touch"?this._onTouchMove(n):this._onMouseMove(n))}function $_(n){switch(this._removePointer(n),this._pointers.length){case 0:this.domElement.releasePointerCapture(n.pointerId),this.domElement.ownerDocument.removeEventListener("pointermove",this._onPointerMove),this.domElement.ownerDocument.removeEventListener("pointerup",this._onPointerUp),this.dispatchEvent(lf),this.state=Qt.NONE,this._cursorStyle==="grab"&&(this.domElement.style.cursor="grab");break;case 1:let t=this._pointers[0],e=this._pointerPositions[t];this._onTouchStart({pointerId:t,pageX:e.x,pageY:e.y});break}}function j_(n){let t;switch(n.button){case 0:t=this.mouseButtons.LEFT;break;case 1:t=this.mouseButtons.MIDDLE;break;case 2:t=this.mouseButtons.RIGHT;break;default:t=-1}switch(t){case yn.DOLLY:if(this.enableZoom===!1)return;this._handleMouseDownDolly(n),this.state=Qt.DOLLY;break;case yn.ROTATE:if(n.ctrlKey||n.metaKey||n.shiftKey){if(this.enablePan===!1)return;this._handleMouseDownPan(n),this.state=Qt.PAN}else{if(this.enableRotate===!1)return;this._handleMouseDownRotate(n),this.state=Qt.ROTATE}break;case yn.PAN:if(n.ctrlKey||n.metaKey||n.shiftKey){if(this.enableRotate===!1)return;this._handleMouseDownRotate(n),this.state=Qt.ROTATE}else{if(this.enablePan===!1)return;this._handleMouseDownPan(n),this.state=Qt.PAN}break;default:this.state=Qt.NONE}this.state!==Qt.NONE&&this.dispatchEvent(gh)}function Q_(n){switch(this.state){case Qt.ROTATE:if(this.enableRotate===!1)return;this._handleMouseMoveRotate(n);break;case Qt.DOLLY:if(this.enableZoom===!1)return;this._handleMouseMoveDolly(n);break;case Qt.PAN:if(this.enablePan===!1)return;this._handleMouseMovePan(n);break}}function t1(n){this.enabled===!1||this.enableZoom===!1||this.state!==Qt.NONE||(n.preventDefault(),this.dispatchEvent(gh),this._handleMouseWheel(this._customWheelEvent(n)),this.dispatchEvent(lf))}function e1(n){this.enabled!==!1&&this._handleKeyDown(n)}function i1(n){switch(this._trackPointer(n),this._pointers.length){case 1:switch(this.touches.ONE){case Li.ROTATE:if(this.enableRotate===!1){this.state=Qt.NONE;break}this._handleTouchStartRotate(n),this.state=Qt.TOUCH_ROTATE;break;case Li.PAN:if(this.enablePan===!1){this.state=Qt.NONE;break}this._handleTouchStartPan(n),this.state=Qt.TOUCH_PAN;break;default:this.state=Qt.NONE}break;case 2:switch(this.touches.TWO){case Li.DOLLY_PAN:if(this.enableZoom===!1&&this.enablePan===!1)return;this._handleTouchStartDollyPan(n),this.state=Qt.TOUCH_DOLLY_PAN;break;case Li.DOLLY_ROTATE:if(this.enableZoom===!1&&this.enableRotate===!1)return;this._handleTouchStartDollyRotate(n),this.state=Qt.TOUCH_DOLLY_ROTATE;break;default:this.state=Qt.NONE}break;default:this.state=Qt.NONE}this.state!==Qt.NONE&&this.dispatchEvent(gh)}function n1(n){switch(this._trackPointer(n),this.state){case Qt.TOUCH_ROTATE:if(this.enableRotate===!1)return;this._handleTouchMoveRotate(n),this.update();break;case Qt.TOUCH_PAN:if(this.enablePan===!1)return;this._handleTouchMovePan(n),this.update();break;case Qt.TOUCH_DOLLY_PAN:if(this.enableZoom===!1&&this.enablePan===!1)return;this._handleTouchMoveDollyPan(n),this.update();break;case Qt.TOUCH_DOLLY_ROTATE:if(this.enableZoom===!1&&this.enableRotate===!1)return;this._handleTouchMoveDollyRotate(n),this.update();break;default:this.state=Qt.NONE}}function s1(n){this.enabled!==!1&&n.preventDefault()}function r1(n){n.key==="Control"&&(this._controlActive=!0,this.domElement.ownerDocument.addEventListener("keyup",this._interceptControlUp,{passive:!0,capture:!0}))}function a1(n){n.key==="Control"&&(this._controlActive=!1,this.domElement.ownerDocument.removeEventListener("keyup",this._interceptControlUp,{passive:!0,capture:!0}))}function El(n,t){let e=new wl(n,t);return e.enableDamping=!0,e.dampingFactor=.1,e.minDistance=1,e.maxDistance=45,e.maxPolarAngle=Math.PI*.6,e.enablePan=!0,e.screenSpacePanning=!0,e.touches.ONE=Li.ROTATE,e.touches.TWO=Li.DOLLY_PAN,e}function En(n,t,e,i){let[s,r,a]=e;if(i==="overview")n.position.set(s*.58,r*1.25,a*.77),t.target.set(0,r*.3,-a*.08);else if(i==="audience")n.position.set(s*.09,r*.48,a*.76),t.target.set(0,r*.38,-a*.28);else throw new Error("Unknown preview camera");t.update()}function cf(n,t,e,i){return i==="Home"?(En(n,t,e,"audience"),!0):["+","=","-"].includes(i)?(t.dollyIn(i==="-"?1.1:.9),!0):!1}function hf(n){let[t,e]=n.values();return{x:(t.x+e.x)/2,y:(t.y+e.y)/2,span:Math.hypot(e.x-t.x,e.y-t.y)}}var Tl=class{points=new Map;pair=null;kind=null;add(t,e,i){this.points.set(t,{x:e,y:i}),this.rebase()}remove(t){this.points.delete(t),this.rebase()}rebase(){this.pair=this.points.size===2?hf(this.points):null,this.kind=null}sample(){if(!this.pair)return null;let t=hf(this.points),e=t.x-this.pair.x,i=t.y-this.pair.y,s=Math.abs(t.span-this.pair.span)/2,r=Math.hypot(e,i);if(!this.kind){if(Math.max(r,s)<4)return null;this.kind=s>r?"pinch":"walk"}let a=Math.log(Math.max(1,t.span)/Math.max(1,this.pair.span));return this.pair=t,{kind:this.kind,x:e,y:i,zoom:a}}};var xh={KeyW:"forward",KeyK:"forward",ArrowUp:"forward",KeyS:"backward",KeyJ:"backward",ArrowDown:"backward",KeyA:"left",KeyH:"left",ArrowLeft:"left",KeyD:"right",KeyL:"right",ArrowRight:"right",KeyQ:"down",KeyE:"up"},o1=new Set(Object.values(xh)),uf=[];function l1(n,t,e){let i=t.lengthSq()?13:19,s=-Math.expm1(-i*e),r=n.clone().sub(t).multiplyScalar(s/i).addScaledVector(t,e);return n.lerp(t,s),r}var _h=n=>Array.isArray(n)&&n.length===3&&n.every(Number.isFinite),ff=n=>[n?.[0]||12,n?.[1]||5,n?.[2]||10].map(t=>Ce(t,1,100)),c1=n=>n?.isContentEditable||/^(INPUT|TEXTAREA|SELECT)$/.test(n?.tagName||""),df=(n,t,e)=>`${n},${t},${e}`;function pf(n){return new Map(n.map(t=>[String(t.id),t]))}function h1(n,t=pf(n?.rig?.fixtures||[]),e){let i=n?.output?.values||{},s=[];for(let r of e||Object.keys(i)){let a=t.get(r)?.position;if(!_h(a))continue;let o=Xs(i[r]),l=o[0]*.2126+o[1]*.7152+o[2]*.0722;l>1e-4&&s.push({id:r,position:a.slice(),energy:l})}return s.sort((r,a)=>a.energy-r.energy||r.id.localeCompare(a.id)).slice(0,16)}function mf(n,t,e=[],i=n){let[s,r,a]=ff(t),o=[-s/2+.25,.2,-a/2+.25],l=[s/2-.25,r-.2,a/2-.25],c=n.map((h,u)=>Ce(h,Math.min(o[u],i[u]),Math.max(l[u],i[u])));for(let h of e){if(!_h(h.position))continue;let u=c.map((f,p)=>f-h.position[p]),d=Math.hypot(...u);if(d<.45){let f=d>1e-5?u.map(p=>p/d):[0,-1,0];c=h.position.map((p,v)=>p+f[v]*.45)}}return c.map((h,u)=>Ce(h,Math.min(o[u],i[u]),Math.max(l[u],i[u])))}function u1(n,t,e=0){let[i,s,r]=ff(t);if(!n)return{position:[i*.28,s*.55,r*.38],target:[0,s*.4,-r*.12]};let[a,o,l]=n.position,c=Math.atan2(-l||1,-a)+Math.sin(e)*.32,h=Ce(Math.min(i,r)*.22,1.1,2.4);return{position:mf([a+Math.cos(c)*h,Ce(o-.65,.5,s-.4),l+Math.sin(c)*h],t,[],[0,s/2,0]),target:n.position.slice()}}var Zs=class{constructor({camera:t,controls:e,canvas:i,room:s,snapshot:r,visibleLights:a,active:o,reducedMotion:l,changed:c=()=>{}}){Object.assign(this,{camera:t,controls:e,canvas:i,room:s,snapshot:r,visibleLights:a,active:o,reducedMotion:l,changed:c}),this.mode="orbit",this.speed=2,this.keys=new Set,this.buttons=new Set,this.listeners=new Set,this.velocity=new A,this.swipe=new et,this.look=new et,this.gesture=new Tl,this.zoom=0,this.interacting=!1,this.disposed=!1,this.clock=0,this.subjectId=null,this.visitStarted=0,this.abort=new AbortController;let h=this.abort.signal;i.addEventListener("keydown",u=>this.keydown(u),{signal:h}),i.addEventListener("keyup",u=>{this.keys.delete(u.code),this.boost=u.shiftKey},{signal:h}),i.addEventListener("blur",()=>this.clearInput(),{signal:h}),window.addEventListener("blur",()=>this.clearInput(),{signal:h}),document.addEventListener("visibilitychange",()=>{this.clearInput(),this.lastTime=null},{signal:h}),i.addEventListener("pointerdown",u=>this.pointerDown(u),{signal:h,capture:!0}),i.addEventListener("pointermove",u=>this.pointerMove(u),{signal:h,capture:!0});for(let u of["pointerup","pointercancel","lostpointercapture"])i.addEventListener(u,d=>this.pointerUp(d),{signal:h,capture:!0});i.addEventListener("wheel",u=>this.wheel(u),{signal:h,capture:!0,passive:!1}),this.animate=u=>{this.frame=null,!(this.disposed||!this.active())&&(this.tick(u),this.refreshActivity())},this.refreshActivity()}refreshActivity(){this.disposed||!this.active()?(this.frame!=null&&cancelAnimationFrame(this.frame),this.frame=null,this.lastTime=null,this.clearInput(),this.controls.enabled=!1):this.frame==null&&(this.controls.enabled=this.mode==="orbit"&&!this.interacting,this.frame=requestAnimationFrame(this.animate))}state(){return{mode:this.mode,speed:this.speed,interacting:this.interacting,"reduced-motion":!!this.reducedMotion(),"tour-fixture":this.subjectId,"tour-waiting":this.mode==="tour"&&!this.subjectId}}notify(){let t=this.state(),e=JSON.stringify(t);if(e!==this.lastNotice){this.lastNotice=e;for(let i of this.listeners)i(t)}}subscribe(t){return this.listeners.add(t),t(this.state()),()=>this.listeners.delete(t)}setMode(t,e=!1){if(!["orbit","walk","tour"].includes(t))throw new Error("Unknown camera navigation mode");this.clearInput(e),this.mode=t,this.lastTime=null,this.subjectId=null,this.visitStarted=this.clock,this.controls.enabled=t==="orbit"&&!this.interacting,this.notify()}setSpeed(t){if(!Number.isFinite(t)||t<.25||t>8)throw new Error("Camera speed must be 0.25\u20138 metres per second");this.speed=t,this.notify()}setInteractionActive(t){this.interacting=!!t,this.clearInput(),this.lastTime=null,this.controls.enabled=this.mode==="orbit"&&!this.interacting,this.notify()}clearInput(t=!1){if(!t){let e=[...this.gesture.points.keys()];this.gesture.points.clear(),this.gesture.rebase();for(let i of e)this.canvas.hasPointerCapture?.(i)&&this.canvas.releasePointerCapture(i)}this.velocity.set(0,0,0),this.swipe.set(0,0),this.look.set(0,0),this.zoom=0,this.keys.clear(),this.buttons.clear(),this.boost=!1,this.pointer&&this.canvas.hasPointerCapture?.(this.pointer.id)&&this.canvas.releasePointerCapture(this.pointer.id),this.pointer=null}move(t,e){if(!o1.has(t))throw new Error("Unknown camera direction");if(!e){this.buttons.delete(t);return}this.interacting||!this.active()||(this.mode!=="walk"&&this.setMode("walk"),this.buttons.add(t))}keydown(t){if(!(c1(t.target)||t.altKey||t.metaKey||t.ctrlKey||this.interacting||!this.active())){if(xh[t.code]){t.preventDefault(),this.mode!=="walk"&&this.setMode("walk"),this.keys.add(t.code),this.boost=t.shiftKey;return}if(t.key==="Escape"){this.setMode("orbit");return}["+","=","-","Home"].includes(t.key)&&(this.mode!=="orbit"&&this.setMode("orbit"),cf(this.camera,this.controls,this.room(),t.key)&&(t.preventDefault(),this.changed()))}}queueSwipe(t,e){let i=this.speed*2/Math.max(1,this.canvas.clientHeight);this.swipe.x+=t*i,this.swipe.y-=e*i}wheel(t){if(!this.active()||this.interacting||t.altKey||t.metaKey||!t.ctrlKey&&t.deltaMode!==0&&this.mode==="orbit")return;if(t.preventDefault(),t.stopImmediatePropagation(),t.ctrlKey){this.mode==="tour"&&this.setMode("orbit"),this.zoom-=Ce(t.deltaY*.01,-1,1);return}this.mode!=="walk"&&this.setMode("walk");let e=t.deltaMode===1?16:t.deltaMode===2?this.canvas.clientHeight:1;this.queueSwipe(t.deltaX*e,t.deltaY*e)}pointerDown(t){if(!(this.interacting||!this.active()||t.button!==0)){if(this.canvas.focus({preventScroll:!0}),this.mode==="tour"&&this.setMode("walk"),t.pointerType==="touch"){t.preventDefault(),t.stopImmediatePropagation(),this.gesture.add(t.pointerId,t.clientX,t.clientY),this.canvas.setPointerCapture(t.pointerId);return}this.mode!=="walk"||this.pointer||(t.preventDefault(),t.stopImmediatePropagation(),this.pointer={id:t.pointerId,x:t.clientX,y:t.clientY},this.canvas.setPointerCapture(t.pointerId))}}pointerMove(t){let e=this.gesture.points.get(t.pointerId),i=e||(this.pointer?.id===t.pointerId?this.pointer:null);!i||this.interacting||(t.preventDefault(),t.stopImmediatePropagation(),(!e||this.gesture.points.size===1)&&(this.look.x+=t.clientX-i.x,this.look.y+=t.clientY-i.y),i.x=t.clientX,i.y=t.clientY)}pointerUp(t){let e=this.gesture.points.has(t.pointerId);if(!e&&this.pointer?.id!==t.pointerId)return;t.stopImmediatePropagation();let i=t.type!=="pointerup";e?(i||this.sampleGesture(),this.gesture.remove(t.pointerId)):this.pointer=null,i&&(this.swipe.set(0,0),this.zoom=0,this.look.set(0,0)),this.canvas.hasPointerCapture?.(t.pointerId)&&this.canvas.releasePointerCapture(t.pointerId)}sampleGesture(){let t=this.gesture.sample();if(t){if(t.kind==="pinch"){this.zoom+=t.zoom;return}this.mode!=="walk"&&this.setMode("walk",!0),this.queueSwipe(t.x,t.y)}}updateView(t,e){let i=this.reducedMotion()?1:-Math.expm1(-24*t);if(Math.abs(this.zoom)>1e-4){let a=this.zoom*i;this.zoom-=a,this.mode==="orbit"?this.controls.dollyIn(Math.exp(-a)):this.translate(this.camera.getWorldDirection(new A).multiplyScalar(a*this.speed),e),this.changed()}if(this.look.lengthSq()<1e-4)return;let s=this.look.x*i,r=this.look.y*i;if(this.look.multiplyScalar(1-i),this.mode==="orbit"){let a=2*Math.PI/Math.max(1,this.canvas.clientHeight);this.controls.rotateLeft(s*a),this.controls.rotateUp(r*a)}else{let a=new gi().setFromQuaternion(this.camera.quaternion,"YXZ");a.y-=s*.004,a.x=Ce(a.x-r*.004,-Math.PI*.48,Math.PI*.48),this.camera.quaternion.setFromEuler(a),this.controls.target.copy(this.camera.position).add(this.camera.getWorldDirection(new A).multiplyScalar(2))}this.changed()}walk(t,e){if(!this.keys.size&&!this.buttons.size&&this.velocity.lengthSq()<1e-7&&this.swipe.lengthSq()<1e-9)return;let i=new Set([...this.buttons,...Array.from(this.keys,h=>xh[h])]),s=new A(Number(i.has("right"))-Number(i.has("left")),Number(i.has("up"))-Number(i.has("down")),Number(i.has("forward"))-Number(i.has("backward")));s.normalize().multiplyScalar(this.speed*(this.boost?2:1));let r=l1(this.velocity,s,t),a=this.reducedMotion()?1:-Math.expm1(-24*t);r.x+=this.swipe.x*a,r.z+=this.swipe.y*a,this.swipe.multiplyScalar(1-a);let o=this.camera.getWorldDirection(new A);o.y=0,o.normalize();let l=new A().crossVectors(o,this.camera.up).normalize(),c=o.multiplyScalar(r.z).addScaledVector(l,r.x);c.y=r.y,this.translate(c,e),this.changed()}tick(t){let e=this.active()&&!this.interacting;!e&&this.wasActive&&this.clearInput(),this.wasActive=e;let i=e&&this.lastTime!=null?Ce((t-this.lastTime)/1e3,0,.05):0;if(this.lastTime=e?t:null,this.controls.enabled=e&&this.mode==="orbit",!e||(this.sampleGesture(),!i))return;if(this.mode==="orbit"){this.updateView(i,uf),this.controls.update(i);return}let s=this.snapshot(),r=s?.rig?.fixtures||uf;if(this.updateView(i,r),this.mode==="walk"){this.walk(i,r);return}if(this.reducedMotion())return;this.clock+=i;let a=this.visibleLights?.();(s!==this.subjectSnapshot||a!==this.subjectLights)&&(r!==this.subjectFixtures&&(this.subjectFixtures=r,this.fixtureIndex=pf(r)),this.subjectLights=a,this.subjectSnapshot=s,this.subjects=h1(s,this.fixtureIndex,a));let o=this.subjects||[],l=o.find(f=>f.id===this.subjectId);if(!l||this.clock-this.visitStarted>=8){let f=l?(o.indexOf(l)+1)%o.length:0;l=o[f],this.subjectId=l?.id||null,this.visitStarted=this.clock,this.notify()}let c=u1(l,this.room(),this.clock*.22),u=new A(...c.position).sub(this.camera.position),d=u.length();d>0&&u.multiplyScalar(Math.min(1,this.speed*i/d)),this.translate(u,r),this.controls.target.lerp(new A(...c.target),1-Math.exp(-i*2)),this.camera.lookAt(this.controls.target),this.changed()}translate(t,e){if(e!==this.collisionFixtures){this.collisionFixtures=e,this.collisionGrid=new Map;for(let r of e)if(_h(r.position)){let a=df(...r.position.map(Math.floor));this.collisionGrid.has(a)||this.collisionGrid.set(a,[]),this.collisionGrid.get(a).push(r)}}let i=Math.max(1,Math.ceil(t.length()/.15)),s=t.clone().divideScalar(i);for(let r=0;r<i;r++){let a=this.camera.position.clone(),o=a.clone().add(s),[l,c,h]=o.toArray().map(Math.floor),u=[];for(let p=-1;p<=1;p++)for(let v=-1;v<=1;v++)for(let g=-1;g<=1;g++)u.push(...this.collisionGrid.get(df(l+p,c+v,h+g))||[]);let f=new A(...mf(o.toArray(),this.room(),u,a.toArray())).sub(a).clampLength(0,s.length());this.camera.position.copy(a).add(f),this.controls.target.add(this.camera.position.clone().sub(a))}}dispose(){this.disposed||(this.disposed=!0,this.clearInput(),this.abort.abort(),cancelAnimationFrame(this.frame),this.listeners.clear())}};var Al=class extends ai{constructor(){super(),this.name="RoomEnvironment",this.position.y=-3.5;let t=new ve;t.deleteAttribute("uv");let e=new be({side:ke}),i=new be,s=new Ur(16777215,900,28,2);s.position.set(.418,16.199,.3),this.add(s);let r=new Ut(t,e);r.position.set(-.757,13.219,.717),r.scale.set(31.713,28.305,28.591),this.add(r);let a=new kn(t,i,6),o=new de;o.position.set(-10.906,2.009,1.846),o.rotation.set(0,-.195,0),o.scale.set(2.328,7.905,4.651),o.updateMatrix(),a.setMatrixAt(0,o.matrix),o.position.set(-5.607,-.754,-.758),o.rotation.set(0,.994,0),o.scale.set(1.97,1.534,3.955),o.updateMatrix(),a.setMatrixAt(1,o.matrix),o.position.set(6.167,.857,7.803),o.rotation.set(0,.561,0),o.scale.set(3.927,6.285,3.687),o.updateMatrix(),a.setMatrixAt(2,o.matrix),o.position.set(-2.017,.018,6.124),o.rotation.set(0,.333,0),o.scale.set(2.002,4.566,2.064),o.updateMatrix(),a.setMatrixAt(3,o.matrix),o.position.set(2.291,-.756,-2.621),o.rotation.set(0,-.286,0),o.scale.set(1.546,1.552,1.496),o.updateMatrix(),a.setMatrixAt(4,o.matrix),o.position.set(-2.193,-.369,-5.547),o.rotation.set(0,.516,0),o.scale.set(3.875,3.487,2.986),o.updateMatrix(),a.setMatrixAt(5,o.matrix),this.add(a);let l=new Ut(t,Ks(50));l.position.set(-16.116,14.37,8.208),l.scale.set(.1,2.428,2.739),this.add(l);let c=new Ut(t,Ks(50));c.position.set(-16.109,18.021,-8.207),c.scale.set(.1,2.425,2.751),this.add(c);let h=new Ut(t,Ks(17));h.position.set(14.904,12.198,-1.832),h.scale.set(.15,4.265,6.331),this.add(h);let u=new Ut(t,Ks(43));u.position.set(-.462,8.89,14.52),u.scale.set(4.38,5.441,.088),this.add(u);let d=new Ut(t,Ks(20));d.position.set(3.235,11.486,-12.541),d.scale.set(2.5,2,.1),this.add(d);let f=new Ut(t,Ks(100));f.position.set(0,20,0),f.scale.set(1,.1,1),this.add(f)}dispose(){let t=new Set;this.traverse(e=>{e.isMesh&&(t.add(e.geometry),t.add(e.material))});for(let e of t)e.dispose()}};function Ks(n){return new Lr({color:0,emissive:16777215,emissiveIntensity:n})}var Tn={name:"CopyShader",uniforms:{tDiffuse:{value:null},opacity:{value:1}},vertexShader:`

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


		}`};var ti=class{constructor(){this.isPass=!0,this.enabled=!0,this.needsSwap=!0,this.clear=!1,this.renderToScreen=!1}setSize(){}render(){console.error("THREE.Pass: .render() must be implemented in derived pass.")}dispose(){}},d1=new qn(-1,1,1,-1,0,1),vh=class extends ee{constructor(){super(),this.setAttribute("position",new Kt([-1,3,0,-1,-1,0,3,-1,0],3)),this.setAttribute("uv",new Kt([0,2,0,0,2,0],2))}},f1=new vh,Mi=class{constructor(t){this._mesh=new Ut(f1,t)}dispose(){this._mesh.geometry.dispose()}render(t){t.render(this._mesh,d1)}get material(){return this._mesh.material}set material(t){this._mesh.material=t}};var Qn=class extends ti{constructor(t,e="tDiffuse"){super(),this.textureID=e,this.uniforms=null,this.material=null,t instanceof oe?(this.uniforms=t.uniforms,this.material=t):t&&(this.uniforms=Fi.clone(t.uniforms),this.material=new oe({name:t.name!==void 0?t.name:"unspecified",defines:Object.assign({},t.defines),uniforms:this.uniforms,vertexShader:t.vertexShader,fragmentShader:t.fragmentShader})),this._fsQuad=new Mi(this.material)}render(t,e,i){this.uniforms[this.textureID]&&(this.uniforms[this.textureID].value=i.texture),this._fsQuad.material=this.material,this.renderToScreen?(t.setRenderTarget(null),this._fsQuad.render(t)):(t.setRenderTarget(e),this.clear&&t.clear(t.autoClearColor,t.autoClearDepth,t.autoClearStencil),this._fsQuad.render(t))}dispose(){this.material.dispose(),this._fsQuad.dispose()}};var la=class extends ti{constructor(t,e){super(),this.scene=t,this.camera=e,this.clear=!0,this.needsSwap=!1,this.inverse=!1}render(t,e,i){let s=t.getContext(),r=t.state;r.buffers.color.setMask(!1),r.buffers.depth.setMask(!1),r.buffers.color.setLocked(!0),r.buffers.depth.setLocked(!0);let a,o;this.inverse?(a=0,o=1):(a=1,o=0),r.buffers.stencil.setTest(!0),r.buffers.stencil.setOp(s.REPLACE,s.REPLACE,s.REPLACE),r.buffers.stencil.setFunc(s.ALWAYS,a,4294967295),r.buffers.stencil.setClear(o),r.buffers.stencil.setLocked(!0),t.setRenderTarget(i),this.clear&&t.clear(),t.render(this.scene,this.camera),t.setRenderTarget(e),this.clear&&t.clear(),t.render(this.scene,this.camera),r.buffers.color.setLocked(!1),r.buffers.depth.setLocked(!1),r.buffers.color.setMask(!0),r.buffers.depth.setMask(!0),r.buffers.stencil.setLocked(!1),r.buffers.stencil.setFunc(s.EQUAL,1,4294967295),r.buffers.stencil.setOp(s.KEEP,s.KEEP,s.KEEP),r.buffers.stencil.setLocked(!0)}},Rl=class extends ti{constructor(){super(),this.needsSwap=!1}render(t){t.state.buffers.stencil.setLocked(!1),t.state.buffers.stencil.setTest(!1)}};var Cl=class{constructor(t,e){if(this.renderer=t,this._pixelRatio=t.getPixelRatio(),e===void 0){let i=t.getSize(new et);this._width=i.width,this._height=i.height,e=new ce(this._width*this._pixelRatio,this._height*this._pixelRatio,{type:fe}),e.texture.name="EffectComposer.rt1"}else this._width=e.width,this._height=e.height;this.renderTarget1=e,this.renderTarget2=e.clone(),this.renderTarget2.texture.name="EffectComposer.rt2",this.writeBuffer=this.renderTarget1,this.readBuffer=this.renderTarget2,this.renderToScreen=!0,this.passes=[],this.copyPass=new Qn(Tn),this.copyPass.material.blending=Ni,this.timer=new Or}swapBuffers(){let t=this.readBuffer;this.readBuffer=this.writeBuffer,this.writeBuffer=t}addPass(t){this.passes.push(t),t.setSize(this._width*this._pixelRatio,this._height*this._pixelRatio)}insertPass(t,e){this.passes.splice(e,0,t),t.setSize(this._width*this._pixelRatio,this._height*this._pixelRatio)}removePass(t){let e=this.passes.indexOf(t);e!==-1&&this.passes.splice(e,1)}isLastEnabledPass(t){for(let e=t+1;e<this.passes.length;e++)if(this.passes[e].enabled)return!1;return!0}render(t){this.timer.update(),t===void 0&&(t=this.timer.getDelta());let e=this.renderer.getRenderTarget(),i=!1;for(let s=0,r=this.passes.length;s<r;s++){let a=this.passes[s];if(a.enabled!==!1){if(a.renderToScreen=this.renderToScreen&&this.isLastEnabledPass(s),a.render(this.renderer,this.writeBuffer,this.readBuffer,t,i),a.needsSwap){if(i){let o=this.renderer.getContext(),l=this.renderer.state.buffers.stencil;l.setFunc(o.NOTEQUAL,1,4294967295),this.copyPass.render(this.renderer,this.writeBuffer,this.readBuffer,t),l.setFunc(o.EQUAL,1,4294967295)}this.swapBuffers()}la!==void 0&&(a instanceof la?i=!0:a instanceof Rl&&(i=!1))}}this.renderer.setRenderTarget(e)}reset(t){if(t===void 0){let e=this.renderer.getSize(new et);this._pixelRatio=this.renderer.getPixelRatio(),this._width=e.width,this._height=e.height,t=this.renderTarget1.clone(),t.setSize(this._width*this._pixelRatio,this._height*this._pixelRatio)}this.renderTarget1.dispose(),this.renderTarget2.dispose(),this.renderTarget1=t,this.renderTarget2=t.clone(),this.writeBuffer=this.renderTarget1,this.readBuffer=this.renderTarget2}setSize(t,e){this._width=t,this._height=e;let i=this._width*this._pixelRatio,s=this._height*this._pixelRatio;this.renderTarget1.setSize(i,s),this.renderTarget2.setSize(i,s);for(let r=0;r<this.passes.length;r++)this.passes[r].setSize(i,s)}setPixelRatio(t){this._pixelRatio=t,this.setSize(this._width,this._height)}dispose(){this.renderTarget1.dispose(),this.renderTarget2.dispose(),this.copyPass.dispose()}};var Pl=class extends ti{constructor(t,e=1){super();let i=Tn;this.map=t,this.opacity=e,this.needsSwap=!1,this.uniforms=Fi.clone(i.uniforms),this.material=new oe({uniforms:this.uniforms,vertexShader:i.vertexShader,fragmentShader:i.fragmentShader,depthTest:!1,depthWrite:!1,premultipliedAlpha:!0}),this._fsQuad=new Mi(null)}render(t,e,i){let s=t.autoClear;t.autoClear=!1,this._fsQuad.material=this.material,this.uniforms.opacity.value=this.opacity,this.uniforms.tDiffuse.value=this.map,this.material.transparent=this.opacity<1,t.setRenderTarget(this.renderToScreen?null:i),this.clear&&t.clear(),this._fsQuad.render(t),t.autoClear=s}dispose(){this.material.dispose(),this._fsQuad.dispose()}};var gf={name:"LuminosityHighPassShader",uniforms:{tDiffuse:{value:null},luminosityThreshold:{value:1},smoothWidth:{value:1},defaultColor:{value:new Mt(0)},defaultOpacity:{value:0}},vertexShader:`

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

		}`};var Js=class n extends ti{constructor(t,e=1,i,s){super(),this.strength=e,this.radius=i,this.threshold=s,this.resolution=t!==void 0?new et(t.x,t.y):new et(256,256),this.clearColor=new Mt(0,0,0),this.needsSwap=!1,this.renderTargetsHorizontal=[],this.renderTargetsVertical=[],this.nMips=5;let r=Math.round(this.resolution.x/2),a=Math.round(this.resolution.y/2);this.renderTargetBright=new ce(r,a,{type:fe,depthBuffer:!1}),this.renderTargetBright.texture.name="UnrealBloomPass.bright",this.renderTargetBright.texture.generateMipmaps=!1;for(let h=0;h<this.nMips;h++){let u=new ce(r,a,{type:fe,depthBuffer:!1});u.texture.name="UnrealBloomPass.h"+h,u.texture.generateMipmaps=!1,this.renderTargetsHorizontal.push(u);let d=new ce(r,a,{type:fe,depthBuffer:!1});d.texture.name="UnrealBloomPass.v"+h,d.texture.generateMipmaps=!1,this.renderTargetsVertical.push(d),r=Math.round(r/2),a=Math.round(a/2)}let o=gf;this.highPassUniforms=Fi.clone(o.uniforms),this.highPassUniforms.luminosityThreshold.value=s,this.highPassUniforms.smoothWidth.value=.01,this.materialHighPassFilter=new oe({uniforms:this.highPassUniforms,vertexShader:o.vertexShader,fragmentShader:o.fragmentShader}),this.separableBlurMaterials=[];let l=[6,10,14,18,22];r=Math.round(this.resolution.x/2),a=Math.round(this.resolution.y/2);for(let h=0;h<this.nMips;h++)this.separableBlurMaterials.push(this._getSeparableBlurMaterial(l[h])),this.separableBlurMaterials[h].uniforms.invSize.value=new et(1/r,1/a),r=Math.round(r/2),a=Math.round(a/2);this.compositeMaterial=this._getCompositeMaterial(this.nMips),this.compositeMaterial.uniforms.blurTexture1.value=this.renderTargetsVertical[0].texture,this.compositeMaterial.uniforms.blurTexture2.value=this.renderTargetsVertical[1].texture,this.compositeMaterial.uniforms.blurTexture3.value=this.renderTargetsVertical[2].texture,this.compositeMaterial.uniforms.blurTexture4.value=this.renderTargetsVertical[3].texture,this.compositeMaterial.uniforms.blurTexture5.value=this.renderTargetsVertical[4].texture,this.compositeMaterial.uniforms.bloomStrength.value=e,this.compositeMaterial.uniforms.bloomRadius.value=.1;let c=[1,.8,.6,.4,.2];this.compositeMaterial.uniforms.bloomFactors.value=c,this.bloomTintColors=[new A(1,1,1),new A(1,1,1),new A(1,1,1),new A(1,1,1),new A(1,1,1)],this.compositeMaterial.uniforms.bloomTintColors.value=this.bloomTintColors,this.copyUniforms=Fi.clone(Tn.uniforms),this.blendMaterial=new oe({uniforms:this.copyUniforms,vertexShader:Tn.vertexShader,fragmentShader:Tn.fragmentShader,premultipliedAlpha:!0,blending:zr,depthTest:!1,depthWrite:!1,transparent:!0}),this._oldClearColor=new Mt,this._oldClearAlpha=1,this._basic=new _i,this._fsQuad=new Mi(null)}dispose(){for(let t=0;t<this.renderTargetsHorizontal.length;t++)this.renderTargetsHorizontal[t].dispose();for(let t=0;t<this.renderTargetsVertical.length;t++)this.renderTargetsVertical[t].dispose();this.renderTargetBright.dispose();for(let t=0;t<this.separableBlurMaterials.length;t++)this.separableBlurMaterials[t].dispose();this.compositeMaterial.dispose(),this.blendMaterial.dispose(),this._basic.dispose(),this._fsQuad.dispose()}setSize(t,e){let i=Math.round(t/2),s=Math.round(e/2);this.renderTargetBright.setSize(i,s);for(let r=0;r<this.nMips;r++)this.renderTargetsHorizontal[r].setSize(i,s),this.renderTargetsVertical[r].setSize(i,s),this.separableBlurMaterials[r].uniforms.invSize.value=new et(1/i,1/s),i=Math.round(i/2),s=Math.round(s/2)}render(t,e,i,s,r){t.getClearColor(this._oldClearColor),this._oldClearAlpha=t.getClearAlpha();let a=t.autoClear;t.autoClear=!1,t.setClearColor(this.clearColor,0),r&&t.state.buffers.stencil.setTest(!1),this.renderToScreen&&(this._fsQuad.material=this._basic,this._basic.map=i.texture,t.setRenderTarget(null),t.clear(),this._fsQuad.render(t)),this.highPassUniforms.tDiffuse.value=i.texture,this.highPassUniforms.luminosityThreshold.value=this.threshold,this._fsQuad.material=this.materialHighPassFilter,t.setRenderTarget(this.renderTargetBright),t.clear(),this._fsQuad.render(t);let o=this.renderTargetBright;for(let l=0;l<this.nMips;l++)this._fsQuad.material=this.separableBlurMaterials[l],this.separableBlurMaterials[l].uniforms.colorTexture.value=o.texture,this.separableBlurMaterials[l].uniforms.direction.value=n.BlurDirectionX,t.setRenderTarget(this.renderTargetsHorizontal[l]),t.clear(),this._fsQuad.render(t),this.separableBlurMaterials[l].uniforms.colorTexture.value=this.renderTargetsHorizontal[l].texture,this.separableBlurMaterials[l].uniforms.direction.value=n.BlurDirectionY,t.setRenderTarget(this.renderTargetsVertical[l]),t.clear(),this._fsQuad.render(t),o=this.renderTargetsVertical[l];this._fsQuad.material=this.compositeMaterial,this.compositeMaterial.uniforms.bloomStrength.value=this.strength,this.compositeMaterial.uniforms.bloomRadius.value=this.radius,this.compositeMaterial.uniforms.bloomTintColors.value=this.bloomTintColors,t.setRenderTarget(this.renderTargetsHorizontal[0]),t.clear(),this._fsQuad.render(t),this._fsQuad.material=this.blendMaterial,this.copyUniforms.tDiffuse.value=this.renderTargetsHorizontal[0].texture,r&&t.state.buffers.stencil.setTest(!0),this.renderToScreen?(t.setRenderTarget(null),this._fsQuad.render(t)):(t.setRenderTarget(i),this._fsQuad.render(t)),t.setClearColor(this._oldClearColor,this._oldClearAlpha),t.autoClear=a}_getSeparableBlurMaterial(t){let e=[],i=t/3;for(let a=0;a<t;a++)e.push(.39894*Math.exp(-.5*a*a/(i*i))/i);let s=[],r=[];for(let a=1;a<t;a+=2){let o=e[a],l=a+1<t?e[a+1]:0,c=o+l;s.push((a*o+(a+1)*l)/c),r.push(c)}return new oe({defines:{KERNEL_PAIRS:s.length},uniforms:{colorTexture:{value:null},invSize:{value:new et(.5,.5)},direction:{value:new et(.5,.5)},centerWeight:{value:e[0]},gaussianOffsets:{value:s},gaussianWeights:{value:r}},vertexShader:`

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

				}`})}_getCompositeMaterial(t){return new oe({defines:{NUM_MIPS:t},uniforms:{blurTexture1:{value:null},blurTexture2:{value:null},blurTexture3:{value:null},blurTexture4:{value:null},blurTexture5:{value:null},bloomStrength:{value:1},bloomFactors:{value:null},bloomTintColors:{value:null},bloomRadius:{value:0}},vertexShader:`

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

				}`})}};Js.BlurDirectionX=new et(1,0);Js.BlurDirectionY=new et(0,1);var yh={name:"FXAAShader",uniforms:{tDiffuse:{value:null},resolution:{value:new et(1/1024,1/512)}},vertexShader:`

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

		}`};var ca={name:"OutputShader",uniforms:{tDiffuse:{value:null},toneMappingExposure:{value:1}},vertexShader:`
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

		}`};var Il=class extends ti{constructor(){super(),this.isOutputPass=!0,this.uniforms=Fi.clone(ca.uniforms),this.material=new Rs({name:ca.name,uniforms:this.uniforms,vertexShader:ca.vertexShader,fragmentShader:ca.fragmentShader}),this._fsQuad=new Mi(this.material),this._outputColorSpace=null,this._toneMapping=null}render(t,e,i){this.uniforms.tDiffuse.value=i.texture,this.uniforms.toneMappingExposure.value=t.toneMappingExposure,(this._outputColorSpace!==t.outputColorSpace||this._toneMapping!==t.toneMapping)&&(this._outputColorSpace=t.outputColorSpace,this._toneMapping=t.toneMapping,this.material.defines={},Wt.getTransfer(this._outputColorSpace)===Jt&&(this.material.defines.SRGB_TRANSFER=""),this._toneMapping===kr?this.material.defines.LINEAR_TONE_MAPPING="":this._toneMapping===Hr?this.material.defines.REINHARD_TONE_MAPPING="":this._toneMapping===Vr?this.material.defines.CINEON_TONE_MAPPING="":this._toneMapping===Kn?this.material.defines.ACES_FILMIC_TONE_MAPPING="":this._toneMapping===Wr?this.material.defines.AGX_TONE_MAPPING="":this._toneMapping===Xr?this.material.defines.NEUTRAL_TONE_MAPPING="":this._toneMapping===Gr&&(this.material.defines.CUSTOM_TONE_MAPPING=""),this.material.needsUpdate=!0),this.renderToScreen===!0?(t.setRenderTarget(null),this._fsQuad.render(t)):(t.setRenderTarget(e),this.clear&&t.clear(t.autoClearColor,t.autoClearDepth,t.autoClearStencil),this._fsQuad.render(t))}dispose(){this.material.dispose(),this._fsQuad.dispose()}};function xf(){let n=new Il;return n.uniforms.extendedDisplay={value:!1},n.material.fragmentShader=`uniform bool extendedDisplay;
`+n.material.fragmentShader.replace("gl_FragColor.rgb = ACESFilmicToneMapping( gl_FragColor.rgb );",`if (extendedDisplay) {
       vec3 exposed = max(vec3(0.0), gl_FragColor.rgb * toneMappingExposure);
       gl_FragColor.rgb = exposed / (1.0 + max(exposed.r, max(exposed.g, exposed.b)) * 0.25);
     } else {
       gl_FragColor.rgb = ACESFilmicToneMapping(gl_FragColor.rgb);
     }`),n}var Dl=class extends Qn{constructor(){super(yh)}setSize(t,e){this.uniforms.resolution.value.set(1/t,1/e)}},Ll=class extends Qn{constructor(){super({uniforms:{tDiffuse:{value:null},grain:{value:.0015},phase:{value:0}},vertexShader:yh.vertexShader,fragmentShader:`
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
        }`})}};var vf=(n,t)=>{if(typeof document>"u")return new OffscreenCanvas(n,t);let e=document.createElement("canvas");return e.width=n,e.height=t,e};function $s(n,t){let e=Math.sin(n*127.1+t*311.7)*43758.5453123;return e-Math.floor(e)}function _f(n,t){let e=vf(n,n),i=e.getContext("2d"),s=i.createImageData(n,n);for(let o=0;o<n;o++)for(let l=0;l<n;l++){let c=$s(l,o),h=$s(Math.floor(l/24),Math.floor(o/24)),u=t==="floor"?108+c*28+h*10:139+c*28+h*10;t==="floor"&&(l<2||o<2)&&(u*=.62);let d=(o*n+l)*4;s.data[d]=u,s.data[d+1]=u*.98,s.data[d+2]=u*.94,s.data[d+3]=255}i.putImageData(s,0,0),i.strokeStyle="rgba(26,26,25,.12)",i.lineWidth=1;for(let o=0;o<40;o++){let l=$s(o,6)*n,c=$s(o,8)*n;i.beginPath(),i.moveTo(l,c),i.lineTo(l+$s(o,2)*80,c+$s(o,1)*5),i.stroke()}let r=new Es(e);r.wrapS=r.wrapT=Fn,r.colorSpace=Fe,r.anisotropy=4;let a=r.clone();return a.colorSpace=Ui,{map:r,bump:a}}function p1(){let n=vf(64,64),t=n.getContext("2d");t.fillStyle="#353a3c",t.fillRect(0,0,64,64),t.fillStyle="#080a0b";for(let i=0;i<64;i+=8)for(let s=0;s<64;s+=8)t.beginPath(),t.arc(s+(i%16?4:0),i,2.5,0,Math.PI*2),t.fill();let e=new Es(n);return e.wrapS=e.wrapT=Fn,e.repeat.set(3,5),e.colorSpace=Fe,e}function yf(){let n=_f(512,"floor"),t=_f(512,"wall");n.map.repeat.set(5,4),n.bump.repeat.copy(n.map.repeat),t.map.repeat.set(3,2),t.bump.repeat.copy(t.map.repeat);let e={floor:new be({color:12960184,map:n.map,bumpMap:n.bump,bumpScale:.028,roughness:.42,metalness:.12}),wall:new be({color:11972772,map:t.map,bumpMap:t.bump,bumpScale:.035,roughness:.91,metalness:.02}),metal:new be({color:9147033,roughness:.28,metalness:.88}),blackMetal:new be({color:2303786,roughness:.34,metalness:.75}),housing:new be({color:1777186,roughness:.43,metalness:.32}),rubber:new be({color:1118997,roughness:.85,metalness:0}),curtain:new be({color:2632495,roughness:1,metalness:0,side:je}),stage:new be({color:2105894,roughness:.73,metalness:.14}),grille:new be({color:9606807,map:p1(),roughness:.7,metalness:.55}),yellow:new be({color:10189117,roughness:.65,metalness:.05})};return e.floor.userData.meanMapReflectance=.22,e.wall.userData.meanMapReflectance=.34,e.wall.shadowSide=je,e}function Mf(n){let t=new Set;for(let e of Object.values(n)){for(let i of Object.values(e))i?.isTexture&&t.add(i);e.dispose()}for(let e of t)e.dispose()}function ha(n){let t=n["lens-radius-m"];return Number.isFinite(t)&&t>=0?t:.119}function Sf(n,t,e,i){let s=new A(...t).normalize(),r=new A(...n),a=i/Math.tan(e);return{optical:r,virtual:r.clone().addScaledVector(s,-a),axis:s,apexDistance:a,radius:i}}function Mh(n,t,e){n.onBeforeCompile=i=>{i.uniforms.tolglowApexDepth=t,i.uniforms.tolglowPatchRadius=e,i.fragmentShader=`uniform float tolglowApexDepth[18]; uniform float tolglowPatchRadius[18];
`+i.fragmentShader;let s=zt.lights_fragment_begin.replace("getSpotLightInfo( spotLight, geometryPosition, directLight );",`getSpotLightInfo( spotLight, geometryPosition, directLight );
       directLight.color *= step(tolglowApexDepth[i], dot(spotLight.position - geometryPosition, spotLight.direction));
       { float patchD2 = dot(spotLight.position - geometryPosition, spotLight.position - geometryPosition);
       directLight.color *= patchD2 / max(1.e-9, patchD2 + tolglowPatchRadius[i]*tolglowPatchRadius[i]); }`);i.fragmentShader=i.fragmentShader.replace("#include <lights_fragment_begin>",s)},n.customProgramCacheKey=()=>"tolglow-finite-aperture-patch-v2"}var ua=new A;function Si(n,t,e,i,s,r){let a=2*Math.PI*s/4,o=Math.max(r-2*s,0),l=Math.PI/4;ua.copy(t),ua[i]=0,ua.normalize();let c=.5*a/(a+o),h=1-ua.angleTo(n)/l;return Math.sign(ua[e])===1?h*c:o/(a+o)+c+c*(1-h)}var js=class n extends ve{constructor(t=1,e=1,i=1,s=2,r=.1){let a=s*2+1;if(r=Math.min(t/2,e/2,i/2,r),super(1,1,1,a,a,a),this.type="RoundedBoxGeometry",this.parameters={width:t,height:e,depth:i,segments:s,radius:r},a===1)return;let o=this.toNonIndexed();this.index=null,this.attributes.position=o.attributes.position,this.attributes.normal=o.attributes.normal,this.attributes.uv=o.attributes.uv;let l=new A,c=new A,h=new A(t,e,i).divideScalar(2).subScalar(r),u=this.attributes.position.array,d=this.attributes.normal.array,f=this.attributes.uv.array,p=u.length/6,v=new A,g=.5/a;for(let m=0,_=0;m<u.length;m+=3,_+=2)switch(l.fromArray(u,m),c.copy(l),c.x-=Math.sign(c.x)*g,c.y-=Math.sign(c.y)*g,c.z-=Math.sign(c.z)*g,c.normalize(),u[m+0]=h.x*Math.sign(l.x)+c.x*r,u[m+1]=h.y*Math.sign(l.y)+c.y*r,u[m+2]=h.z*Math.sign(l.z)+c.z*r,d[m+0]=c.x,d[m+1]=c.y,d[m+2]=c.z,Math.floor(m/p)){case 0:v.set(1,0,0),f[_+0]=Si(v,c,"z","y",r,i),f[_+1]=1-Si(v,c,"y","z",r,e);break;case 1:v.set(-1,0,0),f[_+0]=1-Si(v,c,"z","y",r,i),f[_+1]=1-Si(v,c,"y","z",r,e);break;case 2:v.set(0,1,0),f[_+0]=1-Si(v,c,"x","z",r,t),f[_+1]=Si(v,c,"z","x",r,i);break;case 3:v.set(0,-1,0),f[_+0]=1-Si(v,c,"x","z",r,t),f[_+1]=1-Si(v,c,"z","x",r,i);break;case 4:v.set(0,0,1),f[_+0]=1-Si(v,c,"x","y",r,t),f[_+1]=1-Si(v,c,"y","x",r,e);break;case 5:v.set(0,0,-1),f[_+0]=Si(v,c,"x","y",r,t),f[_+1]=1-Si(v,c,"y","x",r,e);break}}static fromJSON(t){return new n(t.width,t.height,t.depth,t.segments,t.radius)}};function ge(n,t,e,i,s=0){let r=s?new js(...t,2,s):new ve(...t),a=new Ut(r,i);return a.position.set(...e),a.castShadow=a.receiveShadow=!0,n.add(a),a}function Sh(n,t,e,i,s,r=8){let a=new A().subVectors(new A(...e),new A(...t)),o=new Ut(new vi(i,i,a.length(),r),s);return o.position.set(...t).addScaledVector(a,.5),o.quaternion.setFromUnitVectors(new A(0,1,0),a.normalize()),o.castShadow=o.receiveShadow=!0,n.add(o),o}function bf(n,t){let e=new xe;e.name=`truss:${n.id}`,e.position.fromArray(n.position);let i=(n["rotation-deg"]||[0,0,0]).map(p=>p*Math.PI/180);e.rotation.set(...i,"ZYX");let s=Math.min(40,Math.max(.3,n.length)),r=s/2,a=.17,o=[];for(let p of[-a,a])for(let v of[-a,a])o.push([[-r,p,v],[r,p,v],.025]);let l=Math.ceil(s/.55),c=s/l;for(let p=0;p<l;p++){let v=-r+p*c,g=v+c,m=p%2?-1:1;for(let _ of[-a,a])o.push([[v,-a*m,_],[g,a*m,_],.012]),o.push([[v,_,-a*m],[g,_,a*m],.012])}for(let p=0;p<=l;p++){let v=-r+p*c;o.push([[v,-a,-a],[v,a,-a],.012],[[v,-a,a],[v,a,a],.012],[[v,-a,-a],[v,-a,a],.012],[[v,a,-a],[v,a,a],.012])}let h=new kn(new vi(1,1,1,8),t.metal,o.length),u=new de,d=new A,f=new A(0,1,0);return o.forEach(([p,v,g],m)=>{d.set(v[0]-p[0],v[1]-p[1],v[2]-p[2]);let _=d.length();u.position.set((p[0]+v[0])/2,(p[1]+v[1])/2,(p[2]+v[2])/2),u.quaternion.setFromUnitVectors(f,d.normalize()),u.scale.set(g,_,g),u.updateMatrix(),h.setMatrixAt(m,u.matrix)}),h.castShadow=h.receiveShadow=!0,e.add(h),e}function wf(n,t){let e=ha({}),i=new xe;i.name=`fixture:${n.id}`,i.position.fromArray(n.position||[0,0,0]);let s=ge(i,[.52,.15,.36],[0,.62,0],t.housing,.025);ge(i,[.44,.035,.27],[0,.71,0],t.blackMetal,.012);for(let d of[-.17,.17])ge(i,[.055,.05,.11],[d,.73,0],t.rubber,.008);let r=new xe;i.add(r);for(let d of[-.22,.22]){ge(r,[.065,.36,.11],[d,.39,0],t.housing,.02);let f=new Ut(new vi(.055,.055,.085,16),t.metal);f.rotation.z=Math.PI/2,f.position.set(d,.23,0),r.add(f)}let a=new xe;i.add(a),ge(a,[.34,.36,.3],[0,.24,0],t.housing,.055);let o=new Ut(new vi(e*1.22,e*1.01,.16,32),t.blackMetal);o.position.y=.081,o.castShadow=o.receiveShadow=!0,a.add(o);let l=new Dr({color:2109754,metalness:.35,roughness:.1,clearcoat:1,clearcoatRoughness:.04,emissive:new Mt(0,0,0),emissiveIntensity:1}),c=new Ut(new Ar(e,40),l);c.rotation.x=Math.PI/2,a.add(c);let h=new Ut(new Ir(e+.014,.012,8,40),t.metal);h.rotation.x=Math.PI/2,h.position.y=-.002,a.add(h);for(let d=0;d<6;d++)ge(a,[.27,.012,.012],[0,.17+d*.034,.156],t.rubber);ge(s,[.12,.035,.004],[0,-.015,.182],t.rubber,.003);let u={root:i,head:a,yoke:r,lensMaterial:l,setAperture(d){let f=ha(d),p=f/e;this.lensRadius=f,o.scale.set(p,1,p),c.scale.set(p,p,1);let v=(f+.014)/(e+.014);h.scale.set(v,v,1)},dispose(){l.dispose()}};return u.setAperture(n),u}function bh(n){let t=new Set;n.traverse(e=>{e.geometry&&t.add(e.geometry),e.userData.ownsReceiverMaterial&&e.material.dispose()});for(let e of t)e.dispose()}function m1(n,t){if(t?.method!=="bracketed-neutral-card"||!Number.isFinite(t.estimate)||t.estimate<0)return n;let e=n.clone();return e.map=null,e.metalness=0,e.roughness=1,e.color.setScalar(Math.min(1,t.estimate)),e.userData.meanMapReflectance=1,e.onBeforeCompile=n.onBeforeCompile,e.customProgramCacheKey=n.customProgramCacheKey,e}function Ef(n,t,e=[]){let[i,s,r]=n,a=new xe;a.name="venue-context";let o=new Ut(new Di(i,r),t.floor);o.userData.receiver="floor",o.rotation.x=-Math.PI/2,o.receiveShadow=!0,a.add(o),ge(a,[i+.2,.16,r+.2],[0,-.09,0],t.blackMetal),ge(a,[i,s,.18],[0,s/2,-r/2-.09],t.wall).userData.receiver="back";for(let _ of[-i/2,i/2]){ge(a,[.18,s,r],[_+Math.sign(_)*.09,s/2,0],t.wall).userData.receiver=_<0?"left":"right";for(let S of[-r*.4,r*.12,r*.4])ge(a,[.2,s,.2],[_-Math.sign(_)*.13,s/2,S],t.blackMetal,.008),ge(a,[.36,.08,.36],[_-Math.sign(_)*.13,.04,S],t.metal,.008);Sh(a,[_-Math.sign(_)*.22,s-.25,-r/2],[_-Math.sign(_)*.22,s-.25,r/2],.075,t.blackMetal,12)}let l=i*.68,c=Math.min(2.3,r*.27),h=-r/2+c/2+.16;ge(a,[l,.42,c],[0,.21,h],t.stage,.025).userData.receiver="stage",ge(a,[l,.025,.045],[0,.435,h+c/2-.02],t.stage,.008);for(let _=-l/2+1;_<l/2;_+=Math.max(1.2,l/64))ge(a,[.008,.003,c-.08],[_,.423,h],t.rubber);for(let _=0;_<2;_++)ge(a,[.85,.14*(_+1),.34],[l/2+.35,.07*(_+1),h+c/2-_*.34],t.stage,.015);let u=i*.7,d=s*.72,f=new Di(u,d,120,12),p=f.attributes.position;for(let _=0;_<p.count;_++){let S=p.getX(_),M=p.getY(_);p.setZ(_,.08*Math.sin(S*19)+.018*Math.sin(S*37+M*.5))}f.computeVertexNormals();let v=new Ut(f,t.curtain);v.position.set(0,.45+d/2,-r/2+.15),v.castShadow=v.receiveShadow=!0,a.add(v);for(let _ of[-1,1]){let S=_*i*.395,M=-r*.31;for(let T=0;T<2;T++){ge(a,[.68,.86,.58],[S,.43+T*.88,M],t.housing,.035),ge(a,[.59,.72,.014],[S,.43+T*.88,M+.3],t.grille,.01);for(let w of[-.25,.25])ge(a,[.02,.07,.018],[S+w,.18+T*.88,M+.315],t.metal)}ge(a,[.85,.5,.8],[S,.25,M+1],t.rubber,.035),ge(a,[.72,.37,.015],[S,.26,M+1.41],t.grille,.012)}for(let _ of[-r*.38,r*.05,r*.4])ge(a,[i,.17,.1],[0,s-.12,_],t.blackMetal),Sh(a,[-i/2,s-.3,_+.14],[i/2,s-.3,_+.14],.025,t.metal);let g=new Ts([new A(-i*.4,.025,-r*.1),new A(-i*.39,.025,r*.2),new A(-i*.28,.025,r*.35),new A(-i*.1,.025,r*.38)]),m=new Ut(new Vn(g,40,.012,5,!1),t.rubber);a.add(m);for(let _ of e){let{id:S,point:M,normal:T,size:w}=_;if(!Array.isArray(w)||!w.every(I=>Number.isFinite(I)&&I>0))continue;let x=new A(...T).normalize(),y=new A(..._["u-axis"]);y.addScaledVector(x,-y.dot(x)).normalize();let P=m1(t.wall,_.reflectance),C=new Ut(new Di(...w),P);P!==t.wall&&(C.userData.ownsReceiverMaterial=!0),C.position.fromArray(M),C.quaternion.setFromRotationMatrix(new Gt().makeBasis(y,x.clone().cross(y),x)),C.userData.receiver=String(S),C.receiveShadow=C.castShadow=!0,a.add(C)}return a}function g1(n,t){if(n.length<2)return null;let e=n.length-1,i=Math.min(256,e),s=Array.from({length:i+1},(f,p)=>new A(...n[Math.round(e*p/i)].position)),r=new Pr;for(let f=1;f<s.length;f++)s[f-1].equals(s[f])||r.add(new As(s[f-1],s[f]));if(!r.curves.length)return null;let a=i*2,o=12,l=new Vn(r,a,n[0]["diffuser-radius-m"],o),c=new _e(new Float32Array(l.attributes.position.count*3),3);c.setUsage(bn),l.setAttribute("color",c);let h=new be({color:8488587,roughness:.72,vertexColors:!0});h.onBeforeCompile=f=>{f.fragmentShader=f.fragmentShader.replace("#include <color_fragment>","").replace("#include <emissivemap_fragment>","totalEmissiveRadiance += 5.0 * vColor.rgb;")};let u=new Ut(l,h);u.name="diffuser:"+n[0]["pixel-layout-id"],u.receiveShadow=!0,t.add(u);let d=n.map(f=>({id:String(f.id),fixture:f}));return{rows:d,update(f){let p=d.map(({id:v})=>Xs(f[v]));for(let v=0;v<=a;v++){let g=e*v/a,m=Math.floor(g),_=g-m,S=p[Math.max(0,m-1)],M=p[m],T=p[Math.min(e,m+1)],w=p[Math.min(e,m+2)],x=M.map((y,P)=>(1-_)*(.16*S[P]+.68*y+.16*T[P])+_*(.16*y+.68*T[P]+.16*w[P]));for(let y=0;y<=o;y++)c.setXYZ(v*(o+1)+y,...x)}c.needsUpdate=!0},dispose(){h.dispose()}}}function Tf(n,t){let e=new Map;for(let i of n){let s=i["pixel-layout-id"],r=e.get(s);r?r.push(i):e.set(s,[i])}return[...e.values()].map(i=>g1(i.sort((s,r)=>s["pixel-index"]-r["pixel-index"]),t)).filter(Boolean)}var{exp:x1,max:wh}=Math;function Eh(n,t,e){return x1(-(wh(0,n)*wh(0,t)*wh(0,e)))}var Af=`float fogTransmission(float extinction, float density, float distance) {
  return exp((-((max(0.0, extinction) * max(0.0, density)) * max(0.0, distance))));
}`;var Rf=(n,t,e)=>Math.max(t,Math.min(e,n)),Th=n=>Number.isFinite(n)&&n>=0,Cf=n=>Array.isArray(n)&&n.length===3&&n.every(Number.isFinite);var Pf=(n,t)=>n===t||Array.isArray(n)&&Array.isArray(t)&&n.length===t.length&&n.every((e,i)=>e===t[i]);function _1(n,t){return n===t||n["extinction-per-m"]===t["extinction-per-m"]&&n["reference-path-m"]===t["reference-path-m"]&&n["fog-level"]===t["fog-level"]&&n["volume-m3"]===t["volume-m3"]&&n["mixed-m3"]===t["mixed-m3"]&&n["mixed-density"]===t["mixed-density"]&&n.sources.length===t.sources.length&&n.sources.every((e,i)=>{let s=t.sources[i];return e["fixture-id"]===s["fixture-id"]&&e["flow-m3-s"]===s["flow-m3-s"]&&e["local-m3"]===s["local-m3"]&&e["emitted-m3"]===s["emitted-m3"]&&Pf(e.position,s.position)&&Pf(e.direction,s.direction)})}function Ah(n,t,e){return{position:[...n.position],direction:[...n.direction],flow:n["flow-m3-s"],time:t,mass:e}}function If(n,t){let e=n.mass+t.mass,i=t.mass/e,s=(r,a)=>r+i*(a-r);return{mass:e,time:s(n.time,t.time),flow:s(n.flow,t.flow),position:n.position.map((r,a)=>s(r,t.position[a])),direction:n.direction.map((r,a)=>s(r,t.direction[a]))}}function v1(n,t,e){let i=n["local-m3"],s=n["emitted-m3"];if(!i)return{emitted:s,parcels:[]};if(!t||s<t.emitted){let c=n["flow-m3-s"]>0?[.15,1,2.8]:[1,2.8,4.5];return{emitted:s,parcels:c.map((h,u)=>Ah(n,e-h*1e3,i*[.25,.45,.3][u])).reverse()}}let r=Math.min(i,s-t.emitted),a=i-r,o=t.parcels,l=o.reduce((c,h)=>c+h.mass,0);if(!a)o.length=0;else if(l)for(let c of o)c.mass*=a/l;else o.push(Ah(n,e-1500,a));if(r){let c=Ah(n,e,r),h=o.at(-1);h&&Math.floor(e/180)===Math.floor(h.time/180)?o[o.length-1]=If(h,c):o.push(c)}return o.length>8&&o.splice(0,2,If(o[0],o[1])),{emitted:s,parcels:o}}function y1(n,t,e){let i=Math.max(0,(e-t.time)*.001),s=n.grid,r=n.spacing,a=n.room,o=Math.hypot(...t.direction)||1,l=t.direction.map(I=>I/o),h=Math.min(4.5,.45+Math.sqrt(t.flow))*(1-Math.exp(-i/1.4))+.12,u=.12+.15*Math.sqrt(i)+.035*Math.min(i,15),d=u+.3*Math.exp(-i/3),f=Math.min(i,12),p=t.position[0]*.9+t.position[2]*.65,v=[.07*f+.1*Math.sin(p+f)*Math.min(1,f),.11*f,.05*f+.09*Math.sin(p+f*.6)*Math.min(1,f)],g=[-a[0]/2,0,-a[2]/2],m=t.position.map((I,L)=>Rf(I+h*l[L]+v[L],g[L],g[L]+a[L])),_=r.map(I=>Math.max(u,.48*I)),S=_.map(I=>3*Math.max(I,d)),M=(I,L)=>Rf(Math.floor((L-g[I])/r[I]),0,s[I]-1),T=m.map((I,L)=>M(L,I-S[L])),w=m.map((I,L)=>M(L,I+S[L])),x=n.advected,y=Math.max(d,.48*Math.hypot(...r)),P=0;for(let I=T[2];I<=w[2];I++)for(let L=T[1];L<=w[1];L++)for(let F=T[0];F<=w[0];F++){let D=(F+.5)*r[0]+g[0]-m[0],B=(L+.5)*r[1]+g[1]-m[1],G=(I+.5)*r[2]+g[2]-m[2],k=D*l[0]+B*l[1]+G*l[2],j=Math.exp(-.5*(((D-k*l[0])/_[0])**2+((B-k*l[1])/_[1])**2+((G-k*l[2])/_[2])**2+(k/y)**2));x[F+s[0]*(L+s[1]*I)]=j,P+=j}if(!P)return;let C=t.mass/(P*r[0]*r[1]*r[2]);for(let I=T[2];I<=w[2];I++)for(let L=T[1];L<=w[1];L++)for(let F=T[0];F<=w[0];F++){let D=F+s[0]*(L+s[1]*I);n.data[D]+=C*x[D]}}var Nl=class{constructor(){this.sources=new Map,this.sample=null,this.rebase=!0,this.paintAt=null}suspend(){this.rebase=!0}accept(t,e){let i=e["sample-time-ms"];if(!Number.isFinite(i)||!Th(e["mixed-density"])||!Array.isArray(e.sources))return!1;let s=JSON.stringify([t,e.epoch]),r=this.sample?.["sample-time-ms"];if(s===this.epoch&&(i<r||!this.rebase&&i===r&&_1(e,this.sample)))return!1;let a=this.rebase||s!==this.epoch||i-r>1e3,o=new Map,l=this.localVolume,c=this.sample?.["mixed-density"];this.localVolume=0;for(let h of e.sources){let u=h["fixture-id"];u==null||!Cf(h.position)||!Cf(h.direction)||!["local-m3","emitted-m3","flow-m3-s"].every(d=>Th(h[d]))||(this.localVolume+=h["local-m3"],o.set(u,v1(h,a?null:this.sources.get(u),i)))}return this.sources=o,this.epoch=s,this.sample=e,this.rebase=!1,this.elapsed=0,this.dirty||=a||this.localVolume>0||l>0||c!==e["mixed-density"],!0}paint(t,e){if(!this.sample)return!1;this.elapsed=Math.min(.1,this.elapsed+Math.max(0,e));let i=this.sample["sample-time-ms"]+1e3*this.elapsed;if(!this.dirty&&this.field===t&&(!this.localVolume||Math.abs(i-this.paintAt)<49.99))return!1;t.data.fill(this.sample["mixed-density"]),t.temperature.fill(0),t.sources=[];for(let{parcels:s}of this.sources.values())for(let r of s)y1(t,r,i);return t.time=i*.001%1e4,t.steps++,this.dirty=!1,this.paintAt=i,this.field=t,!0}get extinctionPerM(){let t=this.sample?.["extinction-per-m"];return Th(t)?t:1.25}stats(){return{sampleTimeMs:this.sample?.["sample-time-ms"],mixedDensity:this.sample?.["mixed-density"],extinctionPerM:this.extinctionPerM,referencePathM:this.sample?.["reference-path-m"],fogLevel:this.sample?.["fog-level"],mixedVolumeM3:this.sample?.["mixed-m3"],roomVolumeM3:this.sample?.["volume-m3"],localVolumeM3:this.localVolume,foggerCount:this.sources.size}}};var Df=[[0,0],[.68,0],[-.68,0],[0,.68],[0,-.68]],M1=n=>Array.isArray(n)&&n.length===3&&n.every(Number.isFinite),Rh=n=>Math.max(n.r,n.g,n.b);function S1(n,t,e,i,s=1.25){if(t<=0||s<=0)return 1;let r=6,[a,o,l]=n.room,[c,h,u]=n.grid,d=0;for(let f=0;f<r;f++){let p=(f+.5)/r,v=(e.x+(i.x-e.x)*p)/a+.5,g=(e.y+(i.y-e.y)*p)/o,m=(e.z+(i.z-e.z)*p)/l+.5;d+=n.sample(v*c-.5,g*h-.5,m*u-.5)}return Eh(s,t*d/r,e.distanceTo(i))}function b1(n,t,e){if(!e||!["hit","center","normal","major-axis","minor-axis"].every(d=>M1(e[d])))return;let i=e["major-radius-m"],s=e["minor-radius-m"];if(!(i>0&&s>0&&Number.isFinite(i*s)))return;let r=new A(...e.normal).normalize(),a=-t.direction.dot(r);if(a<=1e-5)return;let o=new A(...e.hit),l=o.clone().sub(t.position).dot(r)/-a;if(!(l>0&&l<t.distance-t.apexDistance))return;let c=t.position.clone().addScaledVector(t.direction,l),h=new A(...e.center).add(c.clone().sub(o)),u=t.intensity*a/(l+t.apexDistance)**2;return{id:n.id,fixture:n.root,surface:String(e["surface-id"]),origin:t.position,center:h,normal:r,major:new A(...e["major-axis"]).multiplyScalar(i),minor:new A(...e["minor-axis"]).multiplyScalar(s),radius:Math.sqrt(i*s),power:u*i*s,color:t.color}}var Ul=class{constructor(t,e,i,s=[]){if(this.chunks=[],this.targets=[],e){t.updateMatrixWorld(!0);let r=new Map(s.map(a=>[a.root,a]));for(let a of t.children){let o=[];if(a.traverse(l=>{l.isMesh&&o.push(l)}),!!o.length)if(r.has(a))this.chunks.push({meshes:o,root:a,fixture:r.get(a),sphere:new $e});else for(let l of o){let c=l.isInstancedMesh?l:l.geometry;c.boundingSphere||c.computeBoundingSphere(),this.chunks.push({meshes:[l],mesh:l,local:c.boundingSphere,sphere:new $e})}}}this.ray=new vn,this.hits=[],this.lights=Array.from({length:e},()=>{let r=new Xn(0,0,15,Math.PI*.47,1,2);return r.castShadow=!0,r.shadow.autoUpdate=!1,r.shadow.needsUpdate=!0,r.shadow.mapSize.set(i,i),r.shadow.camera.near=.015,r.shadow.bias=-1e-4,r.shadow.normalBias=.005,t.add(r,r.target),r}),this.patches=[]}addCandidate(t,e,i,s){if(!this.lights.length||!e.fixture["pseudo-fixture"])return;let r=b1(e,i,s);r&&t.push(r)}sample(t,e){let i=e.clone().sub(t.origin),s=i.length();this.ray.set(t.origin,i.normalize()),this.ray.near=.003,this.ray.far=s+.04,this.hits.length=0,this.targets.length=0;for(let{root:a,meshes:o,sphere:l}of this.chunks)a===t.fixture||t.origin.distanceToSquared(l.center)>(l.radius+this.ray.far)**2||this.ray.ray.intersectsSphere(l)&&this.targets.push(...o);let[r]=this.ray.intersectObjects(this.targets,!1,this.hits);if(!(r?.object.userData.receiver!==t.surface||Math.abs(r.distance-s)>.04))return r.object.material}resolve(t){if(this.patches.length=0,!!t.length){for(let e of this.chunks)e.fixture?(e.sphere.center.setFromMatrixPosition(e.root.matrixWorld),e.sphere.radius=Math.max(1,1.5*ha(e.fixture.fixture)+.3)):e.sphere.copy(e.local).applyMatrix4(e.mesh.matrixWorld);t.sort((e,i)=>Rh(i.color)*i.power-Rh(e.color)*e.power);for(let e of t){let i=new A,s=new Mt(0,0,0),r=0;for(let[a,o]of Df){let l=e.center.clone().addScaledVector(e.major,a).addScaledVector(e.minor,o),c=this.sample(e,l);if(!c)continue;let h=c.color.clone().multiplyScalar((1-c.metalness)*(c.userData.meanMapReflectance??1));s.add(h),i.add(l),r++}if(!(!r||Rh(s)<=0)&&(e.position=i.divideScalar(r),e.reflected=s.multiplyScalar(1/Df.length).multiply(e.color),this.patches.push(e),this.patches.length===this.lights.length))break}for(let e=0;e<this.patches.length;e++)this.lights[e].shadow.needsUpdate=!0}}illuminate(t,e,i){this.lights.forEach((s,r)=>{let a=this.patches[r];if(!a){s.intensity=0,s.color.setRGB(0,0,0);return}s.color.copy(a.reflected),s.intensity=a.power*S1(t,e,a.origin,a.position,i),s.userData.patchRadius=a.radius,s.position.copy(a.position).addScaledVector(a.normal,.018),s.target.position.copy(s.position).add(a.normal),s.target.updateMatrixWorld()})}dispose(){for(let t of this.lights)t.dispose()}};var Bl=class extends Ut{constructor(t,e,i=!1,s=!1,r=1e4){let a=new ee;super(a,e),this.isMarchingCubes=!0;let o=this,l=new Float32Array(36),c=new Float32Array(36),h=new Float32Array(36);this.enableUvs=i,this.enableColors=s,this.init=function(_){this.resolution=_,this.isolation=80,this.size=_,this.size2=this.size*this.size,this.size3=this.size2*this.size,this.halfsize=this.size/2,this.delta=2/this.size,this.yd=this.size,this.zd=this.size2,this.field=new Float32Array(this.size3),this.normal_cache=new Float32Array(this.size3*3),this.palette=new Float32Array(this.size3*3),this.count=0;let S=r*3;this.positionArray=new Float32Array(S*3);let M=new _e(this.positionArray,3);M.setUsage(bn),a.setAttribute("position",M),this.normalArray=new Float32Array(S*3);let T=new _e(this.normalArray,3);if(T.setUsage(bn),a.setAttribute("normal",T),this.enableUvs){this.uvArray=new Float32Array(S*2);let w=new _e(this.uvArray,2);w.setUsage(bn),a.setAttribute("uv",w)}if(this.enableColors){this.colorArray=new Float32Array(S*3);let w=new _e(this.colorArray,3);w.setUsage(bn),a.setAttribute("color",w)}a.boundingSphere=new $e(new A,1)};function u(_,S,M){return _+(S-_)*M}function d(_,S,M,T,w,x,y,P,C,I){let L=(M-y)/(P-y),F=o.normal_cache;l[S+0]=T+L*o.delta,l[S+1]=w,l[S+2]=x,c[S+0]=u(F[_+0],F[_+3],L),c[S+1]=u(F[_+1],F[_+4],L),c[S+2]=u(F[_+2],F[_+5],L),h[S+0]=u(o.palette[C*3+0],o.palette[I*3+0],L),h[S+1]=u(o.palette[C*3+1],o.palette[I*3+1],L),h[S+2]=u(o.palette[C*3+2],o.palette[I*3+2],L)}function f(_,S,M,T,w,x,y,P,C,I){let L=(M-y)/(P-y),F=o.normal_cache;l[S+0]=T,l[S+1]=w+L*o.delta,l[S+2]=x;let D=_+o.yd*3;c[S+0]=u(F[_+0],F[D+0],L),c[S+1]=u(F[_+1],F[D+1],L),c[S+2]=u(F[_+2],F[D+2],L),h[S+0]=u(o.palette[C*3+0],o.palette[I*3+0],L),h[S+1]=u(o.palette[C*3+1],o.palette[I*3+1],L),h[S+2]=u(o.palette[C*3+2],o.palette[I*3+2],L)}function p(_,S,M,T,w,x,y,P,C,I){let L=(M-y)/(P-y),F=o.normal_cache;l[S+0]=T,l[S+1]=w,l[S+2]=x+L*o.delta;let D=_+o.zd*3;c[S+0]=u(F[_+0],F[D+0],L),c[S+1]=u(F[_+1],F[D+1],L),c[S+2]=u(F[_+2],F[D+2],L),h[S+0]=u(o.palette[C*3+0],o.palette[I*3+0],L),h[S+1]=u(o.palette[C*3+1],o.palette[I*3+1],L),h[S+2]=u(o.palette[C*3+2],o.palette[I*3+2],L)}function v(_){let S=_*3;o.normal_cache[S]===0&&(o.normal_cache[S+0]=o.field[_-1]-o.field[_+1],o.normal_cache[S+1]=o.field[_-o.yd]-o.field[_+o.yd],o.normal_cache[S+2]=o.field[_-o.zd]-o.field[_+o.zd])}function g(_,S,M,T,w){let x=T+1,y=T+o.yd,P=T+o.zd,C=x+o.yd,I=x+o.zd,L=T+o.yd+o.zd,F=x+o.yd+o.zd,D=0,B=o.field[T],G=o.field[x],k=o.field[y],j=o.field[C],W=o.field[P],K=o.field[I],Q=o.field[L],dt=o.field[F];B<w&&(D|=1),G<w&&(D|=2),k<w&&(D|=8),j<w&&(D|=4),W<w&&(D|=16),K<w&&(D|=32),Q<w&&(D|=128),dt<w&&(D|=64);let yt=w1[D];if(yt===0)return 0;let q=o.delta,at=_+q,gt=S+q,Et=M+q;yt&1&&(v(T),v(x),d(T*3,0,w,_,S,M,B,G,T,x)),yt&2&&(v(x),v(C),f(x*3,3,w,at,S,M,G,j,x,C)),yt&4&&(v(y),v(C),d(y*3,6,w,_,gt,M,k,j,y,C)),yt&8&&(v(T),v(y),f(T*3,9,w,_,S,M,B,k,T,y)),yt&16&&(v(P),v(I),d(P*3,12,w,_,S,Et,W,K,P,I)),yt&32&&(v(I),v(F),f(I*3,15,w,at,S,Et,K,dt,I,F)),yt&64&&(v(L),v(F),d(L*3,18,w,_,gt,Et,Q,dt,L,F)),yt&128&&(v(P),v(L),f(P*3,21,w,_,S,Et,W,Q,P,L)),yt&256&&(v(T),v(P),p(T*3,24,w,_,S,M,B,W,T,P)),yt&512&&(v(x),v(I),p(x*3,27,w,at,S,M,G,K,x,I)),yt&1024&&(v(C),v(F),p(C*3,30,w,at,gt,M,j,dt,C,F)),yt&2048&&(v(y),v(L),p(y*3,33,w,_,gt,M,k,Q,y,L)),D<<=4;let rt,Dt,se,Rt=0,Ht=0;for(;Ol[D+Ht]!=-1;)rt=D+Ht,Dt=rt+1,se=rt+2,m(l,c,h,3*Ol[rt],3*Ol[Dt],3*Ol[se]),Ht+=3,Rt++;return Rt}function m(_,S,M,T,w,x){let y=o.count*3;if(o.positionArray[y+0]=_[T],o.positionArray[y+1]=_[T+1],o.positionArray[y+2]=_[T+2],o.positionArray[y+3]=_[w],o.positionArray[y+4]=_[w+1],o.positionArray[y+5]=_[w+2],o.positionArray[y+6]=_[x],o.positionArray[y+7]=_[x+1],o.positionArray[y+8]=_[x+2],o.material.flatShading===!0){let P=(S[T+0]+S[w+0]+S[x+0])/3,C=(S[T+1]+S[w+1]+S[x+1])/3,I=(S[T+2]+S[w+2]+S[x+2])/3;o.normalArray[y+0]=P,o.normalArray[y+1]=C,o.normalArray[y+2]=I,o.normalArray[y+3]=P,o.normalArray[y+4]=C,o.normalArray[y+5]=I,o.normalArray[y+6]=P,o.normalArray[y+7]=C,o.normalArray[y+8]=I}else o.normalArray[y+0]=S[T+0],o.normalArray[y+1]=S[T+1],o.normalArray[y+2]=S[T+2],o.normalArray[y+3]=S[w+0],o.normalArray[y+4]=S[w+1],o.normalArray[y+5]=S[w+2],o.normalArray[y+6]=S[x+0],o.normalArray[y+7]=S[x+1],o.normalArray[y+8]=S[x+2];if(o.enableUvs){let P=o.count*2;o.uvArray[P+0]=_[T+0],o.uvArray[P+1]=_[T+2],o.uvArray[P+2]=_[w+0],o.uvArray[P+3]=_[w+2],o.uvArray[P+4]=_[x+0],o.uvArray[P+5]=_[x+2]}o.enableColors&&(o.colorArray[y+0]=M[T+0],o.colorArray[y+1]=M[T+1],o.colorArray[y+2]=M[T+2],o.colorArray[y+3]=M[w+0],o.colorArray[y+4]=M[w+1],o.colorArray[y+5]=M[w+2],o.colorArray[y+6]=M[x+0],o.colorArray[y+7]=M[x+1],o.colorArray[y+8]=M[x+2]),o.count+=3}this.addBall=function(_,S,M,T,w,x){let y=Math.sign(T);T=Math.abs(T);let P=x!=null,C=new Mt(_,S,M);if(P)try{C=x instanceof Mt?x:Array.isArray(x)?new Mt(Math.min(Math.abs(x[0]),1),Math.min(Math.abs(x[1]),1),Math.min(Math.abs(x[2]),1)):new Mt(x)}catch{C=new Mt(_,S,M)}let I=this.size*Math.sqrt(T/w),L=M*this.size,F=S*this.size,D=_*this.size,B=Math.floor(L-I);B<1&&(B=1);let G=Math.floor(L+I);G>this.size-1&&(G=this.size-1);let k=Math.floor(F-I);k<1&&(k=1);let j=Math.floor(F+I);j>this.size-1&&(j=this.size-1);let W=Math.floor(D-I);W<1&&(W=1);let K=Math.floor(D+I);K>this.size-1&&(K=this.size-1);let Q,dt,yt,q,at,gt,Et,rt,Dt,se,Rt;for(yt=B;yt<G;yt++)for(at=this.size2*yt,rt=yt/this.size-M,Dt=rt*rt,dt=k;dt<j;dt++)for(q=at+this.size*dt,Et=dt/this.size-S,se=Et*Et,Q=W;Q<K;Q++)if(gt=Q/this.size-_,Rt=T/(1e-6+gt*gt+se+Dt)-w,Rt>0){this.field[q+Q]+=Rt*y;let Ht=Math.sqrt((Q-D)*(Q-D)+(dt-F)*(dt-F)+(yt-L)*(yt-L))/I,$t=1-Ht*Ht*Ht*(Ht*(Ht*6-15)+10);this.palette[(q+Q)*3+0]+=C.r*$t,this.palette[(q+Q)*3+1]+=C.g*$t,this.palette[(q+Q)*3+2]+=C.b*$t}},this.addPlaneX=function(_,S){let M=this.size,T=this.yd,w=this.zd,x=this.field,y,P,C,I,L,F,D,B=M*Math.sqrt(_/S);for(B>M&&(B=M),y=0;y<B;y++)if(F=y/M,I=F*F,L=_/(1e-4+I)-S,L>0)for(P=0;P<M;P++)for(D=y+P*T,C=0;C<M;C++)x[w*C+D]+=L},this.addPlaneY=function(_,S){let M=this.size,T=this.yd,w=this.zd,x=this.field,y,P,C,I,L,F,D,B,G=M*Math.sqrt(_/S);for(G>M&&(G=M),P=0;P<G;P++)if(F=P/M,I=F*F,L=_/(1e-4+I)-S,L>0)for(D=P*T,y=0;y<M;y++)for(B=D+y,C=0;C<M;C++)x[w*C+B]+=L},this.addPlaneZ=function(_,S){let M=this.size,T=this.yd,w=this.zd,x=this.field,y,P,C,I,L,F,D,B,G=M*Math.sqrt(_/S);for(G>M&&(G=M),C=0;C<G;C++)if(F=C/M,I=F*F,L=_/(1e-4+I)-S,L>0)for(D=w*C,P=0;P<M;P++)for(B=D+P*T,y=0;y<M;y++)x[B+y]+=L},this.setCell=function(_,S,M,T){let w=this.size2*M+this.size*S+_;this.field[w]=T},this.getCell=function(_,S,M){let T=this.size2*M+this.size*S+_;return this.field[T]},this.blur=function(_=1){let S=this.field,M=S.slice(),T=this.size,w=this.size2;for(let x=0;x<T;x++)for(let y=0;y<T;y++)for(let P=0;P<T;P++){let C=w*P+T*y+x,I=M[C],L=1;for(let F=-1;F<=1;F+=2){let D=F+x;if(!(D<0||D>=T))for(let B=-1;B<=1;B+=2){let G=B+y;if(!(G<0||G>=T))for(let k=-1;k<=1;k+=2){let j=k+P;if(j<0||j>=T)continue;let W=w*j+T*G+D,K=M[W];L++,I+=_*(K-I)/L}}}S[C]=I}},this.reset=function(){for(let _=0;_<this.size3;_++)this.normal_cache[_*3]=0,this.field[_]=0,this.palette[_*3]=this.palette[_*3+1]=this.palette[_*3+2]=0},this.update=function(){this.count=0;let _=this.size-2;for(let S=1;S<_;S++){let M=this.size2*S,T=(S-this.halfsize)/this.halfsize;for(let w=1;w<_;w++){let x=M+this.size*w,y=(w-this.halfsize)/this.halfsize;for(let P=1;P<_;P++){let C=(P-this.halfsize)/this.halfsize,I=x+P;g(C,y,T,I,this.isolation)}}}this.geometry.setDrawRange(0,this.count),a.getAttribute("position").needsUpdate=!0,a.getAttribute("normal").needsUpdate=!0,this.enableUvs&&(a.getAttribute("uv").needsUpdate=!0),this.enableColors&&(a.getAttribute("color").needsUpdate=!0),this.count/3>r&&console.warn("THREE.MarchingCubes: Geometry buffers too small for rendering. Please create an instance with a higher poly count.")},this.init(t)}},w1=new Int32Array([0,265,515,778,1030,1295,1541,1804,2060,2309,2575,2822,3082,3331,3593,3840,400,153,915,666,1430,1183,1941,1692,2460,2197,2975,2710,3482,3219,3993,3728,560,825,51,314,1590,1855,1077,1340,2620,2869,2111,2358,3642,3891,3129,3376,928,681,419,170,1958,1711,1445,1196,2988,2725,2479,2214,4010,3747,3497,3232,1120,1385,1635,1898,102,367,613,876,3180,3429,3695,3942,2154,2403,2665,2912,1520,1273,2035,1786,502,255,1013,764,3580,3317,4095,3830,2554,2291,3065,2800,1616,1881,1107,1370,598,863,85,348,3676,3925,3167,3414,2650,2899,2137,2384,1984,1737,1475,1226,966,719,453,204,4044,3781,3535,3270,3018,2755,2505,2240,2240,2505,2755,3018,3270,3535,3781,4044,204,453,719,966,1226,1475,1737,1984,2384,2137,2899,2650,3414,3167,3925,3676,348,85,863,598,1370,1107,1881,1616,2800,3065,2291,2554,3830,4095,3317,3580,764,1013,255,502,1786,2035,1273,1520,2912,2665,2403,2154,3942,3695,3429,3180,876,613,367,102,1898,1635,1385,1120,3232,3497,3747,4010,2214,2479,2725,2988,1196,1445,1711,1958,170,419,681,928,3376,3129,3891,3642,2358,2111,2869,2620,1340,1077,1855,1590,314,51,825,560,3728,3993,3219,3482,2710,2975,2197,2460,1692,1941,1183,1430,666,915,153,400,3840,3593,3331,3082,2822,2575,2309,2060,1804,1541,1295,1030,778,515,265,0]),Ol=new Int32Array([-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,0,8,3,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,0,1,9,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,1,8,3,9,8,1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,1,2,10,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,0,8,3,1,2,10,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,9,2,10,0,2,9,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,2,8,3,2,10,8,10,9,8,-1,-1,-1,-1,-1,-1,-1,3,11,2,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,0,11,2,8,11,0,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,1,9,0,2,3,11,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,1,11,2,1,9,11,9,8,11,-1,-1,-1,-1,-1,-1,-1,3,10,1,11,10,3,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,0,10,1,0,8,10,8,11,10,-1,-1,-1,-1,-1,-1,-1,3,9,0,3,11,9,11,10,9,-1,-1,-1,-1,-1,-1,-1,9,8,10,10,8,11,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,4,7,8,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,4,3,0,7,3,4,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,0,1,9,8,4,7,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,4,1,9,4,7,1,7,3,1,-1,-1,-1,-1,-1,-1,-1,1,2,10,8,4,7,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,3,4,7,3,0,4,1,2,10,-1,-1,-1,-1,-1,-1,-1,9,2,10,9,0,2,8,4,7,-1,-1,-1,-1,-1,-1,-1,2,10,9,2,9,7,2,7,3,7,9,4,-1,-1,-1,-1,8,4,7,3,11,2,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,11,4,7,11,2,4,2,0,4,-1,-1,-1,-1,-1,-1,-1,9,0,1,8,4,7,2,3,11,-1,-1,-1,-1,-1,-1,-1,4,7,11,9,4,11,9,11,2,9,2,1,-1,-1,-1,-1,3,10,1,3,11,10,7,8,4,-1,-1,-1,-1,-1,-1,-1,1,11,10,1,4,11,1,0,4,7,11,4,-1,-1,-1,-1,4,7,8,9,0,11,9,11,10,11,0,3,-1,-1,-1,-1,4,7,11,4,11,9,9,11,10,-1,-1,-1,-1,-1,-1,-1,9,5,4,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,9,5,4,0,8,3,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,0,5,4,1,5,0,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,8,5,4,8,3,5,3,1,5,-1,-1,-1,-1,-1,-1,-1,1,2,10,9,5,4,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,3,0,8,1,2,10,4,9,5,-1,-1,-1,-1,-1,-1,-1,5,2,10,5,4,2,4,0,2,-1,-1,-1,-1,-1,-1,-1,2,10,5,3,2,5,3,5,4,3,4,8,-1,-1,-1,-1,9,5,4,2,3,11,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,0,11,2,0,8,11,4,9,5,-1,-1,-1,-1,-1,-1,-1,0,5,4,0,1,5,2,3,11,-1,-1,-1,-1,-1,-1,-1,2,1,5,2,5,8,2,8,11,4,8,5,-1,-1,-1,-1,10,3,11,10,1,3,9,5,4,-1,-1,-1,-1,-1,-1,-1,4,9,5,0,8,1,8,10,1,8,11,10,-1,-1,-1,-1,5,4,0,5,0,11,5,11,10,11,0,3,-1,-1,-1,-1,5,4,8,5,8,10,10,8,11,-1,-1,-1,-1,-1,-1,-1,9,7,8,5,7,9,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,9,3,0,9,5,3,5,7,3,-1,-1,-1,-1,-1,-1,-1,0,7,8,0,1,7,1,5,7,-1,-1,-1,-1,-1,-1,-1,1,5,3,3,5,7,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,9,7,8,9,5,7,10,1,2,-1,-1,-1,-1,-1,-1,-1,10,1,2,9,5,0,5,3,0,5,7,3,-1,-1,-1,-1,8,0,2,8,2,5,8,5,7,10,5,2,-1,-1,-1,-1,2,10,5,2,5,3,3,5,7,-1,-1,-1,-1,-1,-1,-1,7,9,5,7,8,9,3,11,2,-1,-1,-1,-1,-1,-1,-1,9,5,7,9,7,2,9,2,0,2,7,11,-1,-1,-1,-1,2,3,11,0,1,8,1,7,8,1,5,7,-1,-1,-1,-1,11,2,1,11,1,7,7,1,5,-1,-1,-1,-1,-1,-1,-1,9,5,8,8,5,7,10,1,3,10,3,11,-1,-1,-1,-1,5,7,0,5,0,9,7,11,0,1,0,10,11,10,0,-1,11,10,0,11,0,3,10,5,0,8,0,7,5,7,0,-1,11,10,5,7,11,5,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,10,6,5,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,0,8,3,5,10,6,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,9,0,1,5,10,6,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,1,8,3,1,9,8,5,10,6,-1,-1,-1,-1,-1,-1,-1,1,6,5,2,6,1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,1,6,5,1,2,6,3,0,8,-1,-1,-1,-1,-1,-1,-1,9,6,5,9,0,6,0,2,6,-1,-1,-1,-1,-1,-1,-1,5,9,8,5,8,2,5,2,6,3,2,8,-1,-1,-1,-1,2,3,11,10,6,5,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,11,0,8,11,2,0,10,6,5,-1,-1,-1,-1,-1,-1,-1,0,1,9,2,3,11,5,10,6,-1,-1,-1,-1,-1,-1,-1,5,10,6,1,9,2,9,11,2,9,8,11,-1,-1,-1,-1,6,3,11,6,5,3,5,1,3,-1,-1,-1,-1,-1,-1,-1,0,8,11,0,11,5,0,5,1,5,11,6,-1,-1,-1,-1,3,11,6,0,3,6,0,6,5,0,5,9,-1,-1,-1,-1,6,5,9,6,9,11,11,9,8,-1,-1,-1,-1,-1,-1,-1,5,10,6,4,7,8,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,4,3,0,4,7,3,6,5,10,-1,-1,-1,-1,-1,-1,-1,1,9,0,5,10,6,8,4,7,-1,-1,-1,-1,-1,-1,-1,10,6,5,1,9,7,1,7,3,7,9,4,-1,-1,-1,-1,6,1,2,6,5,1,4,7,8,-1,-1,-1,-1,-1,-1,-1,1,2,5,5,2,6,3,0,4,3,4,7,-1,-1,-1,-1,8,4,7,9,0,5,0,6,5,0,2,6,-1,-1,-1,-1,7,3,9,7,9,4,3,2,9,5,9,6,2,6,9,-1,3,11,2,7,8,4,10,6,5,-1,-1,-1,-1,-1,-1,-1,5,10,6,4,7,2,4,2,0,2,7,11,-1,-1,-1,-1,0,1,9,4,7,8,2,3,11,5,10,6,-1,-1,-1,-1,9,2,1,9,11,2,9,4,11,7,11,4,5,10,6,-1,8,4,7,3,11,5,3,5,1,5,11,6,-1,-1,-1,-1,5,1,11,5,11,6,1,0,11,7,11,4,0,4,11,-1,0,5,9,0,6,5,0,3,6,11,6,3,8,4,7,-1,6,5,9,6,9,11,4,7,9,7,11,9,-1,-1,-1,-1,10,4,9,6,4,10,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,4,10,6,4,9,10,0,8,3,-1,-1,-1,-1,-1,-1,-1,10,0,1,10,6,0,6,4,0,-1,-1,-1,-1,-1,-1,-1,8,3,1,8,1,6,8,6,4,6,1,10,-1,-1,-1,-1,1,4,9,1,2,4,2,6,4,-1,-1,-1,-1,-1,-1,-1,3,0,8,1,2,9,2,4,9,2,6,4,-1,-1,-1,-1,0,2,4,4,2,6,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,8,3,2,8,2,4,4,2,6,-1,-1,-1,-1,-1,-1,-1,10,4,9,10,6,4,11,2,3,-1,-1,-1,-1,-1,-1,-1,0,8,2,2,8,11,4,9,10,4,10,6,-1,-1,-1,-1,3,11,2,0,1,6,0,6,4,6,1,10,-1,-1,-1,-1,6,4,1,6,1,10,4,8,1,2,1,11,8,11,1,-1,9,6,4,9,3,6,9,1,3,11,6,3,-1,-1,-1,-1,8,11,1,8,1,0,11,6,1,9,1,4,6,4,1,-1,3,11,6,3,6,0,0,6,4,-1,-1,-1,-1,-1,-1,-1,6,4,8,11,6,8,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,7,10,6,7,8,10,8,9,10,-1,-1,-1,-1,-1,-1,-1,0,7,3,0,10,7,0,9,10,6,7,10,-1,-1,-1,-1,10,6,7,1,10,7,1,7,8,1,8,0,-1,-1,-1,-1,10,6,7,10,7,1,1,7,3,-1,-1,-1,-1,-1,-1,-1,1,2,6,1,6,8,1,8,9,8,6,7,-1,-1,-1,-1,2,6,9,2,9,1,6,7,9,0,9,3,7,3,9,-1,7,8,0,7,0,6,6,0,2,-1,-1,-1,-1,-1,-1,-1,7,3,2,6,7,2,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,2,3,11,10,6,8,10,8,9,8,6,7,-1,-1,-1,-1,2,0,7,2,7,11,0,9,7,6,7,10,9,10,7,-1,1,8,0,1,7,8,1,10,7,6,7,10,2,3,11,-1,11,2,1,11,1,7,10,6,1,6,7,1,-1,-1,-1,-1,8,9,6,8,6,7,9,1,6,11,6,3,1,3,6,-1,0,9,1,11,6,7,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,7,8,0,7,0,6,3,11,0,11,6,0,-1,-1,-1,-1,7,11,6,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,7,6,11,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,3,0,8,11,7,6,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,0,1,9,11,7,6,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,8,1,9,8,3,1,11,7,6,-1,-1,-1,-1,-1,-1,-1,10,1,2,6,11,7,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,1,2,10,3,0,8,6,11,7,-1,-1,-1,-1,-1,-1,-1,2,9,0,2,10,9,6,11,7,-1,-1,-1,-1,-1,-1,-1,6,11,7,2,10,3,10,8,3,10,9,8,-1,-1,-1,-1,7,2,3,6,2,7,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,7,0,8,7,6,0,6,2,0,-1,-1,-1,-1,-1,-1,-1,2,7,6,2,3,7,0,1,9,-1,-1,-1,-1,-1,-1,-1,1,6,2,1,8,6,1,9,8,8,7,6,-1,-1,-1,-1,10,7,6,10,1,7,1,3,7,-1,-1,-1,-1,-1,-1,-1,10,7,6,1,7,10,1,8,7,1,0,8,-1,-1,-1,-1,0,3,7,0,7,10,0,10,9,6,10,7,-1,-1,-1,-1,7,6,10,7,10,8,8,10,9,-1,-1,-1,-1,-1,-1,-1,6,8,4,11,8,6,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,3,6,11,3,0,6,0,4,6,-1,-1,-1,-1,-1,-1,-1,8,6,11,8,4,6,9,0,1,-1,-1,-1,-1,-1,-1,-1,9,4,6,9,6,3,9,3,1,11,3,6,-1,-1,-1,-1,6,8,4,6,11,8,2,10,1,-1,-1,-1,-1,-1,-1,-1,1,2,10,3,0,11,0,6,11,0,4,6,-1,-1,-1,-1,4,11,8,4,6,11,0,2,9,2,10,9,-1,-1,-1,-1,10,9,3,10,3,2,9,4,3,11,3,6,4,6,3,-1,8,2,3,8,4,2,4,6,2,-1,-1,-1,-1,-1,-1,-1,0,4,2,4,6,2,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,1,9,0,2,3,4,2,4,6,4,3,8,-1,-1,-1,-1,1,9,4,1,4,2,2,4,6,-1,-1,-1,-1,-1,-1,-1,8,1,3,8,6,1,8,4,6,6,10,1,-1,-1,-1,-1,10,1,0,10,0,6,6,0,4,-1,-1,-1,-1,-1,-1,-1,4,6,3,4,3,8,6,10,3,0,3,9,10,9,3,-1,10,9,4,6,10,4,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,4,9,5,7,6,11,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,0,8,3,4,9,5,11,7,6,-1,-1,-1,-1,-1,-1,-1,5,0,1,5,4,0,7,6,11,-1,-1,-1,-1,-1,-1,-1,11,7,6,8,3,4,3,5,4,3,1,5,-1,-1,-1,-1,9,5,4,10,1,2,7,6,11,-1,-1,-1,-1,-1,-1,-1,6,11,7,1,2,10,0,8,3,4,9,5,-1,-1,-1,-1,7,6,11,5,4,10,4,2,10,4,0,2,-1,-1,-1,-1,3,4,8,3,5,4,3,2,5,10,5,2,11,7,6,-1,7,2,3,7,6,2,5,4,9,-1,-1,-1,-1,-1,-1,-1,9,5,4,0,8,6,0,6,2,6,8,7,-1,-1,-1,-1,3,6,2,3,7,6,1,5,0,5,4,0,-1,-1,-1,-1,6,2,8,6,8,7,2,1,8,4,8,5,1,5,8,-1,9,5,4,10,1,6,1,7,6,1,3,7,-1,-1,-1,-1,1,6,10,1,7,6,1,0,7,8,7,0,9,5,4,-1,4,0,10,4,10,5,0,3,10,6,10,7,3,7,10,-1,7,6,10,7,10,8,5,4,10,4,8,10,-1,-1,-1,-1,6,9,5,6,11,9,11,8,9,-1,-1,-1,-1,-1,-1,-1,3,6,11,0,6,3,0,5,6,0,9,5,-1,-1,-1,-1,0,11,8,0,5,11,0,1,5,5,6,11,-1,-1,-1,-1,6,11,3,6,3,5,5,3,1,-1,-1,-1,-1,-1,-1,-1,1,2,10,9,5,11,9,11,8,11,5,6,-1,-1,-1,-1,0,11,3,0,6,11,0,9,6,5,6,9,1,2,10,-1,11,8,5,11,5,6,8,0,5,10,5,2,0,2,5,-1,6,11,3,6,3,5,2,10,3,10,5,3,-1,-1,-1,-1,5,8,9,5,2,8,5,6,2,3,8,2,-1,-1,-1,-1,9,5,6,9,6,0,0,6,2,-1,-1,-1,-1,-1,-1,-1,1,5,8,1,8,0,5,6,8,3,8,2,6,2,8,-1,1,5,6,2,1,6,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,1,3,6,1,6,10,3,8,6,5,6,9,8,9,6,-1,10,1,0,10,0,6,9,5,0,5,6,0,-1,-1,-1,-1,0,3,8,5,6,10,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,10,5,6,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,11,5,10,7,5,11,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,11,5,10,11,7,5,8,3,0,-1,-1,-1,-1,-1,-1,-1,5,11,7,5,10,11,1,9,0,-1,-1,-1,-1,-1,-1,-1,10,7,5,10,11,7,9,8,1,8,3,1,-1,-1,-1,-1,11,1,2,11,7,1,7,5,1,-1,-1,-1,-1,-1,-1,-1,0,8,3,1,2,7,1,7,5,7,2,11,-1,-1,-1,-1,9,7,5,9,2,7,9,0,2,2,11,7,-1,-1,-1,-1,7,5,2,7,2,11,5,9,2,3,2,8,9,8,2,-1,2,5,10,2,3,5,3,7,5,-1,-1,-1,-1,-1,-1,-1,8,2,0,8,5,2,8,7,5,10,2,5,-1,-1,-1,-1,9,0,1,5,10,3,5,3,7,3,10,2,-1,-1,-1,-1,9,8,2,9,2,1,8,7,2,10,2,5,7,5,2,-1,1,3,5,3,7,5,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,0,8,7,0,7,1,1,7,5,-1,-1,-1,-1,-1,-1,-1,9,0,3,9,3,5,5,3,7,-1,-1,-1,-1,-1,-1,-1,9,8,7,5,9,7,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,5,8,4,5,10,8,10,11,8,-1,-1,-1,-1,-1,-1,-1,5,0,4,5,11,0,5,10,11,11,3,0,-1,-1,-1,-1,0,1,9,8,4,10,8,10,11,10,4,5,-1,-1,-1,-1,10,11,4,10,4,5,11,3,4,9,4,1,3,1,4,-1,2,5,1,2,8,5,2,11,8,4,5,8,-1,-1,-1,-1,0,4,11,0,11,3,4,5,11,2,11,1,5,1,11,-1,0,2,5,0,5,9,2,11,5,4,5,8,11,8,5,-1,9,4,5,2,11,3,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,2,5,10,3,5,2,3,4,5,3,8,4,-1,-1,-1,-1,5,10,2,5,2,4,4,2,0,-1,-1,-1,-1,-1,-1,-1,3,10,2,3,5,10,3,8,5,4,5,8,0,1,9,-1,5,10,2,5,2,4,1,9,2,9,4,2,-1,-1,-1,-1,8,4,5,8,5,3,3,5,1,-1,-1,-1,-1,-1,-1,-1,0,4,5,1,0,5,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,8,4,5,8,5,3,9,0,5,0,3,5,-1,-1,-1,-1,9,4,5,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,4,11,7,4,9,11,9,10,11,-1,-1,-1,-1,-1,-1,-1,0,8,3,4,9,7,9,11,7,9,10,11,-1,-1,-1,-1,1,10,11,1,11,4,1,4,0,7,4,11,-1,-1,-1,-1,3,1,4,3,4,8,1,10,4,7,4,11,10,11,4,-1,4,11,7,9,11,4,9,2,11,9,1,2,-1,-1,-1,-1,9,7,4,9,11,7,9,1,11,2,11,1,0,8,3,-1,11,7,4,11,4,2,2,4,0,-1,-1,-1,-1,-1,-1,-1,11,7,4,11,4,2,8,3,4,3,2,4,-1,-1,-1,-1,2,9,10,2,7,9,2,3,7,7,4,9,-1,-1,-1,-1,9,10,7,9,7,4,10,2,7,8,7,0,2,0,7,-1,3,7,10,3,10,2,7,4,10,1,10,0,4,0,10,-1,1,10,2,8,7,4,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,4,9,1,4,1,7,7,1,3,-1,-1,-1,-1,-1,-1,-1,4,9,1,4,1,7,0,8,1,8,7,1,-1,-1,-1,-1,4,0,3,7,4,3,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,4,8,7,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,9,10,8,10,11,8,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,3,0,9,3,9,11,11,9,10,-1,-1,-1,-1,-1,-1,-1,0,1,10,0,10,8,8,10,11,-1,-1,-1,-1,-1,-1,-1,3,1,10,11,3,10,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,1,2,11,1,11,9,9,11,8,-1,-1,-1,-1,-1,-1,-1,3,0,9,3,9,11,1,2,9,2,11,9,-1,-1,-1,-1,0,2,11,8,0,11,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,3,2,11,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,2,3,8,2,8,10,10,8,9,-1,-1,-1,-1,-1,-1,-1,9,10,2,0,9,2,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,2,3,8,2,8,10,0,1,8,1,10,8,-1,-1,-1,-1,1,10,2,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,1,3,8,9,1,8,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,0,9,1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,0,3,8,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1]);var E1=`
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
}`,T1=`
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
}`;function A1({resolution:n,field:t,padding:e}){let i=new _i,s=new Bl(n,i,!1,!1,18e3);s.field.set(t),s.isolation=0,s.update();let r=new ee,a=Math.min(s.count,s.geometry.attributes.position.count)*3;for(let o of["position","normal"]){let l=s.geometry.attributes[o].array.slice(0,a);r.setAttribute(o,new _e(l,3))}return s.geometry.dispose(),i.dispose(),r.scale(e,e,e),r.computeBoundingSphere(),r}function R1(n){switch(n.kind){case"sphere":return new nn(1,48,32);case"box":return new ve(2,2,2);case"cylinder":return new vi(1,1,2,48);case"field":return A1(n);case"rounded-box":{let[t,e,i]=n.size,s=Math.max(0,n.rounding)*Math.min(t,e,i)/2;return(s>0?new js(t,e,i,5,s):new ve(t,e,i)).scale(2/t,2/e,2/i)}default:throw new Error("Unknown optical surface")}}var Ie=n=>({value:n}),Ch=()=>Array.from({length:4},()=>new A);function C1(n,t,e,i){let s=new oe({vertexShader:E1,fragmentShader:T1,side:je,uniforms:{...t,tint:Ie(new Mt),sizeM:Ie(1),strength:Ie(1),velocity:Ie(0)}}),r=new Ut(n,s);return r.position.copy(e),{mesh:r,start:e.clone(),end:e.clone(),at:i,period:25,speed:0}}function P1(n,t,e,i){let{mesh:s}=n;if(!n.end.equals(e)){let u=i-n.at;n.speed=Math.min(6,e.distanceTo(n.end)/(Math.max(1,u)/1e3)),n.start.copy(u>220?e:s.position),n.end.copy(e),n.at=i,n.period=ks.clamp(u,8,80)}let r=ks.clamp(t.deformation||0,0,.5),a=1/Math.sqrt(1-r),o=s.scale.fromArray(t["half-size"]);o.set(o.x*a,o.y*(1-r),o.z*a);let{sizeM:l,tint:c,strength:h}=s.material.uniforms;l.value=Math.max(.02,Math.min(o.x,o.y,o.z)),c.value.fromArray(t.color||[.65,.85,1]),h.value=t.strength??1}function I1(n,t){let e=t.filter(i=>i.intensity>0).sort((i,s)=>s.intensity-i.intensity).slice(0,4);for(let i=0;i<4;i++){let s=e[i],r=n.sourceColor.value[i];if(!s){r.set(0,0,0);continue}let a=n.sourcePos.value[i].setFromMatrixPosition(s.matrixWorld);n.sourceDir.value[i].setFromMatrixPosition(s.target.matrixWorld).sub(a).normalize(),r.set(s.color.r,s.color.g,s.color.b).multiplyScalar(s.intensity/50),n.coneCos.value.setComponent(i,Math.cos(s.angle))}}var zl=class extends ti{constructor(t){super(),this.engine=t,this.enabled=!1,this.needsSwap=!1,this.scene=new ai,this.geometry=new Map,this.instances=new Map,this.target=new A,this.uniforms={sceneColor:Ie(null),roomEnvironment:Ie(null),opaqueDepth:Ie(null),atmosphere:Ie(null),resolution:Ie(new et(1,1)),lens:Ie(new Gt),clockS:Ie(0),workLight:Ie(0),fogAmount:Ie(0),nearZ:Ie(.1),farZ:Ie(100),sourcePos:Ie(Ch()),sourceColor:Ie(Ch()),sourceDir:Ie(Ch()),coneCos:Ie(new ie)}}setSize(t,e){this.uniforms.resolution.value.set(t,e)}update(t,e,i=performance.now()){let s=new Set,r=new Set;for(let a of(e||[]).slice(0,12)){let o=t?.[a.mesh];if(!o)continue;this.geometry.has(a.mesh)||this.geometry.set(a.mesh,R1(o));let l=this.geometry.get(a.mesh),c=this.target.fromArray(a.position),h=this.instances.get(a.id);h||(h=C1(l,this.uniforms,c,i),this.instances.set(a.id,h),this.scene.add(h.mesh)),h.mesh.geometry=l,P1(h,a,c,i),s.add(a.mesh),r.add(a.id)}for(let[a,{mesh:o}]of this.instances)r.has(a)||(this.scene.remove(o),o.material.dispose(),this.instances.delete(a));for(let[a,o]of this.geometry)s.has(a)||(o.dispose(),this.geometry.delete(a));this.enabled=r.size>0}render(t,e,i){let{camera:s,clock:r,compositeTarget:a,sceneTarget:o,fog:l,fogAmount:c,workLight:h,spots:u,environmentTarget:d}=this.engine,f=this.uniforms,p=performance.now();for(let m of this.instances.values()){let _=Math.max(0,p-m.at);m.mesh.position.lerpVectors(m.start,m.end,Math.min(1,_/m.period)),m.mesh.material.uniforms.velocity.value=m.speed*Math.exp(-Math.max(0,_-m.period)/220)}f.sceneColor.value=a.texture,f.opaqueDepth.value=o.depthTexture,f.atmosphere.value=l.target.texture,f.roomEnvironment.value=d.texture,f.clockS.value=r,f.fogAmount.value=c,f.workLight.value=h,f.lens.value.copy(s.projectionMatrix),f.nearZ.value=s.near,f.farZ.value=s.far,I1(f,u||[]);let v=t.autoClear,g=t.getRenderTarget();try{t.setRenderTarget(i),t.autoClear=!1,t.clearDepth(),t.render(this.scene,s)}finally{t.autoClear=v,t.setRenderTarget(g)}}dispose(){this.update({},[])}};var ts=(n,t,e)=>Math.max(t,Math.min(e,n)),kl=(n,t)=>{if(!Number.isFinite(n))throw new Error(`Invalid ${t}`);return n},fa=Object.freeze({high:Object.freeze({grid:[48,24,48],scale:.67,steps:72,shadows:4}),desktop:Object.freeze({grid:[32,18,32],scale:.5,steps:48,shadows:4}),mobile:Object.freeze({grid:[24,14,24],scale:.4,steps:32,shadows:2}),phone:Object.freeze({grid:[20,12,20],scale:.33,steps:24,shadows:1})});var da=class n{constructor({grid:t=fa.desktop.grid,room:e=[12,5,10],initialHaze:i=.07}={}){if(!Array.isArray(t)||t.length!==3||t.some(r=>!Number.isInteger(r)||r<4||r>64))throw new Error("Invalid fog grid");this.grid=[...t],this.initialHaze=ts(kl(i,"haze"),0,1);let s=t[0]*t[1]*t[2];this.data=new Float32Array(s),this.temperature=new Float32Array(s),this.advected=new Float32Array(s),this.advectedHeat=new Float32Array(s),this.next=new Float32Array(s),this.nextHeat=new Float32Array(s),this.steps=0,this.time=0,this.accumulator=0,this.droppedSeconds=0,this.sources=[],this.setRoom(e)}setRoom(t){if(!Array.isArray(t)||t.length!==3||t.some(e=>!Number.isFinite(e)||e<=0))throw new Error("Invalid fog room");return this.room?.every((e,i)=>e===t[i])?!1:(this.room=[...t],this.spacing=t.map((e,i)=>e/this.grid[i]),this.clear(this.initialHaze),!0)}clear(t=0){kl(t,"haze"),this.data.fill(ts(t,0,6)),this.temperature.fill(0),this.sources=[],this.accumulator=0,this.time=0,this.steps=0,this.droppedSeconds=0}copyFrom(t){if(!(t instanceof n))throw new Error("Expected an owned fog field");this.setRoom(t.room);let[e,i,s]=this.grid,[r,a,o]=t.grid;for(let l=0;l<s;l++)for(let c=0;c<i;c++)for(let h=0;h<e;h++){let u=h+c*e+l*e*i,d=(h+.5)*r/e-.5,f=(c+.5)*a/i-.5,p=(l+.5)*o/s-.5;this.data[u]=t.sample(d,f,p),this.temperature[u]=t.sample(d,f,p,t.temperature)}this.sources=t.sources.map(l=>({...l,position:[...l.position],direction:[...l.direction]}));for(let l of["time","steps","accumulator","droppedSeconds"])this[l]=t[l]}burst({position:t=[-this.room[0]*.34,.22,-this.room[2]*.24],direction:e=[.7,.12,.7],duration:i=2.8,strength:s=1.2}={}){if(![t,e].every(c=>Array.isArray(c)&&c.length===3&&c.every(Number.isFinite)))throw new Error("Invalid fog source");let r=Math.hypot(...e);if(r<1e-6)throw new Error("Fog source needs a direction");let a=e.map(c=>c/r),o={position:[...t],direction:a,remaining:ts(kl(i,"burst duration"),0,8),strength:ts(kl(s,"burst strength"),0,8)},l=this.sources.find(c=>c.position.every((h,u)=>h===t[u]));l?(l.remaining=Math.min(8,l.remaining+o.remaining),l.strength=o.strength):(this.sources.length>=4&&this.sources.shift(),this.sources.push(o))}sample(t,e,i,s=this.data){let[r,a,o]=this.grid;t=ts(t,0,r-1),e=ts(e,0,a-1),i=ts(i,0,o-1);let l=Math.floor(t),c=Math.floor(e),h=Math.floor(i),u=Math.min(l+1,r-1),d=Math.min(c+1,a-1),f=Math.min(h+1,o-1),p=t-l,v=e-c,g=i-h,m=r*a,_=l+c*r+h*m,S=u+c*r+h*m,M=l+d*r+h*m,T=u+d*r+h*m,w=(f-h)*m,x=(s[_]*(1-p)+s[S]*p)*(1-v)+(s[M]*(1-p)+s[T]*p)*v,y=(s[_+w]*(1-p)+s[S+w]*p)*(1-v)+(s[M+w]*(1-p)+s[T+w]*p)*v;return x*(1-g)+y*g}advance(t){if(!Number.isFinite(t)||t<0)throw new Error("Fog dt must be finite and nonnegative");let e=Math.min(t,.1);this.droppedSeconds+=t-e,this.accumulator+=e;let i=0;for(;this.accumulator+1e-10>=.05&&i<2;)this.step(.05),this.accumulator=Math.max(0,this.accumulator-.05),i++;return i}step(t){let[e,i,s]=this.grid,[r,a,o]=this.spacing,l=e*i,c=this.data,h=this.temperature,u=this.advected,d=this.advectedHeat,f=this.time,p=this.room[0],v=this.room[2];for(let w=0;w<s;w++)for(let x=0;x<i;x++)for(let y=0;y<e;y++){let P=y+x*e+w*l,C=(y+.5)*r-p/2,I=(x+.5)*a,L=(w+.5)*o-v/2,F=Math.min(1,h[P])*.17,D=.12+.11*(Math.cos(I*1.7+f*.43)-Math.sin(L*1.3-f*.31))+F*(Math.cos(I*4.1-f*.8)-Math.sin(L*3.7+f*.7)),B=h[P]*.38+.035+.08*(Math.sin(L*1.3+f*.31)-Math.cos(C*1.5+f*.43))+F*(Math.sin(L*3.7+f*.7)-Math.cos(C*4.3-f*.8)),G=.025+.11*(Math.cos(C*1.5-f*.43)-Math.sin(I*1.7+f*.31))+F*(Math.cos(C*4.3-f*.8)-Math.sin(I*4.1+f*.7)),k=0;for(let j of this.sources){let W=C-j.position[0],K=I-j.position[1],Q=L-j.position[2],dt=W*j.direction[0]+K*j.direction[1]+Q*j.direction[2];if(dt<-.2||dt>1.8)continue;let yt=.2+Math.max(0,dt)*.24,q=Math.max(0,W*W+K*K+Q*Q-dt*dt),at=Math.exp(-q/(2*yt*yt)-((dt-.45)/.85)**2),gt=Math.sin(dt*8-f*5.5+I*3),Et=.72+.28*gt*gt,rt=at*Et*j.strength;k+=rt,D+=j.direction[0]*rt*.9,B+=j.direction[1]*rt*.9,G+=j.direction[2]*rt*.9}u[P]=Math.min(6,this.sample(y-t*D/r,x-t*B/a,w-t*G/o,c)+k*t*3.2),d[P]=Math.min(4,this.sample(y-t*D/r,x-t*B/a,w-t*G/o,h)+k*t*2.4)}let g=t*.025/(r*r),m=t*.025/(a*a),_=t*.025/(o*o),S=Math.min(1,.45/(g+m+_));g*=S,m*=S,_*=S;let M=Math.exp(-t/100),T=Math.exp(-t/4.5);for(let w=0;w<s;w++)for(let x=0;x<i;x++)for(let y=0;y<e;y++){let P=y+x*e+w*l,C=y?P-1:P,I=y<e-1?P+1:P,L=x?P-e:P,F=x<i-1?P+e:P,D=w?P-l:P,B=w<s-1?P+l:P;this.next[P]=Math.max(0,(u[P]+g*(u[C]+u[I]-2*u[P])+m*(u[L]+u[F]-2*u[P])+_*(u[D]+u[B]-2*u[P]))*M),this.nextHeat[P]=Math.max(0,(d[P]+g*(d[C]+d[I]-2*d[P])+m*(d[L]+d[F]-2*d[P])+_*(d[D]+d[B]-2*d[P]))*T)}[this.data,this.next]=[this.next,this.data],[this.temperature,this.nextHeat]=[this.nextHeat,this.temperature];for(let w of this.sources)w.remaining-=t;this.sources=this.sources.filter(w=>w.remaining>1e-8),this.time+=t,this.steps++}stats(){let t=0,e=0,i=0;for(let s=0;s<this.data.length;s++)t+=this.data[s],e=Math.max(e,this.data[s]),i+=this.temperature[s];return{cells:this.data.length,steps:this.steps,time:this.time,droppedSeconds:this.droppedSeconds,meanDensity:t/this.data.length,maximumDensity:e,heat:i/this.data.length,activeSources:this.sources.length,massEstimate:t*this.spacing[0]*this.spacing[1]*this.spacing[2]}}};var D1=`out vec2 vUv;
void main(){vUv=uv;gl_Position=vec4(position.xy,0.0,1.0);}`,Uf=`precision highp float;
precision highp sampler3D;
#include <packing>
${Af}
in vec2 vUv;
uniform sampler2D sceneDepth;
uniform sampler3D densityTexture;
uniform mat4 inverseProjection, cameraWorld;
uniform vec3 eye, room;
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
vec2 roomHit(vec3 ro,vec3 rd){
 vec3 lower=vec3(-room.x*.5,0.,-room.z*.5),upper=vec3(room.x*.5,room.y,room.z*.5);
 vec3 safe=vec3(abs(rd.x)<1e-7?(rd.x<0.?-1e-7:1e-7):rd.x,abs(rd.y)<1e-7?(rd.y<0.?-1e-7:1e-7):rd.y,abs(rd.z)<1e-7?(rd.z<0.?-1e-7:1e-7):rd.z);
 vec3 a=(lower-ro)/safe,b=(upper-ro)/safe,lo=min(a,b),hi=max(a,b);
 return vec2(max(max(lo.x,lo.y),lo.z),min(min(hi.x,hi.y),hi.z));
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
 vec4 lightIntervals[16];
 for(int i=0;i<16;i++){
  if(i>=lightCount)break;
  lightIntervals[i]=vec4(1.,0.,1.,0.);
  if(dot(lightColor[i],lightColor[i])>.000001 && lightShape[i].y>0.)
   lightIntervals[i]=vec4(coneHit(i,lightShape[i].x,eye,ray,start,finish),
                          coneHit(i,lightProfile[i].w,eye,ray,start,finish));
 }
 float stepSize=(finish-start)/float(raySteps);
 // Stable, decorrelated offsets avoid diagonal patterns in dense fog. Keep
 // samples near each step's midpoint to limit high-frequency brightness noise.
 float jitter=mix(.25,.75,hash(vec3(gl_FragCoord.xy,13.)));
 vec3 radiance=vec3(0.);float transmission=1.;
 for(int n=0;n<72;n++){
  if(n>=raySteps||transmission<.002)break;
  float begin=start+float(n)*stepSize;
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
  for(int i=0;i<16;i++){
   if(i>=lightCount)break;
   for(int layer=0;layer<2;layer++){
    bool halo=layer==1;
    vec2 span=halo?lightIntervals[i].zw:lightIntervals[i].xy;
    float lo=max(begin,span.x),hi=min(begin+stepSize,span.y);
    if(hi<=lo)continue;
    vec3 samplePosition=eye+ray*((lo+hi)*.5);
    float localDensity=densityAt(samplePosition);
    if(localDensity<=0.)continue;
    float foreground=fogTransmission(extinctionPerM,density,lo-begin);
    radiance+=transmission*foreground*(1.-fogTransmission(extinctionPerM,localDensity,hi-lo))*.92*
       incident(i,samplePosition,localDensity,halo);
   }
  }
  transmission*=segment;
 }
 volumeOutput=vec4(radiance,transmission);
}`,L1=`${Uf}
out vec4 volumeOutput;
void main(){integrateFog(volumeOutput);}`,N1=`${Uf}
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
}`,Lf=()=>globalThis.performance?.now?.()??Date.now(),U1=()=>{let n=new zn(new Uint32Array([4294967295]),1,1,Ai,Qe);return n.internalFormat="DEPTH_COMPONENT24",n.compareFunction=Bs,n.minFilter=n.magFilter=ae,n.needsUpdate=!0,n};function Nf(n){let t=new ys(new Uint16Array(n.data.length),...n.grid);return Object.assign(t,{format:Os,type:fe,internalFormat:"R16F",minFilter:ae,magFilter:ae,unpackAlignment:1})}var Hl=class{constructor(t,{quality:e="desktop"}={}){if(!fa[e])throw new Error("Unknown fog quality");this.renderer=t,this.quality=e,this.budget=fa[e],this.disposed=!1,this.field=new da({grid:this.budget.grid}),this.field.burst(),this.amount=.5,this.density=Nf(this.field),this.packed=this.density.image.data,this.dummyShadow=U1(),this.target=new ce(1,1,{type:fe,format:ci,depthBuffer:!1,stencilBuffer:!1,minFilter:ae,magFilter:ae}),this.lights=[],this.updateMs=0,this.uploads=0,this.width=1,this.height=1;let i=r=>Array.from({length:r},()=>new A);this.uniforms={sceneDepth:{value:null},densityTexture:{value:this.density},inverseProjection:{value:new Gt},cameraWorld:{value:new Gt},eye:{value:new A},room:{value:new A(...this.field.room)},amount:{value:this.amount},time:{value:0},mixedDensity:{value:0},extinctionPerM:{value:1.25},raySteps:{value:this.budget.steps},lightCount:{value:0},lightPosition:{value:i(16)},lightDirection:{value:i(16)},lightColor:{value:i(16)},lightShape:{value:i(16)},lightShadow:{value:new Int32Array(16).fill(-1)},lightProfile:{value:Array.from({length:16},()=>new ie)},lightStart:{value:new Float32Array(16)},ambientSky:{value:new A},ambientGround:{value:new A},shadowMatrix:{value:Array.from({length:4},()=>new Gt)},shadowTexel:{value:Array.from({length:4},()=>new et(1,1))},shadowBias:{value:new Float32Array(4)}};for(let r=0;r<4;r++)this.uniforms[`shadow${r}`]={value:this.dummyShadow};this.compositeUniforms={...this.uniforms,sceneColor:{value:null},volume:{value:this.target.texture},volumeSize:{value:new et(1,1)},nearPlane:{value:.08},farPlane:{value:80}};let s=(r,a)=>new oe({vertexShader:D1,fragmentShader:r,uniforms:a,glslVersion:ta,depthTest:!1,depthWrite:!1,blending:Ni,toneMapped:!1});this.volumeMaterial=s(L1,this.uniforms),this.compositeMaterial=s(N1,this.compositeUniforms),this.quad=new Ut(new Di(2,2),this.volumeMaterial),this.quad.frustumCulled=!1,this.scene=new ai,this.scene.add(this.quad),this.camera=new Wn,this.upload()}upload(){for(let t=0;t<this.packed.length;t++)this.packed[t]=_r.toHalfFloat(Math.min(65504,this.field.data[t]));this.density.needsUpdate=!0,this.uploads++}setQuality(t){let e=fa[t];if(!e)throw new Error("Unknown fog quality");if(this.disposed||this.quality===t)return;let i=new da({grid:e.grid});i.copyFrom(this.field);let s=Nf(i);this.density.dispose(),this.field=i,this.density=s,this.packed=s.image.data,this.quality=t,this.budget=e,this.uniforms.densityTexture.value=s,this.uniforms.raySteps.value=e.steps,this.upload()}setRoom(t){this.field.setRoom(t)&&(this.uniforms.room.value.fromArray(t),this.showAtmosphere?(this.showAtmosphere.suspend(),this.field.clear()):this.field.burst(),this.upload())}get extinctionPerM(){return this.showAtmosphere?.extinctionPerM??1.25}setAmount(t){if(!Number.isFinite(t))throw new Error("Invalid fog amount");this.amount=Math.max(0,Math.min(1,t))}setAmbient({color:t,intensity:e=0,groundColor:i=t}={}){if(!Number.isFinite(e)||e<0)throw new Error("Invalid atmosphere ambient intensity");let s=(r,a)=>{if(!a)return r.value.set(0,0,0);if(![a.r,a.g,a.b].every(o=>Number.isFinite(o)&&o>=0))throw new Error("Invalid atmosphere ambient color");r.value.set(a.r,a.g,a.b).multiplyScalar(e)};s(this.uniforms.ambientSky,t),s(this.uniforms.ambientGround,i)}setAtmosphere(t,e){e!=null?(this.showAtmosphere||(this.showAtmosphere=new Nl,this.field.clear(0),this.upload()),this.showAtmosphere.accept(t,e)):this.showAtmosphere&&(this.showAtmosphere=null,this.field.clear(this.field.initialHaze),this.field.burst(),this.upload())}burst(t){return this.showAtmosphere?!1:(this.field.burst(t),!0)}setSize(t,e){[t,e].every(i=>Number.isFinite(i)&&i>0)&&(this.width=Math.max(1,Math.floor(t*this.budget.scale)),this.height=Math.max(1,Math.floor(e*this.budget.scale)),this.target.setSize(this.width,this.height),this.compositeUniforms.volumeSize.value.set(this.width,this.height))}update(t,e=[]){if(this.disposed)return;let i=Lf();this.lights=e.slice(0,16),(this.showAtmosphere?this.showAtmosphere.paint(this.field,t):this.field.advance(t))&&this.upload(),this.updateMs=Lf()-i}render(t,e,i=null){if(this.disposed)return;if(!t?.depthTexture||!t?.texture)throw new Error("Fog requires opaque scene color and depth");let s=this.renderer,r=s.getRenderTarget(),a=s.autoClear,o=this.uniforms,l=this.compositeUniforms;o.sceneDepth.value=t.depthTexture,o.inverseProjection.value.copy(e.projectionMatrixInverse),o.cameraWorld.value.copy(e.matrixWorld),o.eye.value.setFromMatrixPosition(e.matrixWorld);let c=this.showAtmosphere,h=c&&!c.localVolume&&!c.sample?.["mixed-density"]?0:this.amount;o.extinctionPerM.value=this.extinctionPerM,o.mixedDensity.value=c?.sample?.["mixed-density"]||0,o.amount.value=h,o.time.value=this.field.time,o.lightCount.value=this.lights.length,o.lightShadow.value.fill(-1);let u=0;for(let d=0;d<4;d++)o[`shadow${d}`].value=this.dummyShadow;this.lights.forEach((d,f)=>{o.lightPosition.value[f].copy(d.virtualPosition||d.position),o.lightDirection.value[f].copy(d.direction).normalize(),o.lightColor.value[f].set(d.color.r,d.color.g,d.color.b),o.lightShape.value[f].set(Math.cos(d.angle),Math.max(0,d.intensity??1),Math.max(0,d.distance??0));let p=Math.max(0,Math.min(1,d.penumbra??.55)),v=Math.max(d.angle,Math.min(1.54,d.angle+.23)),g=Math.tan(d.angle);if(o.lightProfile.value[f].set(Math.cos(d.angle*(1-p)),g,Math.tan(v)-g,Math.cos(v)),o.lightStart.value[f]=Math.max(0,d.apexDistance??0),d.shadow?.map?.depthTexture&&u<this.budget.shadows){if(d.shadow.map.depthTexture.compareFunction!==Bs)throw new Error("Fog requires forward-depth PCF comparison shadow textures");let m=u++;o.lightShadow.value[f]=m,o[`shadow${m}`].value=d.shadow.map.depthTexture,o.shadowMatrix.value[m].copy(d.shadow.matrix),o.shadowTexel.value[m].set(1/d.shadow.map.width,1/d.shadow.map.height),o.shadowBias.value[m]=d.shadow.bias??-2e-4}}),this.shadowCount=u,l.sceneColor.value=t.texture,l.sceneDepth.value=t.depthTexture,l.nearPlane.value=e.near,l.farPlane.value=e.far,l.amount.value=h;try{s.autoClear=!0,h>0&&(s.setRenderTarget(this.target),this.quad.material=this.volumeMaterial,s.render(this.scene,this.camera)),s.setRenderTarget(i),this.quad.material=this.compositeMaterial,s.render(this.scene,this.camera)}finally{s.setRenderTarget(r),s.autoClear=a}}stats(){return{quality:this.quality,targetSize:[this.width,this.height],raySteps:this.budget.steps,lightCount:this.lights.length,shadowCount:this.shadowCount??0,cpuUpdateMs:this.updateMs,uploads:this.uploads,atmosphere:this.showAtmosphere?.stats(),...this.field.stats()}}dispose(){this.disposed||(this.disposed=!0,this.target.dispose(),this.density.dispose(),this.dummyShadow.dispose(),this.volumeMaterial.dispose(),this.compositeMaterial.dispose(),this.quad.geometry.dispose(),this.scene.clear(),this.lights=[])}};var Qs=class{constructor(t,e,i=document,s=window){this.focused=i.hasFocus(),this.visibleDocument=!i.hidden,this.external=!1,this.externalOwners=new Set,this.inViewport=!1,this.pageActive=!0,this.active=!1,this.abort=new AbortController;let r=this.abort.signal,a=()=>{let l=this.pageActive&&(this.external||this.inViewport&&this.visibleDocument&&this.focused);l!==this.active&&(this.active=l,t.dispatchEvent?.(new CustomEvent("tolglow-preview-activity",{detail:l})),e(l))};t.addEventListener?.("tolglow-preview-external",l=>{if(l.detail&&typeof l.detail=="object"){let{owner:c,active:h}=l.detail;h?this.externalOwners.add(c):this.externalOwners.delete(c)}else l.detail===!0?this.externalOwners.add("pip"):this.externalOwners.delete("pip");this.external=this.externalOwners.size>0,a()},{signal:r});let o=l=>{this.focused=l,a()};s.addEventListener("focus",()=>o(!0),{signal:r}),s.addEventListener("blur",()=>o(!1),{signal:r}),i.addEventListener("visibilitychange",()=>{this.visibleDocument=!i.hidden,a()},{signal:r}),s.addEventListener("pagehide",()=>{this.pageActive=!1,a()},{signal:r}),s.addEventListener("pageshow",()=>{this.pageActive=!0,this.visibleDocument=!i.hidden,this.focused=i.hasFocus(),a()},{signal:r}),this.observer=new s.IntersectionObserver(l=>{let c=l.at(-1);!c||r.aborted||(this.inViewport=c.isIntersecting&&c.intersectionRatio>0,a())},{threshold:[0,Number.EPSILON]}),this.observer.observe(t)}dispose(){this.abort.abort(),this.observer.disconnect(),this.active=!1}};var Vl=class{constructor({now:t=()=>performance.now(),raf:e=globalThis.requestAnimationFrame?.bind(globalThis),cancelRaf:i=globalThis.cancelAnimationFrame?.bind(globalThis),timer:s=globalThis.setTimeout.bind(globalThis),cancelTimer:r=globalThis.clearTimeout.bind(globalThis)}={}){Object.assign(this,{now:t,raf:e,cancelRaf:i,timer:s,cancelTimer:r}),this.pending=!1}cancel(){this.rafId!=null&&this.cancelRaf?.(this.rafId),this.timerId!=null&&this.cancelTimer(this.timerId),this.rafId=this.timerId=null,this.pending=!1}at(t,e,i=.5){this.cancel(),this.pending=!0;let s=a=>{if(this.rafId=this.timerId=null,a+i<t){r();return}this.pending=!1,e(a)},r=()=>{let a=Math.max(0,t-this.now());if(this.raf&&a<=18)try{this.rafId=this.raf(s);return}catch(o){if(o.name!=="NotSupportedError")throw o;this.raf=null}this.raf?this.timerId=this.timer(r,a-16):this.timerId=this.timer(()=>s(this.now()),Math.max(1,a))};r()}},tr=class{constructor(t,e=globalThis){this.changed=t,this.host=e,this.active=!1,this.tick=this.tick.bind(this)}setActive(t){t===this.active||!this.host.requestAnimationFrame||(this.active=t,t?this.start():(this.host.cancelAnimationFrame(this.raf),this.host.clearTimeout(this.timer)))}start(){this.gaps=[],this.samples=0,this.previous=null,this.raf=this.host.requestAnimationFrame(this.tick)}tick(t){if(!this.active)return;this.samples++;let e=this.previous===null?0:t-this.previous;if(this.previous=t,e>=3&&e<=120&&this.gaps.push(e),this.gaps.length<16&&this.samples<40){this.raf=this.host.requestAnimationFrame(this.tick);return}if(this.gaps.length>=8){this.gaps.sort((a,o)=>a-o);let i=this.gaps[Math.floor(this.gaps.length/4)],s=this.gaps.map(a=>a/Math.max(1,Math.round(a/i))).sort((a,o)=>a-o),r=Math.round(1e3/s[Math.floor(s.length/2)]);(this.hz==null||Math.abs(this.hz-r)>=2)&&(this.hz=r,this.changed(r))}this.active&&(this.timer=this.host.setTimeout(()=>this.start(),3e4))}};function Ph(n,t){let e=new Set(t),i=new Set(n),s=t.filter(a=>!i.has(a)),r=0;return n.map(a=>e.has(a)?a:s[r++])}function Ff(n,t,e,i,s,r){let a=new Set(s),o=[];for(let[p,v]of Object.entries(t)){let g=n.get(p),m=v?.["emitted-color"];if(!g||!Array.isArray(m)||m.length!==3||!m.every(Number.isFinite))continue;let _=Math.max(...m)*(g["relative-brightness"]??1);if(_<=0)continue;let S=g.channels?.pan!=null&&g.channels?.tilt!=null;o.push({id:p,level:_,moving:S})}o.sort((p,v)=>+(v.id===e)-+(p.id===e)||Number(v.moving)-Number(p.moving)||v.level-p.level||Number(a.has(v.id))-Number(a.has(p.id))||(p.id<v.id?-1:1));let l=Array.from(new Set([o[0],o.find(p=>!p.moving),...o].filter(Boolean).map(p=>p.id))),c=new Set;n.has(e)&&c.add(e);for(let p of l)c.add(p);for(let p of i)c.add(p);let h=Ph(i,Array.from(c).slice(0,i.length)),u=new Set(h),d=new Set(l.filter(p=>u.has(p)));for(let p of s)u.has(p)&&d.add(p);for(let p of h)d.add(p);let f=Array.from(d).slice(0,s.length);return{meshes:h,lights:[...Ph(s.slice(0,r),f.slice(0,r)),...Ph(s.slice(r),f.slice(r))]}}var F1=Object.freeze({}),Of=n=>Array.isArray(n)&&n.length===3&&n.every(Number.isFinite),Gl=(n,t,e)=>n+(t-n)*e;function O1(n,t,e,i){if(!n||!t||t["cut?"]||n.strobe>0||t.strobe>0||n.channels?.strobe>0||t.channels?.strobe>0||n.shutter!==t.shutter||n.channels?.shutter!==t.channels?.shutter)return t;let s={...t};for(let l of["intensity","dimmer"])Number.isFinite(n[l])&&Number.isFinite(t[l])&&(s[l]=Gl(n[l],t[l],i));let r=n["emitted-color"],a=t["emitted-color"];Of(r)&&Of(a)&&(s["emitted-color"]=a.map((l,c)=>Gl(r[c],l,i)));let o=["pan","tilt"];if(o.every(l=>wn(n,l)!==void 0&&wn(t,l)!==void 0&&Math.abs(an(e,t,l)-an(e,n,l))<180)){for(let l of o)s[l]=Gl(wn(n,l),wn(t,l),i),s[l+"-deg"]=Gl(an(e,n,l),an(e,t,l),i);s.pose={"pan-deg":s["pan-deg"],"tilt-deg":s["tilt-deg"]},delete s.direction,delete s.aim}return s}var Wl=class{constructor(){this.mode="hold"}setMode(t){if(!["hold","interpolate"].includes(t))throw new RangeError("Unknown preview lighting mode");this.mode!==t&&(this.mode=t,this.previous=null)}accept(t,e,i){let s=this.latest,r=t["time-ms"]-s?.["time-ms"],a=this.mode==="interpolate"&&s&&this.rigKey===e&&Xi(t)===Xi(s)&&t.revision===s.revision&&!t["blackout?"]&&!s["blackout?"]&&t.master!==0&&s.master!==0&&!t["cut?"]&&t["frame-number"]===s["frame-number"]+1&&r>0&&r<=50&&i>=this.receivedAt&&i-this.receivedAt<75;this.latest=t,this.previous=a?s:null,this.rigKey=e,this.period=r,this.receivedAt=i}values(t,e){let i=this.latest?.output?.values||F1;if(!this.previous)return i;let s=Math.max(0,(e-this.receivedAt)/this.period);if(s>=1)return this.previous=null,i;let r=this.previous.output?.values||{},a=Object.create(null);for(let{id:o,fixture:l}of t)a[o]=O1(r[o],i[o],l,s);return a}};var B1=new A(0,-1,0),er=16,An=class{constructor(t,e,i={}){if(this.displayHz=i.displayHz??(i.worker?null:60),this.platform=i,this.displayClock=new Vl,this.lighting=new Wl,this.canvas=t,t.tolglowPreview=this,this.status=e,this.ready=!1,this.disposed=!1,this.room=[12,5,10],this.snapshot=null,this.key=null,this.fixtures=[],this.filmSettings={bloom:.28,exposure:.85,grain:.0015},this.quality=this.autoQuality(),this.options=Bi[this.quality],this.qualityChoice="auto",this.fogOverride=null,this.workLight=.12,this.visible=!!i.worker,this.lost=!1,this.lastFrame=0,this.renderScale=1,this.scaleChangedAt=0,this.clock=0,this.fogAnimationUntil=0,this.draws=0,this.lastCPU=0,this.abort=new AbortController,this.reducedMotion=i.worker?{matches:!!i.reducedMotion}:matchMedia("(prefers-reduced-motion: reduce)"),this.whenReady=new Promise(r=>{this.resolveReady=r}),this.setStatus("Building venue and lighting shaders\u2026"),this.camera=new Te(49,1,.08,100),this.controls=i.worker?{target:new A,update:()=>this.camera.lookAt(this.controls.target),dispose(){},touches:{}}:El(this.camera,this.canvas),this.setCamera("audience"),i.worker)try{this.initialize()}catch(r){this.fail(r);return}i.worker||(this.refresh=new tr(r=>this.setDisplayHz(r)),this.resizeObserver=new ResizeObserver(()=>this.resize()),this.resizeObserver.observe(t),this.activity=new Qs(t,r=>this.changeActivity(r)));let s=this.abort.signal;t.addEventListener("webglcontextlost",r=>{r.preventDefault(),this.lost=!0,this.ready=!1,this.setStatus("Graphics context lost \xB7 waiting for recovery")},{signal:s}),t.addEventListener("webglcontextrestored",()=>{try{this.restorePending=!0,this.visible&&(this.restoreContext(),this.scheduleFrame(0))}catch(r){this.fail(r)}},{signal:s}),i.worker||(this.navigation=new Zs({camera:this.camera,controls:this.controls,canvas:t,room:()=>this.room,snapshot:()=>this.snapshot,visibleLights:()=>this.lightIds,active:()=>this.visible&&!this.paused&&!this.disposed,reducedMotion:()=>this.reducedMotion.matches}),this.reducedMotion.addEventListener("change",()=>this.navigation.notify(),{signal:s})),i.autoStart!==!1&&this.scheduleFrame(0)}get targetHz(){return qh(this.quality,this.qualityChoice,this.softwareRenderer,this.displayHz)}setDisplayHz(t){!t||t===this.displayHz||(this.displayHz=t,this.overBudget=this.underBudget=0)}autoQuality(t=this.platform.viewport?.width??this.canvas.getBoundingClientRect().width){return this.softwareRenderer?"low":(this.platform.worker?this.platform.coarsePointer:matchMedia("(pointer: coarse)").matches)||t<(this.autoLayoutTier==="mobile"?740:660)?"mobile":"desktop"}setStatus(t){this.status&&this.status.textContent!==t&&(this.status.textContent=t)}initialize(){this.hdrRequested=!this.platform.worker&&xa(),this.renderer=new gl({canvas:this.canvas,alpha:this.hdrRequested,antialias:!1,powerPreference:"high-performance",preserveDrawingBuffer:!1});let t=this.renderer.getContext(),e=t.getExtension("WEBGL_debug_renderer_info");this.maxDimension=Math.min(t.getParameter(t.MAX_RENDERBUFFER_SIZE),t.getParameter(t.MAX_TEXTURE_SIZE)),this.samples=Zh(t),this.gpuTimer=this.platform.worker?null:new nr(t),this.deviceRenderer=e?t.getParameter(e.UNMASKED_RENDERER_WEBGL):t.getParameter(t.RENDERER),this.softwareRenderer=/swiftshader|llvmpipe|softpipe|software rasterizer/i.test(this.deviceRenderer||""),this.quality=this.autoLayoutTier=this.autoQuality(),this.options=Bi[this.quality],this.frameInterval=1e3/this.targetHz,this.renderer.outputColorSpace=Fe,this.renderer.toneMapping=Kn,this.renderer.toneMappingExposure=.85,this.renderer.shadowMap.enabled=!0,this.renderer.shadowMap.type=Yn,this.renderer.debug.onShaderError=(i,s,r,a)=>{let o=i.getProgramInfoLog(s)||i.getShaderInfoLog(a)||i.getShaderInfoLog(r);throw new Error(`Preview shader rejected: ${String(o).slice(0,500)}`)},this.scene=new ai,this.scene.background=new Mt(.0015,.002,.003),this.materials=yf(),this.apertureUniform={value:new Float32Array(er+2)},this.patchRadii={value:new Float32Array(er+2)};for(let i of Object.values(this.materials))Mh(i,this.apertureUniform,this.patchRadii);this.refreshEnvironment(),this.hemisphere=new Cs(12966384,3812384,0),this.scene.add(this.hemisphere),this.diffuseBounce=new Cs(0,0,1),this.scene.add(this.diffuseBounce),this.workLamp=new Fr(16769988,0),this.workLamp.position.set(2,6,4),this.scene.add(this.workLamp),this.setWorkLight(this.workLight),this.sceneTarget=new ce(1,1,{type:fe,minFilter:ae,magFilter:ae,depthBuffer:!0}),this.sceneTarget.texture.colorSpace=pn,this.sceneTarget.depthTexture=new Ii(1,1,Qe),this.compositeTarget=new ce(1,1,{type:fe,depthBuffer:!1}),this.compositeTarget.texture.colorSpace=pn,this.fog=new Hl(this.renderer,{quality:this.options.fog||this.quality}),this.fog.setRoom(this.room),this.fog.setAmount(this.fogAmount),this.composer=new Cl(this.renderer,new ce(1,1,{type:fe,depthBuffer:!1})),this.inputPass=new Pl(this.compositeTarget.texture),this.composer.addPass(this.inputPass),this.glass=new zl(this),this.composer.addPass(this.glass),this.bloom=new Js(new et(1,1),.28,.4,1.1),this.composer.addPass(this.bloom),this.outputPass=xf(),this.composer.addPass(this.outputPass),this.edges=new Dl,this.composer.addPass(this.edges),this.film=new Ll,this.composer.addPass(this.film),this.setFilm(this.filmSettings),this.rebuild({room:{width:12,height:5,depth:10},fixtures:[],trusses:[]}),this.resize()}refreshEnvironment(){this.environmentTarget?.dispose();let t=new Al,e=new Vs(this.renderer);this.environmentTarget=e.fromScene(t,.04),this.scene.environment=this.environmentTarget.texture,t.dispose(),e.dispose()}setFilm(t){for(let[e,i]of[["bloom",2],["exposure",3],["grain",.08]])Number.isFinite(t?.[e])&&(this.filmSettings[e]=Ce(t[e],0,i));this.film&&(this.renderer.toneMappingExposure=this.filmSettings.exposure,this.bloom.strength=this.filmSettings.bloom,this.bloom.enabled=this.bloom.strength>0,this.film.uniforms.grain.value=this.filmSettings.grain)}setWorkLight(t){this.workLight=Ce(Number(t)||0,0,1),this.hemisphere&&(this.hemisphere.intensity=this.workLight*1.8),this.workLamp&&(this.workLamp.intensity=this.workLight*3.6),this.scene&&(this.scene.environmentIntensity=this.workLight*1.25),this.ready&&this.updateStatus()}get fogAmount(){return this.fogOverride??(this.snapshot?.output?.atmosphere?1:.3)}setFog(t){this.fogOverride=t==null?null:Ce(Number(t)||0,0,1),this.fog?.setAmount(this.fogAmount)}burst(t){return!this.visible||!this.fog?.burst(t)?!1:(this.fogAnimationUntil=this.clock+8,!0)}setQuality(t="auto"){if(!["auto",...Object.keys(Bi)].includes(t))throw new Error("Unknown preview quality");let e=this.qualityChoice!==t;if(e&&(this.renderScale=1,this.overBudget=this.underBudget=0,this.scaleChangedAt=performance.now()),this.qualityChoice=t,!this.visible||!this.renderer)return;t==="auto"&&e&&(this.autoLayoutTier=this.autoQuality());let i=t==="auto"?e?this.autoLayoutTier:this.quality:t;if(i===this.quality){e&&this.resize();return}this.applyTier(i)}applyTier(t){this.fog.setQuality(Bi[t].fog||t),this.quality=t,this.options=Bi[t],this.frameInterval=1e3/this.targetHz,this.width&&this.fog.setSize(this.width,this.height),this.snapshot&&this.update(this.snapshot,!0),this.resize()}setLightingMode(t){t!==this.lighting.mode&&(this.lighting.setMode(t),this.visible&&this.renderer&&this.snapshot&&this.applyLighting(this.snapshot.output?.values||{}))}update(t,e=!1,i=performance.now()){if(!t||this.disposed)return;if(this.platform.worker||(this.editorInput||=new Ys(this),this.editorInput.update(t)),!this.visible||!this.renderer){this.snapshot=t;return}let s=Xi(t);if(this.opticalShow!==s&&this.glass?.dispose(),this.opticalShow=s,this.glass){this.glass.update(t["glass-meshes"],t["glass-volumes"]);for(let u of[this.composer.readBuffer,this.composer.writeBuffer])u.depthBuffer!==this.glass.enabled&&(u.dispose(),u.depthBuffer=this.glass.enabled)}this.editorOverlay||=new Sl,this.editorOverlay.update(t),this.snapshot=t;let r=t.rig||{},a=r===this.lastRig?this.key:rf(r);r!==this.lastRig&&(this.fixtureById=new Map((r.fixtures||[]).map(u=>[String(u.id),u])),this.beamById=new Map([...this.fixtureById].filter(([,u])=>!(u["pixel-layout-id"]&&u["diffuser-radius-m"])))),this.lastRig=r,(e||this.key!==a)&&(this.ready=!1,this.rebuild(r),this.key=a);for(let u of this.trusses||[]){let d=(r.trusses||[]).find(f=>String(f.id)===u.id);d&&(u.root.position.fromArray(d.position),u.root.rotation.set(...(d["rotation-deg"]||[0,0,0]).map(f=>f*Math.PI/180),"ZYX"))}this.fog?.setAtmosphere(s,t.output?.atmosphere),this.fog?.setAmount(this.fogAmount),this.lighting.accept(t,r,i);let o=t.editor?.selection,l=o?.kind==="fixtures"?String(o.id):null,c=Ff(this.beamById,t.output?.values||{},l,this.fixtures.map(u=>u.id),this.lightIds,this.options.shadows);this.lightIds=c.lights;let h=new Map(this.lightIds.map((u,d)=>[u,this.spots[d]]));this.fixtures.forEach((u,d)=>{let f=c.meshes[d],p=this.fixtureById.get(f);u.fixture!==p&&(u.setAperture(p),u.fixture=p,u.id=f,u.root.name="fixture:"+f),u.root.position.fromArray(p.position||[0,0,0]),u.root.rotation.set(...(p["rotation-deg"]||[0,0,0]).map(v=>v*Math.PI/180),"ZYX"),u.light=h.get(f)}),this.applyLighting(this.lighting.values(this.fixtureSamples,i))}applyLighting(t){this.displayedValues=t;for(let s of this.diffuserTubes)s.update(t);this.activeLights=0,this.lightEntries=[];let e=[];for(let s of this.fixtures){let r=t[s.id]||{},a=nf(s.fixture,r),o=Xs(r),l=new Je().setFromUnitVectors(B1,new A(...a));s.head.quaternion.copy(s.root.quaternion).invert().multiply(l),s.yoke.rotation.y=an(s.fixture,r,"pan")*Math.PI/180;let c=s.fixture["relative-brightness"]??1;if(s.lensMaterial.emissive.setRGB(...o).multiplyScalar(8*c),!s.light)continue;let h=s.light,u=c>0&&Math.max(...o)>0;u&&this.activeLights++,h.color.setRGB(...o),h.intensity=145*c,h.angle=sf(s.fixture,r),h.penumbra=Ce(.72-(Number(r.focus)||0)*.52,.2,.8);let d=Sf(s.fixture.position,a,h.angle,s.lensRadius);if(h.position.copy(d.virtual),h.distance=Math.max(...this.room)*2.5+d.apexDistance,h.shadow.camera.near!==d.apexDistance+.006&&(h.shadow.camera.near=d.apexDistance+.006,h.shadow.camera.updateProjectionMatrix()),h.shadow.camera.far=h.distance,h.userData.apexDistance=d.apexDistance,h.target.position.copy(h.position).add(new A(...a)),h.target.updateMatrixWorld(),!u)continue;let f={position:d.optical,virtualPosition:d.virtual,apexDistance:d.apexDistance,apertureRadius:d.radius,direction:d.axis,color:h.color,angle:h.angle,penumbra:h.penumbra,intensity:h.intensity,distance:h.distance,shadow:h.castShadow?h.shadow:null,pseudo:s.fixture["pseudo-fixture"]};this.lightEntries.push(f),this.reflections.addCandidate(e,s,f,r["beam-footprint"])}let i=new Mt(0,0,0);for(let s of this.lightEntries){if(s.pseudo||s.direction.y>=0)continue;let r=-s.position.y/s.direction.y,a=s.position.x+r*s.direction.x,o=s.position.z+r*s.direction.z;if(r<=0||Math.abs(a)>this.room[0]/2||Math.abs(o)>this.room[2]/2)continue;let l=-s.direction.y*.045*s.intensity/145/(1+r*r/24);i.r+=s.color.r*l,i.g+=s.color.g*l,i.b+=s.color.b*l}this.diffuseBounce.color.copy(i).multiplyScalar(.14),this.diffuseBounce.groundColor.copy(i).multiplyScalar(.55),this.bounceColor=i,this.spots.length&&(e.length&&this.content.updateMatrixWorld(!0),this.reflections.resolve(e),this.reflections.illuminate(this.fog.field,this.fogAmount,this.fog.extinctionPerM),this.spotUniformOrder.forEach((s,r)=>{this.apertureUniform.value[r]=s.userData.apexDistance||0,this.patchRadii.value[r]=s.userData.patchRadius||0})),this.ready&&this.updateStatus()}rebuild(t){if(this.content){this.scene.remove(this.content),bh(this.content);for(let r of this.fixtures)r.dispose();for(let r of this.diffuserTubes||[])r.dispose();for(let r of this.spots||[])r.dispose();this.reflections?.dispose()}this.room=yl(t.room),this.camera.far=Math.max(100,3*Math.hypot(...this.room)),this.camera.updateProjectionMatrix(),this.fog?.setRoom(this.room),this.content=new xe,this.scene.add(this.content),this.content.add(Ef(this.room,this.materials,t.surfaces||[])),this.trusses=[];for(let r of(t.trusses||[]).slice(0,24))if(Array.isArray(r.position)&&r.position.every(Number.isFinite)&&Number.isFinite(r.length)&&r.length>0){let a=bf(r,this.materials);this.content.add(a),this.trusses.push({id:String(r.id),root:a})}this.spots=[],this.lightIds=[];let e=t.fixtures||[];this.diffuserTubes=Tf(e.filter(r=>r["pixel-layout-id"]&&r["diffuser-radius-m"]),this.content),this.fixtures=e.filter(r=>!(r["pixel-layout-id"]&&r["diffuser-radius-m"])).slice(0,this.options.fixtures).map((r,a)=>{let o=wf(r,this.materials);Mh(o.lensMaterial,this.apertureUniform,this.patchRadii),o.id=String(r.id),o.fixture=r;let l=(r["rotation-deg"]||[0,0,0]).map(c=>(Number(c)||0)*Math.PI/180);if(o.root.rotation.set(...l,"ZYX"),this.content.add(o.root),a<er){let c=new Xn(0,145,Math.max(...this.room)*2.5,Math.PI/20,.15,2);c.position.copy(o.root.position),c.shadow.mapSize.set(this.options.shadowSize,this.options.shadowSize),c.shadow.camera.near=.06,c.shadow.camera.far=c.distance,c.shadow.bias=-8e-5,c.shadow.normalBias=.012,c.castShadow=a<this.options.shadows,this.spots.push(c),this.lightIds.push(o.id),this.content.add(c,c.target),o.light=c}return o}),this.fixtureSamples=this.fixtures.concat(this.diffuserTubes.flatMap(r=>r.rows));let i=e.filter(r=>r["pseudo-fixture"]).length,s=this.quality==="mobile"||this.quality==="low"?1:2;this.reflections=new Ul(this.content,Math.min(s,i),Math.min(512,this.options.shadowSize),this.fixtures),this.spotUniformOrder=[...this.spots.filter(r=>r.castShadow),...this.reflections.lights,...this.spots.filter(r=>!r.castShadow)],this.totalFixtureCount=(t.fixtures||[]).length,this.lightEntries=[]}measureLoad(t,e){if(this.qualityChoice!=="auto"||this.draws<2||!this.visible)return;this.measuredFrameMs=t;let i=1e3/this.targetHz;if(this.overBudget=t>i*1.35?(this.overBudget||0)+1:0,this.underBudget=t<i*.65?(this.underBudget||0)+1:0,e-this.scaleChangedAt<1e3)return;let s=this.renderScale;if(this.overBudget>=4?s=Math.max(.5,Math.min(s*.85,s*Math.sqrt(i/t))):this.underBudget>=60&&(s=Math.min(1,s+.1)),s===this.renderScale&&!this.softwareRenderer){let r=this.overBudget>=8&&s===.5?-1:this.underBudget>=60&&s===1?1:0,a=Jh(this.quality,r);if(a!==this.quality){let o=ga(a,this.qualityChoice,this.softwareRenderer)===ga(this.quality,this.qualityChoice,this.softwareRenderer);this.renderScale=r<0?o?s:.65:.9,this.scaleChangedAt=e,this.overBudget=this.underBudget=0,this.applyTier(a);return}}s!==this.renderScale&&(this.renderScale=s,this.scaleChangedAt=e,this.overBudget=this.underBudget=0,this.resizePending=!0)}resize(){if(!this.visible||!this.renderer||this.disposed)return;this.resizePending=!1;let t=this.platform.worker?this.platform.viewport:this.canvas.getBoundingClientRect();if(!t.width||!t.height)return;if(this.qualityChoice==="auto"){let l=this.autoQuality(t.width);if(l!==this.autoLayoutTier&&(this.autoLayoutTier=l,this.renderScale=1,this.overBudget=this.underBudget=0,this.scaleChangedAt=performance.now(),l!==this.quality)){this.applyTier(l);return}}let e=this.platform.worker?t.pixelRatio:devicePixelRatio,{scene:[i,s],display:[r,a]}=Yh(t.width,t.height,e,ga(this.quality,this.qualityChoice,this.softwareRenderer),this.renderScale,this.maxDimension);this.camera.aspect=t.width/t.height,this.camera.updateProjectionMatrix(),(r!==this.displayWidth||a!==this.displayHeight)&&(this.renderer.setSize(r,a,!1),this.displayWidth=r,this.displayHeight=a,this.displayRange=$h(this.renderer.getContext(),r,a,this.hdrRequested),this.outputPass.uniforms.extendedDisplay.value=this.displayRange==="extended");let o=Kh(this.quality,i,s,this.samples);i===this.width&&s===this.height&&this.sceneTarget.samples===o||(o!==this.sceneTarget.samples&&(this.sceneTarget.dispose(),this.sceneTarget.samples=o),this.sceneTarget.setSize(i,s),this.compositeTarget.setSize(i,s),this.composer.setSize(i,s),this.fog.setSize(i,s),this.width=i,this.height=s)}setCamera(t){this.camera&&(this.navigation?.setMode("orbit"),En(this.camera,this.controls,this.room,t),this.cameraName=t)}resetCamera(){this.setCamera("audience")}setNavigationMode(t){this.navigation?.setMode(t)}setNavigationSpeed(t){this.navigation?.setSpeed(t)}setInteractionActive(t){this.navigation?.setInteractionActive(t)}moveCamera(t,e){this.navigation?.move(t,e)}getNavigationState(){return this.navigation?.state()||null}onNavigationChange(t){return this.navigation?.subscribe(t)||(()=>{})}focusFixture(t){let e=this.fixtureById?.get(String(t));return e?(this.navigation?.setMode("orbit"),this.controls.target.fromArray(e.position||[0,0,0]),this.camera.position.copy(this.controls.target).add(new A(1.3,.5,2)),this.controls.update(),this.cameraName="fixture",!0):!1}changeActivity(t){if(this.refresh?.setActive(t&&!this.paused),this.visible=t,t||this.fog?.showAtmosphere?.suspend(),this.lastFrame=0,this.lighting.latest=this.lighting.previous=null,this.displayClock.cancel(),t||this.gpuTimer?.dispose(),this.navigation?.refreshActivity(),!(!t||this.disposed||this.error))try{this.renderer||this.initialize(),this.restorePending&&this.restoreContext(),this.setQuality(this.qualityChoice),this.snapshot&&this.update(this.snapshot),this.resize(),this.scheduleFrame(0)}catch(e){this.fail(e)}}restoreContext(){this.restorePending=!1,this.displayWidth=this.displayHeight=null,this.gpuTimer?.dispose(),this.platform.worker||(this.gpuTimer=new nr(this.renderer.getContext())),this.resize(),this.refreshEnvironment(),this.fog?.upload(),this.lost=!1,this.ready=!1,this.lastFrame=0,this.setStatus("Restoring venue shaders\u2026")}scheduleFrame(t){this.disposed||this.paused||!this.visible||this.lost||this.platform.autoStart===!1||(this.nextFrameAt=performance.now()+t,this.displayClock.at(this.nextFrameAt,e=>this.animate(e)))}pause(){this.paused=!0,this.refresh?.setActive(!1),this.gpuTimer?.dispose(),this.navigation?.refreshActivity(),this.lighting.previous=null,this.displayClock.cancel()}resume(){this.disposed||(this.paused=!1,this.refresh?.setActive(this.visible),this.navigation?.refreshActivity(),this.lastFrame=0,this.scheduleFrame(0))}animate(t){if(this.disposed||this.paused||!this.visible||this.lost)return;let e=this.lastFrame?Math.min(.1,(t-this.lastFrame)/1e3):0;this.lastFrame=t,this.clock+=e;try{let i=this.gpuTimer?.sample(),s=this.gpuTimer?.begin();try{this.draw(e)}finally{s&&this.gpuTimer.end()}i!=null&&(this.lastGpuMs=i,this.measureLoad(Math.max(this.lastCPU,i||0),performance.now())),this.scheduleFrame(Math.max(0,t+this.frameInterval-performance.now()))}catch(i){this.fail(i)}}draw(t=0){if(this.disposed||this.lost||!this.visible||!this.renderer)return!1;this.resizePending&&this.resize();let e=performance.now();if(this.platform.worker&&this.controls.update(),this.lighting.latest){let o=this.lighting.values(this.fixtureSamples,e);o!==this.displayedValues&&this.applyLighting(o)}let i=!this.reducedMotion.matches||this.clock<this.fogAnimationUntil,s=this.hemisphere.color.clone().multiplyScalar(this.workLight*.8).add(this.workLamp.color.clone().multiplyScalar(this.workLight*.65));this.bounceColor&&s.add(this.bounceColor),this.fog.setAmbient?.({color:s,intensity:1,groundColor:s.clone().multiplyScalar(.5)}),this.fog.update(i?t:0,this.lightEntries||[]),this.reflections.illuminate(this.fog.field,this.fogAmount,this.fog.extinctionPerM),this.renderer.setRenderTarget(this.sceneTarget),this.renderer.clear(),this.renderer.render(this.scene,this.camera),this.renderedShadows=this.fixtures.filter(o=>o.light?.castShadow&&o.light.shadow.map).length,this.inputPass.enabled=this.glass.enabled,this.fog.render(this.sceneTarget,this.camera,this.glass.enabled?this.compositeTarget:this.composer.readBuffer),this.film.uniforms.phase.value=this.reducedMotion.matches?0:Math.floor(this.clock*24)%1024,this.composer.render(t),this.editorOverlay?.render(this.renderer,this.camera),this.lastCPU=performance.now()-e,this.draws++,this.canvas.tolglowCaptureVideo?.(this.canvas),this.observedFrame?.(this.canvas,this.snapshot?.["show-id"],this.snapshot?.epoch,this.snapshot?.["frame-number"]??0);let r=1e3/this.targetHz,a=this.frameInterval||r;this.platform.worker||(this.frameInterval=Math.max(r,this.lastCPU*(this.softwareRenderer?4:1.5),a*.95)),this.ready||(this.ready=!0,this.resolveReady?.(!0)),this.updateStatus()}updateStatus(){let t=this.totalFixtureCount>er?` \xB7 ${this.activeLights} lit \xB7 ${er} light slots`:"",e=this.qualityChoice==="auto"&&this.softwareRenderer?"low \xB7 software GPU":this.quality;this.setStatus(`${e} \xB7 ${this.renderedShadows||0} shadows \xB7 ${this.workLight?"work light":"dark show"}${t}`)}getTelemetry(){return{lightingMode:this.lighting.mode,targetRefreshHz:this.targetHz,tierMaxHz:this.options.fps,displayHz:this.displayHz,ready:this.ready,failed:this.error||null,renderer:`Three.js r${"187"}`,quality:this.quality,deviceRenderer:this.deviceRenderer,softwareRenderer:this.softwareRenderer,pixels:[this.width,this.height],displayPixels:[this.displayWidth,this.displayHeight],sceneSamples:this.sceneTarget?.samples,edgeFilter:"FXAA",displayRange:this.displayRange,gpuTimeMs:this.lastGpuMs,film:{...this.filmSettings},measuredFrameMs:this.measuredFrameMs,renderScale:this.renderScale,frameCPUms:this.lastCPU,frameIntervalMs:this.frameInterval,idleBudgetMs:Math.max(0,this.frameInterval-this.lastCPU),nextFrameAt:this.nextFrameAt,frames:this.draws,geometryCount:this.renderer?.info.memory.geometries||0,textureCount:this.renderer?.info.memory.textures||0,programCount:this.renderer?.info.programs?.length||0,activeLights:this.activeLights||0,reflectedPatches:this.reflections?.patches.map(t=>({fixture:t.id,surface:t.surface,center:t.position.toArray(),majorRadius:t.major.length(),minorRadius:t.minor.length()}))||[],lightIds:this.lightIds,fixtureCount:this.totalFixtureCount,visibleFixtures:this.fixtures.length,lightCap:er,shadowCount:this.renderedShadows||0,workLight:this.workLight,fogAmount:this.fogAmount,camera:this.camera?.position.toArray(),target:this.controls?.target.toArray(),fog:this.fog?.stats(),visible:this.visible,hidden:this.platform.worker?this.platform.hidden:document.hidden,lost:this.lost,epoch:this.snapshot?.epoch,revision:this.snapshot?.revision,frameNumber:this.snapshot?.["frame-number"]}}fail(t){this.error=String(t?.message||t),this.ready=!1,this.setStatus(`Preview unavailable: ${this.error}`),this.pause(),this.resolveReady?.(!1),console.error("Tolglow preview",t)}dispose(){if(!this.disposed){this.disposed=!0,this.ready=!1,this.pause(),this.abort.abort(),this.resizeObserver?.disconnect(),this.activity?.dispose(),this.editorInput?.dispose(),this.editorOverlay?.dispose(),this.navigation?.dispose(),this.controls?.dispose(),this.fog?.dispose();for(let t of this.fixtures)t.dispose();for(let t of this.diffuserTubes||[])t.dispose();for(let t of this.spots||[])t.dispose();this.reflections?.dispose(),this.content&&bh(this.content),this.glass?.dispose(),this.inputPass?.dispose(),this.bloom?.dispose(),this.outputPass?.dispose(),this.edges?.dispose(),this.film?.dispose(),this.gpuTimer?.dispose(),this.composer?.dispose(),this.sceneTarget?.dispose(),this.compositeTarget?.dispose(),this.environmentTarget?.dispose(),this.materials&&Mf(this.materials),this.renderer?.dispose(),this.resolveReady?.(!1),this.canvas.tolglowPreview===this&&delete this.canvas.tolglowPreview}}};var Xl=class{constructor(t){this.send=t,this.sequence=0,this.inFlight=null,this.pending=null,this.ready=!1,this.closed=!1,this.coalesced=0}offer(t){this.closed||(this.pending&&this.coalesced++,this.pending=t,this.flush())}open(){this.ready=!0,this.flush()}flush(){if(!this.closed&&this.ready&&!this.inFlight&&this.pending){let t={type:"state",id:++this.sequence,state:this.pending};this.pending=null,this.inFlight=t.id,this.send(t)}}acknowledge(t){return t!==this.inFlight?!1:(this.inFlight=null,this.flush(),!0)}close(){this.closed=!0,this.pending=null,this.inFlight=null}};function Bf(){return typeof Worker<"u"&&typeof OffscreenCanvas<"u"&&typeof OffscreenCanvas.prototype.transferToImageBitmap=="function"}var ql=class{constructor(t,e,i={}){this.canvas=t,t.tolglowPreview=this,this.status=e,this.disposed=!1,this._ready=!1,this.presented=!1,this.room=[12,5,10],this.snapshot=null,this.identity=Xi(null),this.visible=!1,this.generation=0,this.paused=!1,this.filmSettings={},this.lightingMode="hold",this.fogOverride=null,this.workLight=.12,this.qualityChoice="auto",this.actions=[],this.actionId=0,this.lastFrame=0,this.discardedFrames=0,this.telemetry={},this.rendering=!1,this.navigationListeners=new Set,this.abort=new AbortController,this.whenReady=new Promise(r=>{this.resolveReady=r}),this.camera=new Te(49,1,.08,100),this.controls=El(this.camera,t),En(this.camera,this.controls,this.room,"audience"),this.cameraName="audience",this.controls.addEventListener("change",()=>this.sendState()),this.reducedMotion=matchMedia("(prefers-reduced-motion: reduce)"),this.coarsePointer=matchMedia("(pointer: coarse)");let s=this.abort.signal;this.reducedMotion.addEventListener("change",()=>{this.navigation?.notify(),this.sendState()},{signal:s}),this.coarsePointer.addEventListener("change",()=>this.sendState(),{signal:s}),this.navigation=new Zs({camera:this.camera,controls:this.controls,canvas:t,room:()=>this.room,snapshot:()=>this.snapshot,visibleLights:()=>this.telemetry.lightIds,active:()=>this.visible&&!this.paused&&!this.disposed,reducedMotion:()=>this.reducedMotion.matches,changed:()=>this.sendState()}),this.navigation.subscribe(r=>this.notifyNavigation(r)),this.resizeObserver=new ResizeObserver(()=>this.resize()),this.resizeObserver.observe(t),this.displayHz=60,this.refresh=new tr(r=>{this.displayHz=r,this.sendState()}),this.activity=new Qs(t,r=>this.changeActivity(r)),this.setStatus("Starting background room renderer\u2026");try{let r=i.workerFactory||(a=>new Worker(a,{type:"module",name:"tolglow-room-preview"}));this.worker=r(new URL("./preview-worker.js",import.meta.url)),this.channel=new Xl(a=>{let o=a.state.snapshot,c=o!==this.lastSentSnapshot&&o?{...o}:null;c&&o.rig===this.lastSentSnapshot?.rig&&delete c.rig;for(let h of["editor","sculpt-guide","volume-guides","glass-meshes","glass-volumes"])c&&o[h]===this.lastSentSnapshot?.[h]&&delete c[h];this.worker.postMessage({...a,state:{...a.state,snapshot:c}}),this.lastSentSnapshot=o}),this.worker.onmessage=a=>this.receive(a.data),this.worker.onerror=a=>{a.preventDefault(),this.fail(a.message||"Background renderer failed")},this.worker.onmessageerror=()=>this.fail("Background renderer message could not be read"),this.checkStartup=()=>{this.ready||this.disposed||this.fallback||this.visible&&!this.paused&&this.fail("Background renderer did not produce its first frame")},this.startupTimeout=i.startupTimeout??45e3,this.resize()}catch(r){this.fail(r)}}get ready(){return this.fallback?this.fallback.ready:this._ready}setStatus(t){this.status&&this.status.textContent!==t&&(this.status.textContent=t)}changeActivity(t){this.refresh.setActive(t&&!this.paused),!(this.disposed||this.fallback)&&(this.visible=t,++this.generation,this.navigation.refreshActivity(),clearTimeout(this.startupTimer),this.worker?.postMessage({type:"activity",active:t,generation:this.generation}),this.lastSentSnapshot=null,t?(!this.ready&&!this.paused&&(this.startupTimer=setTimeout(this.checkStartup,this.startupTimeout)),this.sendState()):(this.channel&&(this.channel.pending=null),this.actions=[],this.drawRequest?.resolve(!1),this.drawRequest=null))}sendState(){if(this.disposed||this.fallback||!this.channel||!this.visible)return;let t=this.canvas.getBoundingClientRect();this.camera.aspect=Math.max(1,t.width)/Math.max(1,t.height),this.camera.updateProjectionMatrix(),this.channel.offer({snapshot:this.snapshot,receivedAt:this.receivedAt,lightingMode:this.lightingMode,viewport:{width:Math.max(1,t.width),height:Math.max(1,t.height),pixelRatio:devicePixelRatio||1},generation:this.generation,paused:this.paused,reducedMotion:this.reducedMotion.matches,coarsePointer:this.coarsePointer.matches,quality:this.qualityChoice,displayHz:this.displayHz,film:this.filmSettings,fog:this.fogOverride,workLight:this.workLight,camera:{position:this.camera.position.toArray(),target:this.controls.target.toArray(),up:this.camera.up.toArray(),fov:this.camera.fov,name:this.cameraName},actions:this.actions.slice()})}receive(t){if(this.disposed||this.fallback){t.bitmap?.close();return}if(t.type==="ready")this.channel.open();else if(t.type==="state-ack")this.actions=this.actions.filter(e=>e.id>t.actionThrough),this.channel.acknowledge(t.id);else if(t.type==="status")this.setStatus(t.text);else if(t.type==="render-start")this.rendering=!0,this.startedRendering=!0;else if(t.type==="render-retired")this.rendering=!1;else if(t.type==="error")this.fail(t.message);else if(t.type==="frame"){this.rendering=!1;try{if(!this.visible||t.generation!==this.generation){t.bitmap.close(),this.discardedFrames++;return}let e=jh(t,this.identity,this.lastFrame,i=>{if(!this.presenter&&(this.presenter=this.canvas.getContext("bitmaprenderer"),this.bitmapPresenter=!!this.presenter,this.presenter||(this.presenter=this.canvas.getContext("2d",{alpha:!1})),!this.presenter))throw new Error("No canvas bitmap presentation context");(this.canvas.width!==i.width||this.canvas.height!==i.height)&&(this.canvas.width=i.width,this.canvas.height=i.height),this.canvas.tolglowCaptureVideo?.(i),this.bitmapPresenter?this.presenter.transferFromImageBitmap(i):this.presenter.drawImage(i,0,0),this.presented=!0,this.observedFrame?.(this.canvas,t.showId,t.epoch,t.showFrame)});this.lastFrame=Math.max(this.lastFrame,t.frameId),e?(this.telemetry=t.telemetry,this._ready=!0,clearTimeout(this.startupTimer),this.resolveReady(!0),this.drawRequest&&t.actionThrough>=this.drawRequest.id&&(this.drawRequest.resolve(!0),this.drawRequest=null)):(this.discardedFrames++,this.drawRequest&&(this.drawRequest.id=++this.actionId,this.actions.push({id:this.actionId,type:"draw"}),this.sendState()))}catch(e){this.fail(e)}finally{this.worker&&this.worker.postMessage({type:"frame-ack",frameId:t.frameId})}}}update(t){if(this.fallback)return this.fallback.update(t);if(!t||this.disposed)return;this.editorInput||=new Ys(this),this.editorInput.update(t);let e=Xi(t);this.identity!==e&&(this.identity=e,this._ready=!1,this.bitmapPresenter?this.presenter.transferFromImageBitmap(null):this.presenter?.clearRect(0,0,this.canvas.width,this.canvas.height)),this.receivedAt=performance.timeOrigin+performance.now(),this.snapshot={"runtime-id":t["runtime-id"],"time-ms":t["time-ms"],"blackout?":t["blackout?"],master:t.master,"cut?":t["cut?"],editor:t.editor??null,"show-id":t["show-id"],epoch:t.epoch,revision:t.revision,"frame-number":t["frame-number"],rig:t.rig,"spatial-volumes":t["spatial-volumes"],"glass-meshes":t["glass-meshes"]??null,"glass-volumes":t["glass-volumes"]??null,"sculpt-guide":t["sculpt-guide"]??null,"volume-guides":t["volume-guides"]??null,output:{values:t.output?.values,atmosphere:t.output?.atmosphere}},this.room=yl(t.rig?.room),this.camera.far=Math.max(100,3*Math.hypot(...this.room)),this.camera.updateProjectionMatrix(),this.sendState()}resize(){if(this.fallback)return this.fallback.resize();this.sendState()}setLightingMode(t){if(!["hold","interpolate"].includes(t))throw new RangeError("Unknown preview lighting mode");if(this.lightingMode=t,this.fallback)return this.fallback.setLightingMode(t);this.sendState()}setQuality(t="auto"){if(!["auto","low","mobile","desktop","high"].includes(t))throw new Error("Unknown preview quality");if(this.qualityChoice=t,this.fallback)return this.fallback.setQuality(t);this.sendState()}get fogAmount(){return this.fogOverride??(this.snapshot?.output?.atmosphere?1:.3)}setFog(t){if(this.fogOverride=t==null?null:Ce(Number(t)||0,0,1),this.fallback)return this.fallback.setFog(this.fogOverride);this.sendState()}setFilm(t){if(Object.assign(this.filmSettings,t),this.fallback)return this.fallback.setFilm(this.filmSettings);this.sendState()}setWorkLight(t){if(this.workLight=Ce(Number(t)||0,0,1),this.fallback)return this.fallback.setWorkLight(t);this.sendState()}burst(t){return this.fallback?this.fallback.burst(t):!this.visible||this.snapshot?.output?.atmosphere||this.actions.length>=32?!1:(this.actions.push({id:++this.actionId,type:"burst",options:t}),this.sendState(),!0)}setCamera(t){if(this.fallback){this.fallback.setCamera(t),this.cameraName=t;return}this.navigation.setMode("orbit"),En(this.camera,this.controls,this.room,t),this.cameraName=t,this.sendState()}resetCamera(){this.setCamera("audience")}setNavigationMode(t){if(this.fallback)return this.fallback.setNavigationMode(t);this.navigation.setMode(t)}setNavigationSpeed(t){if(this.fallback)return this.fallback.setNavigationSpeed(t);this.navigation.setSpeed(t)}setInteractionActive(t){if(this.fallback)return this.fallback.setInteractionActive(t);this.navigation.setInteractionActive(t)}moveCamera(t,e){if(this.fallback)return this.fallback.moveCamera(t,e);this.navigation.move(t,e)}getNavigationState(){return this.fallback?this.fallback.getNavigationState():this.navigation.state()}notifyNavigation(t){for(let e of this.navigationListeners)e(t)}onNavigationChange(t){return this.navigationListeners.add(t),t(this.getNavigationState()),()=>this.navigationListeners.delete(t)}focusFixture(t){if(this.fallback)return this.fallback.focusFixture(t);let e=this.snapshot?.rig?.fixtures?.find(i=>String(i.id)===String(t));return e?.position?(this.navigation.setMode("orbit"),this.controls.target.fromArray(e.position),this.camera.position.fromArray(e.position).add(new A(1.3,.5,2)),this.controls.update(),this.cameraName="fixture",this.sendState(),!0):!1}pause(){if(this.paused=!0,this.refresh.setActive(!1),this.fallback)return this.fallback.pause();this.navigation.refreshActivity(),this.sendState()}resume(){if(this.paused=!1,this.fallback||this.refresh.setActive(this.visible),this.fallback)return this.fallback.resume();this.navigation.refreshActivity(),this.sendState()}draw(){if(this.disposed||this.error)return Promise.resolve(!1);if(this.fallback)return Promise.resolve(this.fallback.draw()!==!1);if(!this.visible)return Promise.resolve(!1);if(this.drawRequest)return this.drawRequest.promise;let t=++this.actionId,e,i=new Promise(s=>{e=s});return this.drawRequest={id:t,promise:i,resolve:e},this.actions.push({id:t,type:"draw"}),this.sendState(),i}getTelemetry(){return this.fallback?{...this.fallback.getTelemetry(),thread:"main",fallbackReason:this.fallbackReason}:{...this.telemetry,ready:this.ready,failed:this.error||null,thread:"worker",rendering:this.rendering,pendingState:!!this.channel?.pending,inFlightState:this.channel?.inFlight,coalescedStates:this.channel?.coalesced||0,discardedFrames:this.discardedFrames,visible:this.visible,hidden:document.hidden,paused:this.paused,navigation:this.getNavigationState(),camera:this.camera.position.toArray(),target:this.controls.target.toArray()}}stopProxy(){this.refresh?.setActive(!1),this.editorInput?.dispose(),this.editorInput=null,clearTimeout(this.startupTimer),this.navigation?.dispose(),this.abort.abort(),this.resizeObserver?.disconnect(),this.activity?.dispose(),this.controls.dispose(),this.channel?.close(),this.worker?.terminate(),this.worker=null}fail(t){if(this.disposed||this.fallback)return;let e=this.navigation?.state();this.stopProxy(),this.rendering=!1;let i=String(t?.message||t);if(!this.presenter&&!this.startedRendering){if(this.fallbackReason=i,this.fallback=new An(this.canvas,this.status),this.canvas.tolglowPreview=this,Object.defineProperty(this.fallback,"observedFrame",{get:()=>this.observedFrame}),this.fallback.error){this.resolveReady(!1),this.drawRequest?.resolve(!1),this.drawRequest=null;return}this.fallback.setLightingMode(this.lightingMode),this.fallback.setQuality(this.qualityChoice),this.fallback.setFilm(this.filmSettings),this.fallback.setWorkLight(this.workLight),this.fallback.setFog(this.fogOverride),this.snapshot&&this.fallback.update(this.snapshot),this.fallback.camera.position.copy(this.camera.position),this.fallback.controls.target.copy(this.controls.target),this.fallback.controls.update(),this.camera=this.fallback.camera,this.controls=this.fallback.controls,e&&(this.fallback.setNavigationMode(e.mode),this.fallback.setNavigationSpeed(e.speed),this.fallback.setInteractionActive(e.interacting)),this.fallback.onNavigationChange(s=>this.notifyNavigation(s)),this.paused&&this.fallback.pause(),this.fallback.whenReady.then(this.resolveReady)}else this.error=i,this._ready=!1,this.resolveReady(!1),this.setStatus(`Preview unavailable: ${i}`);this.drawRequest?.resolve(!1),this.drawRequest=null}dispose(){this.disposed||(this.disposed=!0,this._ready=!1,this.stopProxy(),this.fallback?.dispose(),this.navigationListeners.clear(),this.bitmapPresenter?this.presenter.transferFromImageBitmap(null):this.presenter?.clearRect(0,0,this.canvas.width,this.canvas.height),this.resolveReady(!1),this.drawRequest?.resolve(!1),this.drawRequest=null,this.canvas.tolglowPreview===this&&delete this.canvas.tolglowPreview)}};function z1(n,t,e=document){return typeof n?.captureStream=="function"&&!!(e.pictureInPictureEnabled&&t.requestPictureInPicture||t.webkitSetPresentationMode&&t.webkitSupportsPresentationMode)}var Ih=class{constructor(t,e,i,s){this.canvas=t,this.video=e,this.changed=i,this.failed=s,this.state="idle",this.external=!1,this.abort=new AbortController;let r={signal:this.abort.signal},a=()=>{this.stream&&(this.publish(!0),this.set("active"))};e.addEventListener("enterpictureinpicture",a,r),e.addEventListener("leavepictureinpicture",()=>this.release(),r),e.addEventListener("webkitpresentationmodechanged",()=>{e.webkitPresentationMode==="picture-in-picture"?a():this.state==="active"&&this.release()},r)}set(t){this.state=t,this.changed(t)}publish(t){this.external!==t&&(this.external=t,this.canvas.dispatchEvent(new CustomEvent("tolglow-preview-external",{detail:t})))}async prepare(){if(this.state==="idle"){this.set("preparing");try{let t=this.canvas.captureStream(60);this.stream=t,this.video.srcObject=t,await this.video.play(),this.stream===t&&!this.abort.signal.aborted&&this.set("ready")}catch(t){!this.abort.signal.aborted&&this.state==="preparing"&&(this.release(),this.failed(t.message))}}}async present(){if(this.state==="ready"){this.set("opening");try{if(this.video.ownerDocument.pictureInPictureEnabled&&this.video.requestPictureInPicture)await this.video.requestPictureInPicture();else if(this.video.webkitSupportsPresentationMode?.("picture-in-picture"))this.video.webkitSetPresentationMode("picture-in-picture");else throw new Error("This browser cannot present captured canvas video in Picture in Picture.")}catch(t){this.abort.signal.aborted||(this.release(),this.failed(t.message))}}}close(){let t=this.video.ownerDocument;t.pictureInPictureElement===this.video?t.exitPictureInPicture().catch(()=>{}):this.video.webkitPresentationMode==="picture-in-picture"&&this.video.webkitSetPresentationMode("inline"),this.release()}release(){this.publish(!1),this.video.pause(),this.video.srcObject=null,this.stream?.getTracks().forEach(t=>t.stop()),this.stream=null,this.state!=="idle"&&this.set("idle")}dispose(){this.abort.abort(),this.close()}};var zf=960,kf=160,Dh=zf/kf,Hf=68*Math.PI/180,k1=100*Math.PI/180;function H1(n,t){let e=n/(t*Math.cos(n));return{aspect:Math.tan(n)/e,fov:2*Math.atan(e)*180/Math.PI}}function V1(n,t,e,i){let[s,r,a]=i,o=e?.min||[-s/3,0,-a/4],l=e?.max||[s/3,r*.8,0],c=new xi(new A(...o),new A(...l));c.expandByPoint(new A((o[0]+l[0])/2,0,Math.min(a/2,l[2]+a*.3))),c.expandByScalar(.35);let h=c.getCenter(new A),u=new A(0,.08,1).normalize(),d=new A(0,u.z,-u.y),f=Math.tan(n.fov*Math.PI/360),p=f*n.aspect,v=1;for(let g of[c.min.x,c.max.x])for(let m of[c.min.y,c.max.y])for(let _ of[c.min.z,c.max.z]){let S=new A(g,m,_).sub(h);v=Math.max(v,S.dot(u)+1.08*Math.max(Math.abs(S.x)/p,Math.abs(S.dot(d))/f))}t.target.copy(h),n.position.copy(h).addScaledVector(u,v),n.far=Math.max(100,v+c.getSize(new A).length()+a),t.update()}function G1(n,t,e,i){let[s,r,a]=i,o=e?.min||[-s/3,0,-a/4],l=e?.max||[s/3,r*.8,0],c=(o[0]+l[0])/2,h=(o[2]+l[2])/2,u=Math.max(a*.24,(l[0]-o[0])/(2*Math.tan(Math.PI*.265)));n.position.set(c,r*.38,h+u),t.target.set(c,o[1]+(l[1]-o[1])*.74,h),n.far=Math.max(100,u+s+a),t.update()}var Lh=class{constructor(t){let e=t.getDrawingBufferSize(new et);this.image=new br(e.x,e.y),this.image.minFilter=this.image.magFilter=ae,this.material=new oe({uniforms:{image:{value:this.image},bend:{value:Hf}},depthTest:!1,depthWrite:!1,toneMapped:!1,vertexShader:"varying vec2 p; void main(){p=uv*2.0-1.0; gl_Position=vec4(position.xy,0.0,1.0);}",fragmentShader:`uniform sampler2D image; uniform float bend; varying vec2 p;
        void main(){
          float theta=p.x*bend;
          vec2 source=vec2(tan(theta)/tan(bend),p.y*cos(bend)/cos(theta));
          gl_FragColor=texture2D(image,0.5+0.5*source);
        }`}),this.quad=new Mi(this.material)}render(t){let e=t.getRenderTarget();t.setRenderTarget(null);try{t.copyFramebufferToTexture(this.image),this.quad.render(t)}finally{t.setRenderTarget(e)}}dispose(){this.image.dispose(),this.material.dispose(),this.quad.dispose()}},Nh=class{constructor(t){if(this.canvas=t,this.engine=new An(t,null,{worker:!0,autoStart:!1,coarsePointer:!0,reducedMotion:!1,viewport:{width:zf,height:kf,pixelRatio:1}}),this.engine.error||!this.engine.renderer){let e=this.engine.error||"Venue graphics are unavailable";throw this.dispose(),new Error(e)}this.engine.setQuality(this.engine.softwareRenderer?"low":"mobile")}update(t){if(!this.engine||document.hidden||!this.canvas.isConnected)return;let e=this.engine,i=performance.now(),s=JSON.stringify(t.rig);this.rigKey!==s&&(this.rigKey=s,this.rig=t.rig),t.rig=this.rig;let r=t.camera||"fit";if(e.lost)return;if(e.update(t,!1,i),r!==this.camera||this.framedRig!==this.rig){let o=e.camera;r==="panorama"?Object.assign(o,H1(Hf,Dh)):(o.aspect=Dh,o.fov=2*Math.atan(Math.tan(k1/2)/Dh)*180/Math.PI),r==="fit"?V1(o,e.controls,t.bounds,e.room):r==="panorama"?G1(o,e.controls,t.bounds,e.room):En(o,e.controls,e.room,"audience"),o.updateProjectionMatrix(),this.camera=r,this.framedRig=this.rig}e.setWorkLight(t.editor?.42:.12);let a=this.lastFrame===void 0?0:Math.min(.1,(i-this.lastFrame)/1e3);this.lastFrame=i,e.clock+=a,e.draw(a),r==="panorama"&&(this.lens||=new Lh(e.renderer),this.lens.render(e.renderer))}dispose(){if(this.lens?.dispose(),this.engine){let t=this.engine.renderer;this.engine.dispose(),t?.forceContextLoss(),this.engine=null}}};var Vf=class{constructor(t,e,i={}){return i.worker!==!1&&Bf()&&!xa()?new ql(t,e,i):new An(t,e)}};export{Ih as CanvasPlayback,Nh as PushScene,Vf as RoomPreview,An as SceneRenderer,z1 as canPresentCanvas};
/*! Bundled license information:

three/build/three.core.js:
three/build/three.module.js:
  (**
   * @license
   * Copyright 2010-2026 Three.js Authors
   * SPDX-License-Identifier: MIT
   *)
*/
