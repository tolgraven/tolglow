import * as THREE from 'three';
import {GlassPass} from '../../../js/preview/glass.js';

export function probe(canvas) {
  const w=320,h=240,renderer=new THREE.WebGLRenderer({canvas,antialias:false});
  renderer.setSize(w,h,false);
  const camera=new THREE.PerspectiveCamera(42,w/h,.1,100);
  camera.position.set(0,0,6); camera.lookAt(0,0,0);
  const scene=new THREE.Scene(),background=new THREE.ShaderMaterial({
    vertexShader:'varying vec2 p; void main(){p=uv;gl_Position=projectionMatrix*modelViewMatrix*vec4(position,1.0);}',
    fragmentShader:'varying vec2 p;void main(){float v=step(0.5,fract(p.x*32.0+p.y*1.5));gl_FragColor=vec4(mix(vec3(.025,.16,.30),vec3(.85,.39,.04),v),1.0);}'});
  const backdrop=new THREE.Mesh(new THREE.PlaneGeometry(10,10),background);
  backdrop.position.z=-1.5;scene.add(backdrop);
  const blocker=new THREE.Mesh(new THREE.PlaneGeometry(.28,1.6),new THREE.MeshBasicMaterial({color:0x1de51a}));
  blocker.position.set(-.55,0,1.7);scene.add(blocker);
  const source=new THREE.WebGLRenderTarget(w,h),result=new THREE.WebGLRenderTarget(w,h);
  source.depthTexture=new THREE.DepthTexture(w,h);
  const environment=new THREE.WebGLCubeRenderTarget(8);
  const pass=new GlassPass({camera,compositeTarget:source,sceneTarget:source,environmentTarget:environment,
    fog:{target:source},fogAmount:0,workLight:.35,clock:1,spots:[]});
  pass.setSize(w,h);
  const r=30,field=new Float32Array(r**3);
  for(let z=0;z<r;z++)for(let y=0;y<r;y++)for(let x=0;x<r;x++){
    const [a,b,c]=[x,y,z].map(v=>(2*v/r-1)*1.24);
    field[x+r*(y+r*z)]=-Math.max(Math.hypot(a,b,c)-1,.35-Math.hypot(a,b));
  }
  try{
    renderer.setRenderTarget(source);renderer.render(scene,camera);
    renderer.setRenderTarget(result);renderer.render(scene,camera);
    const before=new Uint8Array(w*h*4),after=new Uint8Array(w*h*4);
    renderer.readRenderTargetPixels(result,0,0,w,h,before);
    pass.update({ring:{kind:'field',resolution:r,field,padding:1.24}},
      [{id:'orb',mesh:'ring',position:[0,0,0],'half-size':[1,1,1],color:[.2,.7,1],strength:1}]);
    pass.render(renderer,null,result);
    renderer.readRenderTargetPixels(result,0,0,w,h,after);
    const difference=(x,y)=>{const i=(y*w+x)*4;return Math.max(...[0,1,2].map(c=>Math.abs(before[i+c]-after[i+c])));};
    const screen=(x,y,z)=>{const p=new THREE.Vector3(x,y,z).project(camera);
      return [Math.floor((p.x+1)*w/2),Math.floor((p.y+1)*h/2)];};
    let altered=0,shiftedStripes=0;
    for(let y=0;y<h;y++)for(let x=0;x<w;x++) {
      if(difference(x,y)>5) altered++;
      if(difference(x,y)>90) shiftedStripes++;
    }
    return {altered,shiftedStripes,hole:difference(...screen(0,0,0)),
      foreground:difference(...screen(-.55,0,1.7)),outside:difference(8,12)};
  } finally {
    pass.dispose(); source.dispose();result.dispose();environment.dispose();
    for(const object of scene.children){object.geometry.dispose();object.material.dispose();}
    renderer.dispose();
  }
}
