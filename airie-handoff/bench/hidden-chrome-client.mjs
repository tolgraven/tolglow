import {setTimeout as sleep} from 'node:timers/promises';

export async function startHiddenChrome() {
  const endpoint=await (await fetch('http://127.0.0.1:9223/json/version')).json();
  const ws=new WebSocket(endpoint.webSocketDebuggerUrl);
  await new Promise((resolve,reject)=>{
    ws.addEventListener('open',resolve,{once:true});
    ws.addEventListener('error',reject,{once:true});
  });
  const pending=new Map(),pageErrors=[];let seq=0;
  ws.addEventListener('message',({data})=>{
    const item=JSON.parse(data);
    if(item.method==='Runtime.exceptionThrown')pageErrors.push(item.params.exceptionDetails.exception?.description||item.params.exceptionDetails.text);
    if(!item.id||!pending.has(item.id))return;
    const p=pending.get(item.id);pending.delete(item.id);clearTimeout(p.timer);
    if(item.error)p.reject(Error(item.error.message));else p.resolve(item.result);
  });
  const send=(method,params={},sessionId)=>new Promise((resolve,reject)=>{
    const id=++seq;
    const timer=setTimeout(()=>{pending.delete(id);reject(Error(method+' timed out'));},10000);
    pending.set(id,{resolve,reject,timer});
    ws.send(JSON.stringify({id,method,params,...(sessionId?{sessionId}:{})}));
  });
  let targetId;
  try{
    ({targetId}=await send('Target.createTarget',{url:'about:blank',background:true,hidden:true,focus:false}));
    const {sessionId}=await send('Target.attachToTarget',{targetId,flatten:true});
    const page=(method,params)=>send(method,params,sessionId);
    await page('Page.enable');await page('Runtime.enable');
    const evaluate=async(fn,...args)=>{
      const expression='('+fn.toString()+')(...'+JSON.stringify(args)+')';
      const result=await page('Runtime.evaluate',{expression,returnByValue:true,awaitPromise:true});
      if(result.exceptionDetails)throw Error(result.exceptionDetails.exception?.description||result.exceptionDetails.text);
      return result.result.value;
    };
    const waitFor=async(fn,limit=15000)=>{
      const start=Date.now();
      while(Date.now()-start<limit){if(await evaluate(fn))return;await sleep(90);}
      throw Error('Hidden target condition timed out: '+String(fn).slice(0,200));
    };
    return {pageErrors,page,evaluate,waitFor,
      async close(){await send('Target.closeTarget',{targetId}).catch(()=>{});ws.close();}};
  }catch(error){
    if(targetId)await send('Target.closeTarget',{targetId}).catch(()=>{});
    ws.close();throw error;
  }
}
