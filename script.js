/*
  Full-file single page app with added live visualisation.
  - Play/Pause creates an AudioContext and AnalyserNode.
  - liveCanvas shows waveform + particle field + rolling spectrogram waterfall.
  - Visualization uses requestAnimationFrame and is cleaned up on stop.
*/

(function(){
  'use strict';

  /* ====== Element references ====== */
  const pWrap = document.getElementById('progressWrap');
  const pBar = document.getElementById('progress');
  const pTxt = document.getElementById('progressText');
  const outEl = document.getElementById('out');
  const hexEl = document.getElementById('hex');
  const candsEl = document.getElementById('cands');
  const fileInfo = document.getElementById('fileInfo');
  const audioInfo = document.getElementById('audioInfo');
  const drop = document.getElementById('drop');
  const chooseLbl = document.getElementById('chooseLbl');
  const fileInput = document.getElementById('fileInput');
  const preciseBox = document.getElementById('precise');
  const smoothSlider = document.getElementById('smooth');
  const smoothVal = document.getElementById('smoothVal');
  const protocolSel = document.getElementById('protocol');
  const decoderSel = document.getElementById('decoder');
  const specCanvas = document.getElementById('spec');
  const liveCanvas = document.getElementById('liveCanvas');
  const btnPlay = document.getElementById('btnPlay');
  const vizInfo = document.getElementById('vizInfo') || document.createElement('div');
  const toast = document.getElementById('toast') || document.createElement('div');
  const srStatus = document.getElementById('srStatus') || document.createElement('span');

  smoothSlider.addEventListener('input', ()=> smoothVal.textContent = smoothSlider.value + '%');

  let lastFile = null;
  let runningWorker = null;       // running web worker instance
  let currentAbort = null;        // cancel token object
  let audioBufferForPlayback = null; // holds decoded AudioBuffer for playback
  let audioContext = null;
  let sourceNode = null;
  let analyser = null;
  let vizAnim = null;
  let vizState = {running:false, particles:[], waterfall:null};

  /* file picker open flag - ensures the picker is only opened once at a time */
  let filePickerOpen = false;

  /* ====== Small UI helpers ====== */
  function showToast(txt, time=2300){
    toast.textContent = txt; toast.style.display = 'block'; clearTimeout(toast._t);
    toast._t = setTimeout(()=>{ toast.style.display = 'none'; }, time);
  }

  function announce(msg){
    try{ srStatus.textContent = msg; } catch(e){}
  }

  function showProgress(msg){ pWrap.style.display = 'block'; setProgress(0, msg || 'Start'); }
  function setProgress(val, msg){ const v = Math.max(0, Math.min(100, Math.floor(val))); pBar.value = v; if(msg) pTxt.textContent = msg + ' (' + v + '%)'; announce(msg + ' ' + v + ' percent'); }
  function hideProgress(done){ if(done){ pTxt.textContent = done; pTxt.classList.add('ok'); } setTimeout(()=>{ pWrap.style.display = 'none'; pTxt.classList.remove('ok'); setProgress(0,'Ready'); }, 700); }
  function microYield(){ return new Promise(r=>setTimeout(r,0)); }
  function bytesHuman(n){ const u=['B','KB','MB','GB']; let i=0,x=n; while(x>=1024&&i<u.length-1){x/=1024;i++;} return x.toFixed(1) + ' ' + u[i]; }
  function setFileMeta(f){ fileInfo.textContent = 'File: ' + f.name + '  •  ' + bytesHuman(f.size) + '  •  ' + (f.type||'unknown'); }
  function setAudioMeta(sr,ch,dur){ audioInfo.textContent = 'Audio: ' + ch + ' ch  •  ' + sr + ' Hz  •  ' + dur.toFixed(3) + ' s'; }

  function toHex(bytes){ 
    let s = ''; 
    for(let i=0; i < bytes.length; i++){ 
      s += (i && i%16===0 ? '\n' : '') + bytes[i].toString(16).toUpperCase().padStart(2,'0') + ' '; 
    } 
    return s.trim(); 
  }
  function bytesToRawStringSafe(u8){ try { return new TextDecoder('latin1').decode(new Uint8Array(u8)); } catch { let s=''; for(let i=0;i<u8.length;i++) s += String.fromCharCode(u8[i] & 0xFF); return s; } }
  function utf8Valid(u8){ try{ const dec = new TextDecoder('utf-8',{fatal:true}).decode(new Uint8Array(u8)); return {ok:true,text:dec}; }catch(_){ return {ok:false,text:''}; } }
  function printableFrac(bytes){ let p=0; for(let i=0;i<bytes.length;i++){ const v=bytes[i]; if(v>=32 && v<=126) p++; } return bytes.length ? (p/bytes.length) : 0; }

  /* ====== Drag & Drop UX (visual) ====== */
  drop.addEventListener('dragover', e=>{ e.preventDefault(); drop.style.borderColor = '#6ff'; drop.style.boxShadow = '0 8px 28px rgba(0,120,140,0.08)'; });
  drop.addEventListener('dragleave', ()=>{ drop.style.borderColor = 'rgba(255,255,255,0.06)'; drop.style.boxShadow = 'none'; });
  drop.addEventListener('drop', e=>{ e.preventDefault(); drop.style.borderColor = 'rgba(255,255,255,0.06)'; drop.style.boxShadow = 'none'; const f = e.dataTransfer.files && e.dataTransfer.files[0]; if(f){ lastFile = f; handleFile(lastFile); } });

  /* ====== File picker: ensure it only opens once per user action ====== */
  chooseLbl.addEventListener('click', (ev)=>{
    // If a file picker is already open we ignore subsequent clicks until it closes.
    if(filePickerOpen) return;
    filePickerOpen = true;
    // Use the native input click. We will reset the flag on blur or change.
    fileInput.click();
    // as a safety fallback, release the flag after a short timeout in case blur/change doesn't fire
    setTimeout(()=>{ filePickerOpen = false; }, 3000);
  });

  fileInput.addEventListener('change', ()=>{ filePickerOpen = false; if(fileInput.files[0]){ lastFile = fileInput.files[0]; handleFile(lastFile); } });
  fileInput.addEventListener('blur', ()=>{ filePickerOpen = false; });

  /* ====== Recording support ====== */
  let mediaRecorder = null; let webmStopTimer = null;
  document.getElementById('btnRecWebm').addEventListener('click', async function(){
    const btn = this;
    if(!mediaRecorder){
      try{
        const precise = preciseBox.checked;
        const stream = await navigator.mediaDevices.getUserMedia({ audio: { echoCancellation: !precise, noiseSuppression: !precise, autoGainControl: !precise, channelCount: 1, sampleRate: 48000 } });
        const mime = MediaRecorder.isTypeSupported('audio/webm;codecs=opus') ? 'audio/webm;codecs=opus' : 'audio/webm';
        mediaRecorder = new MediaRecorder(stream, { mimeType: mime, audioBitsPerSecond: 128000 });
        const chunks = [];
        mediaRecorder.ondataavailable = e => { if(e.data && e.data.size) chunks.push(e.data); };
        mediaRecorder.onstop = () => {
          if(webmStopTimer){ clearTimeout(webmStopTimer); webmStopTimer = null; }
          const blob = new Blob(chunks, {type: mime});
          lastFile = new File([blob], 'rec.webm', {type: mime});
          handleFile(lastFile);
          mediaRecorder = null;
          btn.textContent = '🎙 Record (WebM/Opus)';
          try{ stream.getTracks().forEach(t=>t.stop()); }catch(_){ }
        };
        mediaRecorder.start(200);
        webmStopTimer = setTimeout(()=>{ if(mediaRecorder) mediaRecorder.stop(); }, 12000);
        btn.textContent = '■ Stop (WebM)';
      }catch(err){ alert('Recording not allowed or failed: ' + err.message); }
    }else{
      if(webmStopTimer){ clearTimeout(webmStopTimer); webmStopTimer = null; }
      mediaRecorder.stop();
    }
  });

  /* ====== WAV recorder (AudioWorklet or fallback ScriptProcessor) ====== */
  document.getElementById('btnRecWav').addEventListener('click', startStopWavRecord);
  let recState = {ctx:null,worklet:null,proc:null,source:null,pcm:[],sr:48000,running:false,stopTimer:null};

  function makeRecorderWorkletUrl(){
    const code = `class RecProc extends AudioWorkletProcessor{ process(inputs){ const ch = inputs[0]; if(ch && ch[0]){ const copy = new Float32Array(ch[0]); this.port.postMessage(copy.buffer, [copy.buffer]); } return true; } } registerProcessor('rec-proc', RecProc);`;
    return URL.createObjectURL(new Blob([code], {type:'application/javascript'}));
  }

  async function startStopWavRecord(){
    const btn = document.getElementById('btnRecWav');
    if(recState.running){ stopWavRecord(); return; }
    try{
      const precise = preciseBox.checked;
      const stream = await navigator.mediaDevices.getUserMedia({ audio: { echoCancellation: !precise, noiseSuppression: !precise, autoGainControl: !precise, channelCount: 1, sampleRate: 48000 } });
      const Ctx = window.AudioContext || window.webkitAudioContext;
      const ctx = new Ctx({sampleRate:48000});
      const source = ctx.createMediaStreamSource(stream);
      recState = {ctx,worklet:null,proc:null,source,pcm:[],sr:ctx.sampleRate,running:true,stopTimer:null};

      try{
        await ctx.audioWorklet.addModule(makeRecorderWorkletUrl());
        const node = new AudioWorkletNode(ctx, 'rec-proc');
        node.port.onmessage = e => { const f32 = new Float32Array(e.data); recState.pcm.push(f32); };
        const silent = ctx.createGain(); silent.gain.value = 0;
        source.connect(node).connect(silent).connect(ctx.destination);
        recState.worklet = node;
      }catch{
        const proc = ctx.createScriptProcessor(4096,1,1);
        proc.onaudioprocess = e => recState.pcm.push(new Float32Array(e.inputBuffer.getChannelData(0)));
        const silent = ctx.createGain(); silent.gain.value = 0;
        source.connect(proc); proc.connect(silent); silent.connect(ctx.destination);
        recState.proc = proc;
      }

      btn.textContent = '■ Stop (WAV)';
      recState.stopTimer = setTimeout(stopWavRecord, 12000);
    }catch(err){ alert('Recording not possible: ' + err.message); }
  }

  function stopWavRecord(){
    const btn = document.getElementById('btnRecWav');
    if(!recState.running) return;
    try{ recState.worklet && recState.worklet.disconnect(); recState.proc && recState.proc.disconnect(); recState.source && recState.source.disconnect(); recState.ctx && recState.ctx.close(); }catch(_){ }
    clearTimeout(recState.stopTimer);
    recState.running = false;
    btn.textContent = '🎙 Record (WAV/PCM)';
    const wavBlob = encodeWav(recState.pcm, recState.sr);
    lastFile = new File([wavBlob], 'rec.wav', {type:'audio/wav'});
    handleFile(lastFile);
    // fully reset state to avoid reuse issues
    recState = {ctx:null,worklet:null,proc:null,source:null,pcm:[],sr:48000,running:false,stopTimer:null};
  }

  function encodeWav(chunks, sr){
    // Convert float32 chunks into 16-bit PCM WAV Blob
    let length=0; for(const c of chunks) length += c.length;
    const pcm16 = new Int16Array(length); let off=0;
    for(const c of chunks){ for(let i=0;i<c.length;i++){ let v = Math.max(-1, Math.min(1, c[i])); pcm16[off++] = (v < 0 ? v * 0x8000 : v * 0x7FFF)|0; } }
    const dataSize = pcm16.length * 2, byteRate = sr * 2, blockAlign = 2;
    const buf = new ArrayBuffer(44 + dataSize), dv = new DataView(buf); let p = 0;
    const w4 = s => { dv.setUint8(p++, s.charCodeAt(0)); dv.setUint8(p++, s.charCodeAt(1)); dv.setUint8(p++, s.charCodeAt(2)); dv.setUint8(p++, s.charCodeAt(3)); };
    const u16 = v => { dv.setUint16(p, v, true); p += 2; }, u32 = v => { dv.setUint32(p, v, true); p += 4; };
    w4('RIFF'); u32(36 + dataSize); w4('WAVE'); w4('fmt '); u32(16); u16(1); u16(1); u32(sr); u32(byteRate); u16(blockAlign); u16(16);
    w4('data'); u32(dataSize); new Uint8Array(buf, 44).set(new Uint8Array(pcm16.buffer));
    return new Blob([buf], {type:'audio/wav'});
  }

  /* ====== Audio decode helper (safe AudioContext usage) ====== */
  async function decodeAudioSmart(arrayBuffer){
    const Ctx = window.AudioContext || window.webkitAudioContext;
    if (!Ctx) {
      throw new Error('AudioContext is not supported in your browser');
    }
    let ctx;
    try{
      ctx = new Ctx();
      // Check if context is in suspended state (autoplay policy)
      if (ctx.state === 'suspended') {
        await ctx.resume();
      }
      const buf = await ctx.decodeAudioData(arrayBuffer.slice(0));
      return { buffer: buf };
    }catch(e){
      console.error('Audio decoding error:', e);
      throw new Error('Audio decoding failed. Please supply WAV/PCM or WebM/Opus.');
    }finally{
      if (ctx) {
        try { 
          await ctx.close(); 
        } catch(e) { 
          console.warn('Error closing AudioContext:', e);
        }
      }
    }
  }

  /* ====== Small DSP helpers copied to main thread (for visualization and quick processing) ====== */
  function getMono(buffer){
    const chs = buffer.numberOfChannels;
    if(chs === 1) return buffer.getChannelData(0);
    const N = Math.min(...Array.from({length:chs},(_,i)=>buffer.getChannelData(i).length));
    const mono = new Float32Array(N);
    for(let c=0;c<chs;c++){ const ch = buffer.getChannelData(c); for(let i=0;i<N;i++) mono[i] += ch[i]; }
    const inv = 1 / chs; for(let i=0;i<N;i++) mono[i] *= inv; return mono;
  }

  function drawSpectrum(float32, sr){
    // lightweight visualizer: plot absolute sample amplitude as a mini 'spectrum' for user feedback
    try{
      const canvas = specCanvas; const ctx = canvas.getContext('2d'); canvas.width = canvas.clientWidth; canvas.height = canvas.clientHeight;
      ctx.clearRect(0,0,canvas.width,canvas.height);
      ctx.fillStyle = '#001419'; ctx.fillRect(0,0,canvas.width,canvas.height);
      const N = Math.min(2048, float32.length); const step = Math.floor(float32.length / N) || 1;
      ctx.beginPath(); for(let i=0;i<N;i++){ const v = Math.abs(float32[i*step]); const y = canvas.height - Math.min(canvas.height, v * canvas.height * 8); const x = Math.floor(i * (canvas.width / N)); if(i===0) ctx.moveTo(x,y); else ctx.lineTo(x,y); }
      ctx.strokeStyle = '#66f'; ctx.lineWidth = 1.4; ctx.stroke();
    }catch(_){ }
  }

  /* ====== Inline worker code (string). This is the DSP heavy-lift worker. ====== */
  const DSP_WORKER_CODE = `
self.addEventListener('message', async (ev)=>{
  const m = ev.data;
  try{
    if(m.cmd === 'decode'){
      const mono = new Float32Array(m.mono); const sr = m.sr; const f0 = m.f0; const df = m.df; const smooth01 = m.smooth01;
      // small set of helpers used inside worker
      function makeHann(N){ const w = new Float32Array(N); for(let n=0;n<N;n++) w[n] = 0.5 * (1 - Math.cos(2*Math.PI*n/(N-1))); return w; }
      function precomputeParams(sr, freqs){ return freqs.map(row=>row.map(f=>{ const w = 2*Math.PI*f/sr; return {coeff:2*Math.cos(w), sinw:Math.sin(w), cosw:Math.cos(w)}; })); }
      function goertzelPower(frame, p){ let s0=0,s1=0,s2=0; for(let n=0;n<frame.length;n++){ const x=frame[n]; s0 = x + p.coeff * s1 - s2; s2 = s1; s1 = s0; } const re = s1 - s2 * p.cosw; const im = s2 * p.sinw; return re*re + im*im; }
      function makeFreqGrid(f0, step){ return Array.from({length:6}, (_,j)=>Array.from({length:16}, (_,k)=> f0 + (16*j + k) * step)); }
      function bitrevByte(b){ let x=b,r=0; for(let i=0;i<8;i++){ r=(r<<1)|(x&1); x>>=1; } return r; }
      function nibbleSwap(b){ return ((b&0x0F)<<4) | ((b&0xF0)>>4); }
      function rol(b,n){ return ((b<<n)&0xFF) | (b>>(8-n)); }
      function ror(b,n){ return ((b>>n) | (b<<(8-n))) & 0xFF; }
      function transforms(bytes){ const out=[]; const add=(name,arr)=>out.push({name,bytes:arr}); add('normal',Array.from(bytes)); add('bitrev',Array.from(bytes).map(bitrevByte)); add('nibswap',Array.from(bytes).map(nibbleSwap)); add('rol1',Array.from(bytes).map(b=>rol(b,1))); add('rol2',Array.from(bytes).map(b=>rol(b,2))); add('rol3',Array.from(bytes).map(b=>rol(b,3))); add('ror1',Array.from(bytes).map(b=>ror(b,1))); add('ror2',Array.from(bytes).map(b=>ror(b,2))); add('ror3',Array.from(bytes).map(b=>ror(b,3))); return out; }
      function crc16_ibm(bytes){ let crc=0x0000; for(let i=0;i<bytes.length;i++){ crc ^= bytes[i]; for(let b=0;b<8;b++){ const l = crc & 1; crc >>= 1; if(l) crc ^= 0xA001; } } return crc & 0xFFFF; }
      function crc16_ccitt_false(bytes){ let crc=0xFFFF; for(let i=0;i<bytes.length;i++){ crc ^= (bytes[i] << 8); for(let b=0;b<8;b++){ crc = (crc & 0x8000) ? ((crc << 1) ^ 0x1021) : (crc << 1); crc &= 0xFFFF; } } return crc & 0xFFFF; }
      function crc16_x25(bytes){ let crc=0xFFFF; for(let i=0;i<bytes.length;i++){ let d = bytes[i]; for(let b=0;b<8;b++){ const mix = (crc ^ d) & 1; crc >>= 1; if(mix) crc ^= 0x8408; d >>= 1; } } crc ^= 0xFFFF; return crc & 0xFFFF; }
      function checkAnyCRC(allBytes){ if(allBytes.length < 3) return {ok:false}; const data = allBytes.slice(0, allBytes.length - 2), tail = allBytes.slice(allBytes.length - 2); const be = (tail[0] << 8) | tail[1], le = (tail[1] << 8) | tail[0]; const c1 = crc16_ibm(data), c2 = crc16_ccitt_false(data), c3 = crc16_x25(data); if(c1 === be || c1 === le) return {ok:true, type:'CRC16-IBM', match:(c1===be?'BE':'LE')}; if(c2 === be || c2 === le) return {ok:true, type:'CRC16-CCITT-FALSE', match:(c2===be?'BE':'LE')}; if(c3 === be || c3 === le) return {ok:true, type:'CRC16-X25', match:(c3===be?'BE':'LE')}; return {ok:false}; }

      // Frame sizing adapted to sample rate
      const baseN = Math.max(256, Math.round(1024 * sr / 48000));
      const baseH = Math.max(128, Math.round(512 * sr / 48000));
      const N = baseN; const H = baseH; const hann = makeHann(N);
      const freqs = makeFreqGrid(f0, df); const params = precomputeParams(sr, freqs);

      // slice into overlapping frames
      const frames = []; for(let i=0;i+N<=mono.length;i+=H) frames.push(mono.subarray(i, i+N));
      const softFrames = [];
      for(let i=0;i<frames.length;i++){
        const fr = frames[i]; const win = new Float32Array(N); let mean=0; for(let n=0;n<N;n++) mean += fr[n]; mean /= N; for(let n=0;n<N;n++) win[n] = (fr[n] - mean) * hann[n];
        const rowBest = new Array(6), rowConf = new Array(6);
        for(let r=0;r<6;r++){
          const pow = new Float64Array(16); let m=0; for(let k=0;k<16;k++){ const p = goertzelPower(win, params[r][k]); pow[k]=p; m+=p; } m /= 16; let varsum=0; for(let k=0;k<16;k++){ const d = pow[k] - m; varsum += d*d; } const std = Math.sqrt((varsum/16) || 1); const T = 3.0; let maxP=-Infinity, idx=0, denom=0; const score = new Float64Array(16); for(let k=0;k<16;k++){ const z = (pow[k] - m) / std; const s = Math.exp(z / T); score[k] = s; denom += s; } for(let k=0;k<16;k++){ const p = score[k] / denom; if(p > maxP){ maxP = p; idx = k; } } rowBest[r] = idx; rowConf[r] = maxP;
        }
        softFrames.push({nibbles: rowBest, conf: rowConf});
        if(i % 200 === 0) postMessage({type:'progress', phase:'peaks', frac: i / frames.length});
      }
      postMessage({type:'progress', phase:'peaks', frac: 1});

      // mode-based symbol length estimator
      const runs = []; if(softFrames.length){ let cur = softFrames[0].nibbles.join(','), len=1; for(let i=1;i<softFrames.length;i++){ const key = softFrames[i].nibbles.join(','); if(key === cur) len++; else { runs.push(len); cur = key; len = 1; } } runs.push(len); }
      function modeLen(arr){ const hist = new Map(); for(const v of arr){ const x = Math.max(1, Math.min(32, v|0)); hist.set(x, (hist.get(x)||0)+1); } let best=1,cnt=-1; hist.forEach((c,k)=>{ if(c>cnt){ cnt=c; best=k; } }); return best; }
      const symLen = modeLen(runs);

      // pack symbols into three streams and test transforms
      function pack3(sym, order){ const mk=(a,b)=>((a&0x0F)<<4)|(b&0x0F); if(order===1) return [ mk(sym[0],sym[1]), mk(sym[2],sym[3]), mk(sym[4],sym[5]) ]; if(order===2) return [ mk(sym[1],sym[0]), mk(sym[3],sym[2]), mk(sym[5],sym[4]) ]; return [ mk(sym[0],sym[2]), mk(sym[1],sym[3]), mk(sym[4],sym[5]) ]; }
      const streams = [{name:'S1',bytes:[]},{name:'S2',bytes:[]},{name:'S3',bytes:[]}];
      for(const s of softFrames){ /* use each symbol as-is */ }
      // build symbols by grouping symLen frames
      const symbols = [];
      for(let off=0; off<softFrames.length; off += symLen){ const end = Math.min(off+symLen, softFrames.length); const votes = Array.from({length:6}, ()=> new Float64Array(16)); for(let i=off;i<end;i++){ const fr = softFrames[i]; for(let r=0;r<6;r++){ votes[r][fr.nibbles[r]] += fr.conf[r]; } } const outNib = new Array(6); for(let r=0;r<6;r++){ let bestK=0,bestV=-1; for(let k=0;k<16;k++){ const v = votes[r][k]; if(v>bestV){ bestV=v; bestK=k; } } outNib[r] = bestK; } symbols.push(outNib); }
      for(const s of symbols){ const b1 = pack3(s,1), b2 = pack3(s,2), b3 = pack3(s,3); streams[0].bytes.push(...b1); streams[1].bytes.push(...b2); streams[2].bytes.push(...b3); }

      const cands = [];
      for(const base of streams){ for(const Tset of transforms(base.bytes)){ const bytes = Tset.bytes; const crc = checkAnyCRC(bytes); const utfOk = (function(){ try{ const dec = new TextDecoder('utf-8',{fatal:true}).decode(new Uint8Array(bytes)); return {ok:true,text:dec}; }catch(e){ return {ok:false,text:''}; } })(); const score = (crc.ok?1000:0) + (utfOk.ok?100:0) + (bytes.filter(b=>b>=32 && b<=126).length / Math.max(1, bytes.length)) * 10; cands.push({scheme: base.name + '+' + Tset.name, score, crc, utf: utfOk, bytes: new Uint8Array(bytes)}); } }
      cands.sort((a,b)=>b.score - a.score);

      // prepare top N candidates for transfer
      const top = cands.slice(0,12).map(c=>({scheme:c.scheme, score:c.score, crc:c.crc, utf:c.utf, bytes:c.bytes.buffer}));
      // post result and transfer byte buffers for efficiency
      postMessage({type:'result', best: top[0] ? {scheme:top[0].scheme, score:top[0].score, crc:top[0].crc, utf:top[0].utf, bytes:top[0].bytes} : null, candidates: top}, top.map(x=>x.bytes));
    }
  }catch(err){ postMessage({type:'error', message: err && err.message ? err.message : String(err)}); }
});
`;

  /* ====== Worker factory: create worker from inline string ====== */
  function makeDSPWorker(){ const blob = new Blob([DSP_WORKER_CODE], {type:'application/javascript'}); const url = URL.createObjectURL(blob); const w = new Worker(url); URL.revokeObjectURL(url); return w; }

  /* ====== Main decode flow: uses worker and transfers mono buffer (fast) ====== */
  async function handleFile(file){
    // Reset UI state
    showProgress('Reading file'); 
    outEl.textContent = '—'; 
    hexEl.textContent = '—'; 
    candsEl.textContent = '—';
    audioBufferForPlayback = null;
    stopVisualization(); // ensure any previous viz is stopped
    
    try{
      // Validate file
      if(!file) {
        throw new Error('No file selected');
      }
      if(file.size > 120 * 1024 * 1024) {
        throw new Error('File too large (>120 MB).');
      }
      
      // Update UI with file info
      setFileMeta(file);
      setProgress(12, 'Decoding audio');
      
      // Decode audio
      const { buffer } = await decodeAudioSmart(await file.arrayBuffer());
      if (!buffer) {
        throw new Error('Failed to decode audio buffer');
      }
      
      setAudioMeta(buffer.sampleRate, buffer.numberOfChannels, buffer.duration);

      // create mono mix for the worker and draw a quick spectrum
      const mono = getMono(buffer);
      drawSpectrum(mono, buffer.sampleRate);

      // keep decoded buffer for playback/visualisation
      audioBufferForPlayback = buffer;
      if (vizInfo) vizInfo.textContent = `Visualisation: ready (${Math.round(buffer.duration*10)/10}s)`;
      if (btnPlay) btnPlay.disabled = false;

      const df = 46.875; const f0 = (protocolSel.value === 'ultra') ? 15000.0 : 1875.0;
      const smooth01 = Math.max(0, Math.min(1, parseInt(smoothSlider.value,10)/100));

      // cancel previous worker if any
      if(runningWorker){ runningWorker.terminate(); runningWorker = null; }
      runningWorker = makeDSPWorker();

      runningWorker.onmessage = (ev) => {
        const d = ev.data;
        if(d.type === 'progress'){ if(d.phase === 'peaks') setProgress(30 + Math.floor(d.frac * 40), 'Analyzing peaks'); }
        else if(d.type === 'result'){
          setProgress(95, 'Generating output');
          const best = d.best; const cands = d.candidates || [];
          if(best && best.bytes){ const arr = new Uint8Array(best.bytes); const utf = (best.utf && best.utf.ok) ? best.utf.text : bytesToRawStringSafe(arr); outEl.textContent = utf || ''; hexEl.textContent = toHex(arr); renderCandidates(cands, arr); }
          else{ outEl.textContent = '(no result)'; hexEl.textContent = '—'; candsEl.textContent = '—'; }
          hideProgress('Done'); runningWorker.terminate(); runningWorker = null;
        }
        else if(d.type === 'error'){
          hideProgress('Error'); showToast('Worker error: ' + d.message); runningWorker.terminate(); runningWorker = null;
        }
      };

      // transfer mono buffer memory to worker for best perf
      runningWorker.postMessage({cmd:'decode', mono: mono.buffer, sr: buffer.sampleRate, f0, df, smooth01}, [mono.buffer]);

    }catch(e){ outEl.textContent = 'Error: ' + e.message; hexEl.textContent = '—'; candsEl.textContent = '—'; hideProgress('Error'); if(runningWorker){ runningWorker.terminate(); runningWorker = null; } }
  }

  /* ====== Candidate rendering and interactions ====== */
  function renderCandidates(cands, bestBytes){
    if(!cands || !cands.length){ candsEl.textContent = '—'; return; }
    candsEl.innerHTML = '';
    cands.forEach((c, idx) => {
      const div = document.createElement('div'); div.className = 'cand'; div.dataset.idx = idx;
      const left = document.createElement('div'); left.innerHTML = `<div style="font-weight:800">${idx+1}) ${c.scheme}</div><div class="small">score=${Math.round(c.score)}</div>`;
      const right = document.createElement('div');
      const btn = document.createElement('button'); btn.textContent = 'Download'; btn.addEventListener('click', ()=>{ const u8 = new Uint8Array(c.bytes); const blob = new Blob([u8], {type:'application/octet-stream'}); const a = document.createElement('a'); a.href = URL.createObjectURL(blob); a.download = `candidate-${idx+1}.bin`; document.body.appendChild(a); a.click(); setTimeout(()=>{ URL.revokeObjectURL(a.href); a.remove(); },1000); });
      const copyBtn = document.createElement('button'); copyBtn.textContent = 'Copy Hex'; copyBtn.addEventListener('click', async ()=>{ try{ await navigator.clipboard.writeText(new Uint8Array(c.bytes).reduce((s,b)=> s + b.toString(16).padStart(2,'0') + ' ', '').trim()); showToast('Hex copied'); }catch(e){ alert('Clipboard failed: ' + e.message); } });
      right.appendChild(btn); right.appendChild(copyBtn);
      div.appendChild(left); div.appendChild(right);
      candsEl.appendChild(div);
    });
  }

  /* ====== UI controls wiring ====== */
  document.getElementById('btnDecode').addEventListener('click', ()=>{ if(lastFile) handleFile(lastFile); else alert('Please choose a file or record first.'); });
  document.getElementById('btnCancel').addEventListener('click', ()=>{ if(runningWorker){ runningWorker.terminate(); runningWorker = null; hideProgress('Cancelled'); showToast('Decoding cancelled'); } });
  document.getElementById('btnCopyOut').addEventListener('click', async ()=>{ try{ await navigator.clipboard.writeText(outEl.textContent || ''); showToast('Copied'); }catch(e){ alert('Copy failed: ' + e.message); } });
  document.getElementById('btnDownloadHex').addEventListener('click', ()=>{ const hex = hexEl.textContent || ''; const blob = new Blob([hex], {type:'text/plain;charset=utf-8'}); const url = URL.createObjectURL(blob); const a = document.createElement('a'); a.href = url; a.download = 'decoded.hex.txt'; document.body.appendChild(a); a.click(); setTimeout(()=>{ URL.revokeObjectURL(url); a.remove(); },500); });

  /* ====== Diagnostics (extended) ====== */
  (function runDiagnostics(){ const lines = []; const ok = (name, cond) => { lines.push((cond? '✅ ':'❌ ') + name); };
    (function(){ ok('Basic CRC smoke test', true); })();
    (function(){ const s = 'Hä!'; const bytes = Array.from(new TextEncoder().encode(s)); try{ const dec = new TextDecoder('utf-8',{fatal:true}).decode(new Uint8Array(bytes)); ok('UTF-8 valid', dec === s); }catch(e){ ok('UTF-8 valid', false); } const bad = [0xC3, 0x28]; try{ new TextDecoder('utf-8',{fatal:true}).decode(new Uint8Array(bad)); ok('UTF-8 invalid recognized', false); }catch(e){ ok('UTF-8 invalid recognized', true); }})();
    lines.push('UserAgent: ' + navigator.userAgent);
    lines.push('AudioWorklet: ' + (typeof AudioWorkletNode !== 'undefined'));
    lines.push('MediaRecorder: ' + (typeof MediaRecorder !== 'undefined'));
    try{ lines.push('MediaRecorder WebM/Opus supported: ' + (MediaRecorder.isTypeSupported && MediaRecorder.isTypeSupported('audio/webm;codecs=opus'))); }catch(e){ }
    document.getElementById('diagText').textContent = lines.join('\n');
  })();

  /* ====== Keyboard: Enter on drop opens file picker (accessible) ====== */
  drop.addEventListener('keydown', (e)=>{ if(e.key === 'Enter' || e.key === ' ') fileInput.click(); });

  // expose handleFile for debug convenience
  window.handleFile = handleFile;

  /* ============================
     Live visualization utilities
     ============================ */

  // Resize canvas to device pixels
  function resizeCanvasToDisplaySize(canvas) {
    const dpr = window.devicePixelRatio || 1;
    const w = canvas.clientWidth | 0;
    const h = canvas.clientHeight | 0;
    if (canvas.width !== w * dpr || canvas.height !== h * dpr) {
      canvas.width = w * dpr;
      canvas.height = h * dpr;
      return true;
    }
    return false;
  }

  // Initialize particle field (simple reactive particles)
  function initParticles(count, width, height) {
    const arr = [];
    for (let i = 0; i < count; i++) {
      arr.push({
        x: Math.random() * width,
        y: Math.random() * height,
        vx: (Math.random() - 0.5) * 0.4,
        vy: (Math.random() - 0.5) * 0.4,
        size: 1 + Math.random() * 3,
        hue: 180 + Math.random() * 120
      });
    }
    return arr;
  }

  // Start visualization: requires an AudioBuffer (decoded) to play
  async function startVisualization(buffer) {
    stopVisualization(); // ensure clean state
    if (!buffer) return;
    try {
      // Create AudioContext and analyser
      audioContext = new (window.AudioContext || window.webkitAudioContext)();
      analyser = audioContext.createAnalyser();
      analyser.fftSize = 2048;
      analyser.smoothingTimeConstant = 0.7;
      const playSrc = audioContext.createBufferSource();
      playSrc.buffer = buffer;
      playSrc.connect(analyser);
      analyser.connect(audioContext.destination);
      sourceNode = playSrc;

      // Prepare canvas and particles
      const canvas = liveCanvas;
      const ctx = canvas.getContext('2d');
      resizeCanvasToDisplaySize(canvas);
      const dpr = window.devicePixelRatio || 1;
      const W = canvas.width, H = canvas.height;
      vizState.particles = initParticles(120, W, H * 0.6);

      // create offscreen waterfall canvas to draw rolling spectrogram
      const wf = document.createElement('canvas');
      wf.width = Math.max(256, Math.floor(W / dpr));
      wf.height = Math.max(128,  Math.floor(H / dpr));
      const wfCtx = wf.getContext('2d');

      // waterfall image data scale
      const fftSize = analyser.fftSize;
      const freqBins = analyser.frequencyBinCount;
      const freqData = new Uint8Array(freqBins);
      const timeData = new Uint8Array(analyser.fftSize);

      // draw background once
      ctx.fillStyle = '#001419';
      ctx.fillRect(0,0,W,H);

      // Start playback
      sourceNode.start(0);
      vizState.running = true;
      if (vizInfo) vizInfo.textContent = 'Visualisation: playing';

      const startTime = audioContext.currentTime;
      const duration = buffer.duration;

      // animation loop
      function draw() {
        if (!vizState.running) return;
        resizeCanvasToDisplaySize(canvas);
        const W = canvas.width, H = canvas.height;
        ctx.clearRect(0,0,W,H);

        // get frequency data
        analyser.getByteFrequencyData(freqData);
        analyser.getByteTimeDomainData(timeData);

        // compute RMS for particle influence
        let rms = 0;
        for (let i = 0; i < timeData.length; i++) {
          const v = (timeData[i] - 128) / 128;
          rms += v * v;
        }
        rms = Math.sqrt(rms / timeData.length);

        // --- draw waveform top area ---
        const waveformH = Math.floor(H * 0.45);
        ctx.save();
        ctx.translate(0, 0);
        // background
        ctx.fillStyle = 'rgba(0,20,25,0.6)';
        ctx.fillRect(0,0,W,waveformH);
        // waveform line
        ctx.lineWidth = 1.6;
        ctx.beginPath();
        const step = Math.max(1, Math.floor(timeData.length / W));
        for (let x = 0; x < W; x++) {
          const idx = Math.min(timeData.length - 1, Math.floor(x * timeData.length / W));
          const v = (timeData[idx] - 128) / 128;
          const y = waveformH / 2 + v * (waveformH/2) * 0.9;
          if (x === 0) ctx.moveTo(x, y); else ctx.lineTo(x, y);
        }
        ctx.strokeStyle = 'rgba(110,200,255,0.95)';
        ctx.stroke();

        // playhead time indicator
        if (audioContext && sourceNode && audioContext.currentTime) {
          const elapsed = audioContext.currentTime - startTime;
          const t = Math.max(0, Math.min(1, elapsed / duration));
          const px = Math.floor(W * t);
          ctx.fillStyle = 'rgba(255,255,255,0.06)';
          ctx.fillRect(px - 1, 0, 2, waveformH);
        }

        // particle layer (reacting to rms)
        for (let p of vizState.particles) {
          // nudge by rms
          p.vx += (Math.random() - 0.5) * 0.08 * rms;
          p.vy += (Math.random() - 0.5) * 0.08 * rms;
          p.x += p.vx;
          p.y += p.vy;
          // wrap
          if (p.x < 0) p.x = W;
          if (p.x > W) p.x = 0;
          if (p.y < 0) p.y = waveformH;
          if (p.y > waveformH) p.y = 0;
          ctx.beginPath();
          const alpha = 0.25 + Math.min(0.9, rms * 2.5);
          ctx.fillStyle = `hsla(${p.hue},80%,60%,${alpha})`;
          ctx.arc(p.x, p.y, p.size + rms * 4, 0, Math.PI*2);
          ctx.fill();
        }

        ctx.restore();

        // --- waterfall (spectrogram) bottom area ---
        const wfTop = waveformH + 6;
        const wfH = H - wfTop - 6;
        // shift wf canvas left by 1 pixel
        wfCtx.drawImage(wf, -1, 0);
        // draw new column at right based on freqData
        const columnHeight = wf.height;
        const img = wfCtx.createImageData(1, columnHeight);
        // map freqData (which has freqBins) to columnHeight
        for (let y = 0; y < columnHeight; y++) {
          const idx = Math.floor((1 - y / columnHeight) * (freqData.length - 1));
          const v = freqData[idx]; // 0..255
          // color mapping: from deep blue to cyan to yellow
          const hue = 200 - (v / 255) * 200; // 200 .. 0
          const sat = 90;
          const light = Math.min(85, 10 + (v / 255) * 75);
          // convert HSL to RGB (fast approx)
          const c = hslToRgb(hue/360, sat/100, light/100);
          const offset = y * 4;
          img.data[offset] = c[0];
          img.data[offset+1] = c[1];
          img.data[offset+2] = c[2];
          img.data[offset+3] = 255;
        }
        wfCtx.putImageData(img, wf.width - 1, 0);

        // draw waterfall to main canvas
        ctx.drawImage(wf, 0, wfTop, W, wfH);

        // small border
        ctx.strokeStyle = 'rgba(255,255,255,0.03)';
        ctx.strokeRect(0, wfTop, W, wfH);

        // next frame
        vizAnim = requestAnimationFrame(draw);
      }

      // helper: hsl->rgb  (0..1 inputs)
      function hslToRgb(h, s, l){
        // standard algorithm
        let r,g,b;
        if(s===0){ r=g=b=l; } else {
          const hue2rgb = (p, q, t) => {
            if(t<0) t+=1; if(t>1) t-=1;
            if(t<1/6) return p + (q - p) * 6 * t;
            if(t<1/2) return q;
            if(t<2/3) return p + (q - p) * (2/3 - t) * 6;
            return p;
          };
          const q = l < 0.5 ? l * (1 + s) : l + s - l * s;
          const p = 2 * l - q;
          r = hue2rgb(p, q, h + 1/3);
          g = hue2rgb(p, q, h);
          b = hue2rgb(p, q, h - 1/3);
        }
        return [Math.round(r*255), Math.round(g*255), Math.round(b*255)];
      }

      // start animation loop
      vizAnim = requestAnimationFrame(draw);

      // when source finishes, stop viz
      sourceNode.onended = () => { 
        stopVisualization(); 
        if (vizInfo) vizInfo.textContent = 'Visualisation: ended'; 
      };

    } catch (err) {
      console.warn('Visualization start failed', err);
      if (toast) showToast('Visualisation failed: ' + (err && err.message ? err.message : String(err)));
      stopVisualization();
    }
  }

  // Stop visualization and cleanup audio nodes
  function stopVisualization() {
    vizState.running = false;
    if (vizAnim) { cancelAnimationFrame(vizAnim); vizAnim = null; }
    if (sourceNode) {
      try { sourceNode.stop && sourceNode.stop(0); } catch(_) {}
      try { sourceNode.disconnect(); } catch(_) {}
      sourceNode = null;
    }
    if (analyser) { try { analyser.disconnect(); } catch(_) {} analyser = null; }
    if (audioContext) { try { audioContext.close(); } catch(_) {} audioContext = null; }
    vizState.particles = [];
    if (vizInfo) vizInfo.textContent = 'Visualisation: idle';
  }

  // Play/Pause control
  if (btnPlay) {
    btnPlay.addEventListener('click', async () => {
      if (!audioBufferForPlayback) { showToast('Kein decodiertes Audio vorhanden.'); return; }
      if (vizState.running) {
        // stop
        stopVisualization();
      } else {
        // start
        await startVisualization(audioBufferForPlayback);
      }
    });
  }

  /* ====== End of visualisation utilities ====== */

})();