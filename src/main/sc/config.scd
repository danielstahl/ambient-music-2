
(
~customSynthDir = "/Users/danielstahl/Documents/Projects/soundmining-modular/src/main/sc/synths";
~irDir = PathName.new("/Users/danielstahl/Documents/Music/Pieces/Concrete Music/Concrete Music 8/impulse-response/");

~fftsize = 2048;
~bootFunction = {

	s.loadDirectory(~customSynthDir);
	~irDir.entries.do({
		arg path;
		~loadIr.value(path);
	});
	'Supercollider loaded'.postln;
};

~simpleBootFunction = {
	'Supercollider loaded'.postln;
};

~midiBootFunction = {
	var on, off, cc, bend, midiReciever;

	MIDIClient.init;
	MIDIIn.connectAll;
	if(MIDIClient.sources.size > 0,
	{
		'Setting up midi'.postln;
		midiReciever = NetAddr("localhost", 57111);

		MIDIdef.noteOn(\noteOn, {
			arg veloc, num, chan, src;
			midiReciever.sendMsg("/noteOn", num, veloc);
		});

		MIDIdef.noteOff(\noteOff, {
			arg veloc, num, chan, src;
			midiReciever.sendMsg("/noteOff", num, veloc);
		});

		MIDIdef.cc(\cc, {
			// 1 = Modulation wheel
			arg value, control, chan;
			midiReciever.sendMsg("/cc", value, control);
		});

		MIDIdef.bend(\bend, {
			arg value, chan;
			midiReciever.sendMsg("/bend", value);
		});
	},
	{
		'No midi found'.postln;
	})
};

~loadIr = {
	arg irPath;
	var rawIrBufL, rawIrBufR, bufSizeL, bufSizeR, irSpectrumBufL, irSpectrumBufR;
	rawIrBufL = Buffer.readChannel(s, irPath.fullPath, channels: [0]);
	rawIrBufR = Buffer.readChannel(s, irPath.fullPath, channels: [1]);
	s.sync;
	bufSizeL = PartConv.calcBufSize(~fftsize, rawIrBufL);
	bufSizeR = PartConv.calcBufSize(~fftsize, rawIrBufR);
	irSpectrumBufL = Buffer.alloc(s, bufSizeL, 1);
	irSpectrumBufR = Buffer.alloc(s, bufSizeR, 1);
	irSpectrumBufL.preparePartConv(rawIrBufL, ~fftsize);
	irSpectrumBufR.preparePartConv(rawIrBufR, ~fftsize);
	s.sync;
	rawIrBufL.free;
	rawIrBufR.free;
	(irPath.fileName + ' left ' + irSpectrumBufL.bufnum).postln;
	(irPath.fileName + ' right ' + irSpectrumBufR.bufnum).postln;
};

~setupConfig = {
	arg outDevice, numOutputBusChannels;
	s = Server.local;
	s.options.outDevice = outDevice;
	s.options.numOutputBusChannels = numOutputBusChannels;
	s.options.blockSize = 128;
	s.options.sampleRate = 96000;
	s.options.numBuffers = 2056;
	s.options.memSize = 2.pow(20);
	s.dumpOSC(1);
	s.doWhenBooted({
		~midiBootFunction.value;
	});
};
)


// Ordinary config
(
~setupConfig.value("External Headphones", 2);
)
(
~setupConfig.value("Scarlett 2i2 USB", 2);
)
// Mix config
(
~setupConfig.value("64ch Aggregate", 64);
)
(
~setupConfig.value("64ch Scarlett Aggregate", 64);
)

