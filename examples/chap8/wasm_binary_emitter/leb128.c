
#define CONTINUATION_BIT  0x80 
#define SIGN_BIT  0x40

unsigned char low_bits_of_byte(unsigned char byte){
    return byte & 0x7f;
}

unsigned char low_bits_of_word32(unsigned int val){
    unsigned int byte = val &0xff ;
    return low_bits_of_byte(byte) ;
}

/*
    Unsigned LEB128でエンコードしbufferに書き込む
*/
extern void dump_word32(unsigned int word32,unsigned char*buffer){
    
    for(int offset=0;offset<5 ;offset++){
        unsigned char byte = low_bits_of_word32(word32);
        word32>>=7;
        if (word32 != 0){
            byte |= CONTINUATION_BIT;
        }
        buffer[offset]=byte;

        /*エンコード終了*/
        if (word32==0){
            break;
        }
    }
}