#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <elf.h>

int main(int argc, char **argv) {
    if (argc != 3) {
        fprintf(stderr, "Usage: %s <elf-file> <output-file>\n", argv[0]);
        return 1;
    }
    
    FILE *fp = fopen(argv[1], "rb");
    if (!fp) {
        perror("fopen");
        return 1;
    }
    
    // Read ELF header
    Elf64_Ehdr ehdr;
    fread(&ehdr, sizeof(ehdr), 1, fp);
    
    // Read section headers
    fseek(fp, ehdr.e_shoff, SEEK_SET);
    Elf64_Shdr *shdrs = malloc(ehdr.e_shnum * sizeof(Elf64_Shdr));
    fread(shdrs, sizeof(Elf64_Shdr), ehdr.e_shnum, fp);
    
    // Get section names
    char *shstrtab = malloc(shdrs[ehdr.e_shstrndx].sh_size);
    fseek(fp, shdrs[ehdr.e_shstrndx].sh_offset, SEEK_SET);
    fread(shstrtab, shdrs[ehdr.e_shstrndx].sh_size, 1, fp);
    
    // Find .text section
    for (int i = 0; i < ehdr.e_shnum; i++) {
        char *name = shstrtab + shdrs[i].sh_name;
        if (strcmp(name, ".text") == 0) {
            printf("Found .text section: size=%ld offset=%ld\n", 
                   shdrs[i].sh_size, shdrs[i].sh_offset);
            
            // Extract the section
            void *data = malloc(shdrs[i].sh_size);
            fseek(fp, shdrs[i].sh_offset, SEEK_SET);
            fread(data, shdrs[i].sh_size, 1, fp);
            
            // Write to output
            FILE *out = fopen(argv[2], "wb");
            fwrite(data, shdrs[i].sh_size, 1, out);
            fclose(out);
            
            printf("Extracted %ld bytes to %s\n", shdrs[i].sh_size, argv[2]);
            free(data);
            break;
        }
    }
    
    free(shstrtab);
    free(shdrs);
    fclose(fp);
    return 0;
}