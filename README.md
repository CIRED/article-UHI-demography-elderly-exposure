# Code used to make the graphs in the article by Corneille et al.

2 types of data are needed to run the code:

1. Climate simulations conducted with CNRM-AROME come from a previously-published source ([Corneille et al., 2026](https://www.sciencedirect.com/science/article/abs/pii/S2212095526002440)). These are available in their interpolated form (at the municipality-scale) in a [Zenodo reposotory](https://doi.org/10.5281/zenodo.23037957). 
2. Demographic data at the municipality scale from INSEE in 1990 and 2018. they can be accessed online: [here for 1990](https://www.insee.fr/fr/statistiques/1893204) and [here for 2018](https://www.insee.fr/fr/statistiques/5650720). 

---

The zip file `tn_COM_NFR010C_2018-2022.zip` from the Zenodo repository should be uncompressed in a folder called `NFR010C`, itself in a folder called `data`. The zip file `tn_COM_NFR010D_2018-2022.zip` from the Zenodo repository should be similarly uncompressed in a folder called `NFR010d`, in the same folder `data`.

The Excel data from INSEE should be put int the folder `data`, and be renamed:
- `pop_sexe_age_communes_1990.xlsx`
- `pop_sexe_age_communes_2018.xlsx`


